#' Batch process Beckman Coulter LS13 320 Grainsizer txt files
#'
#' @param beckman_gs_txt_files List or vector of txt files that should be parsed
#' @param calculate_depths Logical. Should Depths be calculated from the SampleID field? Defaults to `TRUE`
#'
#' @return A list of 4 dataframes for Parameters, Statistics, Interpolations and Channel data respectively, reflecting the structure of our txt export files that have 4 sections. The dataframes are rowbound for each sample/file.
#' @export
#'
batch_process_beckman_grainsizer <-
  function(beckman_gs_txt_files, calculate_depths = TRUE) {
    gs_data <- purrr::map(
      beckman_gs_txt_files,
      purrr::possibly(
        parse_beckman_grainsizer_txt,
        otherwise = NULL,
        quiet = TRUE
      ),
      calculate_depths
    )
    gs_data <- purrr::transpose(gs_data)
    gs_data <- purrr::map(gs_data, dplyr::bind_rows)
  }

#' Create a human readable table of grainsize categories
#'
#' This function creates a human readable table of grainsize categories, differentiating between clay, silt sand.
#'
#' @param gsdata Expects the output of `batch_process_beckman_grainsizer` as argument.
#'
#' @return Returns a human-readable dataframe/tibble that can be used to write to file
#' @export
#'
make_grainsize_category_table <- function(gsdata = NULL) {
  if (is.null(gsdata))
    stop("Please provide a grainsize data list")
  stopifnot("Error: Unexpected grainsize data list structure" = length(gsdata) == 4L)

  interpolations <- gsdata[[3]]

  catstring <-
    c("", "clay", "silt", "", "", "", "sand", "", "", "", "other")
  qualstring <-
    c(
      "",
      "",
      "very fine",
      "fine",
      "medium",
      "coarse",
      "very fine",
      "fine",
      "medium",
      "coarse",
      ""
    )
  quantstring <-
    c(
      "Particle Diameter (um)",
      "0-4 um",
      "4-8 um",
      "8-16 um",
      "16-31 um",
      "31-62.5 um",
      "62.5-125 um",
      "125-250 um",
      "250-500 um",
      "500-1000 um",
      "1000-2000 um"
    )
  header <-
    c(
      "Sample",
      "Vol %",
      "Vol %",
      "Vol %",
      "Vol %",
      "Vol %",
      "Vol %",
      "Vol %",
      "Vol %",
      "Vol %",
      "Vol %"
    )
  # now we want a wide format table again, i.e. with the different sizes as fields/columns, not in a row
  table_data <- dplyr::select(interpolations, .data$UID, .data$Di, .data$volp)
  table_data <- tidyr::spread(table_data, "Di", "volp")
  table_data <-
    dplyr::bind_rows(catstring, qualstring, quantstring, header, table_data)

  table_data
}

#' Parse a Beckman Coulter LS13 320 Grainsizer txt file
#'
#' This function parses the export files produced by the LS13 320 software in conjunction with the SOP used by the SURF Sedi Group at Eawag. It is most likely specific to the settings chosen for our group but can be adapted to other settings.
#'
#' This function is not exported. Use `batch_process_beckman_grainsizer` instead.
#'
#' @param beckman_gs_txt_path Path to the export txt file
#' @param calculate_depths Logical. Should Depths be calculated from the SampleID field? Defaults to `TRUE`
#'
#' @return A list of 4 dataframes for Parameters, Statistics, Interpolations and Channel data respectively, reflecting the structure of our txt export files that have 4 sections.
#'
parse_beckman_grainsizer_txt <-
  function(beckman_gs_txt_path, calculate_depths = TRUE) {
    gs_txt_filename <- basename(beckman_gs_txt_path)
    safe_validate <- purrr::safely(validate_beckman_grainsizer_txt)
    validate_return_list <- safe_validate(beckman_gs_txt_path)

    if (is.null(validate_return_list$error)) {
      validate_return_list <- validate_return_list$result
    } else {
      stop(
        paste0(
          "Error encountered in ",
          gs_txt_filename,
          "; Skipping file. ",
          validate_return_list$error$message
        )
      )
    }

    funs_parser <-
      list(extract_parameters,
           extract_stats,
           extract_interpolations,
           extract_channels)

    exec_args <-
      list(beckman_gs_txt_path,
           validate_return_list,
           calculate_depths)
    gs_data_tbl_list <- purrr::map(funs_parser, purrr::exec, !!!exec_args)
    names(gs_data_tbl_list) <-
      c("Parameters", "Statistics", "Interpolations", "ChannelData")

    gs_data_tbl_list
  }

validate_beckman_grainsizer_txt <- function(beckman_gs_txt_path) {
  tryCatch({
    con <- file(beckman_gs_txt_path, "r")
    gstxt_lines <- readLines(con)
    close(con)
  },
  error = function(e)
    stop("File validation failed: Unable to open fail for validation."))

  filename_string <- gstxt_lines[2]
  stopifnot("Sample name not found." = !(rlang::is_empty(filename_string) || is.na(filename_string)))

  filename <- stringr::str_extract(gstxt_lines[2], "(?<=File name:\\t).+")
  filename <- stringr::str_replace(filename, stringr::fixed(".$ls"), "")

  start_pattern <-
    c("LS\t|From\t|Particle Diameter\t|Channel Diameter \\(Lower\\)\t")
  start_vec <- stringr::str_which(gstxt_lines, start_pattern)
  stopifnot("File validation failed: Sections not found." = (!rlang::is_empty(start_vec) ||
                                                               length(start_vec) == 4L))
  end_vec <- which(gstxt_lines == "") - 1L
  # make a list of start and end of the sections
  return_list <- list(start_vec, end_vec, filename)
}

extract_parameters <-
  function(beckman_gs_txt_path,
           validate_return_list,
           calculate_depths) {
    start_pos <- validate_return_list[[1]][1]
    end_pos <- validate_return_list[[2]][1]

    # clean up
    paramstbl_names <-
      c(
        "UID",
        "FileID",
        "SampleID",
        "Operator",
        "Barcode",
        "Comment1",
        "Comment2",
        "Instrument",
        "Run",
        "StartTime",
        "RunLength",
        "OpticalModel",
        "Obscuration",
        "PIDSObscuration",
        "ObscurationWarn",
        "Serial"
      )
    tryCatch({
      paramstbl <-
        t(
          readr::read_tsv(
            beckman_gs_txt_path,
            skip = start_pos,
            n_max = end_pos - start_pos,
            col_names = FALSE,
            col_types = readr::cols(.default = "c")
          )
        )[2,]
    },
    error = function(e)
      stop("Error: Extracting parameters failed"))

    names(paramstbl) <- paramstbl_names

    paramstbl <- tibble::as_tibble_row(paramstbl)
    paramstbl <-
      dplyr::mutate_at(paramstbl, dplyr::vars(.data$UID), stringr::str_replace_all, stringr::fixed(".$ls"), "")

    if (calculate_depths)
    {
      paramstbl <-
        dplyr::mutate(
          paramstbl,
          StartDepth = as.numeric(stringr::str_split(.data$SampleID, "-")[[1]][1]),
          EndDepth = as.numeric(stringr::str_split(.data$SampleID, "-")[[1]][2]),
          MeanDepth = (.data$StartDepth + .data$EndDepth) / 2
        )
    }

    paramstbl
  }

extract_stats <-
  function(beckman_gs_txt_path,
           validate_return_list,
           ...) {
    # Get start and end positions of second section (Summary statistics)
    start_pos <- validate_return_list[[1]][2]
    end_pos <- validate_return_list[[2]][2]
    UID_filename <- validate_return_list[[3]]
    # Create an empty table with the necessary fields (columns), then feed data into the table (as before)
    statstbl_names <-
      c(
        "From",
        "To",
        "Volume",
        "Mean",
        "Median",
        "MeanMedianRatio",
        "Mode",
        "SD",
        "Variance",
        "CV",
        "Skewness"
      )
    statstbl <-
      t(
        readr::read_tsv(
          beckman_gs_txt_path,
          skip = start_pos - 1,
          n_max = end_pos - start_pos,
          col_names = FALSE,
          col_types = readr::cols(.default = "c")
        )
      )
    statstbl <- statstbl[2, ]
    names(statstbl) <- statstbl_names

    # we add the UID/Core Name and turn all other fields into numeric fields
    statstbl <- tibble::as_tibble_row(statstbl)
    statstbl <-
      tibble::add_column(statstbl,
                 UID = !!UID_filename,
                 .before = 1)
    statstbl <- dplyr::mutate_at(statstbl, dplyr::vars(-.data$UID), as.numeric)
    statstbl
  }

extract_interpolations <-
  function(beckman_gs_txt_path,
           validate_return_list,
           ...) {
    # Get start and end positions of third section (Interpolated particle diameters)
    start_pos <- validate_return_list[[1]][3]
    end_pos <- validate_return_list[[2]][3]
    UID_filename <- validate_return_list[[3]]
    # Read in the data (this time we don't have to flip it)
    interptbl <-
      dplyr::select(
        readr::read_tsv(
          beckman_gs_txt_path,
          skip = start_pos - 1,
          n_max = end_pos - start_pos,
          col_types = readr::cols(.default = "c")
        ),
        Di = "Particle Diameter",
        volp = "Volume"
      )
    interptbl <- dplyr::slice(interptbl, -c(1, 2))
    interptbl <- dplyr::mutate_all(interptbl, as.numeric)
    interptbl <- dplyr::filter(interptbl, .data$Di != 2000)
    interptbl <-
      tibble::add_column(interptbl,
                 UID = !!UID_filename,
                 .before = 1)
    interptbl <-
      dplyr::mutate(
        interptbl,
        qual1 = dplyr::case_when(
          Di == 0.04 ~ "clay",
          Di >= 4 &
            Di <= 31 ~ "silt",
          Di >= 32 &
            Di < 1000 ~ "sand",
          Di >= 1000 ~ "other"
        ),
        qual2 = dplyr::case_when(
          Di == 0.04 ~ "",
          Di == 4 ~ "very fine",
          Di == 8 ~ "fine",
          Di == 16 ~ "medium",
          Di == 31 ~ "coarse",
          Di == 62.5 ~ "very fine",
          Di == 125 ~ "fine",
          Di == 250 ~ "medium",
          Di == 500 ~ "coarse",
          Di == 1000 ~ ""
        )
      )
    interptbl
  }

extract_channels <-
  function(beckman_gs_txt_path,
           validate_return_list,
           ...) {
    # Get start and end positions of fourth section (raw channel data)
    start_pos <- validate_return_list[[1]][4]
    end_pos <- validate_return_list[[2]][4]
    UID_filename <- validate_return_list[[3]]
    # Read in the data (this time we don't have to flip it)
    channelstbl <-
      dplyr::select(
        readr::read_tsv(
          beckman_gs_txt_path,
          skip = start_pos,
          n_max = end_pos - start_pos,
          col_types = readr::cols(.default = "c")
        ),
        "um",
        "Volume",
        tidyselect::matches("Number")
      )

    channelstbl <- dplyr::slice(channelstbl, -c(1))
    channelstbl <- dplyr::mutate_all(channelstbl, as.numeric)
    channelstbl <-
      tibble::add_column(channelstbl,
                 UID = !!UID_filename,
                 .before = 1)
    channelstbl <- dplyr::rename(channelstbl, lwr = "um", volp = "Volume")
    channelstbl <- dplyr::filter(channelstbl, .data$lwr != 2000)
    channelstbl <-
      dplyr::mutate(channelstbl,
             upr = c(channelstbl[["lwr"]][2:nrow(channelstbl)], 2000),
             Di = sqrt(.data$upr * .data$lwr))
    channelstbl
  }
