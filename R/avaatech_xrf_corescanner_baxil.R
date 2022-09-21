#' Batch process Avaatech XRF-CS bAXIL csv files
#'
#' This function tries to validate and parse Avaatech XRF-Corescanner files created with bAXIL Batch (and saved as csv).
#'
#' The function expects the following fields and conditions in the csv file:
#' * The first field in the file must be called `Spectrum`. The CoreID, Voltage and Current are extracted from that field.
#' * The sample depth must be in a field called `Sample_ID`.
#' * The XRF data must be in a wide form like `" Sn-Ka Chi2" ," Sn-Ka Area" ," Sn-Ka AreaStd" ," Sn-Ka cps" ," Sn-Ka cpsStd"`. Element and Parameter need to be separated with an underline. The function expects to split the names into 4 parts which are Element, Absorption Line, Scattering Type (Inc/Coh e.g. for Rh) and Parameter.
#'
#' WARNING: This function does not filter out ambiguous data (e.g. same Element measured for different runs/voltage)!
#'
#' @param avaa_baxil_csv_path Absolute or relative path to the csv file
#' @param cut_off_set Should the offset at the top of the measurement be substracted (default is TRUE)? The offset caused by material that was used to close the sediment core will be substracted.
#' @param delim What csv file delimiter should be used (default is ",")?
#' @param show_col_types Show column types that were initially guessed by readr::read_delim() when reading the csv file. May be useful for debugging (default is FALSE)
#' @param xrf_data_start Number of the column in which the XRF data starts (e.g. `Sn-Ka Area`).
#' @param ... Further optional arguments that are passed on to read_delim()
#'
#' @return A dataframe/tibble containing tidy Avaatech XRF-CS data. Multiple XRF-CS datasets are rowbound to one big table.
#' @export
#'
batch_process_avaatech_baxil_csv <-
  function(avaa_baxil_csv_path,
           cut_off_set = TRUE,
           delim = ",",
           show_col_types = FALSE,
           xrf_data_start = 8,
           ...) {
    xrf_data <-
      purrr::map_dfr(
        avaa_baxil_csv_path,
        purrr::possibly(
          parse_avaatech_baxil_csv,
          otherwise = NULL,
          quiet = FALSE
        ),
        cut_off_set = cut_off_set,
        delim = delim,
        show_col_types = show_col_types,
        xrf_data_start = xrf_data_start,
        ...
      )
    xrf_data
  }

#' Parse single Avaatech XRF-CS bAXIL csv file
#'
#' This function tries to validate and parse Avaatech XRF-Corescanner files created with bAXIL Batch (and saved as csv).
#'
#' The function expects the following fields and conditions in the csv file:
#' * The first field in the file must be called `Spectrum`. The CoreID, Voltage and Current are extracted from that field.
#' * The sample depth must be in a field called `Sample_ID`.
#' * The XRF data must be in a wide form like `" Sn-Ka Chi2" ," Sn-Ka Area" ," Sn-Ka AreaStd" ," Sn-Ka cps" ," Sn-Ka cpsStd"`. Element and Parameter need to be separated with an underline. The function expects to split the names into 4 parts which are Element, Absorption Line, Scattering Type (Inc/Coh e.g. for Rh) and Parameter.
#'
#' WARNING: This function does not filter out ambiguous data (e.g. same Element measured for different runs/voltage)!
#'
#' This function is not exported. Use `batch_process_avaatech_baxil_csv` instead.
#'
#' @param avaa_baxil_csv_path Absolute or relative path to the csv file
#' @param cut_off_set Should the offset at the top of the measurement be substracted (default is TRUE)? The offset cause by material that was used to close the sediment core will be substracted.
#' @param delim What csv file delimiter should be used (default is ",")?
#' @param show_col_types Show column types that were initially guessed by readr::read_delim() when reading the csv file. May be useful for debugging (default is FALSE)
#' @param xrf_data_start Number of the column in which the XRF data starts (e.g. `Sn-Ka Area`).
#' @param ... Further optional arguments that are passed on to read_delim()
#'
#' @return A dataframe/tibble containing tidy Avaatech XRF-CS data
#'
#'
parse_avaatech_baxil_csv <-
  function(avaa_baxil_csv_path,
           cut_off_set = TRUE,
           delim = ",",
           show_col_types = FALSE,
           xrf_data_start = 8,
           ...) {
    filename <- basename(avaa_baxil_csv_path)

    # using fread for performance reasons
    safe_read <- purrr::safely(data.table::fread)

    avaa_baxil_csv <-
      safe_read(
        avaa_baxil_csv_path,
        sep = delim,
        ...
      )

    avaa_baxil_csv <- avaa_baxil_csv$result

    # We coerce the data.table dataframe back to a base R dataframe, as to maintain compatibility with the tidyverse code.
    avaa_baxil_csv <- methods::as(avaa_baxil_csv, Class = "data.frame")

    names(avaa_baxil_csv) <- stringr::str_trim(names(avaa_baxil_csv))

    # We have to do this, because dplyr 1.0+ do strict type checking and tidyverse functions won't combine different types...even numeric and integer e.g!
    avaa_baxil_csv <- dplyr::mutate(avaa_baxil_csv, dplyr::across(where(is.numeric), as.numeric))

    safe_validate <- purrr::safely(validate_avaa_baxil_csv)

    filename_field_tibble <-
      safe_validate(avaa_baxil_csv, xrf_data_start)

    if (is.null(filename_field_tibble$error)) {
      filename_field_tibble <- filename_field_tibble$result
    } else {
      stop(
        paste0(
          "Error encountered in ",
          filename,
          "; Skipping file. ",
          filename_field_tibble$error$message
        )
      )
    }

    if (!("Sample" %in% names(avaa_baxil_csv))) {
      stop(
        paste0(
          "No Sample field found in ",
          filename,
          "; Skipping file."
        )
      )
    }
    if (cut_off_set) {
      avaa_baxil_csv[["Sample"]] <-
        avaa_baxil_csv[["Sample"]] - min(avaa_baxil_csv[["Sample"]])
    }

    avaa_baxil_csv <-
      dplyr::bind_cols(filename_field_tibble, avaa_baxil_csv)

    avaa_baxil_csv <-
      tidyr::pivot_longer(
        avaa_baxil_csv,
        (!!xrf_data_start + 3):tidyselect::last_col(),
        names_to = c("Element", "AbsLine", "Scattering", "Parameter") ,
        values_to = "Value",
        names_pattern = "(^[\\w\\s]+)-(\\w+)-*(\\w+)*\\s(\\w+)"
      )

    avaa_baxil_csv <-
      dplyr::rename(
        avaa_baxil_csv,
        CoreID = .data$core_id,
        Voltage = .data$voltage,
        Current = .data$current,
        Depth = .data$Sample
      )

    avaa_baxil_csv <-
      dplyr::mutate(
        avaa_baxil_csv,
        CoreID = as.character(.data$CoreID),
        Voltage = as.numeric(.data$Voltage, "\\d+"),
        Current = as.numeric(.data$Current, "\\d+"),
        Depth = as.numeric(.data$Depth),
        Element = stringr::str_trim(.data$Element, side = "both")
      )

    avaa_baxil_csv <-
      tidyr::pivot_wider(avaa_baxil_csv,
                         names_from = "Parameter",
                         values_from = "Value")

    avaa_baxil_csv
  }

validate_avaa_baxil_csv <- function(data, xrf_pos) {
  stopifnot({
    "File validation failed: Incorrect file type or csv file not read correctly (there is only one column). Make sure the correct delimiter is set." = ncol(data) > 1
    "File validation failed: Expected 'Spectrum' field in first column but not found." = names(data[1]) == "Spectrum"
    "File validation failed: Expected 'Sample' column in file but not found." = "Sample" %in% names(data)
    "File validation failed: 'Sample' column (depth) not numeric, check input file." = is.numeric(data[["Sample"]])
    "File validation failed: 'Sample' column ambiguous: Possibly duplicate rows/measurements in file or missing values." = !any(duplicated(data[["Sample"]]))
    "File validation failed: Expected numeric XRF element data" = is.numeric(data[[xrf_pos]])
  })

  tryCatch(
    filename_field_splits <- stringr::str_split(data$Spectrum, "!"),
    error = function(e)
      stop("Unable to split 'Spectrum' column into parts.")
  )
  nr_field_splits <- unique(purrr::map(filename_field_splits, length))

  stopifnot("File validation failed: 'Spectrum' column structure unknown or inconsistent." = nr_field_splits == 17L)

  core_id <-
    filename_field_splits[[1]][1]
  stopifnot(
    "File validation failed: Different CoreIDs found in csv." = !rlang::is_empty(unique(core_id)) &&
      length(unique(core_id)) == 1
  )

  xrf_data_indices <- (xrf_pos):ncol(data)
  xrf_data_names <- colnames(data[, xrf_data_indices])

  tryCatch(
    xrf_data_splits <-
      max(as.numeric(purrr::map(
        stringr::str_split(xrf_data_names, "\\W"), length
      ))),
    error = function(e)
      stop("Unable to split xrf data fields into parts.")
  )

  stopifnot(
    "File validation failed: Structure of XRF data columns unknown." = (xrf_data_splits == 3L |
                                                                          xrf_data_splits == 4L)
  )

  voltage <- filename_field_splits[[1]][8]
  current <- filename_field_splits[[1]][9]
  filename_field_tibble <- tibble::tibble(core_id, voltage, current)

  filename_field_tibble
}
