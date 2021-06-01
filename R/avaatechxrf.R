#' Parse Avaatech bAXIL Batch XRF-Corescanner files
#'
#' This function takes a string with the path to your bAXIL Batch csv-files, reads
#' them and turns them into tidied dataframes.
#'
#' Currently the function automatically removes elements with coherent (Coh) and
#' incoherent (Inc) traces and chooses the less noisy element (smaller relative
#' standard deviation), if present for different voltages. By default, the function removes the top offset of the data. Additionally, repeated measures are removed by default.
#'
#' @param file Character vector with filenames (and full paths) of bAXIL Batch *.csv-files
#' @return Tidied bAXIL XRF data, returned as a dataframe
#' @export
#'

read_xrfcs_avaatech_baxil <- function(file) {
  if (rlang::is_empty(file))
    stop("No csv files found in directory")
  tmp <- purrr::map(file, makeTidyData) %>%
    dplyr::bind_rows()

  xrftbl <- tmp %>%
    # apply to every Element and Voltage
    dplyr::group_by(Element, Voltage) %>%
    # total counts per group
    dplyr::summarise(totalcounts = sum(Area)) %>%
    # sort with descending counts
    dplyr::arrange(Element, dplyr::desc(totalcounts)) %>%
    # get most counts per Element
    dplyr::top_n(1, totalcounts) %>%
    dplyr::select(Element, Voltage) %>%
    # join with original data
    dplyr::left_join(tmp, by = c("Element", "Voltage")) %>%
    dplyr::select(
      CoreID,
      Depth,
      Date,
      Time,
      Duration,
      Run,
      Rep,
      Voltage,
      Current,
      Filter,
      SlitDown,
      SlitCross,
      Excitation,
      Throughput,
      Element,
      dplyr::everything()
    ) %>%
    # remove repeated measures
    dplyr::filter(Rep %in% "Rep0")
  xrftbl
}

makeTidyData <- function(fileName) {
  tmp <- readr::read_csv(fileName, quote = "") %>%
    dplyr::distinct()
  if (!stringr::str_detect((names(tmp)[1]), "Spectrum"))
    stop("No valid Avaatech XRF baxil batch file (csv)") # check structure of file
  names(tmp) <- names(tmp) %>%
    # remove unnecessary quotes
    stringr::str_replace_all('\\"', "") %>%
    # trim whitespace
    stringr::str_trim()

  # Split spectrum field to gain additional information
  tmp2 <- stringr::str_split_fixed(tmp$Spectrum, "\\!", 17) %>%
    tidyr::as_tibble()
  # naming new variables
  names(tmp2) <-
    c(
      "CoreID",
      "unknown1",
      "unknown2",
      "Depth",
      "Date",
      "Time",
      "Duration",
      "Voltage",
      "Current",
      "unknown3",
      "unknown4",
      "Filter",
      "SlitDown",
      "SlitCross",
      "Run",
      "Rep",
      "unknown5"
    )

  tmp3 <- dplyr::bind_cols(tmp2, tmp) %>%
    dplyr::select(
      -dplyr::starts_with("unknown"),
      -Spectrum,
      -`Live time`,
      -`Real time`,
      -Sample,
      -User
    ) %>%
    dplyr::mutate_at(
      dplyr::vars(
        Depth,
        Voltage,
        Current,
        SlitDown,
        SlitCross,
        Duration,
        Throughput
      ),
      as.numeric
    ) %>%
    # substract first Depth per core since there is a offset due to the green stuff
    dplyr::mutate(Depth = Depth - min(Depth)) %>%
    # gather wide-spread counts/element data, turning into long form
    tidyr::gather(-(CoreID:Throughput),
                  key = "Measure",
                  value = "Value") %>%
    # remove Rhodium trace because of ambiguity
    dplyr::filter(!stringr::str_detect(Measure, "Coh|Inc")) %>%
    # split field into Element-Absorption Line-Statistics
    tidyr::separate(Measure,
                    sep = "[\\W]+",
                    into = c("Element", "AbsLine", "Stat")) %>%
    # spread statistics factors (cps,cpsStd,area,areaStd etc) into variables
    tidyr::spread(Stat, Value)
}
