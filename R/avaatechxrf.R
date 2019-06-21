#' Parse Avaatech bAXIL XRF files
#'
#' @param path Directory path to *.csv input files
#' @return
#' @export
#'

read_avaatech_baxil <- function() {
  files <- dir(pattern = ".+\\.csv")
  if (is_empty(files)) stop("No csv files found in directory")
  tmp <- map(files, makeTidyData) %>%
    bind_rows()

  xrftbl <- tmp %>%
    group_by(Element, Voltage) %>% # apply to every Element and Voltage
    summarise(totalcounts = sum(Area)) %>% # total counts per group
    arrange(Element, desc(totalcounts)) %>% # sort with descending counts
    top_n(1, totalcounts) %>% # get most counts per Element
    select(Element, Voltage) %>%
    left_join(tmp, by = c("Element", "Voltage")) %>% # join with original data
    select(CoreID, Depth, Date, Time, Duration, Run, Rep, Voltage, Current, Filter, SlitDown, SlitCross, Excitation, Throughput, Element, everything()) %>%
    filter(Rep %in% "Rep0") # remove repeated measures
  xrftbl
}

makeTidyData <- function(fileName) {
  tmp <- read_csv(fileName, quote = "") %>%
    distinct()
  if (!str_detect((names(tmp)[1]), "Spectrum")) stop("No valid Avaatech XRF baxil batch file (csv)") # check structure of file
  names(tmp) <- names(tmp) %>%
    str_replace_all('\\"', "") %>% # remove unnecessary quotes
    str_trim() # trim whitespace

  tmp2 <- str_split_fixed(tmp$Spectrum, "\\!",17) %>%
    as_tibble() # Split spectrum field to gain additional information
  names(tmp2) <- c("CoreID", "unknown1", "unknown2", "Depth", "Date", "Time", "Duration", "Voltage", "Current", "unknown3", "unknown4", "Filter", "SlitDown", "SlitCross", "Run", "Rep", "unknown5") # naming new variables

  tmp3 <- bind_cols(tmp2, tmp) %>%
    select(-starts_with("unknown"), -Spectrum, -`Live time`, -`Real time`, -Sample, -User) %>%
    mutate_at(vars(Depth, Voltage, Current, SlitDown, SlitCross, Duration, Throughput), as.numeric) %>%
    # substract first Depth per core since there is a offset due to the green stuff
    mutate(Depth = Depth - min(Depth)) %>%
    # gather wide-spread counts/element data, turning into long form
    gather(-(CoreID:Throughput), key = "Measure", value = "Value") %>%
    # remove Rhodium trace because of ambiguity
    filter(!str_detect(Measure, "Rh")) %>%
    # split field into Element-Absorption Line-Statistics
    separate(Measure, sep = "[\\W]+", into = c("Element","AbsLine","Stat")) %>%
    # spread statistics factors (cps,cpsStd,area,areaStd etc) into variables
    spread(Stat, Value)
}
