read_LS13320 <- function(writeTable = TRUE, saveRData = TRUE) {
  # this function calls the specific functions to process data from each section in the raw file.
  files <- dir(pattern = ".+\\.\\$ls\\.txt") # regular expression to filer/select only export files with the correct extension

  GSdata <- map(files, validateFile) %>% # apply function validateFile on all export files contained in variable files, then..
    transpose() %>% # flip the generated structure so it can be bound together as rows
    map(bind_rows) # bind all the dataframes together
  return(GSdata)
}

validateFile <- function(fileName) {
  con <- file(fileName, "r")
  # read file as a unformatted text file (this is done per each file)
  textLines <- readLines(con)
  # close connection to file
  close(con)

  # These keywords mark the start of every section (\t denotes tabulator or new section in this case)
  startPattern <- c("LS\t|From\t|Particle Diameter\t|Channel Diameter \\(Lower\\)\t")
  # Check where the sections in the file start
  startVec <- textLines %>%
    str_which(startPattern)
  # if there are no starts or not 4 sections, abort and throw error
  if (is_empty(startVec) | length(startVec) != 4) stop("Invalid raw data: Sections not found")
  # check where sections end (since sections are separated by empty line)
  endVec <- which(textLines == "") - 1L
  # make a list of start and end of the sections
  startEndList <- list(startVec, endVec)
  # list of all the functions (defined below)
  funs <- list(extractParams, extractStats, extractInterps, extractChannels)
  # now we apply all those functions funs on the file fileName (with extra argument startEndList)
  datatbl <- invoke_map(funs, fileName, startEndList)
  return(datatbl)
}

extractParams <- function(fileName, startEndList) {
  # This function extracts the parameters from the first section of the export file (Device info and parameters)
  # Get start and end positions of first section
  startPos <- startEndList[[1]][1]
  endPos <- startEndList[[2]][1]
  # Create an empty table with the necessary fields (columns)
  paramstbl <- tribble(~UID, ~FileID, ~SampleID, ~Operator, ~Barcode, ~Comment1, ~Comment2, ~Instrument, ~Run, ~StartTime, ~RunLength, ~OpticalModel, ~Obscuration, ~PIDSObscuration, ~ObscurationWarn, ~Serial)
  # The data is ordered wrong (variables in rows, not columns), so we create an empty table and then flip the whole data before reading it in (with t() and attaching them to the second row subsequently)
  paramstbl[1,] <- read_tsv(fileName, skip = startPos, n_max = endPos - startPos, col_names = FALSE, col_types = cols(.default = "c")) %>%
    t() %>%
    .[2,]
  paramstbl <- paramstbl %>%
    mutate_at(vars(UID), str_replace_all, fixed(".$ls"), "") %>% # we cut off the extension of the UID/Core Name
    mutate(StartDepth = as.numeric(str_split(SampleID, "-")[[1]][1]), EndDepth = as.numeric(str_split(SampleID, "-")[[1]][2]), MeanDepth = (StartDepth + EndDepth)/2) # then we use the depth information in the SampleID field to calculate the mean depth and save it as a new variable/column
  return(paramstbl)
}

extractStats <- function(fileName, startEndList) {
  # Get start and end positions of second section (Summary statistics)
  startPos <- startEndList[[1]][2]
  endPos <- startEndList[[2]][2]
  # Create an empty table with the necessary fields (columns), then feed data into the table (as before)
  statstbl <- tribble(~From, ~To, ~Volume, ~Mean, ~Median, ~MeanMedianRatio, ~Mode, ~SD, ~Variance, ~CV, ~Skewness)
  statstbl[1,] <- read_tsv(fileName, skip = startPos - 1, n_max = endPos - startPos, col_names = FALSE, col_types = cols(.default = "c")) %>%
    t() %>%
    .[2,]
  # we add the UID/Core Name and turn all other fields into numeric fields
  statstbl <- statstbl %>%
    add_column(UID = str_replace(fileName, fixed(".$ls.txt"), ""), .before = 1) %>%
    mutate_at(vars(-UID), as.numeric)
  return(statstbl)
}

extractInterps <- function(fileName, startEndList) {
  # Get start and end positions of third section (Interpolated particle diameters)
  startPos <- startEndList[[1]][3]
  endPos <- startEndList[[2]][3]
  # Read in the data (this time we don't have to flip it)
  interptbl <- read_tsv(fileName, skip = startPos - 1, n_max = endPos - startPos, col_types = cols(.default = "c")) %>%
    # we're only selecting the variables that we expect/know (this will fail hard, if variables are not present)
    select("Particle Diameter", "Volume") %>%
    rename("Di" = "Particle Diameter", "volp" = "Volume") %>%
    # first to rows are rubbish, so bye!
    slice(-c(1,2)) %>%
    # turn all other rows to numeric
    mutate_all(as.numeric) %>%
    # we don't need the last line (2000um lower border), as there is no data in it!
    filter(Di != 2000) %>%
    # add UID/Core name
    add_column(UID = str_replace(fileName, fixed(".$ls.txt"), ""), .before = 1) %>%
    # Here we add "qualifiers" (clay, silt, sand = qual1 and very fine, fine, medium, coarse = qual2) for our convenience
    mutate(qual1 = case_when(.$Di == 0.04 ~ "clay", .$Di >= 4 & .$Di <= 31 ~ "silt", .$Di >= 32 & .$Di < 1000 ~ "sand", .$Di >= 1000 ~ "other"), qual2 = case_when(.$Di == 0.04 ~ "", .$Di == 4 ~ "very fine", .$Di == 8 ~ "fine", .$Di == 16 ~ "medium", .$Di == 31 ~ "coarse", .$Di == 62.5 ~ "very fine", .$Di == 125 ~ "fine", .$Di == 250 ~ "medium", .$Di == 500 ~ "coarse", .$Di == 1000 ~ ""))
  return(interptbl)
}

extractChannels <- function(fileName, startEndList) {
  # Get start and end positions of fourth section (raw channel data)
  startPos <- startEndList[[1]][4]
  endPos <- startEndList[[2]][4]
  # Read in the data (this time we don't have to flip it)
  channelstbl <- read_tsv(fileName, skip = startPos, n_max = endPos - startPos, col_types = cols(.default = "c")) %>%
    # subsequent commands as in previous section
    select("um", "Volume", matches("Number")) %>%
    slice(-c(1)) %>%
    mutate_all(as.numeric) %>%
    add_column(UID = str_replace(fileName, fixed(".$ls.txt"), ""), .before = 1) %>%
    rename("lwr" = "um", "volp" = "Volume") %>%
    filter(lwr != 2000) %>%
    # we want to know the mean Diameter (Di) and not the lower (lwr) or upper (upr) boundary. This is calculated by sqrt(lwr * upr)
    mutate(upr = c(unlist(.[2:length(.$lwr), "lwr"]), 2000), Di = sqrt(upr * lwr))
  return(channelstbl)
}
