# ISD_UTILS.R
# Library of useful R functions for generic ISD analysis purposes.
# This should be limited to general use functions -- new library files should be created for topic specific functions.
#
# Author: Bob Taylor / bob.taylor@nhs.net

# hack to effectively silence notes on unbound global variables during compilation
if(getRversion() >= "2.15.1"){
  utils::globalVariables(c(".", "LINK_NO", "CIS_MARKER", "ADMISSION_DATE", "DISCHARGE_DATE",
                           "ADMISSION", "DISCHARGE"))
}

convert_to_finyear <- function(.data, ..., sep="-", format="%Y", drop=FALSE){
  #' Convert dates to financial year format.
  #'
  #' Takes a tibble with some date columns, or a vector of dates and returns
  #' either a tibble with the specified columns converted, or a vector, depending
  #' on the input type.
  #'
  #' @param .data a vector of R Date objects
  #' @param ... column names, to be converted from date format to financial year strings
  #' @param sep optional value for the separator string
  #' @param format optional formatting string for the year, either format="\%y" or format="\%Y"
  #' @param drop optional parameter to drop the first year in the output, i.e. otuput "2005" instead of "2004-2005"
  #'
  #' @return a tibble or vector with dates converted to financial year strings
  #'
  #' @author Bob Taylor, \email{bob.taylor@nhs.net}
  #'
  #' @examples
  #' # 1. what financial year was Christmas 1995?
  #' convert_to_finyear(as.Date("1995-01-25"), sep="/", format="%y")
  #'
  #' # 2. the current financial year in FYXX format
  #' library(lubridate)
  #' paste("FY", convert_to_finyear(today(), format="%y", drop=TRUE), sep="")
  #'
  #' # 3. conversion within a tibble
  #' library(dplyr)
  #' data <- tibble(date = as.Date(c("1972-03-14", "2003-04-04", "1995-01-25" )),
  #' date2 = as.Date(c("1932-02-10", "2013-07-14", "1991-11-15" )),
  #' num = c(1,2,3))
  #'
  #' data %>%
  #' convert_to_finyear(date, date2)
  #' @export
  
  date_cols <- rlang::enquos(...)
  
  # check class of input
  if(all(!("tbl" %in% class(.data)), class(.data) != "Date")){
    warning("Input should be either a tibble or vector of dates.")
    return(.data)
  }
  
  # if user specifies a drop value that is not strictly TRUE or FALSE, use default FALSE
  if(is.logical(drop) == FALSE){
    warning("The  \"drop\" parameter must be logical TRUE or FALSE. Defaulting to FALSE.")
    drop <- FALSE
  }
  
  # warn if user specifies invalid year format
  if(!(format %in% c("%y", "%Y"))){
    warning("Please specifiy a valid year format. Will default to e.g. 2009")
    format = "%Y"
  }
  
  # function to convert date values into finyear strings
  date_to_finyear <- function(date, sep.=sep, format.=format, drop.=drop){
    month <- as.numeric(format(date, "%m"))
    year <- as.numeric(format(date, "%Y"))  # will import year as e.g. 1987, and apply user's formatting later
    year_plus_one <- year + 1
    year_minus_one <- year - 1
    
    if(format. == "%y"){
      year <- year %% 100
      year <- ifelse(year < 10, paste0("0", year), as.character(year))
      
      year_plus_one <- year_plus_one %% 100
      year_plus_one <- ifelse(year_plus_one < 10,
                              paste0("0", year_plus_one),
                              as.character(year_plus_one))
      
      year_minus_one <- year_minus_one %% 100
      year_minus_one <- ifelse(year_minus_one < 10,
                               paste0("0", year_minus_one),
                               as.character(year_minus_one))
      
    } else{
      year <- as.character(year)
      year_plus_one <- as.character(year_plus_one)
      year_minus_one <- as.character(year_minus_one)
    }
    
    # if drop == FALSE return e.g. 2004-2005
    if(drop. == FALSE){
      finYear <- ifelse( month <= 3,
                         paste(year_minus_one, year, sep=sep.),
                         paste(year, year_plus_one, sep=sep.))
    }
    
    # if drop == TRUE return e.g. 2005
    else{
      finYear <- ifelse( month <= 3, year,
                         year_plus_one)
    }
    
    return(finYear)
  }
  
  # if tibble, use mutate_at to run over all specified columns
  if("tbl" %in% class(.data)){
    return(dplyr::mutate_at(.data, .vars=date_cols, .funs=list(~date_to_finyear(.))))
  }
  if(class(.data) == "Date"){
    return(date_to_finyear(.data))
  }
  }

split_opcode_columns <- function(df, opcols, keep){
  #' Split columns of OPCS4 paired codes into two columns.
  #'
  #' Splits columns containing 8-character OPCS4 codes into two 4-character columns. Columns names
  #' can be specified -- if none are specified will default to 'MAIN_OPERATION', 'OTHER_OPERATION_1',
  #' 'OTHER_OPERATION_2', and 'OTHER_OPERATION_3'. The original columns can be kept or dropped using the
  #' *keep* variable.
  #'
  #' Split columns will be given the suffixes "_A" and "_B" respectively.
  #' @param df a dataframe, datatable or tibble
  #' @param opcols vector of columns to split
  #' @param keep boolean variable, FALSE to drop original (unsplit) columns, TRUE to keep
  #'
  #' @return dataframe
  #'
  #' @author Bob Taylor, \email{bob.taylor@nhs.net}
  #'
  #' @examples
  #' # 1. Split all the default OPCS4 columns into two and drop the originals.
  #' library(dplyr)
  #' data("OPdata")
  #' df <- OPdata %>% split_opcode_columns()
  #'
  #' # 2. Split one specific OPCS4 column, and also keep the original.
  #' df <- OPdata %>% split_opcode_columns(opcols="MAIN_OPERATION", keep=TRUE)
  #'
  #' @export
  #'
  # require(tidyverse)
  # require(stringr)
  
  # define default cols as all the SMRA procedure columns
  defaults <- c('MAIN_OPERATION', 'OTHER_OPERATION_1', 'OTHER_OPERATION_2', 'OTHER_OPERATION_3')
  
  # check if input is dataframe
  if (!is.data.frame(df)){
    warning("Input object is not a dataframe, datatable or tibble.")
    return(df)
  }
  
  # if no opcols defined, use defaults
  if(missing(opcols)) {
    opcols <- defaults
  }
  
  # return original data if one of the specified opcols does not exist
  if(any(!(opcols %in% names(df))) == TRUE){
    warning("You have specifed a column that does not exist in the input dataframe.")
    return(df)
  }
  
  # assume want to drop original columns unless otherwise specified
  if(missing(keep)){
    keep <- FALSE
  }
  
  
  
  # split every column and delete originals if required
  for (opcol in opcols){
    df[[paste(opcol, sep='_', 'A')]] <- stringr::str_sub(df[[opcol]], 1, 4)
    df[[paste(opcol, sep='_', 'B')]] <- stringr::str_sub(df[[opcol]], 5, 8)
    
    if(keep==FALSE){
      df[[opcol]] <- NULL
    }
  }
  
  return(df)
  
  }


recode_health_boards <- function(inputObject, hbcols, from, to){
  #' Recode health board identifiers.
  #'
  #' Recodes NHS Scotland Health Board identifiers. Will either
  #' a) take an input vector and return the recoded version, or
  #' b) take an input dataframe/tibble and return said dataframe/tibble with appropriately recoded columns.
  #'
  #' Can recode from/to name (e.g. NHS Fife), code (S08000018), altcodes (SFA20) and cipher (F).
  #'
  #' NB: will use NA value if a mapping does not exist. Will return input value if input value not valid, i.e. attempting
  #' to recode "NHS Vorders" to e.g. "code" form will return "NHS Vorders"
  #'
  #' @param inputObject vector, dataframe, or tibble
  #' @param hbcols which columns in dataframe or tibble to recode, e.g. \code{hbcols=c("HBTREAT_CURRENTDATE", "HBRES_CURRENTDATE")}
  #' @param from format to be recoded from, can be e.g. "name" (e.g. NHS Fife), "code" (S08000018), "altcode" (SFA20) and "cipher" (F)
  #' @param to format to be recoded to, can be e.g. "name" (e.g. NHS Fife), "code" (S08000018), "altcode" (SFA20) and "cipher" (F)
  #'
  #' @return vector of recoded healthboard identifiers, or dataframe/tibble with recoded columns of healthboard identifiers
  #'
  #' @author Bob Taylor, \email{bob.taylor@nhs.net}
  #'
  #' @examples
  #' # 1. Convert vector of e.g. S08000015 codes to readable names, e.g "NHS Ayrshire & Arran"
  #' library(dplyr)
  #' data("HBdata")
  #' recode_health_boards(HBdata$code, from="code", to="name")
  #'
  #' # 2. Do the above but return the full dataframe/tibble with the selected column now recoded.
  #' df <- HBdata %>% recode_health_boards(hbcols="cipher", from="cipher", to="name")
  #'
  #' # 3. Do the above but return the full dataframe/tibble with two columns recoded.
  #' HBdata$HBtreat <- HBdata$name # generate dummy column for Health Board of Treatment
  #' HBdata$HBres <- sample(HBdata$name) # generate dummy column for Health Board of Residence
  #' df <- HBdata %>% recode_health_boards(hbcols=c("HBtreat", "HBres"),
  #' from="name", to="code")
  #' @export
  
  
  ### DEFINITIONS ###
  # types that can be recoded to and from, plus details for all the boards
  validVars <- c("name", "cipher", "code", "altcode")
  
  boardIdents <- list(name=c(    "NHS Ayrshire & Arran", "NHS Borders", "NHS Dumfries & Galloway", "NHS Fife", "NHS Forth Valley", "NHS Grampian", "NHS Greater Glasgow & Clyde", "NHS Highland", "NHS Lanarkshire", "NHS Lothian", "NHS Orkney", "NHS Shetland", "NHS Tayside", "NHS Western Isles", "Non-NHS Provider/Location", "Not Applicable", "NHS National Facility", "NHS24",     "NHS Education for Scotland", "NHS Health Scotland", "NHS National Services Scotland", "NHS Healthcare Improvement Scotland", "Scottish Ambulance Service", "State Hospital"),
                      cipher=c(  "A",                    "B",            "Y",                      "F",         "V",               "N",            "G",                           "H",            "L",               "S",           "R",          "Z",            "T",           "W",                 NA,                          NA,               NA,                      NA,          NA,                           NA,                    NA,                               NA,                                    NA,                           NA),
                      code=c(    "S08000015",            "S08000016",    "S08000017",              "S08000018", "S08000019",       "S08000020",    "S08000021",                   "S08000022",    "S08000023",       "S08000024",   "S08000025", "S08000026",     "S08000027",   "S08000028",         "S27000001",                 "S27000002",      "S08100001",             "S08100002", "S08100003",                  "S08100004",           "S08100005",                      "S08100006",                           "S08100007",                  "S08100008"),
                      altcode=c( "SAA20",                "SBA20",       "SYA20",                   "SFA20",    "SVA20",            "SNA20",        "SGA20",                       "SHA20",        "SLA20",           "SSA20",       "SRA01",      "SZA01",        "STA20",       "SWA01",             NA,                          NA,               NA,                      "SD035",     "SD037",                      "SD040",               "SD021",                          "SD039",                               "SD026",                      "SDA01")
  )
  
  fromIdents <- unlist(boardIdents[from])
  toIdents <- unlist(boardIdents[to])
  
  # if a vector, just do the recode and exit
  if(is.vector(inputObject)){
    out <- do.call(dplyr::recode, c(list(inputObject), stats::setNames(toIdents, fromIdents)))
    return(out)
  }
  
  if(is.data.frame(inputObject) & missing(hbcols)){
    warning("If using a dataframe or tibble, please define which column is to be recoded using e.g. hbcols=\"HBTREAT_CURRENTDATE\".")
    return(inputObject)
  }
  if(is.data.frame(inputObject) & any(!(hbcols %in% names(inputObject)))){
    warning("You have sepcified a value in hbcols that is not a column in the input dataframe/tibble. Possible
            values are: \n", paste(names(inputObject), collapse=", "))
    return(inputObject)
  }
  # if to and from are invalid, output error message and return input; otherwise do the recode
  if( !(from %in% validVars) | !(to %in% validVars) ){
    warning("An invalid argument for either the \"from\" or  \"to\" argument has been specified. Valid arguments are
            \"name\", \"code\", \"cipher\" and \"altcode\".")
    return(inputObject)
  }
  else{
    ### OUTPUT ###
    # if a dataframe or tibble, do the recode for all the columns defined in hbcols
    if(is.data.frame(inputObject)){
      for(hbcol in hbcols){
        inputObject[[hbcol]] <- do.call(dplyr::recode, c(list(inputObject[[hbcol]]), stats::setNames(toIdents, fromIdents)))
      }
      return(inputObject)
    }
    # if not a dataframe, throw a warning and return input
    else{
      warning("Input must be a vector, dataframe or tibble.")
      return(inputObject)
    }
    
  }
  
  }


create_ISD_project <- function(name="ISD Project", path=getwd(), type=""){
  #' Create an RStudio Project using ISD's default template.
  #'
  #' Downloads the current ISD project template from github and creates a project from it,
  #'  using the project name and path defined by the user.
  #'
  #' @param name text, the project's name, e.g. "SurvivalAnalysis". Defaults to "ISD Project".
  #' @param path optional text variable defining where the project should be created
  #' @param type optional text variable: type="shiny" will create a default Shiny project,
  #' any other value will create a normal R project.
  #'
  #' @author Bob Taylor, \email{bob.taylor@nhs.net}
  #'
  #' @examples
  #' # 1. Create a project called "Test" in the current working directory.
  #' create_ISD_project("Test")
  #'
  #' # 2. Create a project with the default name, in the directory above.
  #' create_ISD_project(path="..")
  #'
  #' # 3. Create a project with the default name in the current working directory.
  #' create_ISD_project()
  #' @export
  
  # change these if online repo changes
  githubRepo <- "Health-SocialCare-Scotland"
  projectFolderName <- "r-project-structure"
  
  if(type=="shiny" | type=="Shiny"){
    projectFolderName <- "rshiny-project-structure"
  }
  
  # url of current master branch zip archive
  url <- paste0("https://github.com/", githubRepo, "/", projectFolderName, "/archive/master.zip")
  projectArchiveFolderName <- paste0(projectFolderName, "-master")
  
  # create hidden temp dir with ~random name to unzip in etc. If dir exists will repeat until dir can be created.
  repeat{
    tempDirName <- paste0(".tmp", paste0(sample(1e10, 1)))
    if(!dir.exists(tempDirName)){
      dir.create(tempDirName)
      break
    }
  }
  
  # create useful path names
  projectZipPath <- file.path(tempDirName, ".project.zip")
  projectTempPath <- file.path(tempDirName, ".project")
  
  # download, unzip and rename to the user's chosen project name
  utils::download.file(url=url, destfile = projectZipPath)
  utils::unzip(projectZipPath, exdir = projectTempPath)
  file.rename(file.path(projectTempPath, projectArchiveFolderName), file.path(projectTempPath, name))
  
  # copy to user's chosen project path (defaults to current working dir)
  dirToCopy <- file.path(projectTempPath, name)
  
  if(file.copy(dirToCopy, path, overwrite=TRUE, recursive=TRUE) == TRUE){
    print("Project was created at:")
    print(file.path(path, name))
  } else{
    warning("Project creation failed.")
  }
  
  # tidy up by deleting the temp dir (requires folder refresh to be visible to users though....)
  unlink(tempDirName, recursive=TRUE, force=TRUE)
  }


add_CIS_dates <- function(SMR_tbl, rearrange = FALSE){
  #' Add CIS dates.
  #'
  #' Adds dates of CIS admission and discharge to individual episodes. Creates new variables STAY_ADMISSION_DATE and STAY_DISCHARGE_DATE.
  #'
  #' @param SMR_tbl tibble of SMR data.
  #' @param rearrange optional boolean. TRUE will rearrange the input dataset into CIS order.
  #'
  #' @author Bob Taylor, \email{bob.taylor@nhs.net}
  #' @import magrittr
  #' @export
  
  # if input not a tibble of some sort, warn and return input unaltered
  if(!("tbl" %in% class(SMR_tbl))){
    warning("CIS dates not added - input to add_CIS_dates() should be a tibble.")
    return(SMR_tbl)
  }
  
  # set required column names, depending on whether or not the rearrange step is needed
  required_columns <- c("LINK_NO", "CIS_MARKER", "ADMISSION_DATE", "DISCHARGE_DATE")
  if(rearrange == TRUE){
    required_columns <- c("LINK_NO", "CIS_MARKER", "ADMISSION_DATE", "DISCHARGE_DATE", "ADMISSION", "DISCHARGE")
  }
  
  # warn and return input if any required column not present
  if(any(!(required_columns %in% colnames(SMR_tbl)))){
    warning(paste("CIS dates not added - one of the following columns is required by add_CIS_dates() but not present:", toString(required_columns)))
    return(SMR_tbl)
  }
  
  # group into CISs and add admission and discharge dates for the stay to each episode
  df <- SMR_tbl %>%
    dplyr::group_by(LINK_NO, CIS_MARKER) %>%
    dplyr::mutate(STAY_ADMISSION_DATE = min(ADMISSION_DATE, na.rm = TRUE),
                  STAY_DISCHARGE_DATE = max(DISCHARGE_DATE, na.rm = TRUE)) %>%
    dplyr::ungroup()
  
  # rearrange episodes into standard CIS order if required
  if(rearrange == TRUE){
    df <- df %>%
      dplyr::arrange(LINK_NO, CIS_MARKER, ADMISSION_DATE, DISCHARGE_DATE, ADMISSION, DISCHARGE)
  }
  
  return(df)
  
  }


make_stays <- function(con, link_and_cis, aggregates, 
                       sort_order = c("LINK_NO", "CIS_MARKER", "ADMISSION_DATE", "DISCHARGE_DATE", "ADMISSION", "DISCHARGE", "URI")){
  #' Stays.
  #'
  #' Adds dates of CIS admission and discharge to individual episodes. Creates new variables STAY_ADMISSION_DATE and STAY_DISCHARGE_DATE.
  #'
  #' @param con database tibble of SMR01 data.
  #' @param link_and_sys .
  #' @param aggregates list of quantities to be aggregated and their methods e.g. list(max=DISCHARGE_DATE, last=HB)
  #'
  #' @author Bob Taylor, \email{bob.taylor@nhs.net}
  #' @export
  
  # initial table of non-aggregated stays consisting of all the episodes with supplied
  # LINK_NO and CIS_MARKER
  stay_cols <- unlist(aggregates)
  link_and_cis <- link_and_cis %>% distinct(.) # in case user supplies dupes
  sort_order_sym <- syms(sort_order) # arrange function requires escaped symbol list, !!!sort_order_sym
  
  df <- con %>%
    inner_join(., link_and_cis, copy = TRUE, by = c("LINK_NO", "CIS_MARKER")) %>% 
    select(unique(c(!!stay_cols, !!sort_order, "LINK_NO", "CIS_MARKER"))) %>% # all cols user wants + cols we need
    arrange(!!!sort_order_sym) %>%
    group_by(LINK_NO, CIS_MARKER)
  
  # for every type of aggregation, construct and evaluate aggregation expression for each column
  # and join together using LINK and CIS
  df_aggs <- df %>% select(LINK_NO, CIS_MARKER) %>% distinct(.)
  
  for(agg_type in names(aggregates)){
    for(column in aggregates[[agg_type]]){
      
      # correctly parse col names whether supplied as strings or unquoted col names
      quoted_var <- rlang::enquo(column)
      if(class(rlang::get_expr(quoted_var)) == "character"){
        column <- sym(column)
        quoted_var <- rlang::enquo(column)
      }
      
      # if not first or last aggregation, do via dplyr::summarise
      if(!(agg_type %in% c("first", "last"))){
        expr <- paste0("df %>% dplyr::summarise(!!quo_name(quoted_var) := ", agg_type, "(!!quoted_var, na.rm = TRUE))")
        df_aggs <- df_aggs %>% inner_join(eval(rlang::parse_expr(expr)), by = c("LINK_NO", "CIS_MARKER"))
      } else{
        # if first or last aggregation, do via table joins
        
        # if not already created, make df with order of episodes in each stay
        if(exists("df_indices") == FALSE){
          df_indices <- df %>%  mutate(ep_id = row_number())
        }
        
        # if first aggregation, choose first episode from each stay
        if(agg_type == "first"){
          if(exists("df_first") == FALSE){
            df_first <- df_indices %>%
              mutate(ep_first = if_else(ep_id == min(ep_id, na.rm = TRUE), 1, 0)) %>%
              filter(ep_first == 1)
          }
          df_aggs <- df_aggs %>% inner_join(df_first %>% select("LINK_NO", "CIS_MARKER", !!quoted_var),
                                            by = c("LINK_NO", "CIS_MARKER"))
        }
        
        # if last aggregation, choose last episode from each stay
        if(agg_type == "last"){
          if(exists("df_last") == FALSE){
            df_last <- df_indices %>%
              mutate(ep_last = if_else(ep_id == max(ep_id, na.rm = TRUE), 1, 0)) %>%
              filter(ep_last == 1)
          }
          df_aggs <- df_aggs %>% inner_join(df_last %>% select("LINK_NO", "CIS_MARKER", !!quoted_var),
                                            by = c("LINK_NO", "CIS_MARKER"))
        }
        
      }
      
    }
  }
  # return the required columns, including LINK and CIS
  df_aggs <- df_aggs %>% select(unique(c("LINK_NO", "CIS_MARKER", !!stay_cols)))
  return(df_aggs)
  }



#' convert_to_finyear <- function(date, sep="-", format="%Y", drop=FALSE){
#'   #' Convert a date to financial year format.
#'   #'
#'   #' Takes a vector of dates and returns a character vector containing the appropriate financial years the dates are from.
#'   #'
#'   #' @param date a vector of R Date objects
#'   #' @param sep optional value for the separator string
#'   #' @param format optional formatting string for the year, either format="\%y\%" or format="\%Y\%"
#'   #' @param drop optional parameter to drop the first year in the output, i.e. otuput "2005" instead of "2004-2005"
#'   #'
#'   #' @return a character vector of financial years
#'   #'
#'   #' @author Bob Taylor, \email{bob.taylor@nhs.net}
#'   #'
#'   #' @examples
#'   #' # 1. what financial year was Christmas 1995?
#'   #' convert_to_finyear(as.Date("1995-01-25"), sep="/", format="%y")
#'   #'
#'   #' # 2. the current financial year in FYXX format
#'   #' library(lubridate)
#'   #' paste("FY", convert_to_finyear(today(), format="%y", drop=TRUE), sep="")
#'   #' @export
#'
#'   # return input and warn if not a Date
#'   if(class(date) != "Date"){
#'     warning("Input not in Date format. Convert first using as.Date()")
#'     return(date)
#'   }
#'
#'   # if user specifies a drop value that is not strictly TRUE or FALSE, use default FALSE
#'   if(is.logical(drop) == FALSE){
#'     warning("The  \"drop\" parameter must be logical TRUE or FALSE. Defaulting to FALSE.")
#'     drop <- FALSE
#'   }
#'
#'   # warn if user specifies invalid year format
#'   if(!(format %in% c("%y", "%Y"))){
#'     warning("Please specifiy a valid year format. Will default to e.g. 2009")
#'     format = "%Y"
#'   }
#'
#'   month <- as.numeric(format(date, "%m"))
#'   year <- as.numeric(format(date, "%Y"))  # will import year as e.g. 1987, and apply user's formatting later
#'   year_plus_one <- year + 1
#'   year_minus_one <- year - 1
#'
#'   if(format == "%y"){
#'     year <- year %% 100
#'     year <- ifelse(year < 10, paste0("0", year), as.character(year))
#'
#'     year_plus_one <- year_plus_one %% 100
#'     year_plus_one <- ifelse(year_plus_one < 10,
#'                             paste0("0", year_plus_one),
#'                             as.character(year_plus_one))
#'
#'     year_minus_one <- year_minus_one %% 100
#'     year_minus_one <- ifelse(year_minus_one < 10,
#'                              paste0("0", year_minus_one),
#'                              as.character(year_minus_one))
#'
#'   } else{
#'     year <- as.character(year)
#'     year_plus_one <- as.character(year_plus_one)
#'     year_minus_one <- as.character(year_minus_one)
#'   }
#'
#'   # if drop == FALSE return e.g. 2004-2005
#'   if(drop == FALSE){
#'     finYear <- ifelse( month <= 3,
#'                        paste(year_minus_one, year, sep=sep),
#'                        paste(year, year_plus_one, sep=sep))
#'   }
#'
#'   # if drop == TRUE return e.g. 2005
#'   else{
#'     finYear <- ifelse( month <= 3, year,
#'                        year_plus_one)
#'   }
#'
#'   return(finYear)
#'   }
