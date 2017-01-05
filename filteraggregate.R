
########################################
###  Set Parameters for Import Here  ###
########################################

#Select states to import using 2-digit state abbreviation
states.to.import <- c("MN")

# Folder you will be working in
working.directory <- "I:/User/Williams/2016 Interim/College Scorecard Analysis"

#set path to the data folder
data.folder <- "C:/SeanLocalData/debt_earnings/"
data.files <- list.files(data.folder)


# pick variables to import. Variable names must be in quotes because
# they are passed to the select function via the "one_of" argument. 
# data dictionary is here: https://collegescorecard.ed.gov/data/documentation/
variables.to.select <- c( "UNITID", "INSTNM", "CITY", "STABBR", "ZIP", "PREDDEG", "HIGHDEG", "CONTROL","ST_FIPS", 
                        "DEBT_MDN", "GRAD_DEBT_MDN", "WDRAW_DEBT_MDN", "PELL_DEBT_MDN", "NOPELL_DEBT_MDN",
                              "FEMALE_DEBT_MDN", "MALE_DEBT_MDN", "LOAN_EVER", "PELL_EVER", "data.year"
                          )


########################################
###  Script to create a single data  ###
###  frame with selected years,      ###
###  states, and variables            ###
########################################
setwd(working.directory)


library(dplyr)
library(readr)
library(purrr)

readdata <- function(filename) {
      
      # get filename for a given year
      year <- as.numeric(substr(filename, 7, 10))+1
      
      #paste together filename and path
      path <- paste(data.folder, filename, sep = "")
      
      data <- read_csv(path, na = c("NULL")) %>%
            mutate(data.year = year) %>% 
            filter(STABBR %in% states.to.import) 
      
      names(data)[1] <- "UNITID"     
      data <- select(data, one_of(variables.to.select))
      
      return(data)
}


datalist <- map(data.files, readdata)
debtdata <- bind_rows(datalist)


#save data to .rda file
save(debtdata, file = "debtdata.rda")