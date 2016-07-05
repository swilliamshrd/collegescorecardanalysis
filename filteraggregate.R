
########################################
###  Set Parameters for Import Here  ###
########################################

#select years of data to import
years.to.import <- 1996:2013

#Select states to import using 2-digit state abbreviation
states.to.import <- c("MN")

# Folder you will be working in
working.directory <- "I:/User/Williams/2016 Interim/College Scorecard Analysis"

#set path to the data folder
data.folder <- "C:/SeanLocalData/debt_earnings/"

# pick variables to import. Variable names must be in quotes because
# they are passed to the select function via the "one_of" argument. 
# data dictionary is here: https://collegescorecard.ed.gov/data/documentation/
variables.to.select <- c( "UNITID", "INSTNM", "CITY", "STABBR", "ZIP", "PREDDEG", "HIGHDEG", "CONTROL","st_fips", "LOCALE", 
                              "locale2", "LATITUDE", "LONGITUDE", "ADM_RATE", "DISTANCEONLY", "UGDS", "UG", "CURROPER", "COSTT4_A",
                              "COSTT4_P", "TUITIONFEE_IN", "TUITIONFEE_OUT", "TUITIONFEE_PROG", "TUITFTE", "AVGFACSAL",
                              "PCTPELL", "RET_FT4", "RET_FTL4", "PCTFLOAN", "CDR2", "CDR3", "RPY_1YR_RT", "RPY_3YR_RT",
                              "RPY_5YR_RT", "DEBT_MDN", "GRAD_DEBT_MDN", "WDRAW_DEBT_MDN", "PELL_DEBT_MDN", "NOPELL_DEBT_MDN",
                              "FEMALE_DEBT_MDN", "MALE_DEBT_MDN", "loan_ever", "pell_ever", "faminc", "md_faminc", "median_hh_inc",
                              "gt_25k_p10", "mn_earn_wne_p6", "md_earn_wne_p6", "mn_earn_wne_p7", "mn_earn_wne_p8", "md_earn_wne_p8",
                              "mn_earn_wne_p9","DEBT_MDN_SUPP", "GRAD_DEBT_MDN_SUPP", "GRAD_DEBT_MDN10YR_SUPP", "RPY_3YR_RT_SUPP",
                              "COMPL_RPY_3YR_RT_SUPP", "NONCOM_RPY_3YR_RT_SUPP"  
                          )


########################################
###  Script to create a single data  ###
###  frame with selected years,      ###
###  states, and variables            ###
########################################
setwd(working.directory)


library(dplyr)

#Read in individual files.
for (year in years.to.import) {
      
      
      # get filename for a given year
      filename <- paste("MERGED", as.character(year), "_PP.csv", sep = "")
      
      #paste together filename and path
      path <- paste(data.folder, filename, sep = "")
      
      #Assign variable to temporary variable
      temp <- read.csv(path, header = TRUE, stringsAsFactors=FALSE, na.strings = "NULL")
      
      names(temp)[1] <- "UNITID"
      
      #limit to only mn schools
      temp <- filter(temp, STABBR %in% states.to.import) %>%
            #Select only the variables I care about
            select(one_of(variables.to.select))

      
      #add year variable      
      data.year <- rep(year, nrow(temp))
      temp <- cbind(data.year, temp)
      
      #create string of dataframe name
      df.name <- paste("data",as.character(year), sep = ".")
      
      #save variable to data frame with name data.year
      assign(df.name, temp)
      
      #remove temporary data frame
      remove(list = c("temp", "df.name", "data.year","filename", "year"))
      
}

#Build list of all the data frames names
df.names <- paste("data.", years.to.import, sep = "")

#make a list of all of the single year data frames
df.list <- lapply(df.names, get)

#Convert all of the zip codes to character (to allow merge)
fixzips <- function(df) {
     within(df, ZIP <- as.character(ZIP))
}
df.list <- lapply(df.list, fixzips)

#Merge all of the years in to a single data frame
debtdata <- bind_rows(df.list)

#remove single year data frames
remove(list = df.names)

#save data to .rda file
save(debtdata, file = "debtdata.rda")