##################
### parameters ###
##################
working.directory <- "I:/User/Williams/2016 Interim/College Scorecard Analysis"

setwd(working.directory)

library(dplyr)

##################
### cleaning #####
##################

#Load aggregate data file or run source file to create it.
if (!file.exists("debtdata.rda")) {
      source('filteraggregate.R')
} else {
      load("debtdata.rda")
}

#### Clean up Uppercase Strings ####

#clean up variable names
names(debtdata) <- tolower(names(debtdata))

#clean up institution names

            #convert to lowercase 
debtdata <- mutate(debtdata, instnm = tolower(instnm)) %>%
            #capitalize all word boundaries
            mutate(instnm = gsub("\\b(\\w)", "\\U\\1", instnm, perl = TRUE)) %>%
            #make and and or lowercased
            mutate(instnm = gsub("And", "and", instnm)) %>%
            mutate(instnm = gsub("Of", "of", instnm)) 



#### categorize schools by number of years ## 
debtdata$DegreeLevel <- ""
debtdata[(debtdata$preddeg == 1 | debtdata$preddeg == 2),]$DegreeLevel <- "Certificate/Associates"
debtdata[debtdata$preddeg == 3,]$DegreeLevel <- "Bachelor's"
debtdata[debtdata$preddeg == 4,]$DegreeLevel <- "Bachelor's"


#### categorize schools narrowly ####
debtdata$school.category <- ""
      
      #Publics
      debtdata[debtdata$control == 1 & debtdata$preddeg == 1, ]$school.category <- "Public Certificate"
      debtdata[debtdata$control == 1 & debtdata$preddeg == 2, ]$school.category <- "Public Associate's"
      debtdata[debtdata$control == 1 & debtdata$preddeg == 3, ]$school.category <- "Public Bachelor's"
      debtdata[debtdata$control == 1 & debtdata$preddeg == 4, ]$school.category <- "Public Graduate"
      
      
      #non-profits
      debtdata[debtdata$control == 2 & debtdata$preddeg == 1, ]$school.category <- "Non-Profit Certificate"
      debtdata[debtdata$control == 2 & debtdata$preddeg == 2, ]$school.category <- "Non-Profit Associate's"
      debtdata[debtdata$control == 2 & debtdata$preddeg == 3, ]$school.category <- "Non-Profit Bachelor's"
      debtdata[debtdata$control == 2 & debtdata$preddeg == 4, ]$school.category <- "Non-Profit Graduate"
      
      
      #for-profits
      debtdata[debtdata$control == 3 & debtdata$preddeg == 1, ]$school.category <- "For-Profit Certificate"
      debtdata[debtdata$control == 3 & debtdata$preddeg == 2, ]$school.category <- "For-Profit Associate's"
      debtdata[debtdata$control == 3 & debtdata$preddeg == 3, ]$school.category <- "For-Profit Bachelor's"
      debtdata[debtdata$control == 3 & debtdata$preddeg == 4, ]$school.category <- "For-Profit Graduate"
      
      #not classified
      debtdata[debtdata$control == 0 | debtdata$preddeg == 0, ]$school.category <- "Not Classified"

#### categorize schools into degree categories ####
debtdata$degree.category <- ""
      
      debtdata[debtdata$preddeg== 1 | debtdata$preddeg== 2, ]$degree.category <- "2-Year Degree or Certificate"
      debtdata[debtdata$preddeg== 3, ]$degree.category <- "4-Year Bachelor's"
      debtdata[debtdata$preddeg==4,]$degree.category <- "Graduate Degree"


#### Categorize Schools in to Minnesota categories ####
debtdata$mn.category <- ""

debtdata$mn.category <- replace(debtdata$mn.category, grep("University of Minnesota-", debtdata$instnm), "U of M")
debtdata$mn.category <- replace(debtdata$mn.category,debtdata$control == 1 & debtdata$mn.category != "U of M" , "Minnesota State")
debtdata$mn.category <- replace(debtdata$mn.category,debtdata$control == 2, "Private, Non-Profit")
debtdata$mn.category <- replace(debtdata$mn.category,debtdata$control == 3, "Private, For-Profit")

##Save file
save(debtdata, file = "debtdata_clean.rda")
                                             
