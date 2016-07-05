##################
###   setup    ###
##################
working.directory <- "I:/User/Williams/2016 Interim/College Scorecard Analysis"

setwd(working.directory)

library(dplyr)
library(ggplot2)
library(scales)
library(ggthemes)

load("debtdata.rda")


####################################
## Debt by Institution type Graph ##
####################################

#### Categorize Schools in to Minnesota categories ####
debtdata$mn.category <- ""

debtdata$mn.category <- replace(debtdata$mn.category, grep("University of Minnesota-", debtdata$instnm), "U of M")
debtdata$mn.category <- replace(debtdata$mn.category,debtdata$control == 1 & debtdata$preddeg==3 & debtdata$mn.category != "U of M" , "MnSCU 4-Year")
debtdata$mn.category <- replace(debtdata$mn.category,debtdata$control == 1 & debtdata$preddeg %in% c(1,2) & debtdata$mn.category!="U of M", "MnSCU 2-Year")
debtdata$mn.category <- replace(debtdata$mn.category,debtdata$control == 1 & debtdata$preddeg %in% c(1,2) & debtdata$mn.category!="U of M", "MnSCU 2-Year")
debtdata$mn.category <- replace(debtdata$mn.category,debtdata$control == 2, "Private, Non-Profit")
debtdata$mn.category <- replace(debtdata$mn.category,debtdata$control == 3, "Private, For-Profit")

# ####################################
# ## Debt by Institution type Graph ##
# ####################################
# 
#calculate number of privacy suppressed data points and missing data points for median debt.
missings <- debtdata %>%
      mutate(privacy = ifelse(!is.na(debt_mdn) & debt_mdn == "PrivacySuppressed", 1, 0)) %>%
      mutate(missing = ifelse(is.na(debt_mdn), 1, 0)) %>%
      group_by(instnm) %>%
      summarise(missings = sum(missing), privacys = sum(privacy))
#
# 
# #Get Mean Debt for Bachelor's Degree Granting Institutions
# debt.by.school.type <- debtdata %>%
#       filter(debt_mdn != "PrivacySuppressed") %>%
#       mutate(debt_mdn = as.numeric(debt_mdn)) %>%
#       group_by(school.category, data.year) %>%
#       summarize(mean.debt = mean(debt_mdn, na.rm=TRUE)) %>%
#       filter(school.category != "") 
# 
# #build line graph of debt over time.
# linegraph <- ggplot(debt.by.school.type, 
#                     aes(x=data.year, y=mean.debt, color=factor(school.category), 
#                         group = factor(school.category)))
# 
# linegraph + 
#       geom_line(size = 1) + 
#       scale_x_continuous("Year", limits = c(1997, 2013), breaks = c(1996, 2000, 2004, 2008, 2012)) + 
#       scale_y_continuous("Average Student Debt", limits = c(0,20000), labels=comma) +
#       guides(col = guide_legend(title = "Institution Type", title.theme = element_text(angle = 0, size =12)))
# 
# 
# 
# ####################################
# #### Debt by HRD Grouping Graph ####
# ####################################
# 
# 
# #Get Mean Debt for Bachelor's Degree Granting Institutions
# debt.by.hrd.type <- debtdata %>%
#       filter(debt_mdn != "PrivacySuppressed") %>%
#       mutate(debt_mdn = as.numeric(debt_mdn)) %>%
#       group_by(hrd.category, data.year) %>%
#       summarize(mean.debt = mean(debt_mdn, na.rm=TRUE)) %>%
#       filter(hrd.category != "") 
# 
# #build line graph of debt over time.
# debt.by.hrd.type.graph <- ggplot(debt.by.hrd.type, 
#                     aes(x=data.year, y=mean.debt, color=factor(debt.by.hrd.type), 
#                         group = factor(debt.by.hrd.type)))
# 
# debt.by.hrd.type.graph + 
#       geom_line(size = 1) + 
#       scale_x_continuous("Year", limits = c(1997, 2013), breaks = c(1996, 2000, 2004, 2008, 2012)) + 
#       scale_y_continuous("Average Student Debt", limits = c(0,20000), labels=comma) +
#       guides(col = guide_legend(title = "Institution Type", title.theme = element_text(angle = 0, size =12)))
# 
