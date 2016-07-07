##################
###   setup    ###
##################
#working.directory <- "I:/User/Williams/2016 Interim/College Scorecard Analysis"
working.directory <- "~/Documents/hrd/collegescorecardanalysis"

setwd(working.directory)

library(dplyr)
library(ggplot2)
library(scales)

load("debtdata_clean.rda")

##################
###Inspect Data###
##################

### get number of missings and uncategorized by year
brokenyears <- debtdata %>%
      group_by(data.year) %>%
      summarise(missings=sum(is.na(debt_mdn)),
                uncategorized = sum(mn.category == ""),
                suppressed = sum(debt_mdn == "PrivacySuppressed", na.rm=TRUE),
                datapresent = sum(!(is.na(debt_mdn)) & !(mn.category == "") & !(debt_mdn=="PrivacySuppressed")),
                count=n()
                )

brokenyears

####################################
## Debt by Institution type Graph ##
####################################

#Clean up data for making graph chart
graphcategories <- debtdata %>%
      filter(mn.category != "") %>% #exclude uncategorized schools
      filter(!(is.na(debt_mdn))) %>% #exclude not available data
      filter(debt_mdn != "PrivacySuppressed") %>% #exclude privacy suppressed data
      filter(data.year != 1996) %>% #exclude 1996 because the data is missing
      mutate(debt_mdn = as.numeric(debt_mdn)) %>% #convert median debt number to 
      group_by(mn.category, data.year, DegreeLevel) %>%
      summarise(meandebt = mean(debt_mdn))


####################################
##Side by Side Face Graph by inst ##
####################################

graph <- graphcategories %>%
            filter(DegreeLevel == "Certificate/Associates" | DegreeLevel == "Bachelor's") %>%
            ggplot(aes(x=data.year,
                       y=meandebt,
                       color=factor(mn.category),
                       group = factor(mn.category)))

setEPS()
postscript("debttrendsidebyside.eps", width=1000, height=600)
graph + 
      geom_line(size = 1.5) +
      scale_x_continuous("", limits = c(1997, 2013), breaks = c(1997, 2001, 2005, 2009, 2013)) +
      scale_y_continuous("Average Median Debt Amount Upon Entering Repayment", limits = c(0,20000), labels=dollar) +
      guides(color = guide_legend(title = "")) +
      theme_minimal() +
      theme(legend.position = "bottom") +
      theme(axis.text.y = element_text(size=10))+
      facet_grid(.~DegreeLevel)




####################################
## separate graphs for 2 and 4 yr ##
####################################

twoyeargraph <- graphcategories %>%
                  filter(DegreeLevel == "Certificate/Associates") %>%
                  ggplot(aes(x=data.year,
                             y=meandebt,
                             linetype=factor(mn.category),
                             group = factor(mn.category)))

twoyeargraph + 
      geom_line(size = 1.5) +
      scale_x_continuous("Year", limits = c(1997, 2013), breaks = c(1997, 2001, 2005, 2009, 2013)) +
      scale_y_continuous("Average Median Debt Amount Upon Entering Repayment", limits = c(0,15000), labels=dollar) +
      guides(linetype = guide_legend(title = "")) +
      theme_minimal() +
      theme(legend.position = "bottom")


fouryeargraph <- graphcategories %>%
      filter(DegreeLevel == "Bachelor's") %>%
      ggplot(aes(x=data.year,
                 y=meandebt,
                 color=factor(mn.category),
                 group = factor(mn.category)))

fouryeargraph + 
      geom_line(size = 1.5) +
      scale_x_continuous("Year", limits = c(1997, 2013), breaks = c(1997, 2001, 2005, 2009, 2013)) +
      scale_y_continuous("Average Median Debt Amount Upon Entering Repayment", limits = c(0,20000), labels=dollar) +
      guides(col = guide_legend(title = "Institution Type", title.theme = element_text(angle = 0, size =10)))
