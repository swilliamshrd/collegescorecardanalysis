##################
###   setup    ###
##################
#working.directory <- "~/Documents/hrd/collegescorecardanalysis"
working.directory <- "I:/User/Williams/Publications/College Scorecard Analysis 2016"

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


# ####################################
# ##Side by Side Face Graph by inst ##
# ####################################
# 
# 
# #### Black and white version ####
# graph_bw <- graphcategories %>%
#             filter(DegreeLevel == "Certificate/Associate's" | DegreeLevel == "Bachelor's") %>%
#             mutate(DegreeLevel = factor(DegreeLevel, 
#                                         levels = c("Bachelor's",
#                                                    "Certificate/Associate's"                   ),
#                                         labels=c("Predominantly 4-Year Degree Granting", 
#                                         "Predominantly 2-Year Degree Granting"))) %>%
#             ggplot(aes(x=data.year,
#                        y=meandebt,
#                        shape=factor(mn.category),
#                        group = factor(mn.category)))
# 
# 
# sidebyside_bw <- graph_bw + 
#       geom_line() + 
#       geom_point(size=2) +
#       scale_x_continuous("", limits = c(1997, 2013), breaks = c(1997, 2001, 2005, 2009, 2013)) +
#       scale_y_continuous("Average Median Debt (Nominal $)", limits = c(0,20000), labels=dollar) +
#       guides(shape = guide_legend(title = "")) +
#       theme_bw() +
#       theme(legend.position = "bottom") +
#       theme(axis.text.y = element_text(size=10)) +
#       facet_grid(.~DegreeLevel) +
#       labs(title="Federal Debt Loads Upon Entering Repayment by Institution Type, 1997-2013")    
# 
# 
# #write file out to .emf file for microsoft word
# library(devEMF)
# 
# emf(file = "sidebyside_bw.emf",
#     bg = "transparent",
#     family = "Helvetica", 
#     width = 10,
#     height = 6)
# sidebyside_bw
# dev.off()
# 
# #### Color line version ####
# graph_color <- graphcategories %>%
#       filter(DegreeLevel == "Certificate/Associate's" | DegreeLevel == "Bachelor's") %>%
#       mutate(DegreeLevel = factor(DegreeLevel, 
#                                   levels = c("Bachelor's",
#                                              "Certificate/Associate's"                   ),
#                                   labels=c("Predominantly 4-Year Degree Granting", 
#                                            "Predominantly 2-Year Degree Granting"))) %>%
#       ggplot(aes(x=data.year,
#                  y=meandebt,
#                  color=factor(mn.category),
#                  group = factor(mn.category)))
# 
# 
# sidebyside_color <- graph_color+ 
#       geom_line() + 
#       scale_x_continuous("", limits = c(1997, 2013), breaks = c(1997, 2001, 2005, 2009, 2013)) +
#       scale_y_continuous("Average Median Debt (Nominal $)", limits = c(0,20000), labels=dollar) +
#       guides(color = guide_legend(title = "")) +
#       theme_minimal() +
#       theme(legend.position = "bottom") +
#       theme(axis.text.y = element_text(size=10)) +
#       facet_grid(.~DegreeLevel) +
#       labs(title="Federal Debt Loads Upon Entering Repayment by Institution Type, 1997-2013")    
# 
# #write file out to .png file for microsoft word
# sidebyside_color
# ggsave("sidebyside_color.png", width=10, height=6)
# 
# 
# # #write file out to .emf file for microsoft word
# # library(devEMF)
# # emf(file = "sidebyside_color.emf",
# #     bg = "transparent",
# #     family = "Helvetica", 
# #     width = 10,
# #     height = 6)
# # sidebyside_color
# # dev.off()
# 
# ####################################
# ## separate graphs for 2 and 4 yr ##
# ####################################


#### Black and white 2yr graph ####
graph_twoyr <- graphcategories %>%
      filter(DegreeLevel == "Certificate/Associate's") %>%
      ggplot(aes(x=data.year,
                 y=meandebt,
                 shape=factor(mn.category),
                 group=factor(mn.category)))

graph_twoyr <-graph_twoyr + 
      geom_line() + 
      geom_point(size=2) +
      scale_x_continuous("", limits = c(1997, 2015), breaks = c(1997, 2001, 2005, 2009, 2013, 2015)) +
      scale_y_continuous("Average Median Debt (Nominal $)", limits = c(0,13000), labels=dollar) +
      guides(shape = guide_legend(title = "")) +
      theme_bw() +
      theme(legend.position = "bottom") +
      theme(axis.text.y = element_text(size=10)) +
      labs(title="Federal Debt Loads Upon Entering Repayment, Two-year Schools, 1997-2013")    

graph_twoyr

#write file out to .emf file for microsoft word
library(devEMF)

emf(file = "twoyearschools.emf",
    bg = "transparent",
    family = "Helvetica", 
    width = 8,
    height = 6)
graph_twoyr
dev.off()


#### Black and white 4yr graph ####
graph_fouryr <- graphcategories %>%
      filter(DegreeLevel == "Bachelor's") %>%
      ggplot(aes(x=data.year,
                 y=meandebt,
                 shape=factor(mn.category),
                 group=factor(mn.category)))

graph_fouryr <-graph_fouryr + 
      geom_line() + 
      geom_point(size=2) +
      scale_x_continuous("", limits = c(1997, 2013), breaks = c(1997, 2001, 2005, 2009, 2013)) +
      scale_y_continuous("Average Median Debt (Nominal $)", limits = c(0,20000), labels=dollar) +
      guides(shape = guide_legend(title = "")) +
      theme_bw() +
      theme(legend.position = "bottom") +
      theme(axis.text.y = element_text(size=10)) +
      labs(title="Federal Debt Loads Upon Entering Repayment, Four-year Schools, 1997-2013")    

graph_fouryr

#write file out to .emf file for microsoft word
library(devEMF)

emf(file = "fouryearschools.emf",
    bg = "transparent",
    family = "Helvetica", 
    width = 10,
    height = 6)
graph_fouryr
dev.off()

