library(tidyverse)
library(MatchIt)

control <- read.csv("retail/control.csv")
treatment <- read.csv("retail/treatment.csv")
demographics <- read.csv("retail/customer_demographics.csv")
features <- read.csv("retail/customer_features.csv")
treatment <- dplyr::select(treatment, - X)
control <- dplyr::select(control, -X)


control$event_date = as.Date(control$event_date)
treatment$event_date = as.Date(treatment$event_date)
control_ramadan <- subset(control, event_date >= "2017-05-19" & event_date <= "2017-07-02")
treatment_ramadan <- subset(treatment, event_date >= "2017-05-19" & event_date <= "2017-07-02")


# no active campaigns on control group during Ramadan period
unique(control_ramadan$dim_campaign_key) # 0 - only purchases
length(unique(treatment_ramadan$dim_campaign_key)) # 89 campaigns (90th one is '0' campaign, referring to purchases)

# number of customers in control and treatment group during Ramadan
length(unique(control_ramadan$customer_id)) # 239 
length(unique(treatment_ramadan$customer_id)) # 9920

# number of customers in control and treatment group overall (not just Ramadan)
length(unique(control$customer_id)) # 1233 
length(unique(treatment$customer_id)) # 9920


# unique campaigns on control and treatment group before and after Ramadan period 
# (should be same to compare/measure Ramadan campaign success), and distinct from the list of Ramadan campaigns
control_before <- subset(control, event_date < "2017-05-19") # 1225 unique customers (not 1233)
length(unique(control_before$customer_id))

treatment_before <- subset(treatment, event_date < "2017-05-19") # 9809 unique customers (not 9920)
length(unique(treatment_before$customer_id))

control_after <- subset(control, event_date > "2017-07-02") # 1218
treatment_after <- subset(treatment, event_date > "2017-07-02") # 9556


# campaign intersection on treatment/control BEFORE Ramadan
length(unique(control_before$dim_campaign_key)) # 254
length(unique(treatment_before$dim_campaign_key)) # 352

intersecting_campaigns <- intersect(unique(control_before$dim_campaign_key), unique(treatment_before$dim_campaign_key))
# 248 - delete rows from customers who have the 'non-intersecting' campaigns acting on them - in before, during, and after


# identify customers that were part of campaigns not in these 248 - in either control_before_p or treatment_before_p
extra_customers_control <- subset(control_before, !dim_campaign_key %in% intersecting_campaigns)
extra_customers_control <- unique(extra_customers_control$customer_id)

extra_customers_treatment <- subset(treatment_before, !dim_campaign_key %in% intersecting_campaigns)
extra_customers_treatment <- unique(extra_customers_treatment$customer_id)

# remove any rows to do with these customers in control_before, treatment_before, control_ramadan, treatment_ramadan
control_before <- subset(control_before, !customer_id %in% extra_customers_control)
treatment_before <- subset(treatment_before, !customer_id %in% extra_customers_treatment)

control_ramadan <- subset(control_ramadan, !customer_id %in% extra_customers_control)
treatment_ramadan <- subset(treatment_ramadan, !customer_id %in% extra_customers_treatment)

# check
length(unique(treatment_before$dim_campaign_key)) # 248
length(unique(control_before$dim_campaign_key)) # 248

# check campaigns during Ramadan
length(unique(treatment_ramadan$dim_campaign_key)) # 90 - 89 + the 0 campaign
length(unique(control_ramadan$dim_campaign_key)) # 1 - the 0 campaign

# after equalizing 'before' campaigns, check unique customers in all four groups - 
length(unique(treatment_before$customer_id)) # 7950
length(unique(treatment_ramadan$customer_id)) # 8061

length(intersect(unique(treatment_before$customer_id), unique(treatment_ramadan$customer_id))) # 7950 - receiving messages and/or purchasing BEFORE AND AFTER

length(unique(control_before$customer_id)) # 1218
length(unique(control_ramadan$customer_id)) # 237

length(intersect(unique(control_before$customer_id), unique(control_ramadan$customer_id))) # 236 - purchasing BEFORE AND AFTER

# check number of customers in all four groups on sub-setting by event-type = p 
control_before_p <- subset(control_before, event_type %in% c("p"))
length(unique(control_before_p$customer_id)) # 256

control_ramadan_p <- subset(control_ramadan, event_type %in% c("p"))
length(unique(control_ramadan_p$customer_id)) # 237

length(intersect(unique(control_before_p$customer_id), unique(control_ramadan_p$customer_id))) # 90 - whrere are the 237 - 90 coming from: they were getting messages before, but no purchases before, now purchases (and no messages)

treatment_before_p <- subset(treatment_before, event_type %in% c("p"))
length(unique(treatment_before_p$customer_id)) # 1996

treatment_ramadan_p <- subset(treatment_ramadan, event_type %in% c("p"))
length(unique(treatment_ramadan_p$customer_id)) # 2277 

length(intersect(unique(treatment_before_p$customer_id), unique(treatment_ramadan_p$customer_id))) # 890: 2277 - 890 - getting messages before, but made no purchases, now they're making purchases

###################################################################
###################################################################

#########################################
## Difference-in-Differences - no PSM  ##
#########################################

control_before_p$ramadan <- 0
control_ramadan_p$ramadan <- 1

control_before_p$treatment <- 0
control_ramadan_p$treatment <- 0

treatment_before_p$ramadan <- 0
treatment_ramadan_p$ramadan <- 1

treatment_before_p$treatment <- 1
treatment_ramadan_p$treatment <- 1

# rbind control_before and control_ramadan
control_group <- rbind(control_before_p, control_ramadan_p) 
treatment_group <- rbind(treatment_before_p, treatment_ramadan_p) 

length(unique(control_group$customer_id)) # 403 - but some of them buy before, and not after, and some buy not before, and only after
length(unique(treatment_group$customer_id)) # 3383 - again, some buy only before, not during R campaigns, some buy only during R campaigns, and not before 

data <- rbind(control_group, treatment_group)
data$value <- data$amount + data$discount # treatment variable
data$did <- data$ramadan*data$treatment

didreg = lm(value ~ ramadan + treatment + did, data)
summary(didreg)

didreg1 = lm(value ~ ramadan*treatment, data)
summary(didreg1)

# # check for same customers in before and during ramadan PURCHASES ONLY, else delete non-overlap from both, because you don't know their behaviour before:
# length(intersect(unique(control_ramadan$customer_id), unique(control_before$customer_id))) # 236
# length(intersect(unique(treatment_before$customer_id), unique(treatment_ramadan$customer_id))) # 7950
# 
# 
# # check for same treatment customers in before and during, else delete from during, because you don't know their behaviour before:
# 
# length(unique(treatment_before$customer_id)) # 9809
# length(unique(treatment_ramadan$customer_id)) # 9920 - some additional customers who weren't buying before, but now are, because of Ramadan campaign? or just more messages to more ppl?
# 
# intersecting_customers_control <- intersect(unique(control_before$customer_id), unique(control_ramadan$customer_id)) # 236
# intersecting_customers_treatment <- intersect(unique(treatment_before$customer_id), unique(treatment_ramadan$customer_id)) # 988
# 
# # keep only customers who are in this intersection 
# control_before <- subset(control_before, customer_id %in% intersecting_customers_control)
# control_ramadan <- subset(control_ramadan, customer_id %in% intersecting_customers_control)
# 
# length(unique(control_before$customer_id)) # 236
# length(unique(control_ramadan$customer_id)) # 236
# 
# treatment_before <- subset(treatment_before, customer_id %in% intersecting_customers_treatment)
# treatment_ramadan <- subset(treatment_ramadan, customer_id %in% intersecting_customers_treatment)
# 
# length(unique(treatment_before$customer_id)) # 890
# length(unique(treatment_ramadan$customer_id)) # 890
# 
# 
# control_before_p <- subset(control_before, event_type %in% c("p"))
# length(unique(control_before_p$customer_id)) # 92 customers
# 
# control_ramadan_p <- subset(control_ramadan, event_type %in% c("p"))
# length(unique(control_ramadan_p$customer_id)) # 92 customers
# 
# treatment_before_p <- subset(treatment_before, event_type %in% c("p"))
# length(unique(treatment_before_p$customer_id)) # 2251 customers
# 
# treatment_ramadan_p <- subset(treatment_ramadan, event_type %in% c("p"))
# length(unique(treatment_ramadan_p$customer_id)) # 988 customers
# 
# max(treatment_before_p$event_date) - min(treatment_before_p$event_date)
# # Time difference of 47 days
# max(control_before_p$event_date) - min(control_before_p$event_date)
# # Time difference of 47 days
# max(treatment_ramadan_p$event_date) - min(treatment_ramadan_p$event_date)
# # Time difference of 44 days
# max(control_ramadan_p$event_date) - min(control_ramadan_p$event_date)

# 1. Looking at same group - before/after
# 2. Adding controls to the DiD - figuring thse out
# 3. PSM - but remove customers you're not looking at in before/after - 


