###############################################
# Replication Code for:                       #
# Dreier and Lake (Democratization            #
#                                             #
# RECODING VARIABLES FOR ANALYSIS             #
#                                             #
# R version 3.4.4 (2018-03-15)                #
# DATE: 5/12/2019                              #
###############################################

# Content:
# - Load raw Afrobarometer data
# - For herfindahl variables: relable missing/don't know/refused as "NA"
# - Recode and bin religious identities
# - Write/run Herfindahl function
# - Merge back to all Afro data
# NOTE: Select code adapted from Loren Collingwood (https://www.collingwoodresearch.com/)

rm(list=ls())

library(Hmisc)
library(foreign)
library(descr)
library(reshape)
library(plyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(modeest)
library(xtable)
library(stargazer)
library(rpart)
library(arm)
library(data.table)
library(dummies)
library(Amelia)
library(MASS)
library(nlme)
library(verification)
library(simcf)
library(tile)
library(Zelig)
library(cluster)
library(ZeligChoice)
library(RColorBrewer)

#####################
### LOAD PACKAGES ###
#####################

# Function to check and install CRAN packages 
check.packages <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# Check to see if packages are installed, and then load them
packages<-c("Hmisc", "stringr", "DescTools")

# Load packages
suppressWarnings( check.packages(packages) )

##########################################
### LOAD AFROBAROMETER ROUND 6 DATASET ###
##########################################

suppressWarnings(
  afro_base <- spss.get("http://afrobarometer.org/sites/default/files/data/round-6/merged_r6_data_2016_36countries2.sav", use.value.labels=FALSE)
)

data <- afro_base

data$sub_saharan <- 1
data$sub_saharan[data$COUNTRY=="1" | data$COUNTRY=="9" | data$COUNTRY=="33" | data$COUNTRY=="20"] <- 0 # Drop Algeria, Egypt, Tunisia, Morocco

# data <- subset(afro, select=c(RESPNO, COUNTRY, URBRUR, Combinwt, sub_saharan,
#                               Q52H, Q52J, Q52I, Q42A, Q42B, Q42C, #DVs
#                               Q72, Q55I, Q55J, Q55K, Q55L, Q73A, Q73B, Q73C, Q73D, Q73E, Q74A, Q74B, #IVs
#                               Q49, Q50, Q51A, Q51B, Q51C, Q51D, EA.FAC.C, EA.SEC.A, EA.SEC.B, EA.SEC.C, #Controls
#                               Q8A, Q8B, Q8C, Q8D, Q8E, #Q8A-F: measure of poverty / scarcity
#                               
#                               Q1, Q100, Q101, Q4A, Q4B, Q5,
#                               EA.SVC.A, EA.SVC.B, EA.SVC.C, EA.SVC.D,
#                               Q88A, Q87, Q64, Q45D, Q24E, 
#                               Q53A, Q53B, Q53C, Q53D, 
#                               Q53E, Q53F, Q53G, Q53H,
#                               Q52A, Q52B, Q52C, Q52D, Q52E, Q52F, Q52G, Q52H, Q52I, Q52J, Q52K, Q52L,
#                               Q97
# ))


######################################
# Recoding Datars ####################
######################################

func.recode <- function(var)
{
  var[var==-1 | var==9 | var==98 | var==99] <- NA
  return(var)
}

########### DV: TRUST ############# 
#0: not at all, 3: a lot
# trust_pres: NAs; trust_pres2: "Don't know" coded as 1.5

data$trust_pres <- func.recode(data$Q52A)
data$trust_par <- func.recode(data$Q52B)
data$trust_elect <- func.recode(data$Q52C)
data$trust_tax <- func.recode(data$Q52D)
data$trust_local <- func.recode(data$Q52E)
data$trust_police <- func.recode(data$Q52H)
data$trust_army <- func.recode(data$Q52I)
data$trust_courts <- func.recode(data$Q52J)
data$trust_trad <- func.recode(data$Q52K)
data$trust_relig <- func.recode(data$Q52L)

data$trust_state_inst <- data$trust_pres + data$trust_par + data$trust_elect + data$trust_tax +  data$trust_local + data$trust_police + data$trust_army + data$trust_courts 

data$trust_state_inst_no_police <- data$trust_pres + data$trust_par + data$trust_elect + data$trust_tax + data$trust_local + data$trust_army + data$trust_courts 

data$trust_state_inst_no_army <- data$trust_pres + data$trust_par + data$trust_elect + data$trust_tax + data$trust_local + data$trust_police + data$trust_courts 

data$trust_state_inst_no_courts <- data$trust_pres + data$trust_par + data$trust_elect + data$trust_tax + data$trust_local + data$trust_army + data$trust_police 

######### SKD START HERE ###########
### I will start cleaning here in the morning ###

########### DV: Legitimacy of state institutions ############# 
#1: strongly disagree, 5: strongly agree
# courts_right: NAs; courts_right2: "Don't know" coded as 3

#Q42A courts have the right to make decisions that people must abide by

data$courts_right <- func.recode(data$Q42A) #Q42A courts have the rt to make decisions that people must abide by
data$police_right <- func.recode(data$Q42B) #Q42B Police have right to make people obey law
data$taxes_right <- func.recode(data$Q42C) #Q42C Tax authorities have the right to make people pay taxes

# Composites for the "legitimacy" of state institutions
data$institutions_right <- data$police_right + data$courts_right + data$taxes_right
data$inst_right_comp_no_courts <- data$police_right + data$taxes_right
data$inst_right_comp_no_police <- data$courts_right + data$taxes_right

#############################
########### IVs #############
#############################

########### IV 2: Assistance from police ###############
# Q55I: Have you requested assistance from the police in 12 mo? If yes: was it easy or difficult to obtain assistance?

# Dummy: No Contact with Police (0: contact, 1: no contact)
data$no_contact_police <- 0
data$no_contact_police[data$Q55I==7] <- 1
data$no_contact_police[data$Q55I==9 | data$Q55I==-1] <- NA

# Dummy: Easy to obtain assistance from police (0: no contact or hard to obtain assistance, 1: easy to obtain assistance)
data$police_assistance_easy <- 0
data$police_assistance_easy[data$Q55I==1 | data$Q55I==2] <- 1
data$police_assistance_easy[data$Q55I==9 | data$Q55I==-1] <- NA

# Dummy: Difficult to obtain assistance from police (0: no contact or easy to obtain assistance, 1: hard to obtain assistance)
data$police_assistance_hard <- 0
data$police_assistance_hard[data$Q55I==3 | data$Q55I==4] <- 1
data$police_assistance_hard[data$Q55I==9 | data$Q55I==-1] <- NA

# Scale: Difficulty (subset, only includes those who requested assistance) 1=very easy, 4=very difficult
data$police_assist_scale <- data$Q55I
data$police_assist_scale[data$police_assist_scale==7 | data$police_assist_scale==9 | data$police_assist_scale==-1] <- NA
table(data$police_assist_scale)
table(data$Q55I)

# Police Bribe Variable: 0=no bribe, 1= bribe 1-2 times, 2=bribe a few times, 3=bribe often, 7=no police
data$police_bribe <- data$Q55J
data$police_bribe[data$police_bribe==-1 | data$police_bribe==9] <- NA
table(data$police_assistance_hard, data$police_bribe) # more likely to say it was hard to get assistance if you had to bribe

########### IV 3: Positive or Negative Experience with Courts ###############
# Q72 and Q73A-E

### Some people contradicted themselves in Q72 and Q73A-E
### NOTE: these are slightly different in full dataset (Sept 2017); irrelevant for now
testdf <- subset(data, data$Q72==0 & data$Q73B!=7)
table(testdf$Q73B)
testdf2 <- subset(data, data$Q72!=0 & data$Q73A==7) # in theory, this could be people who had a family member who encountered
# courts in last 5 years (therefore answered something other than "none" in 72) but didn't have encounters w the courts themseles
# (therefore answered "none" in 73B)
CrossTable(testdf2$COUNTRY) # 40% of fuckups are from Ghana and 20% are from Nigeria
# for now, drop Q72 and just stick w Q73

## CQ73A-E: recoding variables for court experience
data$q73a <- data$Q73A
data$q73a[data$q73a==-1] <- NA
data$q73a[data$q73a==7] <- 100
data$expensive <- data$expensive2 <- data$q73a
data$expensive[data$expensive==9] <- NA
data$expensive2[data$expensive2==9] <- 1.5

data$q73b <- data$Q73B
data$q73b[data$q73b==-1] <- NA
data$q73b[data$q73b==7] <- 100
data$complex <- data$complex2 <- data$q73b
data$complex[data$complex==9] <- NA
data$complex2[data$complex2==9] <- 1.5

data$q73c <- data$Q73C
data$q73c[data$q73c==-1] <- NA
data$q73c[data$q73c==7] <- 100
data$no_advice <- data$no_advice2 <- data$q73c
data$no_advice[data$no_advice==9] <- NA
data$no_advice2[data$no_advice2==9] <- 1.5

data$q73d <- data$Q73D
data$q73d[data$q73d==-1] <- NA
data$q73d[data$q73d==7] <- 100
data$judge_no_listen <- data$judge_no_listen2 <- data$q73d
data$judge_no_listen[data$judge_no_listen==9] <- NA
data$judge_no_listen2[data$judge_no_listen2==9] <- 1.5

data$q73e <- data$Q73E
data$q73e[data$q73e==-1] <- NA
data$q73e[data$q73e==7] <- 100
data$delays <- data$delays2 <- data$q73e
data$delays[data$delays==9] <- NA
data$delays2[data$delays2==9] <- 1.5

#################################################
#### Scales and Dummies for Court Experience ####
#### Don't know (9) coded as NA #################
#################################################

data$court_exp_scale <- data$expensive + data$complex + data$no_advice + data$judge_no_listen + data$delays
data$court_exp_scale_sub <- data$complex + data$judge_no_listen + data$delays

data$court_exp_scale_sub[data$court_exp_scale_sub==103] <- 3
data$court_exp_scale_sub[data$court_exp_scale_sub==300] <- NA
table(data$court_exp_scale_sub)

### Fixing data based on what makes logical sense...
data$court_exp_scale[data$court_exp_scale==100] <- 0
data$court_exp_scale[data$court_exp_scale==104] <- 4
data$court_exp_scale[data$court_exp_scale==109] <- 9

# Dummy: Negative court experience (0=no neg court experience, 1=negative court experience)
data$neg_court_exp <- data$court_exp_scale
data$neg_court_exp <- ifelse (data$neg_court_exp > 0 & data$neg_court_exp < 16, 1, 0)

# Dummy: NO Negative court experience (0=neg court experience or no court experience, 1=no negative court experience)
data$pos_court_exp <- data$court_exp_scale
data$pos_court_exp <- ifelse (data$pos_court_exp==0,1,0)

# Dummy: NO Court experience (0=neg or pos court experience, 1=no court experience)
data$no_court_exp <- data$court_exp_scale
data$no_court_exp <- ifelse (data$no_court_exp > 399,1,0)

# Scale for court experience: 0=no neg experience, 1-15=negative experience, NA=no court experience
data$court_exp_scale[data$court_exp_scale > 399] <- NA

# Logged scale variable
data$court_exp_scale_log <- log(data$court_exp_scale + 1)


#################################################
#### Scales and Dummies for Court Experience ####
#### Don't know (9) coded as 1.5 ################
#################################################

data$court_exp_scale2 <- data$expensive2 + data$complex2 + data$no_advice2 + data$judge_no_listen2 + data$delays2

### Fixing data based on what makes logical sense...
data$court_exp_scale2[data$court_exp_scale2==100] <- 0
data$court_exp_scale2[data$court_exp_scale2==104] <- 4
data$court_exp_scale2[data$court_exp_scale2==109] <- 9

# Dummy: Negative court experience (0=no neg court experience, 1=negative court experience)
data$neg_court_exp2 <- data$court_exp_scale2
data$neg_court_exp2 <- ifelse (data$neg_court_exp2 > 0 & data$neg_court_exp2 < 16, 1, 0)

# Dummy: NO Negative court experience (0=neg court experience or no court experience, 1=no negative court experience)
data$pos_court_exp2 <- data$court_exp_scale2
data$pos_court_exp2 <- ifelse (data$pos_court_exp2==0,1,0)

# Dummy: NO Court experience (0=neg or pos court experience, 1=no court experience)
data$no_court_exp2 <- data$court_exp_scale2
data$no_court_exp2 <- ifelse (data$no_court_exp2 > 399,1,0)

# Scale for court experience: 0=no neg experience, 1-15=negative experience, NA=no court experience
data$court_exp_scale2[data$court_exp_scale2 > 399] <- NA

# Logged scale variable
data$court_exp_scale_log2 <- log(data$court_exp_scale2 + 1)


########### IV 1: Contact w courts ###############
# Q55K: Have you had contact with Court in 12 mo? If yes: was it easy or difficult to obtain assistance?

# Dummy: No Contact with Courts (0: contact, 1: no contact)
data$no_contact_courts <- 0
data$no_contact_courts[data$Q55K==7] <- 1
data$no_contact_courts[data$Q55K==9] <- NA

# Dummy: Easy to obtain assistance from courts (0: no contact or hard to obtain assistance, 1: easy to obtain assistance)
data$courts_assist_easy <- 0
data$courts_assist_easy[data$Q55K==1 | data$Q55K==2] <- 1
data$courts_assist_easy[data$Q55K==9] <- NA

# Dummy: Difficult to obtain assistance from courts (0: no contact or easy to obtain assistance, 1: hard to obtain assistance)
data$courts_assist_hard <- 0
data$courts_assist_hard[data$Q55K==3 | data$Q55K==4] <- 1
data$courts_assist_hard[data$Q55K==9] <- NA

# Scale: Difficulty (subset, only includes those who had contact) 1=very easy, 4=very difficult
data$courts_assist_scale <- data$Q55K
data$courts_assist_scale[data$courts_assist_scale==7 | data$courts_assist_scale==9] <- NA

# Court Bribe Variable: 0=no bribe, 1= bribe 1-2 times, 2=bribe a few times, 3=bribe often, 7=no courts
data$court_bribe <- data$Q55L
data$court_bribe[data$court_bribe==-1 | data$court_bribe==9] <- NA
table(data$courts_assist_hard, data$court_bribe) # more likely to say it was hard to get assistance if you had to bribe



##################################
########### Controls #############
##################################

### Female ###
data$female <- data$Q101
data$female[data$Q101==1] <- 0
data$female[data$Q101==2] <- 1

### Urban ###
data$urban <- data$URBRUR
data$urban[data$URBRUR==2] <- 0
data$urban[data$URBRUR==3] <- 0.5 # Botswana, guessing that 3 is semi-urban
data$urban[data$URBRUR==460] <- 0.5 # Malawi, 460 represents "peri-urban" spaces

### age ###
data$age <- data$Q1
data$age[data$age > 97 | data$Q1 < 0] <- NA
data$age2 <- data$age^2

### education ###
data$educ <- data$Q97
data$educ[data$educ == -1 | data$educ == 98 | data$educ == 99 ] <- NA # could list 99 (don't know/can't read) as 0

### Scarcity ###
# how often have you gone without __ in the last year: 0=never, 4=always
# var2: Don't know coded as 2 (several times) 

data$scar <- data$Q8A # food
data$scar[data$scar==-1] <- NA
data$food_scar <- data$scar   
data$food_scar[data$food_scar==9] <- NA
data$food_scar2 <- data$scar
data$food_scar2[data$food_scar2==9] <- 2

data$scar <- data$Q8B # water
data$scar[data$scar==-1] <- NA
data$water_scar <- data$scar   
data$water_scar[data$water_scar==9] <- NA
data$water_scar2 <- data$scar
data$water_scar2[data$water_scar2==9] <- 2

data$scar <- data$Q8C # medical care
data$scar[data$scar==-1] <- NA
data$med_scar <- data$scar  
data$med_scar[data$med_scar==9] <- NA
data$med_scar2 <- data$scar
data$med_scar2[data$med_scar2==9] <- 2

data$scar <- data$Q8D # cooking fuel
data$scar[data$scar==-1] <- NA
data$fuel_scar <- data$scar  
data$fuel_scar[data$fuel_scar==9] <- NA
data$fuel_scar2 <- data$scar
data$fuel_scar2[data$fuel_scar2==9] <- 2

data$scar <- data$Q8E # cash income
data$scar[data$scar==-1] <- NA
data$income_scar <- data$scar  
data$income_scar[data$income_scar==9] <- NA
data$income_scar2 <- data$scar
data$income_scar2[data$income_scar2==9] <- 2

### Scarcity scale (Don't know=NA)
data$scarcity <- data$food_scar + data$water_scar + data$med_scar + data$fuel_scar + data$income_scar
data$scarcity[data$scarcity==100] <- NA 

### Scarcity scale (Don't know=2)
data$scarcity2 <- data$food_scar2 + data$water_scar2 + data$med_scar2 + data$fuel_scar2 + data$income_scar2

### Living Conditions ###
# Rate your living conditions compared to others in your country
# 1: much worse, 3: same, 5: much better
data$rel_living_conditions <- data$Q5
data$rel_living_conditions[data$rel_living_conditions==-1 | data$rel_living_conditions2==9] <- NA
data$rel_living_conditions2 <- data$Q5
data$rel_living_conditions2[data$rel_living_conditions2==-1 ] <- NA
data$rel_living_conditions2[data$rel_living_conditions2==9 ] <- 3

### Why no courts? ### 
data$why_no_courts <- data$Q74A
data$why_no_courts2 <- data$Q74B

### public goods ###
# public_goods: "Don't know" coded as NA
# public_goods2: "Don't know" coded as 0
data$electricity <- data$electricity2 <- data$EA.SVC.A
data$electricity[data$electricity==9] <- NA
data$electricity2[data$electricity2==9] <- 0

data$water <- data$water2 <- data$EA.SVC.B
data$water[data$water==9] <- NA
data$water2[data$water2==9] <- 0

data$sewage <- data$sewage2 <- data$EA.SVC.C
data$sewage[data$sewage==9] <- NA
data$sewage2[data$sewage2==9] <- 0

data$cell <- data$cell2 <- data$EA.SVC.D
data$cell[data$cell==9] <- NA
data$cell2[data$cell2==9] <- 0

data$public_goods <- data$electricity + data$water  + data$sewage  + data$cell
data$public_goods2 <- data$electricity2 + data$water2  + data$sewage2  + data$cell2

# how often is your ethnic group treated unfairly by the government? 
# 0=never, 3=always
# 7=NA, 9=Don't know, 98=refused
# ethnic_persec: "Don't know" coded as NA (total NAs: 7072)
# ethnic_persec: "Don't know" coded as 1.5 (total NAs: 5745)
# LOTS OF OBSERVATIONS LOST HERE
data$q88a <- data$Q88A
data$q88a[data$q88a==7 | data$q88a==-1 | data$q88a==98] <- NA
data$ethnic_persec <- data$ethnic_persec2 <- data$q88a
data$ethnic_persec[data$ethnic_persec==9] <- NA
data$ethnic_persec2[data$ethnic_persec2==9] <- 1.5
#sub <- subset(data, is.na(ethnic_persec2))
#unique(sub$country)

### Composite: Support for political opposition
# NAs: 1645 (all in Burkina and Swaziland)
#Q64: Agree/Disagree: The political opposition in xx presents a viable alternative vision and plan for the country.
data$opp_viable <- data$Q64
data$opp_viable[data$opp_viable==9] <- 0 #only interested in 4 or 5 (agree, strongly agree)

#Q45D: In your opinion, how often, in this country are opposition parties or their supporters silenced by the government?
data$opp_silenced <- data$Q45D
data$opp_silenced[data$opp_silenced==9] <- 0  #only interested in 2 or 3 (often, always)

data$support_opp <- ifelse(data$opp_viable > 3 & data$opp_silenced > 1, 1, 0)

### Composite: Support for traditional leader
# how many times have you visited a traditional leader to solve an important problem in the last 12 months?
data$traditional <- data$traditional2 <- data$Q24E
data$traditional[data$traditional==-1 | data$traditional==9] <- NA
data$traditional2[data$traditional2==-1] <- NA
data$traditional2[data$traditional2==9] <- 1.5

data$trad1 <- ifelse(data$Q74A==18, 2, 0)
data$trad2 <- ifelse(data$Q74B==18, 1, 0)

data$trad_comp <- data$traditional + data$trad1 + data$trad2
data$trad_comp2 <- data$traditional2 + data$trad1 + data$trad2

table(data$trad_comp)

table(data$traditional)

### Q49 Fear political violence / intimidation ###
# During election campaigns in this country, how much do you personally fear becoming a victim of political intimidation or violence?
# vlaues (new): 0=not at all, 3=a lot
table(data$Q49) #OLD: 0=a lot, 3=not at all
data$q49 <- data$Q49
data$q49[data$q49==-1] <- NA
data$fear_pol_violence <- data$fear_pol_violence2 <- data$q49
data$fear_pol_violence[data$fear_pol_violence==9] <- NA
data$fear_pol_violence <- abs(data$fear_pol_violence - 3)
data$fear_pol_violence2[data$fear_pol_violence2==9] <- 1.5
data$fear_pol_violence2 <- abs(data$fear_pol_violence2 - 3)
table(data$fear_pol_violence) # vlaues (new): 0=not at all, 3=a lot

### Q51A How often, in this country: do people have to be careful of what they say about politics?
# 0=Never, 1=Rarely, 2=Often, 3=Always, 9=Don’t know, 98=Refused to answer, -1=Missing
data$q51a <- data$Q51A
data$q51a[data$q51a==-1] <- NA
data$careful_talk_politics <- data$careful_talk_politics2 <- data$q51a
data$careful_talk_politics[data$careful_talk_politics==9] <- NA
data$careful_talk_politics2[data$careful_talk_politics2==9] <- 1.5

### Q53E Police corruption 0=none corrupt; 3=all corrupt
data$police_corrupt <- data$Q53E
data$police_corrupt[data$police_corrupt==-1] <- NA

######## Additional recoding Oct 2017 ###########

data$judge_dummy <- NA
data$judge_dummy[data$judge_no_listen == 0 | data$judge_no_listen == 100 ] <- 0
data$judge_dummy[data$judge_no_listen == 1 | data$judge_no_listen == 2 | data$judge_no_listen == 3] <- 1

# EA.FAC.C: police station present: 0 (no), 1 (yes), 9 (can't determine)

## dummy variable: police station present
## 0: no or don't know (572 DK), 1: yes
data$police.station <- rep(0, dim(data)[1])
data$police.station[data$EA.FAC.C==1] <- 1

## dummy variable: interviewer saw a police person
## 0: no or don't know (20 DK), 1: yes
data$saw.police <- rep(0, dim(data)[1])
data$saw.police[data$EA.SEC.A==1] <- 1

cor(data$police.station, data$saw.police) # .48
cor(data$EA.FAC.C, data$EA.SEC.A) # .23

data$traditional[data$traditional==99] <- NA

### MERGE W WORLD BANK RULE OF LAW DATA
setwd("~/Dropbox/Current_Projects/MLake_SDreier_Project/sarah_data_analysis")

wb_data0 <- read.csv("data-raw/wb_wgi_ssa.csv", header = TRUE) 

wb_data0 <- wb_data0 %>%
  mutate(rl.2005 = X2005..YR2005.,
         rl.2010 = X2010..YR2010., 
         rl.2015 = X2015..YR2015.
  )

with(wb_data0, all.equal(rl.2015, X2015..YR2015.)) #check coding

wb_sub <- subset(wb_data0, Series.Code=="RL.EST")
wb_data <- data.frame(wb_sub[,1], wb_sub[,22:24])

wb_data$country <- wb_data[,1]

with(wb_data, all.equal(country, wb_sub...1.)) #check coding

merge_data <- merge(data, wb_data, by.x.y="Country", all=F) ### THIS DROPS NORTH AFRICA

data <- merge_data
all.equal(  sort(unique(merge_data$country)), sort(unique(data$country))    )

# THIS REMOVES LITTLE ISLANDS
data <- subset(data, COUNTRY!=7 & COUNTRY!=19 & COUNTRY!=25 )

write.csv(data, "data/afrob6_courts_police.csv")
