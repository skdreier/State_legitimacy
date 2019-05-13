###############################################
# Replication Code for:                       #
# Dreier and Lake (Democratization            #
#                                             #
# RECODING VARIABLES FOR ANALYSIS             #
#                                             #
# R version 3.4.4 (2018-03-15)                #
# DATE: 5/13/2019                              #
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
# 
# library(foreign)
# library(descr)
# library(reshape)
# library(plyr)
# library(dplyr)
# library(stringr)
# library(ggplot2)
# library(modeest)
# library(xtable)
# library(stargazer)
# library(rpart)
# library(arm)
# library(data.table)
# library(dummies)
# library(Amelia)
# library(MASS)
# library(nlme)
# library(verification)
# library(simcf)
# library(tile)
# library(Zelig)
# library(cluster)
# library(ZeligChoice)
# library(RColorBrewer)

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
  var[var==-1 | var>=9 ] <- NA
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

data$trust_state_inst <- with(data,  
       trust_pres + trust_par + trust_elect + trust_tax + trust_local + trust_police + trust_army + trust_courts
       ) 

data$trust_state_inst_no_police <- with(data,
       trust_pres + trust_par + trust_elect + trust_tax + trust_local + trust_army + trust_courts
       ) 

data$trust_state_inst_no_army <- with(data,
       trust_pres + trust_par + trust_elect + trust_tax + trust_local + trust_police + trust_courts
       ) 

data$trust_state_inst_no_courts <- with(data,
       trust_pres + trust_par + trust_elect + trust_tax + trust_local + trust_army + trust_police
       ) 


########### DV: Legitimacy of state institutions ############# 
#1: strongly disagree, 5: strongly agree
# courts_right: NAs; courts_right2: "Don't know" coded as 3

#Q42A courts have the right to make decisions that people must abide by

data$courts_right <- func.recode(data$Q42A) #Q42A courts have the rt to make decisions that people must abide by
data$police_right <- func.recode(data$Q42B) #Q42B Police have right to make people obey law
data$taxes_right <- func.recode(data$Q42C) #Q42C Tax authorities have the right to make people pay taxes

# Composites for the "legitimacy" of state institutions
data$institutions_right <- with(data, police_right + courts_right + taxes_right)
data$inst_right_comp_no_courts <- with(data, police_right + taxes_right)
data$inst_right_comp_no_police <- with(data, courts_right + taxes_right)

#############################
########### IVs #############
#############################

########### IV 2: Assistance from police ###############
# Q55I: Have you requested assistance from the police in 12 mo? If yes: was it easy or difficult to obtain assistance?

# Contact with police
data$police_assist <- func.recode(data$Q55I)

data$no_contact_police <- ifelse(data$police_assist==7, 1, 0) # Dummy: No contact=1, retains NAs
data$police_assistance_easy <- ifelse(data$police_assist==1 | data$police_assist==2, 1, 0) # Dummy: easy=1
data$police_assistance_hard <- ifelse(data$police_assist==3 | data$police_assist==4, 1, 0) # Dummy: hard=1
data$police_assist_scale <- ifelse(data$police_assist==7, NA, data$police_assist) # Scale, only among those who requested assistance; 1=very easy, 4=very difficult

########### IV 1: Experience with courts ###############
# Q73A: Have you encountered any of these problems in your experience with government courts in the past 5 years? 7=no expepience, 0=never, 1=once or twice, 2=a few times, 3=often, 9=DK, 98=refused, -1=missing

func.recode2 <- function(var)
{
  var[var==-1 | var>=9 ] <- NA
  var[var==7] <- 100
  return(var)
}

data$expensive <- func.recode2(data$Q73A)
data$complex <- func.recode2(data$Q73B)
data$no_advice <- func.recode2(data$Q73C)
data$judge_no_listen <- func.recode2(data$Q73D)
data$delays <- func.recode2(data$Q73E)

table(data$expensive)

data$court_exp_scale <- with(data,  
       expensive + complex + no_advice + judge_no_listen + delays)

# Dummy: Negative court experience (0=no neg court experience, 1=negative court experience)
data$neg_court_exp <- ifelse (data$court_exp_scale > 0 & data$court_exp_scale < 16, 1, 0)

# Dummy: NO Negative court experience (0=neg court experience or no court experience, 1=no negative court experience)
data$pos_court_exp <- ifelse (data$court_exp_scale==0, 1, 0)

# Dummy: NO Court experience (0=neg or pos court experience, 1=no court experience)
data$no_court_exp2 <- ifelse (data$court_exp_scale > 15, 1, 0)

# Scale for court experience: 0=no neg experience, 1-15=negative experience, NA=no court experience
data$court_exp_scale[data$court_exp_scale > 15] <- NA

## SKD stopped here 05/13 ##

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


# THIS REMOVES LITTLE ISLANDS
data <- subset(data, COUNTRY!=7 & COUNTRY!=19 & COUNTRY!=25 )

#data_sub <- data.frame(data[,1:2], data[,353:456])

data_sub <- data[1:5,368:369]

write.csv(data_sub, "afro_courts_police_052019.csv")
