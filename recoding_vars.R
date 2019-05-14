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

##########################################
### LOAD AFROBAROMETER ROUND 6 DATASET ###
##########################################

suppressWarnings(
  afro_base <- spss.get("http://afrobarometer.org/sites/default/files/data/round-6/merged_r6_data_2016_36countries2.sav", use.value.labels=FALSE)
)

data <- afro_base
#afro_old <- read.csv("data/afrob6_courts_police.csv", header = TRUE) #n=45,541

# Subset to mainland sub-saharan Africa
data <- subset(data, COUNTRY!=7 & COUNTRY!=19 & COUNTRY!=25 & # Drop micro islands Cape Verde, Mauritius, São Tomé and Príncipe
                 COUNTRY!=1 & COUNTRY!=9 & COUNTRY!=20 & COUNTRY!=33) # Drop N.Africa (Algeria, Egypt, Morocco, Tunisia)

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

data$court_exp_scale <- with(data,  
       expensive + complex + no_advice + judge_no_listen + delays)

# Dummy: Negative court experience (0=no neg court experience, 1=negative court experience)
data$neg_court_exp <- ifelse (data$court_exp_scale > 0 & data$court_exp_scale < 16, 1, 0)

# Dummy: NO Negative court experience (0=neg court experience or no court experience, 1=no negative court experience)
data$pos_court_exp <- ifelse (data$court_exp_scale==0, 1, 0)

# Dummy: NO Court experience (0=neg or pos court experience, 1=no court experience)
data$no_court_exp <- ifelse (data$court_exp_scale > 15, 1, 0)

# Scale for court experience: 0=no neg experience, 1-15=negative experience, NA=no court experience
data$court_exp_scale[data$court_exp_scale > 15] <- NA

##################################
########### Controls #############
##################################

### Female ###
data$female <- ifelse(data$Q101==1, 0, 1) # Dummy: No contact=1, retains NAs

### Urban ###
data$urban <- ifelse(data$URBRUR==2, 0, ifelse(data$URBRUR>2, .5, 1)) #3 semi-urban (Botswana); 460 peri-urban (Malawi)

### Age ###
data$age <- ifelse(data$Q1 > 97 | data$Q1 < 0, NA, data$Q1)

### Education ###
data$educ <- ifelse(data$Q97 == -1 | data$Q97 > 97, NA, data$Q97) 


### Scarcity ###
# how often have you gone without __ in the last year: 0=never, 4=always

data$food_scar <- func.recode(data$Q8A)
data$water_scar <- func.recode(data$Q8B)
data$med_scar <- func.recode(data$Q8C)
data$fuel_scar <- func.recode(data$Q8D)
data$income_scar <- func.recode(data$Q8E)

data$scarcity <- with(data, food_scar + water_scar + med_scar + fuel_scar + income_scar)

### Living Conditions ###
# Rate your living conditions compared to others in your country
# 1: much worse, 3: same, 5: much better
data$rel_living_conditions <- func.recode(data$Q5)

### public goods ###
# public_goods: "Don't know" coded as NA
data$electricity <- func.recode(data$EA.SVC.A)
data$water <- func.recode(data$EA.SVC.B)
data$sewage <- func.recode(data$EA.SVC.C)
data$cell <- func.recode(data$EA.SVC.D)
data$public_goods <- with(data, electricity + water + sewage + cell)

# how often is your ethnic group treated unfairly by the government? 
# 0=never, 3=always
# 7=NA, 9=Don't know, 98=refused
# LOTS OF OBSERVATIONS LOST HERE; not asked in BDI or SUD

data$ethnic_persec <- func.recode(data$Q88A)
data$ethnic_persec[data$ethnic_persec==7] <- NA

### Composite: Support for political opposition
# NAs: 1645 (all in Burkina and Swaziland)
#Q64: Agree/Disagree: The political opposition in xx presents a viable alternative vision and plan for the country.

data$opp_viable <- func.recode(data$Q64)

#Q45D: In your opinion, how often, in this country are opposition parties or their supporters silenced by the government?
data$opp_silenced <- func.recode(data$Q45D)

data$support_opp <- ifelse(data$opp_viable > 3 & data$opp_silenced > 1, 1, 0)

### Composite: Support for traditional leader
# how many times have you visited a traditional leader to solve an important problem in the last 12 months?
data$traditional <- func.recode(data$Q24E)

### Q49 Fear political violence / intimidation ###
# During election campaigns in this country, how much do you personally fear becoming a victim of political intimidation or violence? # vlaues (new): 0=not at all, 3=a lot
data$fear_pol_violence <- func.recode(data$Q49)
data$fear_pol_violence <- abs(data$fear_pol_violence - 3)
  
### Q51A How often, in this country: do people have to be careful of what they say about politics?
# 0=Never, 1=Rarely, 2=Often, 3=Always, 9=Don’t know, 98=Refused to answer, -1=Missing

data$careful_talk_politics <- func.recode(data$Q51A)

# EA.FAC.C: police station present: 0 (no), 1 (yes), 9 (can't determine)
## dummy variable: police station present
## 0: no or don't know (572 DK), 1: yes

data$police.station <- ifelse(data$EA.FAC.C==1, 1, 0)

data_sub <- data.frame(data[,1:2], data[,353:354], data[,368:425])

write.csv(data_sub, "Repo2/afro_courts_police_052019.csv")







#################### NOT USED? #######################

### Q53E Police corruption 0=none corrupt; 3=all corrupt
data$police_corrupt <- data$Q53E
data$police_corrupt[data$police_corrupt==-1] <- NA

######## Additional recoding Oct 2017 ###########

data$judge_dummy <- NA
data$judge_dummy[data$judge_no_listen == 0 | data$judge_no_listen == 100 ] <- 0
data$judge_dummy[data$judge_no_listen == 1 | data$judge_no_listen == 2 | data$judge_no_listen == 3] <- 1

#data$trad1 <- ifelse(data$Q74A==18, 2, 0) # 18: Trad leaders is #1 reason people don't go to courts 
#data$trad2 <- ifelse(data$Q74B==18, 1, 0) # 18: Trad reason is #2 reason people don't go to courts 
#data$trad_comp <- with(data, traditional + trad1 + trad2)

## dummy variable: interviewer saw a police person
## 0: no or don't know (20 DK), 1: yes
data$saw.police <- rep(0, dim(data)[1])
data$saw.police[data$EA.SEC.A==1] <- 1

cor(data$police.station, data$saw.police) # .48
cor(data$EA.FAC.C, data$EA.SEC.A) # .23
