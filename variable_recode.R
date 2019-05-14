############################################
#### Replication Code for:              ####
#### Dreier and Lake (Democratization)  ####
####                                    ####
#### RECODING VARIABLES FOR ANALYSIS    ####
####                                    ####
#### R version 3.4.4 (2018-03-15)       ####
#### DATE: 5/13/2019                    ####
############################################

# Content:
# - Load raw Afrobarometer data
# - Recode variables for analysis
# - Save new dataset 

rm(list=ls())

############################################
#### LOAD AFROBAROMETER ROUND 6 DATASET ####
############################################

library(Hmisc)

# Load data from afrobarometer.org
suppressWarnings(
  data_base <- spss.get("http://afrobarometer.org/sites/default/files/data/round-6/merged_r6_data_2016_36countries2.sav", use.value.labels=FALSE)
)

# Subset to mainland sub-saharan Africa
data <- subset(data_base, COUNTRY!=7 & COUNTRY!=19 & COUNTRY!=25 & 
                 COUNTRY!=1 & COUNTRY!=9 & COUNTRY!=20 & COUNTRY!=33) 
                 # Drop Algeria, Egypt, Morocco, Tunisia, Cape Verde, Mauritius, São Tomé

##################################
#### RECODE DATA FOR ANALYSIS ####
##################################

# Function to designate missing/DK/refuse as NA
func.recode <- function(var)
{
  var[var==-1 | var>=9 ] <- NA
  return(var)
}

#### DV: Trust in state institutions (0=none, 3=a lot) ####

# Apply recode function to all trust questions
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

# Aggregate trust in state institutions
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

#### DV: Legitimacy of state institutions (1=strongly disagree; 5=strongly agree) ####

# Apply recode function to all legitimacy questions
data$courts_right <- func.recode(data$Q42A) # Q42A courts have the rt to make decisions that people must abide by
data$police_right <- func.recode(data$Q42B) # Q42B Police have right to make people obey law
data$taxes_right <- func.recode(data$Q42C) # Q42C Tax authorities have the right to make people pay taxes

# Aggregate legitimacy of state institutions
data$institutions_right <- with(data, police_right + courts_right + taxes_right)
data$inst_right_comp_no_courts <- with(data, police_right + taxes_right)
data$inst_right_comp_no_police <- with(data, courts_right + taxes_right)

#### IV: Assistance from police easy or difficult to obtain ####

# Apply recode function to all police assistance questions
data$police_assist <- func.recode(data$Q55I)

# Police dummy: 1=No contact; 0=contact
data$no_contact_police <- ifelse(data$police_assist==7, 1, 0) 

# Police dummy: 1=assist easy; 0=assist hard or no contact 
data$police_assistance_easy <- ifelse(data$police_assist==1 | data$police_assist==2, 1, 0) 

# Police dummy: 1=assist hard; 0=assist easy or no contact
data$police_assistance_hard <- ifelse(data$police_assist==3 | data$police_assist==4, 1, 0) 

# Police scale: 1=assist v easy; 4=assist v hard; NA=no police contact
data$police_assist_scale <- ifelse(data$police_assist==7, NA, data$police_assist) 

#### IV: Encountered any problems in experience with courts ####

# Function to designate missing/DK/refuse as NA; recode 7 (no exp) high to filter out later
func.recode2 <- function(var)
{
  var[var==-1 | var>=9 ] <- NA
  var[var==7] <- 100
  return(var)
}

# Forms of negative court experiences
  # 0=never; 1=once or twice; 2=a few times; 3=often
  # Apply recode function to all neg exp questions
data$expensive <- func.recode2(data$Q73A)
data$complex <- func.recode2(data$Q73B)
data$no_advice <- func.recode2(data$Q73C)
data$judge_no_listen <- func.recode2(data$Q73D)
data$delays <- func.recode2(data$Q73E)

# Court scale (aggregate measure)
data$court_exp_scale <- with(data,  
       expensive + complex + no_advice + judge_no_listen + delays)

# Court dummy: 1=neg court experience; 0=positive or no court experience
data$neg_court_exp <- ifelse (data$court_exp_scale > 0 & data$court_exp_scale < 16, 1, 0)

# Court dummy: 1=positive court experience; 0=negative or no court experience
data$pos_court_exp <- ifelse (data$court_exp_scale==0, 1, 0)

# Court dummy: 1=no court experience; 0=neg or pos court experience
data$no_court_exp <- ifelse (data$court_exp_scale > 15, 1, 0)

# Scale for court experience: NAs to remove those with no court experiences
data$court_exp_scale[data$court_exp_scale > 15] <- NA


#### Controls ####

# Female
data$female <- ifelse(data$Q101==1, 0, 1) # Dummy; retains NAs

# Urban (3=semi-urban (Botswana); 460=peri-urban (Malawi))
data$urban <- ifelse(data$URBRUR==2, 0, ifelse(data$URBRUR>2, .5, 1)) 

# Age
data$age <- ifelse(data$Q1 > 97 | data$Q1 < 0, NA, data$Q1)

# Education
data$educ <- ifelse(data$Q97 == -1 | data$Q97 > 97, NA, data$Q97) 

# Scarcity (freq going without resource in last year; 0=never, 4=always)
  # Apply recode function to all scarcity questions
  data$food_scar <- func.recode(data$Q8A)
  data$water_scar <- func.recode(data$Q8B)
  data$med_scar <- func.recode(data$Q8C)
  data$fuel_scar <- func.recode(data$Q8D)
  data$income_scar <- func.recode(data$Q8E)

  # Aggregate scarcity measures
  data$scarcity <- with(data, food_scar + water_scar + med_scar + fuel_scar + income_scar)

# Living Conditions (relative to others in country; 1=much worse, 5=much better)
data$rel_living_conditions <- data$Q5
data$rel_living_conditions[data$rel_living_conditions==-1 | data$rel_living_conditions2==9] <- NA

# Public goods 
  # Apply recode function to all public good questions
  data$electricity <- func.recode(data$EA.SVC.A)
  data$water <- func.recode(data$EA.SVC.B)
  data$sewage <- func.recode(data$EA.SVC.C)
  data$cell <- func.recode(data$EA.SVC.D)
  data$public_goods <- with(data, electricity + water + sewage + cell)

# Ethnic persecution by govt (Not asked: Burundi or Sudan)
data$ethnic_persec <- func.recode(data$Q88A)
data$ethnic_persec[data$ethnic_persec==7] <- NA

# Support for political opposition

  # Pol opposition viable alternative (Not asked: Burkina Faso and Swaziland)
  data$opp_viable <- func.recode(data$Q64)

  # Opposition parties/supporters silenced by govt
  data$opp_silenced <- func.recode(data$Q45D)

  # Composite support for political opposition
  data$support_opp <- ifelse(data$opp_viable > 3 & data$opp_silenced > 1, 1, 0)

# Freq visit trad leader to solve a problem
data$traditional <- func.recode(data$Q24E)

# Fear political violence / intimidation during election
data$fear_pol_violence <- func.recode(data$Q49)
data$fear_pol_violence <- abs(data$fear_pol_violence - 3) # invert for readability

# Do people have to be careful of what they say about politics?
data$careful_talk_politics <- func.recode(data$Q51A)

# Police station present?: 0 (no/DK), 1 (yes)
data$police.station <- ifelse(data$EA.FAC.C==1, 1, 0)

###############################################
#### SUBSET AND WRITE DATASET FOR ANALYSIS ####
###############################################

# Subset to recoded variables
data_sub <- data.frame(data[,1:2], data[,353:354], data[,368:425])

# Write and save new dataset
write.csv(data_sub, "Repo_StateLegit/afro_courts_police_052019.csv")


##################################################
########       END OF RECODE SCRIPT       ########
##################################################
