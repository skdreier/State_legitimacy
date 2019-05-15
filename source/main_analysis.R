############################################
#### Replication Code for:              ####
#### Dreier and Lake (Democratization)  ####
####                                    ####
#### MAIN ANALYSIS                      ####
####                                    ####
#### R version 3.4.4 (2018-03-15)       ####
#### DATE: 5/14/2019                    ####
############################################

# Content:
# - Develop main models
# - Plot main models

# Clear directory
rm(list=ls())

# Function to check and install CRAN packages 
check.packages <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# Check to see if packages are installed, and then install them
packages <- c("broom", "ggplot2", "lme4", "lmtest", "magrittr", "MASS", "multiwayvcov",
            "simcf", "stargazer", "Zelig", "ZeligChoice")

# Load packages
suppressWarnings( check.packages(packages) )
 
# Load source for mv_extract (written by Dr. Loren Collingwood)
source("source/multiplot_code_lc.R")

# Load data
load(file="afro_courts_police.RData")

# Create country fixed effects
dummy_data <- dummy(data$COUNTRY)
data <- data.frame(data,dummy_data)

###################################################################################
#### COURT SCALE MODELS AND MAIN PLOT (subset to those with court experiences) ####
###################################################################################

### Court model 1a: Trust ~ exp (scale)
model <- formula(  as.factor(trust_courts) ~ court_exp_scale + trust_state_inst_no_courts + 
                      careful_talk_politics + fear_pol_violence +  traditional +
                      age + female + educ + rel_living_conditions + 
                      urban + scarcity + public_goods + police_station + Combinwt +
                      as.factor(COUNTRY)
)

# Extract data
mdata <- extractdata(model, data, na.rm=TRUE) 

# Run model (stargazer)
ologit.1a <- polr(model, mdata, method="logistic", Hess=TRUE) 

# Set court range (simulation)
court_range <- seq(min(data$court_exp_scale, na.rm=T), 
                   max(data$court_exp_scale, na.rm=T), length.out=100) 

# Run model and prep simulation data
  z_mod <- zelig (model, data=mdata, model = "ologit")  
  x.out <- setx(z_mod, court_exp_scale = court_range) # est court exp range
  s.out <- Zelig::sim(z_mod, x=x.out) # calculate exp values 
  plotdata <- mv_extract(s.out) # extract evs (low, high, mean) into dataframe
  plotdata1 <- plotdata[seq(1,nrow(plotdata), 4),] # pull people with no trust
  plotdata4 <- plotdata[seq(4,nrow(plotdata), 4),] # pull people with high trust
  colnames(plotdata4) <- paste(colnames(plotdata4),col="_4", sep="") # indicate columns w high trust
  plotdata_trust <- data.frame(plotdata1, plotdata4, court_exp_scale = court_range) # merge no/high in one dataframe

### Court model 1b: Right ~ exp (scale)
model <- formula(  as.factor(courts_right) ~  
                     court_exp_scale + inst_right_comp_no_courts + 
                     careful_talk_politics + fear_pol_violence + traditional + 
                     age + female + educ + rel_living_conditions + 
                     urban + scarcity + public_goods + police_station + Combinwt +
                     as.factor(COUNTRY)
)

mdata <- extractdata(model, data, na.rm=TRUE) # extract
ologit.1b <- polr(model, mdata, method="logistic", Hess=TRUE) # model (stargazer)

# Run model and prep simulation data
  z_mod <- zelig (model, data=mdata, model = "ologit") # model (simulation)
  x.out <- setx(z_mod, court_exp_scale = court_range) # est range
  s.out <- Zelig::sim(z_mod, x=x.out) # calculate evs
  plotdata <- mv_extract(s.out)
  plotdata1 <- plotdata[seq(1,nrow(plotdata), 5),] # pull people with lowest legit
  plotdata5 <- plotdata[seq(5,nrow(plotdata), 5),] # pull people with highest legit
  colnames(plotdata5) <- paste(colnames(plotdata5),col="_5", sep="") # indicate columns w high legit
  plotdata_legit <- data.frame(plotdata1, plotdata5, court_exp_scale = court_range) # merge low/high in one dataframe

# Merge right and legit plotdata into one dataframe 
colnames(plotdata_trust) <- paste(colnames(plotdata_trust),col="_trust", sep="") # indicate trust columns
colnames(plotdata_legit) <- paste(colnames(plotdata_legit),col="_legit", sep="") # indicate legit columns
plotdata_all <- data.frame(plotdata_trust, plotdata_legit, court_exp_scale = court_range)

### Plot models 1a (trust) and 1b (legit)
xvar <- xlab_name <- "Negative Court Experiences" #label x-axis

ggplot(data=plotdata_all, aes(x = court_exp_scale)) + ylim(0,.25) + 
  
  # plot trust simulations
  geom_line(aes(y = mean_trust)) + 
  geom_line(aes(y = high_trust), linetype="dashed", color="red") + 
  geom_line(aes(y = low_trust), linetype="dashed", color="red") + 
  theme_minimal() +
  geom_ribbon(aes(ymin=low_trust, ymax=high_trust), alpha=0.1) +
  
  # plot legit simulations
  geom_line(aes(y = mean_legit)) + 
  geom_line(aes(y = high_legit), linetype="dashed", color="blue") + 
  geom_line(aes(y = low_legit), linetype="dashed", color="blue") + 
  theme_minimal() +
  geom_ribbon(aes(ymin=low_legit, ymax=high_legit), alpha=0.1) +
  
  labs(title = paste("Effect of", xvar, sep=" "),
    subtitle = paste("on respondents' trust in--and reported legitimacy of--govt courts"),
    x = paste(xlab_name, "(0-15)", sep=" "),
    y = ""
  ) +
  
  annotate("text", x = 12, y=.23, label = "Pr( No Trust in Courts )", col="red", cex=3.5) +
  annotate("text", x = 12, y=.22, label = "(p<.01)", col="red", cex=3) +
  annotate("text", x = 12, y=.04, label = "Pr( No Legitimacy of Courts )", col="blue", cex=3.5) +
  annotate("text", x = 12, y=.03, label = "(not significant)", col="blue", cex=3) 

ggsave("figures/courts_scale.png", device="png")

### Court model 1c: Trust army ~ court exp (scale) -- robustness check
model <- formula(  as.factor(trust_army) ~ court_exp_scale + trust_state_inst_no_army +
                     careful_talk_politics + fear_pol_violence +
                     age + female + educ + rel_living_conditions + traditional +
                     urban + scarcity + public_goods + police_station + Combinwt +
                     as.factor(COUNTRY)
)

mdata <- extractdata(model, data, na.rm=TRUE) # extract
ologit.1c <- polr(model, mdata, method="logistic", Hess=TRUE) # model (stargazer)

### Court model 1d: Trust police ~ court exp (scale) -- robustness check
model <- formula(  as.factor(trust_police) ~ court_exp_scale + trust_state_inst_no_police + 
                     careful_talk_politics + fear_pol_violence + 
                     age + female + educ + rel_living_conditions + traditional +
                     urban + scarcity + public_goods + police_station + Combinwt +
                     as.factor(COUNTRY)
)

mdata <- extractdata(model, data, na.rm=TRUE) # extract
ologit.1d <- polr(model, mdata, method="logistic", Hess=TRUE)  # model (stargazer)


#######################################################################################
#### POLICE SCALE MODELS AND MAIN PLOT (subset to those with experiences w police) ####
#######################################################################################

### Police model 2a: Trust ~ assist (scale)
model <- formula(  as.factor(trust_police) ~ police_assist_scale + trust_state_inst_no_police + 
                     careful_talk_politics + fear_pol_violence + traditional + 
                     age + female + educ + rel_living_conditions + 
                     urban + scarcity + public_goods + police_station + Combinwt +
                     as.factor(COUNTRY)
)

mdata <- extractdata(model, data, na.rm=TRUE) # extract
ologit.2a <- polr(model, mdata, method="logistic", Hess=TRUE) # model (stargazer)

police_range <- seq(min(data$police_assist_scale, na.rm=T), 
                    max(data$police_assist_scale, na.rm=T), length.out=100) #set police range for sims

# Run model and prep simulation data
  z_mod <- zelig (model, data=mdata, model = "ologit") 
  x.out <- setx(z_mod, police_assist_scale = police_range)
  s.out <- Zelig::sim(z_mod, x=x.out)
  plotdata <- mv_extract(s.out)
  plotdata1 <- plotdata[seq(1,nrow(plotdata), 4),] # pull people with no trust
  plotdata4 <- plotdata[seq(4,nrow(plotdata), 4),] # pull people with high trust
  colnames(plotdata4) <- paste(colnames(plotdata4),col="_4", sep="") # high trust
  plotdata_trust <- data.frame(plotdata1, plotdata4, police_assist_scale = police_range) 

### Police model 2b: Right ~ assist (scale)
model <- formula(  as.factor(police_right) ~  
                     police_assist_scale + inst_right_comp_no_police + 
                     careful_talk_politics + fear_pol_violence + traditional + 
                     age + female + educ + rel_living_conditions + 
                     urban + scarcity + public_goods + police_station + Combinwt +
                     as.factor(COUNTRY)
)

mdata <- extractdata(model, data, na.rm=TRUE) # extract
ologit.2b <- polr(model, mdata, method="logistic", Hess=TRUE) # model (stargazer)

# Run model and prep simulation data
  z_mod <- zelig (model, data=mdata, model = "ologit")
  x.out <- setx(z_mod, police_assist_scale = police_range)
  s.out <- Zelig::sim(z_mod, x=x.out)
  plotdata <- mv_extract(s.out)
  plotdata1 <- plotdata[seq(1,nrow(plotdata), 5),] # pull people with low legit
  plotdata5 <- plotdata[seq(5,nrow(plotdata), 5),] # pull people with high legit
  colnames(plotdata5) <- paste(colnames(plotdata5),col="_5", sep="") 
  plotdata_legit <- data.frame(plotdata1, plotdata5, police_assist_scale = police_range) 

# Merge right and legit plotdata into one dataframe 
colnames(plotdata_trust) <- paste(colnames(plotdata_trust),col="_trust", sep="")
colnames(plotdata_legit) <- paste(colnames(plotdata_legit),col="_legit", sep="") 
plotdata_all <- data.frame(plotdata_trust, plotdata_legit, police_assist_scale = police_range)

### Plot models 2a (trust) and 2b (legit)
xvar <- xlab_name <- "Difficulty Obtaining Police Assistance"

ggplot(data=plotdata_all, aes(x = police_assist_scale))+ ylim(0,.4)+ 
  
  # plot trust simulations
  geom_line(aes(y =mean_trust)) + 
  geom_line(aes(y =high_trust), linetype="dashed", color="red") + 
  geom_line(aes(y =low_trust), linetype="dashed", color="red") + 
  theme_minimal() +
  geom_ribbon(aes(ymin=low_trust, ymax=high_trust), alpha=0.1) +
  
  # plot legit simulations
  geom_line(aes(y =mean_legit)) + 
  geom_line(aes(y =high_legit), linetype="dashed", color="blue") + 
  geom_line(aes(y =low_legit), linetype="dashed", color="blue") + 
  theme_minimal() +
  geom_ribbon(aes(ymin=low_legit, ymax=high_legit), alpha=0.1) +
  
  labs(
    title = paste("Effect of", xvar, sep=" "),
    subtitle = paste("on respondents' trust in--and reported legitimacy of--police"),
    x = paste(xlab_name, "(4: very difficult)", sep=" "),
    y = ""
  ) +
  
  annotate("text", x = 3.5, y=.375, label = "Pr( No Trust )", col="red", cex=3.5) +
  annotate("text", x = 3.5, y=.36, label = "(p<0.01)", col="red", cex=3) +
  annotate("text", x = 3.5, y=.02, label = "Pr( No Legitimacy )", col="blue", cex=3.5) +
  annotate("text", x = 3.5, y=.003, label = "(not significant)", col="blue", cex=3) 

ggsave("figures/police_scale.png", device="png")

### Police model 2c: Trust army ~ police exp (scale) -- robustness check
model <- formula(  as.factor(trust_army) ~ police_assist_scale + trust_state_inst_no_army +
                     careful_talk_politics + fear_pol_violence + 
                     age + female + educ + rel_living_conditions + traditional +
                     urban + scarcity + public_goods + police_station + Combinwt +
                     as.factor(COUNTRY)
)

mdata <- extractdata(model, data, na.rm=TRUE) # extract
ologit.2c <- polr(model, mdata, method="logistic", Hess=TRUE) # model (stargazer)

### Police model 2d: Trust courts ~ police exp (scale) -- robustness check
model <- formula(  as.factor(trust_courts) ~ police_assist_scale + trust_state_inst_no_courts + 
                     careful_talk_politics + fear_pol_violence + 
                     age + female + educ + rel_living_conditions + traditional +
                     urban + scarcity + public_goods + police_station + Combinwt +
                     as.factor(COUNTRY)
)

mdata <- extractdata(model, data, na.rm=TRUE) # extract
ologit.2d <- polr(model, mdata, method="logistic", Hess=TRUE)  # model (stargazer)

#############################################
#### COURT DUMMY MODELS (entire dataset) ####
#############################################

### Court model 3a: Trust ~ neg court experience (dummy)
model <- formula(  as.factor(trust_courts) ~ neg_court_exp + pos_court_exp + trust_state_inst_no_courts +
                     careful_talk_politics + fear_pol_violence + traditional +
                     age + female + educ + rel_living_conditions + 
                     urban + scarcity + public_goods + police_station + Combinwt +
                     as.factor(COUNTRY)
)

mdata <- extractdata(model, data, na.rm=TRUE) # extract
ologit.3a <- polr(model, mdata, method="logistic", Hess=TRUE) # model (stargazer)

### Court model 3b: Right ~ neg court experience (dummy)
model <- formula(  as.factor(courts_right) ~  neg_court_exp + pos_court_exp + inst_right_comp_no_courts + 
                     careful_talk_politics + fear_pol_violence + traditional +
                     age + female + educ + rel_living_conditions + 
                     urban + scarcity + public_goods + police_station + Combinwt +
                     as.factor(COUNTRY)
)

mdata <- extractdata(model, data, na.rm=TRUE) # extract
ologit.3b <- polr(model, mdata, method="logistic", Hess=TRUE) # model (stargazer)

############################################################
#### POLICE DUMMY MODELS AND MAIN PLOT (entire dataset) ####
############################################################

### Police model 4a: Trust ~ police assistance hard (dummy)
model <- formula(  as.factor(trust_police) ~ police_assistance_hard + police_assistance_easy + trust_state_inst_no_police + 
                     careful_talk_politics + fear_pol_violence + traditional + 
                     age + female + educ + rel_living_conditions + 
                     urban + scarcity + public_goods + police_station + Combinwt +
                     as.factor(COUNTRY)
)

mdata <- extractdata(model, data, na.rm=TRUE) # extract
ologit.4a <- polr(model, mdata, method="logistic", Hess=TRUE) # model (stargazer)

### Police model 4a: Right ~ police assistance hard (dummy)
model <- formula(  as.factor(police_right) ~ police_assistance_hard + police_assistance_easy + inst_right_comp_no_police + 
                     careful_talk_politics + fear_pol_violence + traditional +
                     age + female + educ + rel_living_conditions + 
                     urban + scarcity + public_goods + police_station + Combinwt +
                     as.factor(COUNTRY)
)

mdata <- extractdata(model, data, na.rm=TRUE) # extract
ologit.4b <- polr(model, mdata, method="logistic", Hess=TRUE) # model "stargazer'

#######################
#### MODEL OUTPUTS ####
#######################

# Define variables for stargazer outputs
  control_vars <- c(
            "Cautious talking about politics", "Fear political violence", 
            "Use traditional leaders", "Age", "Female (binary)", 
            "Education level", "Personal living conditions", "Urban (binary)", 
            "Personal resource scarcity", "Community access to public services", 
            "Police station present (binary)", "Combinwt"
            )
  
  ct_vars_1 <- c(
            "Negative court experiences (binary)",
            "Neutral court experiences (binary)",
            "Trust in state institutions (excl courts)",
            "Institutional legitimacy (excl courts)",
            control_vars
            )
  
  ct_vars_2 <- c(
            "Negative court experiences (scale)",
             "Trust in state institutions (excl courts)",
             "Institutional legitimacy (excl courts)",
             "Trust in state institutions (excl police)",
             "Trust in state institutions (excl army)",
             control_vars
            )
  
  pol_vars_1 <- c(
            "Police assistance difficult (binary)",
            "Police assistance easy (binary)",
            "Trust in state institutions (excl police)",
            "Institutional legitimacy (excl police)",
            control_vars
            )
  
  pol_vars_2 <- c(
            "Difficulty with police assistance (scale)",
            "Trust in state institutions (excl police)",
            "Institutional legitimacy (excl police)",
            "Trust in state institutions (excl courts)",
            "Trust in state institutions (excl army)",
            control_vars
            )

# Court neg experience dummy models (entire dataset)
stargazer(ologit.3a, ologit.3b,  
          keep.stat = c("n", "aic"),
          no.space=TRUE,
          dep.var.labels.include = FALSE,
          covariate.labels= ct_vars_1,
          column.labels=c("Trust Courts", "Legit Courts"),
          title = "Effects of negative court experiences, 
                  relative to those with no court experiences (ordered logistic)",
          omit= "COUNTRY",
          notes = c("All models include country-fixed effects")
          )

# Court neg experience scale models (subset)
stargazer(ologit.1a, ologit.1b, ologit.1d, ologit.1c, 
          keep.stat = c("n", "aic"),
          no.space=TRUE,
          dep.var.labels.include = FALSE,
          covariate.labels= ct_vars_2,
          column.labels=c("Trust Courts", "Legit Courts", "Trust Police", "Trust Army"), # "Trust Instit (gen)"),
          title = "Effects of negative court experiences, 
                  among those who had encounter(s) with the courts (ordered logistic)",
          omit= "COUNTRY",
          notes = c("All models include country-fixed effects")
          )

# Police neg experience dummy models (entire dataset)
stargazer(ologit.4a, ologit.4b, 
          keep.stat = c("n", "aic"),
          no.space=TRUE,
          dep.var.labels.include = FALSE,
          covariate.labels= pol_vars_1,
          column.labels=c("Trust Police", "Legit Police"),
          title = "Effects of difficulty obtaining police assistance, 
                  relative to those with no police experiences (ordered logistic)",
          omit= "COUNTRY",
          notes = c("All models include country-fixed effects")
)

# Police neg experience scale models (subset)
stargazer(ologit.2a, ologit.2b, ologit.2d, ologit.2c,
          keep.stat = c("n", "aic"),
          no.space=TRUE,
          dep.var.labels.include = FALSE,
          covariate.labels= pol_vars_2,
          column.labels=c("Trust Police", "Legit Police", "Trust Courts", "Trust Army"),
          title = "Effects of difficulty obtaining police assistance, 
                  among those who had encounter(s) with the police (ordered logistic)",
          omit= "COUNTRY",
          notes = c("All models include country fixed effects")
)


#########################################################
########       END OF MAIN ANALYSIS SCRIPT       ########
#########################################################
