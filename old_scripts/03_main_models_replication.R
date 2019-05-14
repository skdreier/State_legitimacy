####################################
######### Main Models ##############
######### Replication ##############
######### Nov 2017    ##############
####################################

rm(list=ls())

library(lmtest) #coeftest for clustered std err
library(broom) #for clustered std err
library(multiwayvcov) #for clustered std err
library(simcf) #extractdata
library(MASS) #polr to run probit models

source("~/Dropbox/Current_Projects/MLake_SDreier_Project/sarah_data_analysis/source/multiplot_code_lc.R")
library(Zelig)
library(ZeligChoice)
library(magrittr)
library(lme4)
library(ggplot2)

setwd("~/Dropbox/Current_Projects/MLake_SDreier_Project/sarah_data_analysis")  

data <- read.csv("data/afrob6_courts_police.csv", header = TRUE) #n=45,541

# Create country fixed effects
dummy_data <- dummy(data$country)
original_data <- data.frame(data,dummy_data)
data <- original_data

###################
#   MAIN MODELS   #
###################

### Model 1: Courts, scale
### Model 2: Police, scale
### Model 3: Courts, dummy (prelim)
### Model 4: Police, dummy (prelim)

##### COURT SCALE RANGE ######

court_range <- seq(min(data$court_exp_scale, na.rm=T), max(data$court_exp_scale, na.rm=T), length.out=100) #set ct range
xvar <- "Negative Court Experiences"
xlab_name <- "Negative Court Experiences"

### model 1a: Trust courts (scale)
model <- formula(  as.factor(trust_courts) ~ court_exp_scale + trust_state_inst_no_courts + 
                      careful_talk_politics + fear_pol_violence +  traditional +
                      age + female + educ + rel_living_conditions + 
                      urban + scarcity + public_goods + police.station + Combinwt +
                      as.factor(COUNTRY)
)

mdata <- extractdata(model, data, na.rm=TRUE) #extract
summary(ologit.1a <- polr(model, mdata, method="logistic", Hess=TRUE) )  #run model -- for stargazer


table(data$country)
table(data$COUNTRY)

summary(z_mod <- zelig (model, data=mdata, model = "ologit"))  #run model -- for simulation
x.out <- setx(z_mod, court_exp_scale = court_range)#c(0,15))
s.out <- Zelig::sim(z_mod, x=x.out)

plotdata <- mv_extract(s.out)
plotdata1 <- plotdata[seq(1,nrow(plotdata), 4),] # only looking at people with no trust
plotdata4 <- plotdata[seq(4,nrow(plotdata), 4),] # only looking at people with high trust
colnames(plotdata4) <- paste(colnames(plotdata4),col="_4", sep="") # high trust
plot_data_full_trust <- data.frame(plotdata1, plotdata4, court_exp_scale = court_range) 

#plot_data_full_trust$mean[1] #bad exp=0: .107
#plot_data_full_trust$mean[dim(plot_data_full_trust)[1]] #bad exp=15: .171


### model 1b: Legit courts (scale)
model <- formula(  as.factor(courts_right) ~  
                     court_exp_scale + inst_right_comp_no_courts + 
                     careful_talk_politics + fear_pol_violence + traditional + 
                     age + female + educ + rel_living_conditions + 
                     urban + scarcity + public_goods + police.station + Combinwt +
                     as.factor(COUNTRY)
)

mdata <- extractdata(model, data, na.rm=TRUE) #extract
summary(ologit.1b <- polr(model, mdata, method="logistic", Hess=TRUE) )  #run model -- for stargazer

summary(z_mod <- zelig (model, data=mdata, model = "ologit"))  #run model -- for simulation
x.out <- setx(z_mod, court_exp_scale = court_range)#c(0,15))
s.out <- Zelig::sim(z_mod, x=x.out)

plotdata <- mv_extract(s.out)
plotdata1 <- plotdata[seq(1,nrow(plotdata), 5),] # only looking at people with low legit
plotdata5 <- plotdata[seq(5,nrow(plotdata), 5),] # only looking at people with high legit
colnames(plotdata5) <- paste(colnames(plotdata5),col="_5", sep="") # high legit
plot_data_full_legit <- data.frame(plotdata1, plotdata5, court_exp_scale = court_range) 

# Plot Trust courts and Legit courts 
colnames(plot_data_full_trust) <- paste(colnames(plot_data_full_trust),col="_trust", sep="")
colnames(plot_data_full_legit) <- paste(colnames(plot_data_full_legit),col="_legit", sep="") 
plot_data_full_all <- data.frame(plot_data_full_trust, plot_data_full_legit, court_exp_scale = court_range)

ggplot(data=plot_data_full_all, aes(x = court_exp_scale))+ ylim(0,.25)+ 
  geom_line(aes(y =mean_trust)) + 
  geom_line(aes(y =high_trust), linetype="dashed", color="red") + 
  geom_line(aes(y =low_trust), linetype="dashed", color="red") + 
  theme_minimal() +
  geom_ribbon(aes(ymin=low_trust, ymax=high_trust), alpha=0.1) +
  
  geom_line(aes(y =mean_legit)) + 
  geom_line(aes(y =high_legit), linetype="dashed", color="blue") + 
  geom_line(aes(y =low_legit), linetype="dashed", color="blue") + 
  theme_minimal() +
  geom_ribbon(aes(ymin=low_legit, ymax=high_legit), alpha=0.1) +
  
  labs(
    title = paste("Effect of", xvar, sep=" "),
    subtitle = paste("on respondents' trust in--and reported legitimacy of--govt courts"),
    #subtitle = paste("on respondents' trust in--and reported legitimacy of--govt courts (only Q100!=8)"),
    
    x = paste(xlab_name, "(0-15)", sep=" "),
    y = ""
  ) +
  annotate("text", x = 12, y=.23, label = "Pr( No Trust in Courts )", col="red", cex=3.5) +
  annotate("text", x = 12, y=.22, label = "(p<.01)", col="red", cex=3) +
  annotate("text", x = 12, y=.04, label = "Pr( No Legitimacy of Courts )", col="blue", cex=3.5) +
  annotate("text", x = 12, y=.03, label = "(not significant)", col="blue", cex=3) 
ggsave("sims_mar18/courts_trust_legit_scale.png", device="png")
#ggsave("sims/courts_trust_legit_scale_q100.png", device="png")

### model 1c: Trust army (scale)
model <- formula(  as.factor(trust_army) ~ court_exp_scale + trust_state_inst_no_army +
                     careful_talk_politics + fear_pol_violence +
                     age + female + educ + rel_living_conditions + traditional +
                     urban + scarcity + public_goods + police.station + Combinwt +
                     as.factor(COUNTRY)
)

mdata <- extractdata(model, data, na.rm=TRUE) #extract
summary(ologit.1c <- polr(model, mdata, method="logistic", Hess=TRUE) )  #run model -- for stargazer

### model 1d: courts -> Trust police (scale)
model <- formula(  as.factor(trust_police) ~ court_exp_scale + trust_state_inst_no_police + 
                     careful_talk_politics + fear_pol_violence + 
                     age + female + educ + rel_living_conditions + traditional +
                     urban + scarcity + public_goods + police.station + Combinwt +
                     as.factor(COUNTRY)
)

mdata <- extractdata(model, data, na.rm=TRUE) #extract
summary(ologit.1d <- polr(model, mdata, method="logistic", Hess=TRUE) )  #run model -- for stargazer


######## POLICE SCALE ###########

police_range <- seq(min(data$police_assist_scale, na.rm=T), max(data$police_assist_scale, na.rm=T), length.out=100) #set range
xvar <- "Difficulty Obtaining Police Assistance"
xlab_name <- "Difficulty Obtaining Police Assistance"

### model 2a: Police courts (scale)
model <- formula(  as.factor(trust_police) ~ police_assist_scale + trust_state_inst_no_police + 
                     careful_talk_politics + fear_pol_violence + traditional + 
                     age + female + educ + rel_living_conditions + 
                     urban + scarcity + public_goods + police.station + Combinwt +
                     as.factor(COUNTRY)
)

mdata <- extractdata(model, data, na.rm=TRUE) #extract
summary(ologit.2a <- polr(model, mdata, method="logistic", Hess=TRUE) )  #run model -- for stargazer

summary(z_mod <- zelig (model, data=mdata, model = "ologit"))  #run model -- for simulation
x.out <- setx(z_mod, police_assist_scale = police_range)#c(0,3))
s.out <- Zelig::sim(z_mod, x=x.out)

plotdata <- mv_extract(s.out)
plotdata1 <- plotdata[seq(1,nrow(plotdata), 4),] # only looking at people with no trust
plotdata4 <- plotdata[seq(4,nrow(plotdata), 4),] # only looking at people with high trust
colnames(plotdata4) <- paste(colnames(plotdata4),col="_4", sep="") # high trust
plot_data_full_trust <- data.frame(plotdata1, plotdata4, police_assist_scale = police_range) 

#plot_data_full_trust$mean[1] #bad exp=0: .178
#plot_data_full_trust$mean[dim(plot_data_full_trust)[1]] #bad exp=15: .331

### model 2b: Legit police (scale)
model <- formula(  as.factor(police_right) ~  
                     police_assist_scale + inst_right_comp_no_police + 
                     careful_talk_politics + fear_pol_violence + traditional + 
                     age + female + educ + rel_living_conditions + 
                     urban + scarcity + public_goods + police.station + Combinwt +
                     as.factor(COUNTRY)
)

mdata <- extractdata(model, data, na.rm=TRUE) #extract
summary(ologit.2b <- polr(model, mdata, method="logistic", Hess=TRUE) )  #run model -- for stargazer

summary(z_mod <- zelig (model, data=mdata, model = "ologit"))  #run model -- for simulation
x.out <- setx(z_mod, police_assist_scale = police_range)
s.out <- Zelig::sim(z_mod, x=x.out)

plotdata <- mv_extract(s.out)
plotdata1 <- plotdata[seq(1,nrow(plotdata), 5),] # only looking at people with low legit
plotdata5 <- plotdata[seq(5,nrow(plotdata), 5),] # only looking at people with high legit
colnames(plotdata5) <- paste(colnames(plotdata5),col="_5", sep="") # high legit
plot_data_full_legit <- data.frame(plotdata1, plotdata5, police_assist_scale = police_range) 

# Plot Trust police and Legit police together
colnames(plot_data_full_trust) <- paste(colnames(plot_data_full_trust),col="_trust", sep="")
colnames(plot_data_full_legit) <- paste(colnames(plot_data_full_legit),col="_legit", sep="") 
plot_data_full_all <- data.frame(plot_data_full_trust, plot_data_full_legit, police_assist_scale = police_range)

ggplot(data=plot_data_full_all, aes(x = police_assist_scale))+ ylim(0,.4)+ 
  geom_line(aes(y =mean_trust)) + 
  geom_line(aes(y =high_trust), linetype="dashed", color="red") + 
  geom_line(aes(y =low_trust), linetype="dashed", color="red") + 
  theme_minimal() +
  geom_ribbon(aes(ymin=low_trust, ymax=high_trust), alpha=0.1) +
  
  geom_line(aes(y =mean_legit)) + 
  geom_line(aes(y =high_legit), linetype="dashed", color="blue") + 
  geom_line(aes(y =low_legit), linetype="dashed", color="blue") + 
  theme_minimal() +
  geom_ribbon(aes(ymin=low_legit, ymax=high_legit), alpha=0.1) +
  
  labs(
    title = paste("Effect of", xvar, sep=" "),
    subtitle = paste("on respondents' trust in--and reported legitimacy of--police"),
    #subtitle = paste("on respondents' trust in--and reported legitimacy of--police (only Q100!=8)"),
    x = paste(xlab_name, "(4: very difficult)", sep=" "),
    y = ""
  ) +
  annotate("text", x = 3.5, y=.375, label = "Pr( No Trust )", col="red", cex=3.5) +
  annotate("text", x = 3.5, y=.36, label = "(p<0.01)", col="red", cex=3) +
  annotate("text", x = 3.5, y=.02, label = "Pr( No Legitimacy )", col="blue", cex=3.5) +
  annotate("text", x = 3.5, y=.003, label = "(not significant)", col="blue", cex=3) 
ggsave("sims_mar18/police_trust_legit_scale.png", device="png")
#ggsave("sims/police_trust_legit_scale_q100.png", device="png")

### model 2c: Trust army (scale)
model <- formula(  as.factor(trust_army) ~ police_assist_scale + trust_state_inst_no_army +
                     careful_talk_politics + fear_pol_violence + 
                     age + female + educ + rel_living_conditions + traditional +
                     urban + scarcity + public_goods + police.station + Combinwt +
                     as.factor(COUNTRY)
)

mdata <- extractdata(model, data, na.rm=TRUE) #extract
summary(ologit.2c <- polr(model, mdata, method="logistic", Hess=TRUE) )  #run model -- for stargazer

### model 2d: Police -> trust courts (scale)
model <- formula(  as.factor(trust_courts) ~ police_assist_scale + trust_state_inst_no_courts + 
                     careful_talk_politics + fear_pol_violence + 
                     age + female + educ + rel_living_conditions + traditional +
                     urban + scarcity + public_goods + police.station + Combinwt +
                     as.factor(COUNTRY)
)

mdata <- extractdata(model, data, na.rm=TRUE) #extract
summary(ologit.2d <- polr(model, mdata, method="logistic", Hess=TRUE) )  #run model -- for stargazer


########## Courts (dummy) ###################

# Model.3a: trust_courts ~ neg_court_exp (dummy)
model <- formula(  as.factor(trust_courts) ~ neg_court_exp + pos_court_exp + trust_state_inst_no_courts +
                     careful_talk_politics + fear_pol_violence + traditional +
                     age + female + educ + rel_living_conditions + 
                     urban + scarcity + public_goods + police.station + Combinwt +
                     as.factor(COUNTRY)
)

mdata <- extractdata(model, data, na.rm=TRUE) #extract
summary(ologit.3a <- polr(model, mdata, method="logistic", Hess=TRUE) )  #run model -- for stargazer


#hierarchical:
fml <- lmer(
  trust_courts ~ neg_court_exp + (neg_court_exp | country) + 
    pos_court_exp + trust_state_inst_no_courts + 
    careful_talk_politics + fear_pol_violence +  traditional +
    age + female + educ + rel_living_conditions + 
    urban + scarcity + public_goods + police.station, data
)
summary(fml)

#look at pg 83, 

### model 3b: legit_courts ~ neg_court_exp (dummy)
model <- formula(  as.factor(courts_right) ~  neg_court_exp + pos_court_exp + inst_right_comp_no_courts + 
                     careful_talk_politics + fear_pol_violence + traditional +
                     age + female + educ + rel_living_conditions + 
                     urban + scarcity + public_goods + police.station + Combinwt +
                     as.factor(COUNTRY)
)

mdata <- extractdata(model, data, na.rm=TRUE) #extract
summary(ologit.3b <- polr(model, mdata, method="logistic", Hess=TRUE) )  #run model -- for stargazer

########## Police (dummy) ###################

# Model.4a: trust_courts ~ neg_police_exp (dummy)
model <- formula(  as.factor(trust_police) ~ police_assistance_hard + police_assistance_easy + trust_state_inst_no_police + 
                     careful_talk_politics + fear_pol_violence + traditional + 
                     age + female + educ + rel_living_conditions + 
                     urban + scarcity + public_goods + police.station + Combinwt +
                     as.factor(COUNTRY)
)

mdata <- extractdata(model, data, na.rm=TRUE) #extract
summary(ologit.4a <- polr(model, mdata, method="logistic", Hess=TRUE) )  #run model -- for stargazer

### model 4b: legit_courts ~ neg_police_exp (dummy)
model <- formula(  as.factor(police_right) ~ police_assistance_hard + police_assistance_easy + inst_right_comp_no_police + 
                     careful_talk_politics + fear_pol_violence + traditional +
                     age + female + educ + rel_living_conditions + 
                     urban + scarcity + public_goods + police.station + Combinwt +
                     as.factor(COUNTRY)
)

mdata <- extractdata(model, data, na.rm=TRUE) #extract
summary(ologit.4b <- polr(model, mdata, method="logistic", Hess=TRUE) )  #run model -- for stargazer


##### STARGAZER TABLES #####

# Court (dummy) -- Prelim Model in paper

ct_vars_1 <- c("Negative court experiences (binary)",
          "Neutral court experiences (binary)",
          "Trust in state institutions (excl courts)",
          "Institutional legitimacy (excl courts)",
          "Cautious talking about politics",
          "Fear political violence",
          "Use traditional leaders",
          "Age",
          "Female (binary)",
          "Education level",
          "Personal living conditions",
          "Urban (binary)",
          "Personal resource scarcity",
          "Community access to public services",
          "Police station present (binary)", 
          "Combinwt")

stargazer(ologit.3a, ologit.3b,  
          keep.stat = c("n", "aic"),
          no.space=TRUE,
          dep.var.labels.include = FALSE,
          covariate.labels= ct_vars_1,
          column.labels=c("Trust Courts", "Legit Courts"),
          title = "Effects of negative court experiences, relative to those with no court experiences (ordered logistic)",
          omit= "COUNTRY",
          notes = c("All models include country-fixed effects")
          )

# Court scale -- Main court model in paper 

ct_vars_2 <- c("Negative court experiences (scale)",
               "Trust in state institutions (excl courts)",
               "Institutional legitimacy (excl courts)",
               "Trust in state institutions (excl police)",
               "Trust in state institutions (excl army)",
               "Cautious talking about politics",
               "Fear political violence",
               "Use traditional leaders",
               "Age",
               "Female (binary)",
               "Education level",
               "Quality of living conditions",
               "Urban (binary)",
               "Personal resource scarcity",
               "Community access to public services",
               "Police station present (binary)",
               "Combinwt")

stargazer(ologit.1a, ologit.1b, ologit.1d, ologit.1c, 
          keep.stat = c("n", "aic"),
          no.space=TRUE,
          dep.var.labels.include = FALSE,
          covariate.labels= ct_vars_2,
          column.labels=c("Trust Courts", "Legit Courts", "Trust Police", "Trust Army"), # "Trust Instit (gen)"),
          title = "Effects of negative court experiences, among those who had encounter(s) with the courts (ordered logistic)",
          omit= "COUNTRY",
          notes = c("All models include country-fixed effects")
          )

# Police (dummy) -- Prelim Model in paper

pol_vars_1 <- c("Police assistance difficult (binary)",
                "Police assistance easy (binary)",
                "Trust in state institutions (excl police)",
                "Institutional legitimacy (excl police)",
                "Cautious talking about politics",
                "Fear political violence",
                "Use traditional leaders",
                "Age",
                "Female (binary)",
                "Education level",
                "Quality of living conditions",
                "Urban (binary)",
                "Personal resource scarcity",
                "Community access to public services",
                "Police station present (binary)", 
                "Combinwt")

stargazer(ologit.4a, ologit.4b, 
          keep.stat = c("n", "aic"),
          no.space=TRUE,
          dep.var.labels.include = FALSE,
          covariate.labels= pol_vars_1,
          column.labels=c("Trust Police", "Legit Police"),
          title = "Effects of difficulty obtaining police assistance, relative to those with no police experiences (ordered logistic)",
          omit= "COUNTRY",
          notes = c("All models include country-fixed effects")
)

# Main police model in paper

pol_vars_2 <- c("Difficulty with police assistance (scale)",
               "Trust in state institutions (excl police)",
               "Institutional legitimacy (excl police)",
               "Trust in state institutions (excl courts)",
               "Trust in state institutions (excl army)",
               "Cautious talking about politics",
               "Fear political violence",
               "Use traditional leaders",
               "Age",
               "Female (binary)",
               "Education level",
               "Quality of living conditions",
               "Urban (binary)",
               "Personal resource scarcity",
               "Community access to public services",
               "Police station present (binary)", 
               "Combinwt")

stargazer(ologit.2a, ologit.2b, ologit.2d, ologit.2c,
          keep.stat = c("n", "aic"),
          no.space=TRUE,
          dep.var.labels.include = FALSE,
          covariate.labels= pol_vars_2,
          column.labels=c("Trust Police", "Legit Police", "Trust Courts", "Trust Army"),
          title = "Effects of difficulty obtaining police assistance, among those who had encounter(s) with the police (ordered logistic)",
          omit= "COUNTRY",
          notes = c("All models include country fixed effects")
)

##################
## Plot dummies ##
##################

range <- seq(0,1,length.out=100) #setrange

# plot neg court dummies -- Trust
model <- formula(  as.factor(trust_courts) ~ neg_court_exp + trust_state_inst_no_courts + 
                     careful_talk_politics + fear_pol_violence + traditional + 
                     age + female + educ + rel_living_conditions + 
                     urban + scarcity + public_goods + police.station + Combinwt +
                     as.factor(COUNTRY)
)

mdata <- extractdata(model, data, na.rm=TRUE) #extract
summary(z_mod <- zelig (model, data=mdata, model = "ologit"))  #run model -- for simulation

x.out <- setx(z_mod, neg_court_exp = range)
s.out <- Zelig::sim(z_mod, x=x.out)
plotdata <- mv_extract(s.out)
plotdata1 <- plotdata[seq(1,nrow(plotdata), 4),] # only looking at people with no trust
neg_ct_no_trust <- as.numeric((plotdata1)[100,1:3])

# plot pos court dummies -- trust
model <- formula(  as.factor(trust_courts) ~ pos_court_exp + trust_state_inst_no_courts + 
                     careful_talk_politics + fear_pol_violence + traditional +
                     age + female + educ + rel_living_conditions + 
                     urban + scarcity + public_goods + police.station + Combinwt +
                     as.factor(COUNTRY)
)

mdata <- extractdata(model, data, na.rm=TRUE) #extract
summary(z_mod <- zelig (model, data=mdata, model = "ologit"))  #run model -- for simulation

x.out <- setx(z_mod, pos_court_exp = range)
s.out <- Zelig::sim(z_mod, x=x.out)
plotdata <- mv_extract(s.out)
plotdata1 <- plotdata[seq(1,nrow(plotdata), 4),] # only looking at people with no trust
pos_ct_no_trust <- as.numeric((plotdata1)[100,1:3])

# plot no court dummies -- trust
model <- formula(  as.factor(trust_courts) ~ no_court_exp + trust_state_inst_no_courts + 
                     careful_talk_politics + fear_pol_violence + traditional +
                     age + female + educ + rel_living_conditions + 
                     urban + scarcity + public_goods + police.station + Combinwt +
                     as.factor(COUNTRY)
)

mdata <- extractdata(model, data, na.rm=TRUE) #extract
summary(z_mod <- zelig (model, data=mdata, model = "ologit"))  #run model -- for simulation

x.out <- setx(z_mod, no_court_exp = range)
s.out <- Zelig::sim(z_mod, x=x.out)
plotdata <- mv_extract(s.out)
plotdata1 <- plotdata[seq(1,nrow(plotdata), 4),] # only looking at people with no trust
no_ct_no_trust <- as.numeric((plotdata1)[100,1:3])

#pdf(file="sims_mar18/ct_dummies_trust.pdf")
par(mai=c(1,1.9,.7,1.9 ))
plot(1.2, neg_ct_no_trust[3], pch=16, col="dark green", 
     bty="n", ylab="Probability", 
     ylim=c(.06, .115), xlim=c(.9,1.3), xlab="",xaxt="n",
     main="Probability of No Trust in Courts\n(Entire Dataset)", cex.main=.9)
segments(1.2, neg_ct_no_trust[1], 1.2, neg_ct_no_trust[2], lwd=1.5, col="dark green")
points(1.1, no_ct_no_trust[3],  pch=16, col="black")
segments(1.1, no_ct_no_trust[1], 1.1, no_ct_no_trust[2],  lwd=1.5, col="black")
points(1, pos_ct_no_trust[3], pch=20, col="grey")
segments(1, pos_ct_no_trust[1], 1, pos_ct_no_trust[2], col="grey")
abline(a=.06, b=0)

text(1.1, .113, "Barriers Encountered", cex=.7, col="dark green") 
text(1.1, .11, "No Court Experience", cex=.7, col="black") 
text(1.1, .107, "No Barriers Encountered", cex=.7, col="dark grey") 
#dev.off()


# plot neg police dummies -- Trust
model <- formula(  as.factor(trust_police) ~ police_assistance_hard + trust_state_inst_no_police +
                     careful_talk_politics + fear_pol_violence + traditional + 
                     age + female + educ + rel_living_conditions + 
                     urban + scarcity + public_goods + police.station +  Combinwt +
                     as.factor(COUNTRY)
)

mdata <- extractdata(model, data, na.rm=TRUE) #extract
summary(z_mod <- zelig (model, data=mdata, model = "ologit"))  #run model -- for simulation

x.out <- setx(z_mod, police_assistance_hard = range)
s.out <- Zelig::sim(z_mod, x=x.out)
plotdata <- mv_extract(s.out)
plotdata1 <- plotdata[seq(1,nrow(plotdata), 4),] # only looking at people with no trust
neg_pol_no_trust <- as.numeric((plotdata1)[100,1:3])

# plot pos police dummies -- trust
model <- formula(  as.factor(trust_police) ~ police_assistance_easy + trust_state_inst_no_police +
                     careful_talk_politics + fear_pol_violence + traditional + 
                     age + female + educ + rel_living_conditions + 
                     urban + scarcity + public_goods + police.station + Combinwt +
                     as.factor(COUNTRY)
)

mdata <- extractdata(model, data, na.rm=TRUE) #extract
summary(z_mod <- zelig (model, data=mdata, model = "ologit"))  #run model -- for simulation

x.out <- setx(z_mod, police_assistance_easy = range)
s.out <- Zelig::sim(z_mod, x=x.out)
plotdata <- mv_extract(s.out)
plotdata1 <- plotdata[seq(1,nrow(plotdata), 4),] # only looking at people with no trust
pos_pol_no_trust <- as.numeric((plotdata1)[100,1:3])

# plot no court dummies
model <- formula(  as.factor(trust_police) ~ no_contact_police + trust_state_inst_no_police +
                     careful_talk_politics + fear_pol_violence + traditional + 
                     age + female + educ + rel_living_conditions + 
                     urban + scarcity + public_goods + police.station + Combinwt +
                     as.factor(COUNTRY)
)

mdata <- extractdata(model, data, na.rm=TRUE) #extract
summary(z_mod <- zelig (model, data=mdata, model = "ologit"))  #run model -- for simulation

x.out <- setx(z_mod, no_contact_police = range)
s.out <- Zelig::sim(z_mod, x=x.out)
plotdata <- mv_extract(s.out)
plotdata1 <- plotdata[seq(1,nrow(plotdata), 4),] # only looking at people with no trust
no_pol_no_trust <- as.numeric((plotdata1)[100,1:3])

## Plot trust only: Police ##
pdf(file="sims_mar18/pol_dummies_trust.pdf")
par(mai=c(1,1.9,.7,1.9))
plot(1.2, neg_pol_no_trust[3], pch=16, col="dark blue", 
     bty="n", ylab="Probability", 
     ylim=c(.15, .35), xlim=c(.9,1.3), xlab="",xaxt="n",
     main="Probability of No Trust in Police\n(Entire Dataset)", cex.main=.9)
segments(1.2, neg_pol_no_trust[1], 1.2, neg_pol_no_trust[2], lwd=1.5, col="dark blue")
points(1.1, no_pol_no_trust[3],  pch=16, col="black")
segments(1.1, no_pol_no_trust[1], 1.1, no_pol_no_trust[2],  lwd=1.5, col="black")
points(1, pos_pol_no_trust[3], pch=20, col="grey")
segments(1, pos_pol_no_trust[1], 1, pos_pol_no_trust[2], col="grey")

abline(a=.15, b=0)

text(1.1, .35, "Assistance Difficult", cex=.7, col="dark blue") 
text(1.1, .339, "No Police Experience", cex=.7, col="black") 
text(1.1, .328, "Assistance Easy", cex=.7, col="dark grey") 
dev.off()


##################################
### STOPPED HERE ###
##################################


          
###############################
## ROBUSTNESS CHECKES #########
###############################

## Look at specific experiences ##

data$var <- data$expensive
data$var3<-data$var
data$var3[data$var3==100] <- 0
data$var3[data$var3>0]<-1
data$expensive3 <- data$var3

data$var <- data$complex
data$var3<-data$var
data$var3[data$var3==100] <- 0
data$var3[data$var3>0]<-1
data$complex3 <- data$var3

data$var <- data$no_advice
data$var3<-data$var
data$var3[data$var3==100] <- 0
data$var3[data$var3>0]<-1
data$no_advice3 <- data$var3

data$var <- data$judge_no_listen
data$var3<-data$var
data$var3[data$var3==100] <- 0
data$var3[data$var3>0]<-1
data$judge_no_listen3 <- data$var3

data$var <- data$delays
data$var3<-data$var
data$var3[data$var3==100] <- 0
data$var3[data$var3>0]<-1
data$delays3 <- data$var3

sub <- subset(data, court_exp_scale!="NA")

model <- formula(  as.factor(trust_courts) ~ expensive3 + complex3 + 
                     no_advice3 + judge_no_listen3 + delays3 + #pos_court_exp +
                   + trust_state_inst_no_courts + #DV: Trust
                     careful_talk_politics + fear_pol_violence +  traditional + 
                     age + female + educ + rel_living_conditions + 
                     urban + scarcity + public_goods + police.station +  
                     as.factor(COUNTRY)
)

mdata <- extractdata(model, sub, na.rm=TRUE) #extract
summary(ologit.exp <- polr(model, mdata, method="logistic", Hess=TRUE) )  
stargazer(ologit.exp)

ct_exp_vars <- c("Expensive (binary)", "Too complex (binary)", "No legal advice (binary)",
                 "Judge did not listen (binary)", "Delays (binary)",
               "Trust in state institutions (excl courts)",
               "Cautious talking about politics",
               "Fear political violence",
               "Use traditional leaders",
               "Age",
               "Female (binary)",
               "Education level",
               "Quality of living conditions",
               "Urban (binary)",
               "Personal resource scarcity",
               "Community access to public services",
               "Police station present (binary)")

stargazer(ologit.exp,
          keep.stat = c("n", "aic"),
          no.space=TRUE,
          dep.var.labels.include = FALSE,
          column.labels=c("Trust Courts"),
          covariate.labels= ct_exp_vars,
          title = "Effects of specific court barriers on trust in courts, among those who had encounter(s) with the courts (ordered logistic)",
          omit= "COUNTRY",
          notes = c("All models include country-fixed effects")
)

colnames(data)

####################################################
## subset to those who didn't think govt surveyed ##
####################################################

#subset to those who didn't think government conducted the survey
sub <- subset(data, data$Q100!=8)

sum(table(data$Q100)) - 15925 #check

model <- formula(  as.factor(trust_courts) ~ court_exp_scale + trust_state_inst_no_courts +
                     careful_talk_politics + fear_pol_violence +  traditional + 
                     age + female + educ + rel_living_conditions + 
                     urban + scarcity + public_goods + police.station +
                     as.factor(COUNTRY)
)

mdata <- extractdata(model, sub, na.rm=TRUE) #extract
summary(ologit.rob1a <- polr(model, mdata, method="logistic", Hess=TRUE) ) 

model <- formula(  as.factor(courts_right) ~ court_exp_scale + inst_right_comp_no_courts +
                     careful_talk_politics + fear_pol_violence +  traditional + 
                     age + female + educ + rel_living_conditions + 
                     urban + scarcity + public_goods + police.station +
                     as.factor(COUNTRY)
)

mdata <- extractdata(model, sub, na.rm=TRUE) #extract
summary(ologit.rob1b <- polr(model, mdata, method="logistic", Hess=TRUE) ) 


# model <- formula(  as.factor(trust_army) ~ court_exp_scale + trust_state_inst_no_army +
#                      careful_talk_politics + fear_pol_violence +  traditional + 
#                      age + female + educ + rel_living_conditions + 
#                      urban + scarcity + public_goods + police.station +
#                      as.factor(COUNTRY)
# )
# 
# mdata <- extractdata(model, sub, na.rm=TRUE) #extract
# summary(ologit.rob1c <- polr(model, mdata, method="logistic", Hess=TRUE) ) 
# 
# model <- formula(  as.factor(trust_police) ~ court_exp_scale + trust_state_inst_no_police +
#                      careful_talk_politics + fear_pol_violence +  traditional + 
#                      age + female + educ + rel_living_conditions + 
#                      urban + scarcity + public_goods + police.station +
#                      as.factor(COUNTRY)
# )
# 
# mdata <- extractdata(model, sub, na.rm=TRUE) #extract
# summary(ologit.rob1d <- polr(model, mdata, method="logistic", Hess=TRUE) ) 

# police
model <- formula(  as.factor(trust_police) ~ police_assist_scale + trust_state_inst_no_police + 
                     careful_talk_politics + fear_pol_violence + traditional + 
                     age + female + educ + rel_living_conditions + 
                     urban + scarcity + public_goods + police.station + 
                     as.factor(COUNTRY)
)

mdata <- extractdata(model, sub, na.rm=TRUE) #extract
summary(ologit.rob2a <- polr(model, mdata, method="logistic", Hess=TRUE) )  

model <- formula(  as.factor(police_right) ~ police_assist_scale + inst_right_comp_no_police + 
                     careful_talk_politics + fear_pol_violence + traditional + 
                     age + female + educ + rel_living_conditions + 
                     urban + scarcity + public_goods + police.station + 
                     as.factor(COUNTRY)
)

mdata <- extractdata(model, sub, na.rm=TRUE) #extract
summary(ologit.rob2b <- polr(model, mdata, method="logistic", Hess=TRUE) )  

# model <- formula(  as.factor(trust_army) ~ police_assist_scale + trust_state_inst_no_army + 
#                      careful_talk_politics + fear_pol_violence + traditional + 
#                      age + female + educ + rel_living_conditions + 
#                      urban + scarcity + public_goods + police.station + 
#                      as.factor(COUNTRY)
# )
# 
# mdata <- extractdata(model, sub, na.rm=TRUE) #extract
# summary(ologit.rob2c <- polr(model, mdata, method="logistic", Hess=TRUE) )  
# 
# model <- formula(  as.factor(trust_courts) ~ police_assist_scale + trust_state_inst_no_courts + 
#                      careful_talk_politics + fear_pol_violence + traditional + 
#                      age + female + educ + rel_living_conditions + 
#                      urban + scarcity + public_goods + police.station + 
#                      as.factor(COUNTRY)
# )
# 
# mdata <- extractdata(model, sub, na.rm=TRUE) #extract
# summary(ologit.rob2d <- polr(model, mdata, method="logistic", Hess=TRUE) )  

ct_pol_vars_1 <- c("Negative court experiences (scale)",
                    "Trust in state institutions (excl courts)",
                    "Institutional legitimacy (excl courts)",
                   "Difficulty with police assistance (scale)",
                   "Trust in state institutions (excl police)",
                   "Institutional legitimacy (excl police)",
                    "Cautious talking about politics",
                    "Fear political violence",
                    "Use traditional leaders",
                    "Age",
                    "Female (binary)",
                    "Education level",
                    "Quality of living conditions",
                    "Urban (binary)",
                    "Personal resource scarcity",
                    "Community access to public services",
                    "Police station present (binary)")

stargazer(ologit.rob1a, ologit.rob1b, ologit.rob2a, ologit.rob2b,
          keep.stat = c("n", "aic"),
          no.space=TRUE,
          dep.var.labels.include = FALSE,
          covariate.labels=ct_pol_vars_1,
          column.labels=c("Trust Courts", "Legit Courts", "Trust Police", "Police Legit"),
          title = "Effects of court barriers and police difficulty among those with court/police experiences, subsetted to those who did
          not believe the government conducted this survey (ordered logistic)",
          omit= "COUNTRY",
          notes = c("All models include country-fixed effects")
          )


#############################################################
## subset to those who answered ethnic / pol opp questions ##
#############################################################

#q45d: how often are opp parties/supporters silenced by gov?, 0:never, 3:always
data$opp_silenced[data$opp_silenced==-1 | data$opp_silenced==99] <- NA

#q88a: how often is your ethnic group treated unfairly by gov? 0:never, 3:always, 7:NA, 9:DK, 98:ref
data$ethnic_persec[data$ethnic_persec==99] <- NA

model <- formula(  as.factor(trust_courts) ~ court_exp_scale + trust_state_inst_no_courts +
                     ethnic_persec + opp_silenced +
                     careful_talk_politics + fear_pol_violence +  traditional + 
                     age + female + educ + rel_living_conditions + 
                     urban + scarcity + public_goods + police.station +
                     as.factor(COUNTRY)
)

mdata <- extractdata(model, data, na.rm=TRUE) #extract
summary(ologit.rob1a.pers <- polr(model, mdata, method="logistic", Hess=TRUE) ) 

model <- formula(  as.factor(courts_right) ~ court_exp_scale + inst_right_comp_no_courts +
                     ethnic_persec + opp_silenced +
                     careful_talk_politics + fear_pol_violence +  traditional + 
                     age + female + educ + rel_living_conditions + 
                     urban + scarcity + public_goods + police.station +
                     as.factor(COUNTRY)
)

mdata <- extractdata(model, data, na.rm=TRUE) #extract
summary(ologit.rob1b.pers <- polr(model, mdata, method="logistic", Hess=TRUE) ) 


# model <- formula(  as.factor(trust_army) ~ court_exp_scale + trust_state_inst_no_army +
#                      ethnic_persec + opp_silenced +
#                      careful_talk_politics + fear_pol_violence +  traditional + 
#                      age + female + educ + rel_living_conditions + 
#                      urban + scarcity + public_goods + police.station +
#                      as.factor(COUNTRY)
# )
# 
# mdata <- extractdata(model, data, na.rm=TRUE) #extract
# summary(ologit.rob1c <- polr(model, mdata, method="logistic", Hess=TRUE) ) 
# 
# model <- formula(  as.factor(trust_police) ~ court_exp_scale + trust_state_inst_no_police +
#                      ethnic_persec + opp_silenced +
#                      careful_talk_politics + fear_pol_violence +  traditional + 
#                      age + female + educ + rel_living_conditions + 
#                      urban + scarcity + public_goods + police.station +
#                      as.factor(COUNTRY)
# )
# 
# mdata <- extractdata(model, data, na.rm=TRUE) #extract
# summary(ologit.rob1d <- polr(model, mdata, method="logistic", Hess=TRUE) ) 

# police
model <- formula(  as.factor(trust_police) ~ police_assist_scale + trust_state_inst_no_police + 
                     ethnic_persec + opp_silenced +
                     careful_talk_politics + fear_pol_violence + traditional + 
                     age + female + educ + rel_living_conditions + 
                     urban + scarcity + public_goods + police.station + 
                     as.factor(COUNTRY)
)

mdata <- extractdata(model, data, na.rm=TRUE) #extract
summary(ologit.rob2a.pers <- polr(model, mdata, method="logistic", Hess=TRUE) )  

model <- formula(  as.factor(police_right) ~ police_assist_scale + inst_right_comp_no_police + 
                     ethnic_persec + opp_silenced +
                     careful_talk_politics + fear_pol_violence + traditional + 
                     age + female + educ + rel_living_conditions + 
                     urban + scarcity + public_goods + police.station + 
                     as.factor(COUNTRY)
)

mdata <- extractdata(model, data, na.rm=TRUE) #extract
summary(ologit.rob2b.pers <- polr(model, mdata, method="logistic", Hess=TRUE) )  

# model <- formula(  as.factor(trust_army) ~ police_assist_scale + trust_state_inst_no_army + 
#                      ethnic_persec + opp_silenced +
#                      careful_talk_politics + fear_pol_violence + traditional + 
#                      age + female + educ + rel_living_conditions + 
#                      urban + scarcity + public_goods + police.station + 
#                      as.factor(COUNTRY)
# )
# 
# mdata <- extractdata(model, data, na.rm=TRUE) #extract
# summary(ologit.rob2c <- polr(model, mdata, method="logistic", Hess=TRUE) )  
# 
# model <- formula(  as.factor(trust_courts) ~ police_assist_scale + trust_state_inst_no_courts + 
#                      ethnic_persec + opp_silenced +
#                      careful_talk_politics + fear_pol_violence + traditional + 
#                      age + female + educ + rel_living_conditions + 
#                      urban + scarcity + public_goods + police.station + 
#                      as.factor(COUNTRY)
# )
# 
# mdata <- extractdata(model, data, na.rm=TRUE) #extract
# summary(ologit.rob2d <- polr(model, mdata, method="logistic", Hess=TRUE) )  

ct_pol_vars_2 <- c("Negative court experiences (scale)",
                   "Trust in state institutions (excl courts)",
                   "Institutional legitimacy (excl courts)",
                   "Difficulty with police assistance (scale)",
                   "Trust in state institutions (excl police)",
                   "Institutional legitimacy (excl police)",
                   "Ethnicity persecuted",
                   "Political opposition silenced",
                   "Cautious talking about politics",
                   "Fear political violence",
                   "Use traditional leaders",
                   "Age",
                   "Female (binary)",
                   "Education level",
                   "Quality of living conditions",
                   "Urban (binary)",
                   "Personal resource scarcity",
                   "Community access to public services",
                   "Police station present (binary)")

stargazer(ologit.rob1a.pers, ologit.rob1b.pers, ologit.rob2a.pers, ologit.rob2b.pers,
          keep.stat = c("n", "aic"),
          no.space=TRUE,
          dep.var.labels.include = FALSE,
          covariate.labels= ct_pol_vars_2,
          column.labels=c("Trust Courts", "Legit Courts", "Trust Police", "Legit Police"),
          title = "Effects of court barriers and police difficulty among those with court/police 
          experiences, subsetted to those who were surveyed about ethnic and political persecution
          (ordered logistic)",
          omit= "COUNTRY",
          notes = c("All models include country-fixed effects")
          )


