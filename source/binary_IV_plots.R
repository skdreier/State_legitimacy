############################################
#### Replication Code for:              ####
#### Dreier and Lake (Democratization)  ####
####                                    ####
#### PLOTTING BINARY IV MODELS          ####
####                                    ####
#### R version 3.4.4 (2018-03-15)       ####
#### DATE: 5/14/2019                    ####
############################################

# Content:
# - Plot main binary IV models

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

#set range (0-1) for all binary variables 
range <- seq(0,1,length.out=100) 

##################################################################
#### PLOT COURT MODELS WITH BINARY IVS (among entire dataset) ####
##################################################################

#### Court model 1: trust ~ negative experience (binary)
model <- formula(  as.factor(trust_courts) ~ neg_court_exp + trust_state_inst_no_courts + 
                     careful_talk_politics + fear_pol_violence + traditional + 
                     age + female + educ + rel_living_conditions + 
                     urban + scarcity + public_goods + police_station + Combinwt +
                     as.factor(COUNTRY)
)

# Run model 1 and prepare for simulation
  mdata <- extractdata(model, data, na.rm=TRUE) 
  z_mod <- zelig (model, data=mdata, model = "ologit")
  x.out <- setx(z_mod, neg_court_exp = range)
  s.out <- Zelig::sim(z_mod, x=x.out)
  plotdata <- mv_extract(s.out)
  plotdata1 <- plotdata[seq(1,nrow(plotdata), 4),] # pull only people with no trust
  neg_ct_no_trust <- as.numeric((plotdata1)[100,1:3])

#### Court model 2: trust ~ positive experience (binary)
model <- formula(  as.factor(trust_courts) ~ pos_court_exp + trust_state_inst_no_courts + 
                     careful_talk_politics + fear_pol_violence + traditional +
                     age + female + educ + rel_living_conditions + 
                     urban + scarcity + public_goods + police_station + Combinwt +
                     as.factor(COUNTRY)
)

# Run model 2 and prepare for simulation
  mdata <- extractdata(model, data, na.rm=TRUE) 
  z_mod <- zelig (model, data=mdata, model = "ologit") 
  x.out <- setx(z_mod, pos_court_exp = range)
  s.out <- Zelig::sim(z_mod, x=x.out)
  plotdata <- mv_extract(s.out)
  plotdata1 <- plotdata[seq(1,nrow(plotdata), 4),] # pull only people with no trust
  pos_ct_no_trust <- as.numeric((plotdata1)[100,1:3])

#### Court model 3: trust ~ no experience (binary)
model <- formula(  as.factor(trust_courts) ~ no_court_exp + trust_state_inst_no_courts + 
                     careful_talk_politics + fear_pol_violence + traditional +
                     age + female + educ + rel_living_conditions + 
                     urban + scarcity + public_goods + police_station + Combinwt +
                     as.factor(COUNTRY)
)

# Run model 3 and prepare for simulation
  mdata <- extractdata(model, data, na.rm=TRUE)
  z_mod <- zelig (model, data=mdata, model = "ologit")
  x.out <- setx(z_mod, no_court_exp = range)
  s.out <- Zelig::sim(z_mod, x=x.out)
  plotdata <- mv_extract(s.out)
  plotdata1 <- plotdata[seq(1,nrow(plotdata), 4),] # pull only people with no trust
  no_ct_no_trust <- as.numeric((plotdata1)[100,1:3])

#### Plot court models 1-3 in one figure
pdf(file="figures/courts_trust_binaries.pdf")

par(mai=c(1,1.9,.7,1.9))

plot(1.2, neg_ct_no_trust[3], pch=16, col="dark green", 
     bty="n", ylab="Probability", 
     ylim=c(.06, .115), 
     xlim=c(.9,1.3), xlab="",xaxt="n",
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

dev.off()

###################################################################
#### PLOT POLICE MODELS WITH BINARY IVS (among entire dataset) ####
###################################################################

#### Police model 1: trust ~ assistance hard (binary)
model <- formula(  as.factor(trust_police) ~ police_assistance_hard + trust_state_inst_no_police +
                     careful_talk_politics + fear_pol_violence + traditional + 
                     age + female + educ + rel_living_conditions + 
                     urban + scarcity + public_goods + police_station +  Combinwt +
                     as.factor(COUNTRY)
)

# Run model 1 and prepare for simulation
  mdata <- extractdata(model, data, na.rm=TRUE) 
  z_mod <- zelig (model, data=mdata, model = "ologit")
  x.out <- setx(z_mod, police_assistance_hard = range)
  s.out <- Zelig::sim(z_mod, x=x.out)
  plotdata <- mv_extract(s.out)
  plotdata1 <- plotdata[seq(1,nrow(plotdata), 4),] # pull only people with no trust
  neg_pol_no_trust <- as.numeric((plotdata1)[100,1:3])

#### Police model 2: trust ~ assistance easy (binary)
model <- formula(  as.factor(trust_police) ~ police_assistance_easy + trust_state_inst_no_police +
                     careful_talk_politics + fear_pol_violence + traditional + 
                     age + female + educ + rel_living_conditions + 
                     urban + scarcity + public_goods + police_station + Combinwt +
                     as.factor(COUNTRY)
)

# Run model 2 and prepare for simulation
  mdata <- extractdata(model, data, na.rm=TRUE) 
  z_mod <- zelig (model, data=mdata, model = "ologit") 
  x.out <- setx(z_mod, police_assistance_easy = range)
  s.out <- Zelig::sim(z_mod, x=x.out)
  plotdata <- mv_extract(s.out)
  plotdata1 <- plotdata[seq(1,nrow(plotdata), 4),] # pull only people with no trust
  pos_pol_no_trust <- as.numeric((plotdata1)[100,1:3])

#### Police model 3: trust ~ no contact police (binary)
model <- formula(  as.factor(trust_police) ~ no_contact_police + trust_state_inst_no_police +
                     careful_talk_politics + fear_pol_violence + traditional + 
                     age + female + educ + rel_living_conditions + 
                     urban + scarcity + public_goods + police_station + Combinwt +
                     as.factor(COUNTRY)
)

# Run model 3 and prepare for simulation
  mdata <- extractdata(model, data, na.rm=TRUE) 
  z_mod <- zelig (model, data=mdata, model = "ologit")
  x.out <- setx(z_mod, no_contact_police = range)
  s.out <- Zelig::sim(z_mod, x=x.out)
  plotdata <- mv_extract(s.out)
  plotdata1 <- plotdata[seq(1,nrow(plotdata), 4),] # pull only people with no trust
  no_pol_no_trust <- as.numeric((plotdata1)[100,1:3])

#### Plot police models 1-3 in one figure
pdf(file="figures/police_trust_binaries.pdf")
  
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
  
  
##########################################################
########       END OF BINARY IV PLOT SCRIPT       ########
##########################################################
