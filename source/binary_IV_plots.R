
##################
## Plot dummies ##
##################


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