
#data cleanup
getwd()
happiness_data <- read.table("Happiness.txt", header = TRUE)
head(happiness_data)

#Household
happiness_data$Household[happiness_data$Household == 9] <- NA

#Health
happiness_data$Health[happiness_data$Health == 8 | happiness_data$Health == 9 |  happiness_data$Health == 0] <- NA
happiness_data$Health[happiness_data$Health == 1] <- 400
happiness_data$Health[happiness_data$Health == 2] <- 300
happiness_data$Health[happiness_data$Health == 3] <- 2
happiness_data$Health[happiness_data$Health == 4] <- 1
happiness_data$Health[happiness_data$Health == 400] <- 4
happiness_data$Health[happiness_data$Health == 300] <- 3

#OwnHome
happiness_data$OwnHome[happiness_data$OwnHome == 0 | happiness_data$OwnHome == 8 | happiness_data$OwnHome == 9] <- NA

#Instagram - Set 'don't know' and 'inapplicable' to 'No'
happiness_data$Instagram[happiness_data$Instagram == 0 | happiness_data$Instagram == 8] <- 2
happiness_data$Instagram[happiness_data$Instagram == 9] <- NA

#Marital
happiness_data$Marital[happiness_data$Marital == 9] <- NA

#Age
happiness_data$Age[happiness_data$Age == 89 | happiness_data$Age == 98 | happiness_data$Age == 99] <- NA

#Children
happiness_data$Children[happiness_data$Children == 9] <- NA

#Education
happiness_data$Education[happiness_data$Education == 97 | happiness_data$Education == 98 | happiness_data$Education == 99] <- NA

#JobSat
happiness_data$JobSat[happiness_data$JobSat == 0 | happiness_data$JobSat == 8 | happiness_data$JobSat == 9] <- NA

#Income
happiness_data$Income[happiness_data$Income == 0 | happiness_data$Income == 999998 | happiness_data$Income == 999999] <- NA

#WorkHrs
happiness_data$WorkHrs[happiness_data$WorkHrs == -1 | happiness_data$WorkHrs == 998 | happiness_data$WorkHrs == 999] <- NA

#Happy
happiness_data$Happy[happiness_data$Happy == 0 | happiness_data$Happy == 8 | happiness_data$Happy == 9] <- NA
happiness_data$Happy[happiness_data$Happy == 1] <- 100
happiness_data$Happy[happiness_data$Happy == 3] <- 1
happiness_data$Happy[happiness_data$Happy == 100] <- 3

#exploring data
pairs(happiness_data[, -c(1,2,3,4,5,6,8,10)], gap=0.4,cex.labels=1.5)

#Finding the number of NAs 
sum(is.na(happiness_data$WorkHrs))
#Insta = 10, Marital = 1
#Household = 1
#Health = 811
#OwnHome = 812
#JobSat = 1612 
#WorkHrs = 1898   <- Can take out the variable!
#Income = 1039

#Factoring Cateogircal Variables
JobSat.f <- factor(JobSat)
OwnHome.f <- factor(OwnHome)
Marital.f <- factor(Marital)
Instagram.f <- factor(Instagram)
Health.f <- factor(Health)

attach(happiness_data)

library(alr3)
#Couldn't include Health as it throws an error
full_model <- lm(Happy ~ Household + OwnHome.f + Instagram.f + Marital.f + Children + Education + JobSat.f + Income + Age + Sex )
summary(full_model)

par(mfrow = c(2,2))
plot(full_model)

###########Correlating indiviudal variables with Happy
insta <- lm(Happy ~ Instagram)
summary(insta)
plot(insta)
#Instagram is insignificant

marital <- lm(Happy ~ Marital.f)
summary(marital)
plot(marital)
#Marital is significant

Job <- lm(Happy ~ JobSat.f)
summary(Job)
plot(Job)
#Job is significant

House <- lm(happiness_data$Happy ~ happiness_data$Household)
summary(House)
plot(Household)
#Household is significant

sex <- lm(Happy ~ Sex)
summary(sex)
plot(sex)
#Sex is insignificant

#attach(happiness_data)
#boxCox(Happy ~ Household)
#inverse.response.plot(House, key = TRUE)   
#powerTransform(House)
#boxCox(House,lambda=seq(0.28,0.39,length=20))

################

#Using Anova to check the significance of Predictors

noMarital <- lm(Happy ~ Sex + JobSat.f + OwnHome.f + Household + Education + Age + Income + Children + Instagram.f)
anova(full_model, noMarital)
#Marital is Significant
plot(noMarital)

noHousehold <- lm(Happy ~ Sex + JobSat.f + OwnHome.f + Marital.f + Education + Age + Income + Children + Instagram.f)
anova(full_model, noHousehold)
#Household is significant

noSex <- lm(Happy ~ JobSat.f + OwnHome.f + Household + Marital.f + Education + Age + Income + Children + Instagram.f)
anova(full_model, noSex)
#Sex is insignificant

noInstagram <- lm(Happy ~ Sex + JobSat.f + OwnHome.f + Household + Marital.f + Education + Age + Income + Children)
anova(full_model, noInstagram)
#Instagram is insignificant
plot(noInstagram)

noChildren <- lm(Happy ~ Sex + JobSat.f + OwnHome.f + Household + Marital.f + Education + Age + Income + Instagram.f)
anova(full_model, noChildren)
#Children is Insignificant

noEducation <- lm(Happy ~ Sex + JobSat.f + OwnHome.f + Household + Marital.f + Age + Income + Children + Instagram.f)
anova(full_model, noEducation)
#Education is insignificant

noOwnHome <- lm(Happy ~ Sex + JobSat.f + Household + Marital.f + Education + Age + Income + Children + Instagram.f)
anova(full_model, noOwnHome)
#OwnHome Error

noJobSat <- lm(Happy ~ Sex + OwnHome.f + Household + Marital.f + Education + Age + Income + Children + Instagram.f)
anova(full_model, noJobSat)
#JobSat Error
names(happiness_data)

noAge <- lm(Happy ~ Sex + JobSat.f + OwnHome.f + Household + Marital.f + Education  + Income + Children + Instagram.f)
anova(full_model, noAge)
#Age Error

noIncome <- lm(Happy ~ Sex + JobSat.f + OwnHome.f + Household + Marital.f + Education + Age + Children + Instagram.f)
anova(full_model, noIncome)
#Income Error
############################

#####Trying AIC and BIC

#Eliminating education, instagram, children, sex
new_model <- lm(Happy ~ JobSat.f + OwnHome.f + Marital.f + Household + Income + Age)
summary(new_model)
plot(new_model)
Rad <- summary(new_model)$adj.r.squared
Rad


#fAIC <- lm(Happy~1)
#forwardAIC <- step(fAIC, scope=list(lower=~1,upper=~JobSat.f+Household+OwnHome.f+Marital.f + Age + Income),
#                   direction = "forward")
#backAIC <- step(new_model, direction="backward")

om1 <- lm(Happy ~ JobSat.f)
om2 <- lm(Happy ~ JobSat.f + OwnHome.f)
om3 <- lm(Happy ~ JobSat.f + OwnHome.f + Marital.f)
om4 <- lm(Happy ~ JobSat.f + OwnHome.f + Marital.f + Household)
om5 <- lm(Happy ~ JobSat.f + OwnHome.f + Marital.f + Household + Age)
om6 <- lm(Happy ~ JobSat.f + OwnHome.f + Marital.f + Household + Age + Income)
n = length(Happy)


#### subset size = 1 ####
p <- 1
oms1 <- summary(om1)
# AIC
AIC1 <- extractAIC(om1,k=2)[2]
# AICc
AICc1 <- extractAIC(om1,k=2)[2]+2*(p+2)*(p+3)/(n-p-1)
# BIC
BIC1 <- extractAIC(om1,k=log(n))[2]

#### subset size = 2 ####
p <- 2
oms2 <- summary(om2)
# AIC
AIC2 <- extractAIC(om2,k=2)[2]
# AICc
AICc2 <- extractAIC(om2,k=2)[2]+2*(p+2)*(p+3)/(n-p-1)
# BIC
BIC2 <- extractAIC(om2,k=log(n))[2]

#### subset size = 3 ####
p <- 3
oms3 <- summary(om3)
# AIC
AIC3 <- extractAIC(om3,k=2)[2]
# AICc
AICc3 <- extractAIC(om3,k=2)[2]+2*(p+2)*(p+3)/(n-p-1)
# BIC
BIC3 <- extractAIC(om3,k=log(n))[2]

#### subset size = 4 ####
p <- 4
oms4 <- summary(om4)
# AIC
AIC4 <- extractAIC(om4,k=2)[2]
# AICc
AICc4 <- extractAIC(om4,k=2)[2]+2*(p+2)*(p+3)/(n-p-1)
# BIC
BIC4 <- extractAIC(om4,k=log(n))[2]

#### subset size = 5 ####
p <- 5
oms5 <- summary(om5)
# AIC
AIC5 <- extractAIC(om5,k=2)[2]
# AICc
AICc5 <- extractAIC(om5,k=2)[2]+2*(p+2)*(p+3)/(n-p-1)
# BIC
BIC5 <- extractAIC(om5,k=log(n))[2]

#### subset size = 6 ####
p <- 6
oms6 <- summary(om6)
# AIC
AIC6 <- extractAIC(om6,k=2)[2]
# AICc
AICc6 <- extractAIC(om6,k=2)[2]+2*(p+2)*(p+3)/(n-p-1)
# BIC
BIC6 <- extractAIC(om6,k=log(n))[2]

## Answer
AIC <- c(AIC1,AIC2,AIC3, AIC4, AIC5, AIC6)
AICc <- c(AICc1, AICc2, AICc3, AICc4, AICc5, AICc6)
BIC <- c(BIC1, BIC2, BIC3, BIC4, BIC5, BIC6)

opmodel <- data.frame(Size=1:6, Radj2= Rad, AIC=AIC, AICc=AICc, BIC=BIC)
opmodel
#Lowest AIC, AICc, BIC values occur when size = 6. Thus, we are retaining all variables

#Checking Forward Selection 

add1(lm(Happy~1), Happy~ JobSat.f + OwnHome.f + Marital.f + Household + Age + Income, test="F")
#Age seems to be insignificant

#Performing another forward selection to see if age is actually insignificant
add1(lm(Happy ~ JobSat.f), Happy~ JobSat.f + OwnHome.f + Marital.f + Household + Age + Income, test="F")

#Backward Selection to see check the significance of variables
drop1(lm(Happy ~ JobSat.f + OwnHome.f + Marital.f + Household + Age + Income), test="F")

#Ownhome seems to be insignificant
drop1(lm(Happy ~ JobSat.f + OwnHome.f + Marital.f + Household  + Income), test="F")

#No strong evidence to drop Age and OwnHome though! The forward and Backward selections don't seem to agree!
######################################

Final_model <- lm(Happy ~ JobSat.f + OwnHome.f + Marital.f + Household + Income + Age)
summary(Final_model)

Household.f <- factor(Household)
Age_transformed <- Age^0.226

m_new <- lm(log(Happy) ~ JobSat.f + OwnHome.f + Marital.f + Household.f + log(Income) + Age_transformed)
summary(m_new)

