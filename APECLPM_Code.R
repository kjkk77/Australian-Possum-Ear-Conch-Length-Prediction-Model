###############################################
# Group Aussie Possum Enthusiasts Report code
#
# Timmy Diep
# Angelo Randazzo 
# Hongjoon Kim 
# Ryan Sharp
#
#
################################################

library(dplyr) # For general data manipulation
library(tibble) # For data frame manipulation
library(ggplot2)
library(DAAG)
library(gridExtra)
library(car)
library(MASS)
library(reshape2)

#########################################################################
# DATA CLEANING
#######################################################################
dirtypossumDF <- possum # LOAD Possum data set from DAAG library

possumDF <- na.omit(dirtypossumDF) # remove NA's

psDF <- possumsites # Load the possum site data set from the DAAG library

psDF <- rownames_to_column(psDF) #make possum site rownames into colums
site <- seq_len(nrow(psDF)) # Get the site number
psDF <- cbind(psDF, site) # Attach site number as column

poss1 <- right_join(possumDF, psDF, by = "site") # Combine by Site (dpylr)

#head(poss1)

# Split site names into separate columns and put a 1 if a possum was caught in that site
poss1$Cambarville = ifelse(poss1$rowname == "Cambarville", 1, 0)
poss1$Bellbird = ifelse(poss1$rowname == "Bellbird" , 1, 0)
poss1$AllynRiver = ifelse(poss1$rowname == "Allyn River" , 1, 0)
poss1$WhianWhian = ifelse(poss1$rowname == "Whian Whian" , 1, 0)
poss1$Byrangery = ifelse(poss1$rowname == "Byrangery" , 1, 0)
poss1$Conondale = ifelse(poss1$rowname == "Conondale" , 1, 0)
poss1$Bulburin = ifelse(poss1$rowname == "Bulburin" , 1, 0)

# If a possum was caught in Cambarville and Bellbird, aka the Southern possums have a 1 and northern possums have a zero
# Southern possums are Cunninghami species
poss1$Species = poss1$Cambarville + poss1$Bellbird 
poss1$Species = as.factor(poss1$Species) # converting to categorical
poss1$Male = ifelse(poss1$sex == "m", 1, 0)
poss1$Male <- factor(poss1$Male, levels = c(0,1))


######################################################################
# LOOKING FOR A GOOD RESPONSE VARIABLE
# Ear Conch as a good range and since it is had to measure the ear conch, 
# there is a reason to predict it's measurement

#                                 Head Length: Var 12.38: Range = 20.6
var(poss1$hdlngth)
range(poss1$hdlngth)
103.1 - 82.5

#                                 Skull Witdh: Var 9.62 : Range 18.6
var(poss1$skullw)
range(poss1$skullw)
68.6-50

#                                 Total Length: Var 17.61 : Range 21.5
var(poss1$totlngth)
range(poss1$totlngth)
96.5 - 75

#                                 Tail Length: Var  3.88: Range = 11
var(poss1$taill)
range(poss1$taill)
43-32

#                                 Foot Length: Var 19.48 : Range 17.6
var(poss1$footlgth)
range(poss1$footlgth)
77.9 - 60.3

#                                 Ear Conch: Var 16.49 : Range 14.9
var(poss1$earconch)
range(poss1$earconch)
56.2 - 41.3

#                                 Eye Width: Var 1.12 : Range 5
var(poss1$eye)
range(poss1$eye)
17.8-12.8

#                                 Chest Width: var 4.08: Range 10
var(poss1$chest)
range(poss1$chest)
32-22

#                                 Belly Girth: var 7.44: Range 15
var(poss1$belly)
range(poss1$belly)
40 -25

################################################################################
#################### Kim's Exploratory Data Analysis ###########################

# Distribution of Ear Conch
ggplot(poss1, aes(x=earconch, fill=Species)) + 
  geom_histogram(color="black", binwidth=1) +
  ggtitle("Distribution of Possum Ear Conch") + 
  theme(text = element_text(size = 15))

# Prepare Data Frame for dplyr::melt:
newposs2 <- poss1# Assign to new var for minor tweaking

newposs2$Pop <- ifelse(newposs2$Pop == "Vic", 1, 2) # Change Pop to 1 & 2
melt <- dplyr::select(newposs2,-case, -sex, -rowname,
                      -Cambarville, -Bellbird, -AllynRiver,
                      -WhianWhian, -Byrangery, -Conondale, 
                      -Bulburin, -Longitude, -Latitude,
                      -altitude) # Assign new data frame

melt$Species <- as.numeric(melt$Species) # Convert Species to numeric

melt$Male <- as.numeric(melt$Male) # Convert Male to numeric

# Melt 1: Ear Conch Length vs. Continuous Predictors

meltcont <- dplyr::select(melt, -site,-Pop, -Male, -age) # Remove Discrete Var.

meltcont$Species <- ifelse(meltcont$Species == 2, "T. Cunninghami", "T. Caninus")
# Rename Species

earconchdf1 <- melt(data = meltcont, id = c("Species","earconch")) 
# Melt w/ Species & earconch as variables

ggplot(data = earconchdf1, aes(x = value, y = earconch)) +
  geom_point(aes(color = variable)) +
  facet_wrap(~variable, scales = "free") + 
  theme(axis.text.x = element_text(angle = -55, size = 8)) +
  ggtitle("Ear Conch vs. Regressors") +
  geom_smooth(method = "lm", se = FALSE) +
  theme(legend.position = "none")# Scatter Plots w/o Species Distinction

ggplot(data = earconchdf1, aes(x = value, y = earconch)) +
  geom_point(aes(color = Species)) +
  facet_wrap(~variable, scales = "free") + 
  theme(axis.text.x = element_text(angle = -55, size = 8)) +
  ggtitle("Ear Conch vs. Regressors") +
  geom_smooth(method = "lm", se = FALSE) # Scatter Plots w/ Species Distinction


# Melt 2: Ear Conch Length vs. Discrete Predictors:

meltdisc <- dplyr::select(melt, Species, age, Male, earconch) 
# Select Discrete Var.

meltdisc$Species <- ifelse(meltdisc$Species == 2, "T. Cunninghami", "T. Caninus")
# Rename Species

earconchdf2 <- melt(data = meltdisc, id = c("Species","earconch"))
# Melt w/ Species & earconch as variables

for (i in 1:length(earconchdf2$value)) {
  if (earconchdf2$variable[i] == "Male") {
    if(earconchdf2$value[i] == 2) {
      earconchdf2$value[i] <- "Yes"
    }
    else {
      earconchdf2$value[i] <- "No"
    }
  }
  else {
    next
  }
} # Re-assigned values corresponding to Male (Coercion into character vector)


ggplot(data = earconchdf2, aes(x = value, y = earconch, group = value)) +
  geom_boxplot(aes(color = variable))+
  facet_wrap(~variable, scales = "free") + 
  theme(axis.text.x = element_text(angle = -55, size = 8)) + 
  theme(legend.position = "none") +
  ggtitle("Ear Conch vs. Age/Sex") # Box Plots w/o Species Distinction

ggplot(data = earconchdf2, aes(x = value, y = earconch)) +
  geom_boxplot(aes(color = Species)) +
  facet_wrap(~variable, scales = "free") + 
  theme(axis.text.x = element_text(angle = -55, size = 8)) +
  theme(legend.position = "none") +
  ggtitle("Ear Conch vs. Age/Sex") # Box Plots w/ Species Distinction

########### Kim's Influential Analysis: Leverage/Influential Points ############



###############################################################################
# Look at full model and get fitted model
###############################################################################

xAge <- poss1$age
xHL <- poss1$hdlngth
xSW <- poss1$skullw
xTotL <- poss1$totlngth
xTailL <- poss1$taill
xFL <- poss1$footlgth
xEC <- poss1$earconch
xEye <- poss1$eye
xChest <- poss1$chest
xBelly <- poss1$belly
xSpec <- poss1$Species
xSex <- poss1$sex

fullModel <- lm(xEC ~ xAge + xHL + xSW + xTotL + xTailL + xFL + xEye + xChest + xBelly + xSpec + xSex)

summary(fullModel)

vif(fullModel)

################################################################################
# Forward Approach
###############################################################################

FAfit <- lm(xEC ~ xAge)
summary(FAfit)
FAfit <- lm(xEC ~ xHL)
summary(FAfit)
FAfit <- lm(xEC ~ xSW)
summary(FAfit)
FAfit <- lm(xEC ~ xTotL)
summary(FAfit)
FAfit <- lm(xEC ~ xTailL)
summary(FAfit)
FAfit <- lm(xEC ~ xFL)
summary(FAfit)
FAfit <- lm(xEC ~ xEye)
summary(FAfit)
FAfit <- lm(xEC ~ xChest)
summary(FAfit)
FAfit <- lm(xEC ~ xBelly)
summary(FAfit)
FAfit <- lm(xEC ~ xSex)
summary(FAfit)
############################# SPECIES IS BEST
FAfit <- lm(xEC ~ xSpec)
summary(FAfit)
##############################

# Round 2 ###################################################################
fit <- lm(xEC ~ xSpec)

FAfit <- lm(xEC ~ xSpec + xAge)
summary(FAfit)
anova(FAfit, fit)

############################### Head Length is best
FAfit <- lm(xEC ~ xSpec + xHL)
summary(FAfit)
anova(FAfit, fit)
##############################

FAfit <- lm(xEC ~ xSpec + xSW)
summary(FAfit)
anova(FAfit, fit)

FAfit <- lm(xEC ~ xSpec + xTotL)
summary(FAfit)
anova(FAfit, fit)

FAfit <- lm(xEC ~ xSpec + xTailL)
summary(FAfit)
anova(FAfit, fit)

FAfit <- lm(xEC ~ xSpec + xFL)
summary(FAfit)
anova(FAfit, fit)

FAfit <- lm(xEC ~ xSpec + xEye)
summary(FAfit)
anova(FAfit, fit)

FAfit <- lm(xEC ~ xSpec + xChest)
summary(FAfit)
anova(FAfit, fit)

FAfit <- lm(xEC ~ xSpec + xBelly)
summary(FAfit)
anova(FAfit, fit)

FAfit <- lm(xEC ~ xSpec + xSex)
summary(FAfit)
anova(FAfit, fit)


# ROUND 3 ####################################################
fit <- lm(xEC ~ xSpec + xHL)

FAfit <- lm(xEC ~ xSpec + xHL + xChest)
summary(FAfit)
anova(FAfit, fit)

FAfit <- lm(xEC ~ xSpec + xHL + xAge)
summary(FAfit)
anova(FAfit, fit)

FAfit <- lm(xEC ~ xSpec + xHL + xSW)
summary(FAfit)
anova(FAfit, fit)

FAfit <- lm(xEC ~ xSpec + xHL + xTotL)
summary(FAfit)
anova(FAfit, fit)

FAfit <- lm(xEC ~ xSpec + xHL + xTailL)
summary(FAfit)
anova(FAfit, fit)

FAfit <- lm(xEC ~ xSpec + xHL + xFL)
summary(FAfit)
anova(FAfit, fit)

FAfit <- lm(xEC ~ xSpec + xHL + xEye)
summary(FAfit)
anova(FAfit, fit)

######################################### Chest is next best with P of .16
FAfit <- lm(xEC ~ xSpec + xHL + xChest)
summary(FAfit)
anova(FAfit, fit)
############################################

FAfit <- lm(xEC ~ xSpec + xHL + xBelly)
summary(FAfit)
anova(FAfit, fit)

FAfit <- lm(xEC ~ xSpec + xHL + xSex)
summary(FAfit)
anova(FAfit, fit)

# ROUND 4 ##############################################################
fit <- lm(xEC ~ xSpec + xHL + xChest)

############################################## Not of these make and model much better 
FAfit <- lm(xEC ~ xSpec + xHL + xChest + xAge)
summary(FAfit)
anova(FAfit, fit)

FAfit <- lm(xEC ~ xSpec + xHL + xChest + xSW)
summary(FAfit)
anova(FAfit, fit)

FAfit <- lm(xEC ~ xSpec + xHL + xChest + xTotL)
summary(FAfit)
anova(FAfit, fit)

FAfit <- lm(xEC ~ xSpec + xHL + xChest + xTailL)
summary(FAfit)
anova(FAfit, fit)

FAfit <- lm(xEC ~ xSpec + xHL + xChest + xFL)
summary(FAfit)
anova(FAfit, fit)

FAfit <- lm(xEC ~ xSpec + xHL + xChest + xEye)
summary(FAfit)
anova(FAfit, fit)

FAfit <- lm(xEC ~ xSpec + xHL + xChest + xBelly)
summary(FAfit)
anova(FAfit, fit)

FAfit <- lm(xEC ~ xSpec + xHL + xChest + xSex)
summary(FAfit)
anova(FAfit, fit)

###########################################
# FINAL MODEL
fit <- lm(xEC ~ xSpec + xHL + xChest)

anova(fit, fullModel)

#### VARIABLE INFLATION FACTORS
vif(fit)

vif(fullModel)

##############################################################################
# Box - Cox  &  Residual Analysis
#############################################################################

### BOX-COX TRANSFORMATION
bc <- boxcox(fit, plotit = TRUE)
(bc.power <- bc$x[which.max(bc$y)])

# Studentized Residuals
studres(fit)
range(studres(fit))

barplot(height = studres(fit), names.arg = 1:101,
        main = "Studentized Residuals", xlab = "Index",
        ylab = "Studentized Resid", ylim=c(-4,4))
abline(h=3, col = 'Red', lwd = 3)
abline(h=-3, col = 'Red', lwd = 3)

# Rstudent Residuals
Rstudent <- rstudent(fit)
range(Rstudent)
cor.level <- 0.05/(5*101)

cor.qt <- qt(cor.level, 95, lower.tail=F)
Rstudent > cor.qt
barplot(height = Rstudent, names.arg = 1:101,
        main = "R Student Residuals", xlab = "Index",
        ylab = "R Student Resid", ylim=c(-4,4))
abline(h=cor.qt , col = "Red", lwd=3)
abline(h=-cor.qt , col = "Red", lwd=3)

# Residuals vs. Fitted Values
par(mfrow=c(1,2))
hist(studres(fit), breaks=10, freq=F, col="cornflowerblue",
     cex.axis=1.5, cex.lab=1.5, cex.main=2, title = 'Histogram of studres')
qqPlot(fit)

residualPlot(fit, type="rstudent", quadratic=F, col = poss1$Species,
             pch=16, cex=1.5, cex.axis=1.5, cex.lab=1.5, main = 'Reduced Model Residuals'
             , ylim = c(-3, 3))
abline(h=3, col = "Red", lwd=2)
abline(h=-3, col = "Red", lwd=2)

# Species 1 Residuals vs. Fitted Values
residualPlot(fit, type="rstudent", quadratic=F, col = poss1$Species,
             pch=16, cex=1.5, cex.axis=1.5, cex.lab=1.5, main = 'Species 1 Residuals',
             xlim = c(43, 47), ylim = c(-3, 3))
abline(h=3, col = "Red", lwd=2)
abline(h=-3, col = "Red", lwd=2)

# Species 2 Residuals vs. Fitted Values
residualPlot(fit, type="rstudent", quadratic=F, col = poss1$Species,
             pch=16, cex=1.5, cex.axis=1.5, cex.lab=1.5, main = 'Species 2 Residuals',
             xlim = c(51, 54), ylim = c(-3, 3))
abline(h=3, col = "Red", lwd=2)
abline(h=-3, col = "Red", lwd=2)


################################################################################
# Influential Analysis
################################################################################

themod <- lm(earconch ~ Species + hdlngth + chest, data = poss1) # Final Model 
summary(themod) # Summary of the Final Model

myInf <- influence.measures(themod) # Leverage/Influential Points Diagnostics
summary(myInf) # Summary of the Diagnostics

dfbetasPlots(themod,intercept=T) # DFBETA Plots

influenceIndexPlot(themod, vars=c("Cook", "hat")) # Cook's & Hat-value Plots

#############################









