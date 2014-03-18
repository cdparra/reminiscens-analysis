library("car")
# Repeated Measures ANOVA =appversion vs. # stories per session
# 1. Prepare Groups, Participants and Values
View(rses)
Groups <- rses$appversion
Participants <- rses$sid
Values <- rses$stat_stories
# 2. Create a data frame with the three columns: the groups, the participants and the values
data <- data.frame(Participants,Groups,Values)
View(data)
# 3. Create a matrix of Participants vs Groups with values in the cells 
matrix <- with(data, cbind(Values[Groups==1], Values[Groups==2], Values[Groups==3], Values[Groups==4]))
View(matrix)
# 5. Build a multivariate linear model with the matrix you've just created.
model <- lm(matrix ~ 1)
# 6. Define the design of the study, which basically means that you need to make 
#    a list of the independent variable. 
design <- factor(c(1, 2, 3, 4))
# 7. Run the ANOVA Test. The car package has Anova() function, which includes Mauchly's test.
library(car)
options(contrasts=c("contr.sum", "contr.poly"))
aov <- Anova(model, idata=data.frame(design), idesign=~design, type="III")
summary(aov, multivariate=F)

# Example of Results
#
# Univariate Type III Repeated-Measures ANOVA Assuming Sphericity
#
#SS num Df Error SS den Df        F    Pr(>F)    
#(Intercept) 1472.00      1   161.50     22 200.5201 1.556e-12 ***
#  design         4.43      3   518.07     66   0.1883     0.904    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#
#Mauchly Tests for Sphericity
#
#Test statistic  p-value
#design        0.60461 0.064282
#
#
#Greenhouse-Geisser and Huynh-Feldt Corrections
#for Departure from Sphericity
#
#GG eps Pr(>F[GG])
#design 0.77446      0.859
#
#HF eps Pr(>F[HF])
#design 0.8713845  0.8807694