#------And then load these packages, along with the boot package.-----
rm(list=ls())
library(corpcor)
library(GPArotation)
library(psych)
library(ggplot2)
library(ggfortify)
library(nFactors)
library(plyr)
library(gplots)
library(reshape)
library(robustHD)
library(expm)

#********************* RAQ Example ********************

#load data and made labels for each


setwd("C:/Users/jaya/Documents/Great Lakes/Advanced Stats/GA")
getwd()

cereal1<-read.csv("cereal.csv", header = TRUE)
##dim(cereal1)
cereal<-cereal1[2:26]
summary(cereal)
##We can see that there are values of 6 which is not expected; the max. of the scale is 5. Let's replace '6' by '5'.
cereal[cereal==6] <- 5
##Recode the scores on negative variables like Soggy, Boring etc.
cereal[,c(12,25)] <- 6 - cereal[,c(12,25)]

#create a correlation matrix
corrpaste<-cor(cereal)
corrpaste


#Bartlett's test
## Bartlett's sphericity test 
##the null hypothesis is that the data dimension reduction is not possible.
##if p-value is less than 0.05,dimension reduction is possible.
bartlettTest = cortest.bartlett(corrpaste, nrow(cereal))
print(bartlettTest)

#$chisq
#[1] 2877.739
#
#$p.value
#[1] 0
#
#$df
#[1] 300
print(bartlettTest$chisq)
print(bartlettTest$p.value)

#####
####KMO Test###
## KMO calculation
kmoTest = KMO(corrpaste)
print(kmoTest)
kmo = kmoTest$MSA
# Reporting the conclusion
print(kmo)
{
  if (kmo >= 0.00 && kmo < 0.50){test <- 'The KMO test yields a degree of common variance unacceptable for FA.'} 
  else if (kmo >= 0.50 && kmo < 0.60){test <- 'The KMO test yields a degree of common variance negligible.'} 
  else if (kmo >= 0.60 && kmo < 0.70){test <- 'The KMO test yields a degree of common variance moderate'} 
  else if (kmo >= 0.70 && kmo < 0.80){test <- 'The KMO test yields a degree of common variance medium' } 
  else if (kmo >= 0.80 && kmo < 0.90){test <- 'The KMO test yields a degree of common variance admirable' }
  else { test <- 'The KMO test yields a degree of common variance marvelous.' }
}
test

#KMO per variable
kmoVariables = kmoTest$MSAi
print(kmoVariables)

##How many Factors are there in the data?
?fa.parallel
numFactors <- fa.parallel(cereal, fm="pa", fa="fa")
numFactors
numfactors2 <-fa.parallel(cereal,fm="minres", fa="both")
numfactors2
numfactors3 <-fa.parallel(cereal,fm="pa", fa="both")
numfactors3

##no. of factors through initial PCA and understand the eigen values and the variance explained by them.
pc1 <-  principal(cereal, nfactors = length(cereal), rotate = "none")
pc1

?principalAxis
pc2 <- principalAxis(corrpaste, nFactors = 2, communalities = "component")
pc2

#### principal component using princomp and biplot
cereal.pca <- pca(cereal,nfactors=25,rotate="none", scores=TRUE)
cereal.pca
Z<-cereal.pca$scores
cereal.pca$loadings
loadings(cereal.pca)
summary(cereal.pca)
print(cereal.pca$values)
##sum((cereal.pca$sdev)^2) 

## 

##plot(cereal.pca,type="lines") 

plot(cereal.pca$values,type = "b",xlab = "Factors",ylab = "Eigen Values",main = "Screeplot")


#### Using prcomp() function
cereal.pca1 <- prcomp(cereal, xlab = "Factors", scale = TRUE)
screeplot(cereal.pca1, type = "l")
colMeans(cereal)
## Variables means
cereal.pca$center
cereal.pca$scale
## Variables standard deviation
cereal.pca$scale
apply(cereal, 2, sd)

##### Using fa Command
?fa
pcal2<-fa(cereal,nfactors = 4,rotate="none", scores=TRUE, fm = "pa")
pcal2
pcal2$values
ff2<-pcal2$loadings
ff2
pcal2$scores #### The formula based calculations are provided below.

fa.diagram(pcal2)

## communality calculation
sum(ff2[2,]^2)

#### This step proves that the correlation matrix is a proudct 
####of matrices: factor loadings and the transpose of factor loadings

correl3<-ff2%*%t(ff2)
correl3

####Factor Scores are calculated using Thurstone method 
#### Reference ### http://personality-project.org/r/html/factor.scores.html

##### factor Analysis with rotation
pcal3<-fa(cereal,nfactors=4,rotate="varimax", scores=TRUE, fm="ml")
pcal3
pcal3$values
ff3<-pcal3$loadings
ff3
pcal3$scores
fa.diagram(pcal3)

##calculate comparative fit index (a goodness of fit metric)
((pcal3$STATISTIC-pcal3$dof)/(pcal3$null.chisq-pcal3$null.dof))

##reliability of factors:
factor1 <- c(2,3,4,8,9,14,19,23,26)
factor2 <- c(5,7,16,20,22)
factor3 <- c(6,11,13,15)
factor4 <- c(10,12,17,18,21,24,25)
factor1alpha <- psych::alpha(cereal1[,factor1], check.keys = TRUE)
factor2alpha <- psych::alpha(cereal1[,factor2], check.keys = TRUE)
factor3alpha <- psych::alpha(cereal1[,factor3], check.keys = TRUE)
factor4alpha <- psych::alpha(cereal1[,factor4], check.keys = TRUE)

factor1alpha$total$raw_alpha

factor2alpha$total$raw_alpha

factor3alpha$total$raw_alpha

factor4alpha$total$raw_alpha

##Creating Average Factor Scores grouped by the cereal.
cereal1$factor1Score <- apply(cereal1[,factor1],1,mean)
cereal1$factor2Score <- apply(cereal1[,factor2],1,mean)
cereal1$factor3Score <- apply(cereal1[,factor3],1,mean)
cereal1$factor4Score <- apply(cereal1[,factor4],1,mean)
colnames(cereal1)[27:30] <-c("Nutritional Value", "Fat Content", "Value for money", "Taste & Texture")
aggregateCereal<-aggregate(cereal1[,27:30],  list(cereal1[,1]), mean)
format(aggregateCereal, digits = 2) -> x
write.csv(x,file="cerealfa.csv")
?write.csv
                                              
##############################################################################################
##############################################################################################



