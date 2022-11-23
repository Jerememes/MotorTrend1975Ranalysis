source('charger.R')
mondata<-charger(2007279)
View(mondata)

# Phase 1

# a)
cor(mondata[, c('displacement','weight','origin')])

# b)

# Un histogramme et un diagramme de Tukey (ou «Box Plot») ;
hist(mondata$mpg)
boxplot(mondata$mpg)

hist(mondata$displacement)
boxplot(mondata$displacement)

hist(mondata$weight)
boxplot(mondata$weight)

# Une droite de Henry (ou «Normal Probability Plot») et un test de normalité (Shapiro-Wilk) ;
qqnorm(mondata$mpg)
qqline(mondata$mpg)
shapiro.test(mondata$mpg)

qqnorm(mondata$displacement)
qqline(mondata$displacement)
shapiro.test(mondata$displacement)

qqnorm(mondata$weight)
qqline(mondata$weight)
shapiro.test(mondata$weight)

# Un tableau de statistiques descriptives comprenant : 
# moyenne, 
# quartiles, 
# écart type, 
# erreur type, 
# intervalle de confiance pour la moyenne ;

summary1 <-
  list("Miles Per Gallon" =
         list("mean (sd)" = ~ qwraps2::mean_sd(mondata$mpg)),
              "Quartiles" = ~ quantile(mondata$mpg),
     "Standard Deviation" = ~ sd(mondata$mpg),
         "Standard Error" = ~ sqrt(sum((mondata$mpg-mean(mondata$mpg))^2/(length(mondata$mpg)-1)))/sqrt(length(mondata$mpg)),
       "Confidence Range" = ~ ,
       "Displacement" =
         list("mean (sd)" = ~ qwraps2::mean_sd(mondata$displacement)),
              "Quartiles" = ~ quantile(mondata$displacement),
     "Standard Deviation" = ~ sd(mondata$displacement),
         "Standard Error" = ~ sqrt(sum((mondata$displacement-mean(mondata$displacement))^2/(length(mondata$displacement)-1)))/sqrt(length(mondata$displacement)),
       "Confidence Range" = ~ ,
       "Weight"           =
         list("mean (sd)" = ~ qwraps2::mean_sd(mondata$weight)),
              "Quartiles" = ~ quantile(mondata$weight),
     "Standard Deviation" = ~ sd(mondata$weight),
         "Standard Error" = ~ sqrt(sum((mondata$weight-mean(mondata$weight))^2/(length(mondata$weight)-1)))/sqrt(length(mondata$weight)),
       "Confidence Range" = ~ ,
       "Origin"           =
         list("mean (sd)" = ~ qwraps2::mean_sd(mondata$origin)),
              "Quartiles" = ~ quantile(mondata$origin),
     "Standard Deviation" = ~ sd(mondata$origin),
         "Standard Error" = ~ sqrt(sum((mondata$origin-mean(mondata$origin))^2/(length(mondata$origin)-1)))/sqrt(length(mondata$origin)),
       "Confidence Range" = ~ ,
  )
