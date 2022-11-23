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

# c)

summary1 <-
  list("Miles Per Gallon" =
         list("mean (sd)" = ~ mean_sd(mondata$mpg)),
              "Quartiles" = ~ quantile(mondata$mpg),
     "Standard Deviation" = ~ sd(mondata$mpg),
         "Standard Error" = ~ sqrt(sum((mondata$mpg-mean(mondata$mpg))^2/(length(mondata$mpg)-1)))/sqrt(length(mondata$mpg)),
       "Confidence Range" = ~ confint(lm(mondata$mpg ~ 1, mondata)),
       "Displacement" =
         list("mean (sd)" = ~ mean_sd(mondata$displacement)),
              "Quartiles" = ~ quantile(mondata$displacement),
     "Standard Deviation" = ~ sd(mondata$displacement),
         "Standard Error" = ~ sqrt(sum((mondata$displacement-mean(mondata$displacement))^2/(length(mondata$displacement)-1)))/sqrt(length(mondata$displacement)),
       "Confidence Range" = ~ confint(lm(mondata$displacement ~ 1, mondata)),
       "Weight"           =
         list("mean (sd)" = ~ mean_sd(mondata$weight)),
              "Quartiles" = ~ quantile(mondata$weight),
     "Standard Deviation" = ~ sd(mondata$weight),
         "Standard Error" = ~ sqrt(sum((mondata$weight-mean(mondata$weight))^2/(length(mondata$weight)-1)))/sqrt(length(mondata$weight)),
       "Confidence Range" = ~ confint(lm(mondata$weight ~ 1, mondata)),
       "Origin"           =
         list("mean (sd)" = ~ mean_sd(mondata$origin)),
              "Quartiles" = ~ quantile(mondata$origin),
     "Standard Deviation" = ~ sd(mondata$origin),
         "Standard Error" = ~ sqrt(sum((mondata$origin-mean(mondata$origin))^2/(length(mondata$origin)-1)))/sqrt(length(mondata$origin)),
       "Confidence Range" = ~ confint(lm(mondata$origin ~ 1, mondata)),
  )

whole <- summary.table(mondata, summary1)

# Phase 2

# d)

# Effectuez l’ajustement (i.e. obtenir le tableau des coefficients de régression, le tableau d’analyse de la variance).

# Tester la signification du modèle et effectuez une analyse des résidus (normalité, homoscédasticité, points atypiques, etc.)

# Donner un intervalle de confiance pour chacun des paramètres β0 et β1 des modèles 1 et 5.

# En conclusion : effectuez une comparaison et dire lequel des huit modèles est préférable aux autres. Justifiez votre choix en précisant les critères utilisés.


# e)

# Sur la base du meilleur modèle que vous avez obtenu en d), calculez un intervalle de prévision pour l’efficacité en carburant d’un véhicule ayant les caractéristiques suivantes : X1 = 190; X2 = 2500. Commentez brièvement votre résultat.
