source('charger.R')
mondata<-charger(2007279)

# Phase 1
# a)
cor(mondata[, c('displacement','weight','origin')])

# b)
hist(mondata$mpg)
boxplot(mondata$mpg)
hist(mondata$displacement)
boxplot(mondata$displacement)
hist(mondata$weight)
boxplot(mondata$weight)

qqnorm(mondata$mpg)
qqline(mondata$mpg)
shapiro.test(mondata$mpg)
qqnorm(mondata$displacement)
qqline(mondata$displacement)
shapiro.test(mondata$displacement)
qqnorm(mondata$weight)
qqline(mondata$weight)
shapiro.test(mondata$weight)

n <- length(mondata)
T_9 <- StudentsT(df = n - 1)
mesures = data.frame(mondata=c("mpg", "displacement","Weight"),
                     Moyenne=NA, Q1=NA, Mediane=NA, Q3=NA, Écarttype=NA, ErreurType=NA,
                     IntervalleDeConfiancePourLaMoyenneBorneInférieure =NA,
                     IntervalleDeConfiancePourLaMoyenneBorneSupérieure =NA)
mesures
mesures$Moyenne    = sapply(1:3, function(i) mean(mondata[,i]))
mesures
mesures$Mediane = sapply(1:3, function(i) median(mondata[,i]))
mesures
mesures[1, c("Q1", "Q3")] = quantile(mondata$mpg, probs = c(0.25,0.75))
mesures
mesures[2, c("Q1", "Q3")] = quantile(mondata$displacement, probs = c(0.25,0.75))
mesures
mesures[3, c("Q1", "Q3")] = quantile(mondata$weight, probs = c(0.25,0.75))
mesures
mesures$Écarttype  = sapply(1:3, function(i) sd(mondata[,i]))
mesures
mesures$ErreurType = sapply(1:3, function(i) sqrt(sum((mondata[,i]-mean(mondata[,i]))^2/(length(mondata[,i])-1)))
                                           / sqrt(length(mondata[,i])))
mesures
mesures$IntervalleDeConfiancePourLaMoyenneBorneInférieure = sapply(1:3, function(i) mean(mondata[,i]) 
                                                                                    + quantile(T_9, 0.95/ 2) 
                                                                                    * sd(mondata[,i]) / sqrt(n))
mesures
mesures$IntervalleDeConfiancePourLaMoyenneBorneSupérieure = sapply(1:3, function(i) mean(mondata[,i]) 
                                                                        + quantile(T_9, 1 - 0.95/ 2) 
                                                                        * sd(mondata[,i]) / sqrt(n))
mesures

# Un test d’hypothèses sur l’égalité des variances des deux groupes.
# Un test d’hypothèses sur l’égalité des moyennes des deux groupes.


