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

library(distributions3)
n <- length(mondata)
T_9 <- StudentsT(df = n - 1)
mesures = data.frame(mondata=c("mpg", "displacement","Weight"),
                     Moyenne=NA, Q1=NA, Mediane=NA, Q3=NA, Écarttype=NA, ErreurType=NA,
                     IntervalleDeConfiancePourLaMoyenneBorneInférieure =NA,
                     IntervalleDeConfiancePourLaMoyenneBorneSupérieure =NA)
mesures$Moyenne    = sapply(1:3, function(i) mean(mondata[,i]))
mesures$Mediane = sapply(1:3, function(i) median(mondata[,i]))
mesures[1, c("Q1", "Q3")] = quantile(mondata$mpg, probs = c(0.25,0.75))
mesures[2, c("Q1", "Q3")] = quantile(mondata$displacement, probs = c(0.25,0.75))
mesures[3, c("Q1", "Q3")] = quantile(mondata$weight, probs = c(0.25,0.75))
mesures$Écarttype  = sapply(1:3, function(i) sd(mondata[,i]))
mesures$ErreurType = 
  sapply(1:3, 
    function(i) sqrt(
      sum(
        (mondata[,i]-mean(mondata[,i]))^2/(length(mondata[,i])-1)
      )
    )
    / sqrt(length(mondata[,i]))
  )
mesures$IntervalleDeConfiancePourLaMoyenneBorneInférieure = 
  sapply(1:3, function(i)
    mean(mondata[,i]) 
  + quantile(T_9, 0.95/ 2) 
  * sd(mondata[,i]) / sqrt(n)
  )
mesures$IntervalleDeConfiancePourLaMoyenneBorneSupérieure = 
  sapply(1:3, function(i)
    mean(mondata[,i])
  + quantile(T_9, 1 - 0.95/ 2)
  * sd(mondata[,i]) / sqrt(n)
  )
mesures

# c)

library(magrittr)
library(ggplot2)
multi <- data.frame(
  type = c( rep("Origin 0"), rep("Origin 1")),
  value = c( mondata$mpg[mondata$origin=="0"], mondata$mpg[mondata$origin=="1"])
)
multi %>%
  ggplot(aes(x=value, fill=type)) +
  geom_histogram(color="#e9ecef",
                 alpha=0.6,
                 position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  labs(fill="") + ggtitle('mpg')

boxplot(mondata$mpg[mondata$origin=="0"]~mondata$mpg[mondata$origin=="1"],
        data=mondata,
        col=c("lightpink","lightblue"),
        horizontal=TRUE,
        notch=TRUE,
        main=paste("mpg de ",nrow(mondata)," de véhicules"),
        ylab="Origine",
        las=1)


multi <- data.frame(
  type = c( rep("Origin 0"), rep("Origin 1")),
  value = c( mondata$displacement[mondata$origin=="0"], mondata$displacement[mondata$origin=="1"])
)
multi %>%
  ggplot(aes(x=value, fill=type)) +
  geom_histogram(color="#e9ecef",
                 alpha=0.6,
                 position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  labs(fill="") + ggtitle('displacement')
multi <- data.frame(
  type = c( rep("Origin 0"), rep("Origin 1")),
  value = c( mondata$weight[mondata$origin=="0"], mondata$weight[mondata$origin=="1"])
)
multi %>%
  ggplot(aes(x=value, fill=type)) +
  geom_histogram(color="#e9ecef",
                 alpha=0.6,
                 position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  labs(fill="") + ggtitle('weight')



