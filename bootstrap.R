install.packages("tidyverse")
library(tidyverse)

#read in data & format
dyno <- read.csv("Dyno.csv")
dyno <- dyno[order(dyno$Name),]

str(dyno)

ikdc <- read.csv("IKDC.csv")
ikdc <- ikdc[order(ikdc$Name),]

str(ikdc)

acl.rsi <- read.csv("acl.rsi.csv")
acl.rsi <- acl.rsi[order(acl.rsi$Name),]

str(acl.rsi)

hop <- read.csv("hop.csv")
hop <- hop[order(hop$Name),]

str(hop)

#check to see names match across all dataframes
which(dyno$Name != ikdc$Name)
which(acl.rsi$Name != dyno$Name)
which(hop$Name != dyno$Name)

#marge dataframes together based on participant name
data <- merge(dyno, ikdc, by = c("Name")) %>%
  merge(acl.rsi, by = c("Name")) %>%
  merge(hop, by = c("Name"))

data$ID <- c(1:35)
str(data)

#hop test quad strength correlation
cor.test(data$hop.inj, data$quad.60.inj)
cor.test(data$quad.300.inj, data$hop.inj)

#sample size
n <- length(data_2$hop.inj)

#bootstrap 1000 times
b <- 10000
boot.cor.all <- NULL

var <- data$Age ###change var depending on the variable you want to calcualte the CI for

for (i in 1:b){
  index <- sample(1:n, replace = T)
  boot.hop.inj <- data$hop.inj[index]
  boot.quad.60.inj <- var[index]
  temp <- cor(boot.hop.inj, boot.quad.60.inj)
  boot.cor.all <- c(boot.cor.all, temp)
}

#plot bootstrap distribution
hist(boot.cor.all, prob=T)
lines(density(boot.cor.all))

#bootstrap standard error
sd(boot.cor.all)

#bootstrap CI
quantile(boot.cor.all, prob = c(0.025, 0.975))
