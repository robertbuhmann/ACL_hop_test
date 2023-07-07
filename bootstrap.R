install.packages("tidyverse")
library(tidyverse)

#read in data & format
dyno <- read.csv("acl_clinical_tests_de-identified.csv")

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
