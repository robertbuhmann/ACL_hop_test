#Relationship between muscle function and functional performance in 
#recreational athletes following anterior cruciate ligament reconstruction

#Creator: Rob Buhmann
#Date: 5/5/23

#read in data
data <- read.csv("acl_clinical_tests_de-identified.csv")

#descriptives - 1 = injured limb; 2 = opposite limb
library(tidyverse)
names(data)

#ext_60
ext_60 <- data[, c("ID", "quad.60.inj", "quad.60.opp")]
ext_60 <- ext_60 %>%
  pivot_longer(cols = (2:3),
               names_to = "Limb",
               values_to = "ext_60")
ext_60 <- ext_60 %>%
  mutate(Limb = (ifelse(Limb == "quad.60.inj", 1, 2))) %>%
  mutate(Limb = factor(Limb),
         ID = factor(ID))

#ext_300
ext_300 <- data[, c("ID", "quad.300.inj", "quad.300.opp")]
ext_300 <- ext_300 %>%
  pivot_longer(cols = (2:3),
               names_to = "Limb",
               values_to = "ext_300")
ext_300 <- ext_300 %>%
  mutate(Limb = (ifelse(Limb == "quad.300.inj", 1, 2)))%>%
  mutate(Limb = factor(Limb),
         ID = factor(ID))

#flex_60
flex_60 <- data[, c("ID", "ham.60.inj", "ham.60.opp")]
flex_60 <- flex_60 %>%
  pivot_longer(cols = (2:3),
               names_to = "Limb",
               values_to = "flex_60")
flex_60 <- flex_60 %>%
  mutate(Limb = (ifelse(Limb == "ham.60.inj", 1, 2)))%>%
  mutate(Limb = factor(Limb),
         ID = factor(ID))

#flex_300
flex_300 <- data[, c("ID", "ham.300.inj", "ham.300.opp")]
flex_300 <- flex_300 %>%
  pivot_longer(cols = (2:3),
               names_to = "Limb",
               values_to = "flex_300")
flex_300 <- flex_300 %>%
  mutate(Limb = (ifelse(Limb == "ham.300.inj", 1, 2)))%>%
  mutate(Limb = factor(Limb),
         ID = factor(ID))

#flex_ecc
flex_ecc <- data[, c("ID", "ecc.ham.inj", "ecc.ham.opp")]
flex_ecc <- flex_ecc %>%
  pivot_longer(cols = (2:3),
               names_to = "Limb",
               values_to = "flex_ecc")
flex_ecc <- flex_ecc %>%
  mutate(Limb = (ifelse(Limb == "ecc.ham.inj", 1, 2)))%>%
  mutate(Limb = factor(Limb),
         ID = factor(ID))

descrip <- merge(ext_60, ext_300, by = c("Limb", "ID"))%>%
  merge(flex_60, by = c("Limb", "ID")) %>%
  merge(flex_300, by = c("Limb", "ID")) %>%
  merge(flex_ecc, by = c("Limb", "ID"))

means <- aggregate(cbind(ext_60, ext_300, flex_60, flex_300, flex_ecc) ~ Limb, 
                    data = descrip, FUN = mean)

sd <- aggregate(cbind(ext_60, ext_300, flex_60, flex_300, flex_ecc) ~ Limb, 
                data = descrip, FUN = sd)

write.csv(means, file = "means.csv")
write.csv(sd, file = "sd.csv")

#subset necessary columns
names(data)
vars <- c("Age", "quad.60.inj", "quad.60.lsi", "ham.60.inj", "ham.60.lsi", 
          "quad.300.inj", "quad.300.lsi", "ham.300.inj", "ham.60.lsi",
          "hop.inj", "hop.lsi", "ID", "quad.60.lsi.cat", "quad.300.lsi.cat", 
          "ecc.ham.inj", "ecc.ham.opp", "Score.IKDC", "IKDC.q2", "IKDC.q3")
data <- data[,vars]
str(data)

#explore data

for (i in 1:ncol(data)){
    hist(data[,i], main = names(data[i]))
} #histograms of each variable
#quick look- variables with non-normal looking distributions:

#Age
shapiro.test(data$Age)

#hop.lsi
shapiro.test(data$hop.lsi)

#h.q.60.inj
shapiro.test(data$h.q.60.inj)


#correlations
cor.data <- data[,-c(13,14)] #remove character variables
correlations <- data.frame(matrix(ncol = 3, nrow = 1))
colnames(correlations) <- c("p.val", "r.val", "var")

for(i in 1:ncol(cor.data)){
  columns <- colnames(cor.data)
  cor <- cor.test(cor.data$hop.inj, cor.data[,i])
  result <- data.frame(cbind(cor$p.value, cor$estimate, columns[i]))
  colnames(result) <- c("p.val", "r.val", "var")
  correlations <- rbind(result, correlations)
}

correlations <- correlations %>%
  mutate_at(c('p.val', 'r.val'), as.numeric)

correlations[,1:2] <- round(correlations[,1:2], 2)

#create plots for quad strength vs hop and ham strength vs hop
library(ggplot2)
install.packages("ggExtra")
library(ggExtra)

################################plot1#########################################

plot1 <- ggplot(data = data, aes(x = quad.60.inj, y = hop.inj)) +
  geom_point(shape = 21, color = "black", fill = 'lightcoral', size = 3) +
  theme(text = element_text(family = "serif"),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text = element_text(color = "blacK"))+
  labs(x = "Knee extensor strength (Nm/kg)",
       y = "Single leg hop distance (m)")+
  geom_smooth(method = 'lm', color = 'black', fill = 'grey70')

plot1

plot1_marg <- ggMarginal(plot1, type = "density", color = "black", 
                         fill = "lightcoral", alpha = 0.4)
plot1_marg

#########################plot2##############################################

plot2 <- ggplot(data = data, aes(x = quad.300.inj, y = hop.inj)) +
  geom_point(shape = 21, color = "black", fill = 'lightcoral', size = 3) +
  theme(text = element_text(family = "serif"),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text = element_text(color = "black"))+
  labs(x = "Knee extensor strength (Nm/kg)",
       y = "Single leg hop distance (m)")+
  geom_smooth(method = 'lm', color = 'black', fill = 'grey90')

plot2

plot2_marg <- ggMarginal(plot2, type = "density", color = "black", 
                         fill = "lightcoral", alpha = 0.4)
plot2_marg

##############################plot3######################################

plot3 <- ggplot(data = data, aes(x = ham.60.inj, y = hop.inj)) +
  geom_point(shape = 21, color = "black", fill = 'lightcoral', size = 3)+
  theme(text = element_text(family = "serif"),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text = element_text(color = 'black'))+
  labs(x = "Knee flexor strength (Nm/kg)",
       y = "Single leg hop distance (m)")+
  geom_smooth(method = 'lm', color = 'black', fill = 'grey90')

plot3

plot3_marg <- ggMarginal(plot3, type = "density", color = "black", 
                         fill = "lightcoral", alpha = 0.4)
plot3_marg

###############plot4######################################################

plot4 <- ggplot(data = data, aes(x = ham.300.inj, y = hop.inj)) +
  geom_point(shape = 21, color = "black", fill = 'lightcoral', size = 3)+
  theme(text = element_text(family = "serif"),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text = element_text(colour = "black"))+
  labs(x = "Knee flexor strength (Nm/kg)",
       y = "Single leg hop distance (m)")+
  geom_smooth(method = 'lm', color = 'black', fill = 'grey90')

plot4

plot4_marg <- ggMarginal(plot4, type = "density", color = "black", 
                         fill = "lightcoral", alpha = 0.4)
plot4_marg

#save plot
library(gridExtra)
strength_vs_hop <- grid.arrange(plot1, plot2, plot3, plot4, nrow = 2)
ggsave(filename = "strength_vs_hop.jpeg", plot = strength_vs_hop, width = 6, height = 6)

strength_vs_hop_dens <- grid.arrange(plot1_marg, plot2_marg, plot3_marg, plot4_marg, nrow = 2)
ggsave(filename = "strength_vs_hop_dens.jpeg", plot = strength_vs_hop_dens, width = 6, height = 6)

#model strength ~ hop + age
lm.fit <- lm(quad.60.inj ~ hop.inj, data = data)
summary(lm.fit)
confint(lm.fit, level = 0.95)

lm.fit2 <- lm(quad.60.inj ~ hop.inj + ecc.ham.inj + Age, data = data)
plot(lm.fit2)
summary(lm.fit2)
confint(lm.fit2, level = 0.95)

#amount of error when predicitng using lm.fit2
lm.fit2.pred <- predict(lm.fit2, data)
lm.fit.2.error <- sqrt(mean(((data$quad.60.inj-lm.fit2.pred)^2)))
lm.fit.2.error/mean(data$quad.60.inj)

lm.fit3 <- lm(quad.300.inj ~ hop.inj, data = data)
summary(lm.fit3)
confint(lm.fit3, level = 0.95)

data$hop.inj <- data$hop.inj*100
lm.fit4 <- lm(quad.300.inj ~ hop.inj + Age + ecc.ham.inj, data = data)
summary(lm.fit4)
confint(lm.fit4, level = 0.95)

#predict
ke.300.pred <- predict(lm.fit4, data)
lm.fit4.error <- sqrt(mean(((data$quad.300.inj-ke.300.pred)^2)))
lm.fit4.error/mean(data$quad.300.inj)

lm.fit5 <- lm(ham.60.inj ~ hop.inj, data = data)
summary(lm.fit5)
confint(lm.fit5, level = 0.95)

lm.fit6 <- lm(ecc.ham.inj ~ hop.inj + Age, data = data)
summary(lm.fit6)
confint(lm.fit6, level = 0.95)

#predict
ecc.ham.pred <- predict(lm.fit6, data)
lm.fit6.error <-sqrt(mean(((data$ecc.ham.inj-ecc.ham.pred)^2)))
lm.fit6.error/mean(data$ecc.ham.inj)

lm.fit7 <- lm(ham.300.inj ~ hop.inj, data = data)
summary(lm_fit7)
confint(lm_fit7)

lm.fit8 <- lm(ham.300.inj ~ hop.inj + Age, data = data)
summary(lm.fit8)
confint(lm.fit8)

#logistic regression and prediction
lr.fit <- glm(quad.60.lsi.cat ~ hop.lsi, 
              data = data, family = binomial)
confint(lr.fit)
summary(lr.fit)
lr.pred <- rep("less than 85% lsi", times = 35)
lr.probs <- predict(lr.fit, type = "response")
lr.pred[lr.probs > 0.5]="greater than 85% lsi"
table(lr.pred, data$quad.60.lsi.cat)
lr.pred

lr.fit <- glm(quad.300.lsi.cat ~ hop.lsi, 
              data = data, family = binomial)
summary(lr.fit)
plot(lr.fit)
lr.pred <- rep("less than 85% lsi", times = 35)
lr.probs <- predict(lr.fit, type = "response")
lr.pred[lr.probs > 0.5]="greater than 85% lsi"
table(lr.pred, data$quad.60.lsi.cat)

#create ecc.ham.lsi variable for modelling
data <- data %>%
  mutate(ecc.ham.lsi = ecc.ham.inj/ecc.ham.opp)%>%
  mutate(ecc.ham.lsi.cat = ifelse(ecc.ham.lsi>0.85,1,0))

lr.fit2 <- glm(ecc.ham.lsi.cat ~ hop.lsi, data = data, family = binomial)
summary(lr.fit2)
confint(lr.fit2)

#plot for logistic regression model
install.packages("visreg")
library(visreg)

log_reg_plot <- visreg(lr.fit, scale = "response", ,
                       line = list(col = 'firebrick'), 
                       points = list(col = 'black', lwd = 0.5),
                       gg = TRUE)

log_reg_plot <- log_reg_plot+
  theme(text = element_text(family = 'serif', color = 'black',size = 12),
        panel.background = element_blank(),
        axis.line = element_line(color = 'black'),
        axis.text = element_text(color = 'black', size = 10))+
  labs(x = "Single leg hop symmetry index",
       y = "Knee extensor \n strength symmetry category")+
  scale_y_continuous(limits = c(0,1), breaks = c(0,1))
log_reg_plot

ggsave(filename = "logisitic regression.jpeg", plot = log_reg_plot, 
       width = 6, height = 6)
