#In class final project 
#download file
library(here)
dat_delomys = read.csv(here("data", "delomys.csv"))

#what columns have continuous (x, body_mass, body_length, longitude, latitude)
#categorical (genus, binomial, age, sex, status)
head(dat_delomys)

plot(body_length ~ body_mass, data = dat_delomys)
hist(dat_delomys$body_mass)
hist(dat_delomys$body_length)

#Boxplot body mass by sex
boxplot(body_mass ~ sex, data = dat_delomys)
boxplot(body_mass ~ binomial, data = dat_delomys)
boxplot(body_mass ~ binomial* sex, data = dat_delomys)

#fit_1 = lm(b.sidi ~ s.sidi, data = dat_all)
fit_1 = lm(body_length ~ body_mass, data = dat_delomys)

coef(fit_1)

#Coeffiecient table
summary(fit_1)

#Anova 
anova(fit_1)
