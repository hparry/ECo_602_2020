#Lab 9 Questions
library(here)
library(palmerpenguins)
penguins<- penguins

##---------Question 1----------------
bartlett.test(body_mass_g ~ species, data = penguins)

##---------Question 2-----------------
bartlett.test(body_mass_g ~ sex, data = penguins)

##----------Question 3----------------
par(mar = c(8, 4, 2, 2))
boxplot(
  body_mass_g ~ sex * species,
  data = penguins,
  las = 2,
  xlab = NULL,
  ylab = "body mass(g)")

dev.off()

species_sex = aggregate(
  body_mass_g ~ sex * species,
  data = penguins,
  FUN = c)
str(species_sex)
species_sex$body_mass_g
#run the bartlett test to test for homogeneity of variance
bartlett.test(species_sex$body_mass_g)

##-------------Question 4--------------

