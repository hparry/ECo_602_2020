#Using Models 2 Walkthrough
library(here)
library(palmerpenguins)
penguins<- penguins

##---------1-sample t-test--------
t.test(subset(penguins,species == "Gentoo")$flipper_length_mm)

# flipper length is 218, use mu= 218
t.test(
  x = subset(penguins, species == "Gentoo")$flipper_length_mm,
  mu = 218
)

#flipper length smaller than 218, use alternative = "less"
t.test(
  x = subset(penguins, species == "Gentoo")$flipper_length_mm,
  mu = 218,
  alternative = "less"
)

##-----------2-sample t-test---------
#flipper lenghts of 2 species
t.test(flipper_length_mm ~ species, data = subset(penguins, species != "Chinstrap"))

##-----------ANOVA----------##
#Graphical exploration
#---normality
par(mfrow = c(1, 2))
hist(penguins$body_mass_g, breaks = 80, main = "histogram of body mass", xlab = "body mass (g)")
plot(density(penguins$body_mass_g, na.rm = TRUE), main = "density plot of body mass")

#----Conditional Boxplots
boxplot(body_mass_g ~ species, data = penguins)

#numerical
#-----mean
dat_chinstrap = subset(penguins, species == "Chinstrap")
mean(dat_chinstrap$body_mass_g, na.rm = TRUE)

#-------shortcut for calculating all of the means
aggregate(body_mass_g ~ species, data = penguins, FUN = mean)

#----Shapiro test
shapiro.test(dat_chinstrap$body_mass_g)
#p-value= 0.5605 the data is normally distributed
# I got errors using this: aggregate(body_mass_g ~ species, data = penguins, FUN = shapiro.test)

#fit a linear model
fit_species = lm(body_mass_g ~ species, data = penguins)
summary(fit_species)

dev.off()

#Conduct ANOVA
anova(fit_species)


##---------Two-way ANOVA-------------
#graphical data exploration
boxplot(body_mass_g ~ sex * species, data = penguins)

#fit a linear model
fit_both = lm(body_mass_g ~ sex * species, data = penguins)

#Examine model coeffiecients
summary(fit_both)

#Conduct ANOVA
anova(fit_both)


#I was trying to figure out how to calculate the mean body mass of the male chinstrap penguins
#mean(dat_chinstrap$body_mass_g)
#mean(dat_chinstrap$body_mass_g == "female")
#mean(dat_chinstrap$body_mass_g)
#dat_chinstrap = subset(penguins, species == "Chinstrap")
#male_chinstrap = subset(dat_chinstrap, sex == "Male")
