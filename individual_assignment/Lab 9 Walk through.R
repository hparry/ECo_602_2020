#Lab 9 Walkthrough
library(here)
library(boot)
library(simpleboot)
catrate = read.csv(here("data", "catrate.csv"))
head(catrate)

##-------------Binomial Test of Proportions-----------
#we had to first compute the number of successes and the 
#total number of years pooled across all 14 ponds
success = sum(catrate$success)
years = sum(catrate$years)
binom.test(success,years)
# ?binom.test,binom.test(x, n, p = 0.5)
#x= number of success, n = number of trials, p= 0.5 default

#What is the evidence that reproductive success is more or less frequent than the late-filling rate?
#In this scenario, we expect successful reproduction in approximately 5 of every 7 tears.
binom.test(success, years, p = 5/7)

#We might instead prefer the one-sided alternative hypothesis
#that the observed success rate is less than the pond late-fill rate.
#We can perform the test as follows:

binom.test(success, years, p = 5/7, alternative = "less")

##-------------------F-distribution---------------

veg = read.csv(here("data", "vegdata.csv"), header=TRUE)
head(veg)

#graphical data exploration
boxplot(pine ~ treatment, data = veg)

##---------------Variance test------------
var.test(
  pine ~ treatment,
  data = veg,
  subset = treatment %in% c('control','clipped'))

##---------------Normality test----------

shapiro.test(veg$pine[veg$treatment=="control"])
shapiro.test(veg$pine[veg$treatment=="clipped"])

#Non-parametric variance test
#fligner test
fligner.test(pine ~ treatment, 
             data = veg,
             subset = treatment %in% c('control','clipped'))

#Bartlett test for homogeneity of variances
bartlett.test(pine ~ treatment, data=veg)

#Fligner can do more than 2 variances as well.
fligner.test(pine ~ treatment, data = veg)
#Comparing two sample means
#t-test
t.test(pine~treatment,data=veg,subset=treatment %in% c('control','clipped'), conf.int=TRUE)

#Wilcox test
wilcox.test(pine~treatment,data=veg,subset=treatment %in% c('control','clipped'), conf.int=TRUE)

##---------------tests for paired samples-------------
#create separate vectors for the control observations and the clipped observations
control = veg$pine[veg$treatment=='control']
clipped = veg$pine[veg$treatment=='clipped']

t.test(control, clipped, paired=TRUE)

#Wilcox test
#we should use the wilcox test because the data is not normally distributed
wilcox.test(control, clipped, paired=TRUE)

##--------------Correlation---------------
disp = read.csv(here("data", "dispersal.csv"), header=TRUE)
disp

#plot dispersal rate
plot(disp$disp.rate.ftb, disp$disp.rate.eb)

#test the significance of correlation, cor.test()
cor.test(
  disp$disp.rate.ftb,
  disp$disp.rate.eb,
  use='complete.obs')

#For non-bivariate, non-normal distribution, use Spearman or Kendall
cor.test(
  disp$disp.rate.ftb,
  disp$disp.rate.eb,
  use='complete.obs',
  method='spearman')

#--------------Comparing two distributions------------
plot(
  ecdf(disp$disp.rate.ftb),
  verticals=TRUE,
  main = "Juvenile Dispersal Rate")

#Juvenile and adult dispersal rate
plot(
  ecdf(disp$disp.rate.ftb),
  verticals=TRUE)
plot(
  ecdf(disp$disp.rate.eb),
  verticals=TRUE,
  lty=3,
  add=TRUE)

#are these two distributions different? Use the ks test to find out.
ks.test(disp$disp.rate.ftb,disp$disp.rate.eb)


##------Comparing two or more proportions------------
prop.test(c(4,16),c(40,250))

##-------Dependence of variables in a contingency table--------

owls = matrix(c(16, 9, 4, 11), nrow=2)
rownames(owls) = c("present", "absent")
colnames(owls) = c("old", "young")
owls

##----------Contingency Chi square------------
chisq.test(owls)

# If the values in each cell are less than 4 or 5 use a Fisher test
fisher.test(owls)

##---------Bird Habitat Data-------
birds   = read.csv(here("data", "bird.sta.csv"), header=TRUE)
hab     = read.csv(here("data", "hab.sta.csv"), header=TRUE)
birdhab = merge(birds, hab, by=c("basin", "sub", "sta"))

# Create a contingency table for edge/interior and brown creeper presence/absence
table(birdhab$s.edge, birdhab$BRCR > 0)

# set the presence to be in the first column
br_creeper_table = table(birdhab$s.edge, birdhab$BRCR > 0)[, 2:1]

#Are brown creepers present more or less frequently than expected in forest 
#interiors versus forest edges and is the difference significant?

#Conduct a Chi Square test
chisq.test(br_creeper_table)
fisher.test(br_creeper_table)
