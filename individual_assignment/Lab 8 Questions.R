#Lab8 Questions

##-----------Question 1---------------
adelie_flipper = subset(penguin_dat, species =="Adelie")$flipper_length_mm
chinstrap_flipper = subset(penguin_dat, species =="Chinstrap")$flipper_length_mm


pen_boot = two.boot(adelie_flipper, chinstrap_flipper, mean, R = 10000, na.rm = TRUE)
str(pen_boot)  

png(filename = "Lab 8 Histogram flipper length.png")
hist(pen_boot$t, 
     main = "Histogram of Bootstrap Differences in Flipper Length",
     xlab= "difference in flipper length (mm)")

dev.off()

##---------------Question 2--------
mean(pen_boot$t)
quantile(
  pen_boot$t,
  c(0.025, 0.975))

##----------Question 3----------
#create a distribution function from pen_boot using ecdf
?ecdf()
pen_ecdf = ecdf(pen_boot$t)

#pen_ecdf(-5.87) this gives me .4899, which is right about 50% which is what I would expect

##-------------Question 4----------
#alternative hypothesis: adelie have shorter flippers than Chinstrap

#null hypothesis: there is no difference in flipper length between the 2 species of penguin

##-----------Question 5----------
1 - pen_ecdf(-4.5)

#Use pen_ecdf() to calculate the empirical probability of observing the mean difference predicted by the null hypothesis, i.e. 0 or greater.
1- pen_ecdf(0)

##-------------Question 6--------------
t.test(pine~ treatment, data = dat_tree, alternative = "greater")
wilcox.test(pine~ treatment, data = dat_tree, alternative = "greater")
#pvalue is 0.05026
levels(factor(dat_tree$treatment))


##-----------Question 7-------------
tree_boot = 
  two.boot(
    subset(dat_tree, treatment == "clipped")$pine,
    subset(dat_tree, treatment == "control")$pine,
    FUN = mean,
    R = 10000,
    na.rm = TRUE
  )

boot.ci(tree_boot)

quantile(tree_boot$t, c(0.025, 0.975))
#What is the observed difference in mean tree counts and does it 
#fall within the 95% bootstrap CI?
#Is the mean difference the t value in the t-test

mean(dat_tree, na.rm = TRUE)
mean_observed_clipped = mean(subset(dat_tree, treatment == "clipped")$pine)
mean_observed_control = mean(subset(dat_tree, treatment == "control")$pine)

mean_observed_clipped - mean_observed_control
#16

#Trying to do aggregate()
#agg_means = aggregate(body_mass_g ~ species,
                     # data= dat_pen,
                      #FUN= mean,
                     # na.rm = TRUE)
#agg_means_tree = aggregate(treatment == "clipped" ~ pine,
                           treatment == "control" ~ pine,
                           data = dat_tree,
                           FUN = mean,
                           na.rm = TRUE)

##--------------Question 8---------------

dat_all = merge(
  dat_bird, 
  dat_habitat,
  by = c("basin", "sub"))

dat_1 = 
  subset(
    dat_all, 
    select = c(b.sidi, s.sidi))
m= 10000
result = numeric(m)

#create loop, is result[i] the slope parameter
for(i in 1:m)
{
  index_1 = sample(nrow(dat_1), replace = TRUE)
  index_2 = sample(nrow(dat_1), replace = TRUE) 
  
  dat_resampled_i = 
    data.frame(
      b.sidi = dat_1$b.sidi[index_1],
      s.sidi = dat_1$s.sidi[index_2]
    )
  
  fit_resampled_i = lm(b.sidi ~ s.sidi, data = dat_resampled_i)
  result[i] = coef(fit_resampled_i)[2]
}
print(result[i])

##-------------------Question 9----------------

critical_value = quantile(result, c(.05))

png(filename = "Lab 8 Histogram Monte Carlo Simulation.png")
hist(result, main = "Null Distribution of Regression Slope", xlab = "Slope Parameter")
abline(v = slope_observed, lty = 1, col = "blue", lwd = 2.0)
abline(v = critical_value, lty = 2, col = "red", lwd = 2.0)
dev.off()

##-----------------Question 10---------------


