#Lab 10 Walkthrough
library(here)
rope= read.csv(here("data", "rope.csv"))
rope$rope.type

#factor- to convert a string to a factor
rope$rope.type = factor(rope$rope.type)

levels(rope$rope.type)

##--------Number of Observations and Groups------
#total number of observations
n_obs = nrow(rope)
n_groups = length(levels(rope$rope.type))

#boxplot for visual

boxplot(p.cut ~ rope.type, data=rope)

##--------Total of Sum of squares--------
#ss_tot= 


#------find mean of rope cut-----------
#I'm not sure if I am supposed to use rope$p.cut here or rope

grand_mean_cut = mean(rope$p.cut)

# residuals between grand_mean and observed data(rope$p.cut)
resids_rope = c(grand_mean_cut- rope$p.cut)

#Total Sum of squares (ss_tot)- not sure that I did this correctly

ss_tot = sum(resids_rope^2)
#I tried putting this all on one line below, not sure if this is correct
ss_tot_1 = sum(((mean(rope$p.cut))- rope$p.cut)^2)


##Partitioning Variance: Within Group
aggregate(
  x = rope$p.cut,
  by = list(rope$rope.type), 
  FUN = mean)
#you can also do it this way
aggregate(
  x = rope$p.cut,
  by = list(rope$rope.type),
  FUN = function(x) mean(x))

#use aggregate to calculate the resids- I don't think that this is correct

agg_resids = 
  aggregate(
  x = rope$p.cut,
  by = list(rope$rope.type),
  FUN = function(x) x- mean(x))

str(agg_resids)

#sum of Squared residuals
agg_sq_resids = 
  aggregate(
    x = rope$p.cut,
    by = list(rope$rope.type),
    FUN = function(x) (sum((x- mean(x))^2)))

str(agg_sq_resids)

#within-group sum of squares, ss_within
ss_within = sum(agg_sq_resids$x)

#Variation among groups, ss_among
#mean of each group
ss_among = ss_tot- ss_within

##----------------Normalizing--------
#total degrees of freedom
df_tot = n_obs - 1

#degrees of freedom for ss_within
df_within = n_obs - n_groups

df_among = df_tot - df_within

#The mean squares (ms) are obtained simply by dividing each sum of squares 
#by its respective degrees of freedom
ms_among  =  ss_among / (n_groups - 1)
ms_within = ss_within / (n_obs - n_groups)

##----------------The Test Statistic: F---------

f_ratio = ms_among/ms_within

# pf(q= 2.23 , df1= 5 , df2= 115)
pf(q= 2.23 , df1= df_among , df2= df_within)


1- pf(q= 2.23 , df1= df_among , df2= df_within)

#another way to do it
f_pval = pf(q= 2.23 , df1= df_among , df2= df_within, lower.tail= FALSE)

##ANova in R
fit_1 = lm(p.cut ~ rope.type, data=rope)
anova(fit_1)

anova_fit_1 = anova(fit_1)
anova_fit_1$"Sum Sq"
str(anova_fit_1)
