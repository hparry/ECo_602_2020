#Frequectist Concepts
library(here)
#Question 1
#dbinom(c(0, 4), size = 4, prob = 0.75)
#dbinom(x, size = 4, prob = 0.75)
dbinom(3, size = 4, prob = 0.75) # I think this is correct

#Question 2
pbinom(3, size = 4, prob = 0.75)

#Question 3
1- pbinom(3, size = 5, prob = .75)

#Question 4
pnorm(1.2, mean = 2, sd = 3)

#Question 5
1- pnorm(1.2, mean = 2, sd = 3)

#Question 6, pnorm(2.4, 2, 2) is the cumulative value under the curve to 2.4 then
#I used pnorm (1.2,2,2) to subtract the cumulative value under the curve until 1.2 
pnorm(3.2, mean = 2, sd = 2)- pnorm(1.2, mean= 2, sd = 2)

#Question 7

