#Lab 5 Questions
#Question 1
library(here)
dev.off()

#4 plot different values for a and b in an exponential function
png(filename = "Lab 5 Question 1 edit")

a = 1.9
b = 0.1
exp_fun = function (x, a, b)
{
  return(a * exp(-b*x))
}
curve(
  exp_fun(x, a, b), 
  from = 0, to = 40, add = FALSE, 
  main = "Exponential Function with Different Parameters",
  ylab = "f(x)", xlab = "x",
  col= "black")

a = 1.9
b = 0.3
exp_fun = function (x, a, b)
{
  return(a * exp(-b*x))
}
curve(
  exp_fun(x, a, b), 
  from = 0, to = 40, add = TRUE, 
  main = "Exponential function",
  ylab = "f(x)", xlab = "x",
  col= "black",
  lty= 3)

a = 1.2
b = 0.2
exp_fun = function (x, a, b)
{
  return(a * exp(-b*x))
}
curve(
  exp_fun(x, a, b), 
  from = 0, to = 40, add = TRUE, 
  main = "Exponential function",
  ylab = "f(x)", xlab = "x",
  col= "red")

a = 1.2
b = 0.4
exp_fun = function (x, a, b)
{
  return(a * exp(-b*x))
}
curve(
  exp_fun(x, 1.2, 0.4), 
  from = 0, to = 40, add = TRUE, 
  main = "Exponential function",
  ylab = "f(x)", xlab = "x",
  col= "red",
  lty = 3)

dev.off()
