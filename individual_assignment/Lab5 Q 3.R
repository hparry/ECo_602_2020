#Lab 5 Question 3
library(here)
dev.off()

png(filename = "Lab 5 Question 3_ricker")
ricker_fun = function(x, a, b) 
{
  return(a * x * exp(-b* x))
}


a = 25
b = 0.1  
curve(
  ricker_fun(x, a, b),
  from = 0, to = 70, add = FALSE,
  main = "Ricker function (trying different parameters)",
  ylab = "f(x)", xlab = "x")

a = 20
b = 0.2  
curve(
  ricker_fun(x, a, b),
  from = 0, to = 70, add = TRUE,
  main = "Ricker function: a = 1, b= 1",
  ylab = "f(x)", xlab = "x",
  lty = 3)

a = 10
b = 0.2  
curve(
  ricker_fun(x, a, b),
  from = 0, to = 70, add = TRUE,
  main = "Ricker function: a = 1, b= 1",
  ylab = "f(x)", xlab = "x",
  lty = 3)

a = 75
b = 0.3 
curve(
  ricker_fun(x, a, b),
  from = 0, to = 70, add = TRUE,
  main = "Ricker function: a = 1, b= 1",
  ylab = "f(x)", xlab = "x",
  col = "red")

a = 50
b = 0.3 
curve(
  ricker_fun(x, a, b),
  from = 0, to = 70, add = TRUE,
  main = "Ricker function: a = 1, b= 1",
  ylab = "f(x)", xlab = "x",
  col = "red",
  lty = 3)

a = 40
b = 0.3 
curve(
  ricker_fun(x, a, b),
  from = 0, to = 70, add = TRUE,
  main = "Ricker function: a = 1, b= 1",
  ylab = "f(x)", xlab = "x",
  col = "red",
  lty = 3)

dev.off()