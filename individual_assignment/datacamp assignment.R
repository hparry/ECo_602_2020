a <- "heather"
b1 <- 45.6
b2 <- "45.6"
c <- c(0,1,2,3)
class(a)
class(b1)
class(b2)
class(c)
b1+b2
b1+c
#Question 2
# create a vector called v1 that contains the intergers -2 to 2
v1 = c(-2, -1, 0, 1, 2)
#Use v1 to create a new vector called v2 whose elements are the elements of v1 multiplied by 3.
v2 <- c(v1 * 3)
#Find the sum of the values of v2 
sum(v2)
#Question 3
#create a list, named my_list_1
my_list_1 <- list(5.2, "five point two", c(0,1,2,3,4,5) )
my_list_1
#name the elements in my_list_: "two", "one", "three"
names(my_list_1) <- c("two", "one", "three")
#Question 4
#pull out the third element
my_list_1[3]
#pull out the element with the name "one"
my_list_1["one"]
#Question 5
my_vec = rep(1:3, 5)
my_vec == 3
my_bool_vec <- my_vec == 3
my_bool_vec
data.frame(my_vec, my_bool_vec)
#Question 6
my_vec[my_bool_vec]
my_vec = rep(1:3, 5)
my_vec == 3
my_bool_vec <- my_vec == 3
my_bool_vec
my_vec[my_bool_vec]

