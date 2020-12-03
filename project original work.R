##--------my old material--------------
```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## R Markdown

This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:
  
  ```{r cars}
summary(cars)
```

## Including Plots

You can also embed plots, for example:
  
  ```{r pressure, echo=FALSE}
plot(pressure)
```

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.


## Data Structures

## Vectors
### Vectors are arrays that can hold numeric data, character data, or logical data, but it can only hold one data type at a time.
### Create a vector called v1 that contains the integers -2 to 2


```{r vector}
v1 = c(-2, -1, 0, 1, 2)
print(v1)
```

## Data Frame
### A data frame can hold numeric, character or logical data types. Within a column all elements have the same data type, but different columns can be of different data type.

## Matrix
### A matrix is an array that can hold numeric, character or logical data types but it can only hold one data type at a time.

## List
### A list is allows you to gather a variety of objects in an ordered way. These objects can be matrices, vectors, data frames, even other lists, etc.
### create a list, named my_list_1
```{r list}
my_list_1 <- list(5.2, "five point two", c(0,1,2,3,4,5) )
print(my_list_1)
```

## Subsetting Operations and Functions
### []

### $

### [[]]

### subset()


# Required Functions and Arguments
## Data Structures

### c()

### matrix()

### data.frame()

### list ()

## Numerical Data Exploration

### summary()

### mean()

### sd()

## Graphical Data Exploration

### par() required arguments:
#### * mar = 

### plot() required arguments:
#### * col =

#### * pch =

#### * cex =

#### * main =

#### * xlab =

#### * ylab =

#### * xlim =

#### * ylim =

### hist() required arguments:
#### * breaks =

### boxplot() required arguments:

## Distribution Functions
###  dnorm()

###  pnorm()

###  qnorm()

###  dbinom()

###  pbinom()

###  qbinom()

## Other Functions
###  * subset






