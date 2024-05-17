install.packages("pacman")
library(datasets)
head(iris)
?plot


# graphs
plot(iris$Species)
plot(iris$Petal.Length)
plot(iris$Species,iris$Petal.Length)

plot(iris$Petal.Length,iris$Petal.Width)
plot(iris)

# A more detailed plot
plot(iris$Petal.Length,iris$Petal.Width,
     col="#cc0000",# Hex code for red
     pch = 19,
     main="Petal Length vs Width",
     xlab = "Petal Length",
     ylab="Petal Width")


# plot functions

plot(cos,0,2*pi)
plot(dnorm,-3,3,col="#cc0000",lwd=5,
     main='normal distribution',
     xlab="x",ylab='probability'
     )

# Another dataset
head(mtcars)
# a tabel with frequencies for each category

cylinders <- table(mtcars$cyl) # make a table of the 
# feature and store it as cylinders
cylinders # check the number of instances of each value
barplot(cylinders) # actual bars
plot(cylinders) # just single lines

hist(mtcars$mpg) # histograms
hist(mtcars$hp)
hist(mtcars$qsec)
#put graphs into 3 by 1
# compare across distributions
par(mfrow=c(3,1))
hist(mtcars$mpg [mtcars$cyl==4],
     xlim=c(10,40),
     breaks=8,
     main='Distribution plot of mpg of 4 cylinders',
     xlab="mpg",
     col='green')
hist(mtcars$mpg [mtcars$cyl == 6],
     xlim=c(10,40),
     breaks=8, # number of bars Iwant to have
     main='Distribution plot of mpg of 6 cylinders',
     col='orange')
hist(mtcars$mpg [mtcars$cyl==8],
     main='blah',
     col='red',
     xlim=c(10,40),
     breaks=8)
par(mfrow=c(1,1)) # set to one plot again

# scatter plot
plot(mtcars$wt,mtcars$mpg,
     pch = 19, #solid circle
     cex = 1.5, # make dots 150% size
     col='red',
     main='Topic',
     xlab='mpg',
     ylab='weight'
     )

# a new dataset
head(lynx) # a time series
hist(lynx)
hist(lynx,
     breaks=14,# number of bins
     freq=FALSE, # we want proportion instead on y
     col='purple',
     main = paste("long sentence"))

# get a normal distribution
curve(dnorm(x,mean=mean(lynx),sd = sd(lynx)),
     add= TRUE, # add the line in the previous plot
     lwd = 2, # line width of 2 pixels
     col ='black')
# add two kernel density estimators
lines(density(lynx),col='orange',lwd =2)
lines(density(lynx,adjust = 3),lwd = 2, col = "red")

# add a rug plot to see where the data is
rug(lynx, lwd = 2, col='green')

# summary statistics
summary(mtcars$mpg)
summary(mtcars)

# load packages with pacman
pacman::p_load(pacman, dplyr, GGally, ggplot2, ggthemes, ggvis,
               httr, lubridate, plotly, rio, rmarkdown, shiny,
               stringr, tidyr)
p_load(psych)
# install.packages('BiocManager')

p_help(psych,web=F) # get help on methods
describe(iris$Sepal.Length)
describe(iris)

hist(iris$Sepal.Length[iris$Species=='versicolor'&iris$Sepal.Width<3.5])

# create new dataset
setosa <- iris[iris$Species=='setosa',]# leave blank because
# we want all columns
head(setosa)


# data formating
# vector (same data type), matrix/array(2d, same data class),
# dataframe (same as python), List (like python any class, any length)

n1 <- 15
n1
typeof(n1) # gives a double by default

c2 <-'a'
c2
typeof(c2)

s3 <-"hello world"
s3
typeof(s3)

l4 <-TRUE
typeof(l4)

l5 <-T
typeof(l5)

# vector
v1 <- c(1,2,3,4,5)
v1
is.vector(v1)

v2 <- c('a','b','c')
v2

# matrix
m1 <-matrix(c(T,T,F,F),nrow=2)
m1
m2<-matrix(c("a","b",
             "c","d"),
           nrow=2,
           byrow=T)
m2

# array
a1 <- array(c(1:25),c(4,5,1)) # row, column, number of array
a1

# dataframe
vNumeric <-c(1,2,3)
vLogical <-c("TRUE","TRUE","FALSE")
df <-as.data.frame(cbind(vNumeric,vLogical)) # change it into df
df

# list
lst <- list(vNumeric,vLogical)
lst

# factors
# an attribute of a vector specifies the possible
# values and their order

# create data

(x1 <- 1:3)
(y<-1:9)
(df1<-cbind.data.frame(x1,y)) # store them into a df
typeof(df1) # list
# 123 repeated because x1 only has 3 values
# change it to factor
(x2<- as.factor(c(1:3)))
(df2<-cbind.data.frame(x2,y)) # store them into a df
str(x2) # now is not integer but factor of 3 level


# Entering Data
xz <-0:10 
zx <- 10:0
zx
# seq

qq <- seq(10)
11
qq <- seq(30,0,by=-3)
qq
# scan
ff <- scan() # enter numbers in console. 
ff

# rep
tt <- rep("hello",5)
(yy <- rep(c("hello","world"),each=5))
(yy <- rep(c("hello","world"),5))

# import data
rio <- import(".csv")

# data viewer
View(rio)
# this gives us a new tab on top

# read text file
ttxxtt <- read.table(".txt",header=TRUE,sep="\t")
# can read csv too. same as import


# Modeling Data
# hierachical clutering
cars <- mtcars[,c(1:4,6:7,9:11)] # select all rows but some columns
head(cars)

#pipes

hc <- cars %>%
      dist %>%
      hclust
plot(hc)

rect.hclust(hc,k=3,border = 'green')

# PCA
#2D to 1D, maintain useful information
pc <- prcomp(cars,center=TRUE,scale=TRUE)
summary(pc)
plot(pc) # see the importance of each component
pc # what is the assocaitetion between each variable and component

predict(pc) %>% round(2)
biplot(pc)


# regression

head(USJudgeRatings)
data <- USJudgeRatings
x <- as.matrix(data[-12]) # read all except 12 th column
y <- data[,12]
reg1 <- lm(y~x) # linear model, x as a function of y
reg1
summary(reg1)

p_load(lars)
lasso <- lars(x,y,type='lasso')
lasso

r2comp <- c(lasso$R2[6],reg1$R2[6]) %>%
names(r2comp) <- c("Lasso","General")  
