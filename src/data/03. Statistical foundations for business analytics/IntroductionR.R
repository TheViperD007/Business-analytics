# I. Introduction to R

## 1.1. Basic Commands

### 1.1.1. Functions to work with vectors

#### c(), create a vector by combining elements.
    x = c(6, 4, 3)
    y = c(1, 3, 4)
    
#### length(), returns number of elements of a vector
    length(x)
    length(y)
    
#### sum(vector), computes the sum of the elements of a vector.
    sum(x)
    
#### mean(vector), calculate the mean (average) of the elements of a vector.
    mean(x)
    
#### min(vector), max(vector), find the minimum and maximum value of a vector, respectively.
    min(x)
    max(x)  
    
### 1.1.2. Vector Operations
    x
    y
    x+y
    x-y
    x*y
    x**y
    x/y
    round(x/y,3)
### 1.1.3. Inner product or scalar product
    x%*%y
    sum(x*y)
    cumsum(x)
    diff(x)
 # https://rpubs.com/Cesar_AHN/operaciones_con_vectores_aritmeticas_estadisticas_graficos

### 1.1.3. Matrix Operations
?matrix
help(matrix)

mdat <- matrix(c(1,2,3, 11,12,13), nrow = 2, ncol = 3, byrow = TRUE,
               dimnames = list(c("row1", "row2"),
                               c("C.1", "C.2", "C.3")))
mdat 

#example
x <- matrix(data = c(1, 2, 3, 4), nrow = 2, ncol = 2)
x
### or
x <- matrix(c(1, 2, 3, 4), 2, 2)
###
matrix(c(1, 2, 3, 4), 2, 2, byrow = TRUE)
###
x
sqrt(x)
x^2
exp(x)
log(x)
cos(x)
sin(x)

###
help(rnorm)
x <- rnorm(50)
x

y <- x + rnorm(50, mean = 20, sd = 2)
x
y

help(cor)
pearson=cor(x, y)
spearman=cor(x, y,method="spearman")
kendall=cor(x, y,method="kendall")


###
set.seed(123)
rnorm(10)
###
set.seed(3)
y <- rnorm(100)
?rnorm
mean(y)
var(y)
sqrt(var(y))
sd(y)


y <- rnorm(100)
mean(y)
sd(y)

y <- rnorm(1000)
mean(y)
sd(y)

y <- rnorm(10000)
mean(y)
sd(y)

y <- rnorm(100000)
mean(y)
sd(y)



## Graphics

###


###
plot(x, y, col = "green")
dev.off() # clean figure 
###

x <- seq(1, 10)
x
x <- 1:10
x
pi
x <- seq(-pi, pi, length = 20)
x
###
y <- x
y

?outer

parab<-function(x, y) x^2+y^2
hiperb<-function(x, y) x^2-y^2

f <- outer(x, y, parab)
contour(x, y, f,nlevels = 10)
image(x, y, f)
persp(x, y, f)


f <- outer(x, y, hiperb)
contour(x, y, f)
image(x, y, f)
persp(x, y, f)

## Indexing Data

###
A <- matrix(1:16, 4, 4)
A
###
A[2, 3]
###
A[c(1, 3), c(2, 4)]

A[1:3, 2:4]
A[1:2, ]
A[, 1:2]
###
A

A[1, ]

A[,2 ]

###
A
A[-c(1, 3), ]
A[-c(1, 3), -c(1, 3, 4)]
###
dim(A)

## Loading Data

###

setwd("D:/Statistical_Learning_V4")

Auto <- read.table("Auto.data")
head(Auto)
dim(Auto)
###
Auto <- read.table("Auto.data", header = T, na.strings = "?", stringsAsFactors = T)
head(Auto)
dim(Auto)

###
Auto <- read.csv("Auto.csv", na.strings = "?", stringsAsFactors = T)
dim(Auto)

Auto[1:4, ]

Auto[1:4, 1:4]

###
Auto <- na.omit(Auto)
dim(Auto)
###
names(Auto)

Auto.head() # en pandas

head(Auto,10)

names(Auto)

Auto.mpg  # en pandas

Auto$mpg   # en R


# More information about dataset ="Auto" en https://rdrr.io/cran/ISLR/man/Auto.html 

Auto$cylinders

## Additional Graphical and Numerical Summaries

plot(cylinders, mpg) # Debe salir Error


###
plot(Auto$cylinders, Auto$mpg)

attach(Auto)
plot(cylinders, mpg)
###
cylinders <- as.factor(cylinders)

cylinders

###

plot(cylinders, mpg)
plot(cylinders, mpg, col = "red")
plot(cylinders, mpg, col = "red", varwidth = T)

table(cylinders)
#  Set as true to draw width of the box proportionate to the sample size
plot(cylinders, mpg, col = "red", varwidth = T,
    horizontal = T)
plot(cylinders, mpg, col = "red", varwidth = T,
    xlab = "cylinders", ylab = "MPG")
###



plot(origin,mpg, col = "blue", varwidth = T,
     xlab = "origin", ylab = "MPG")

plot(as.factor(origin),mpg, col = "blue", varwidth = T,
     xlab = "origin", ylab = "MPG")

table(origin) # siempre y cuando fije el dataset --usando attach
table(Auto$origin)

plot(as.factor(origin),mpg, col = c("blue","red","green"), varwidth = T,
     xlab = "origin", ylab = "MPG")
###



hist(mpg)
hist(mpg, col = 2)
hist(mpg, col = 2, breaks = 15)
###

pairs(Auto)

dim(Auto)
head(Auto)

df=Auto[-c(8,9)]

x11()
pairs(df)

# Grouping

values <- Auto[, 8]


# Número de grupos
l <- length(unique(values))
x11()
pairs(df, col = c("blue","red","green")[values], oma=c(3,3,3,15)) 
par(xpd = TRUE)
legend("bottomright", fill = c("blue","red","green"),legend=c(1,2,3))


x11()
pairs(df, col = c("blue","red","green")[values],lower.panel = NULL) 
par(xpd = TRUE)
legend("bottomleft", fill=c("blue","red","green"),legend=c(1,2,3))



x11()
pairs(df,       # Datos
      pch = c(1,8,20)[values], # marker
      col = c("blue","red","green")[values],  # Color
      main = "Title",    # Title
      gap = 0,           # Distance betweer graphics 
      row1attop = FALSE, # Orientation of Diagonal
      labels = colnames(df), # Labels
      cex.labels = 0.8,  # Size diagonals texts 
      font.labels = 1,   # Font diagonals texts
      lower.panel = NULL) # Eliminar panel inferior    


pairs(
    ~ mpg + displacement + horsepower + weight + acceleration,
    data = Auto
  )
###
summary(Auto)
###

summary(mpg)
###


# Creating a DataFrame

WEIGHT <- c(60, 82, 57, 90, 95, 72,50)
HEIGHT <- c(1.75, 1.80, 1.65, 1.90, 1.74, 1.91,1.66)
# The body mass index (BMI) is computed as follows:
BMI <- WEIGHT/HEIGHT^2
BMI
# Tables
Tab1=cbind(WEIGHT,HEIGHT,BMI)
Tab1
rownames(Tab1)<-c('Carlos','Pedro','Maria','Claudia','Luis','Manuel','Percy')
Tab1
# DataFrame
Tab1=data.frame(Tab1)
# if-else Condition
Tab1$REPORT<-ifelse(Tab1$BMI<25,'Bueno','Malo')
Tab1
Tab1$CLASSIFICATION <- ifelse(Tab1$BMI < 18.5 , "Bajo Peso", 
                              ifelse(Tab1$BMI < 24.9, "Normal", 
                                     ifelse(Tab1$BMI<29.9,"Sobrepeso","Obeso")))
Tab1 

# Scatter plot
plot(Tab1$WEIGHT, Tab1$HEIGHT)

plot(Tab1$WEIGHT, Tab1$HEIGHT,
     pch=16,
     col='blue',
     xlab='Peso',
     ylab='Altura',
     main='Figura 1')

# adding lines
abline(v=80,col="red", lty=2)

abline(h=1.75,col="red", lty=2)

# More information in : https://r-coder.com/plot-en-r/

# Assume that the optimal value of BMI=22.5 and coloring by REPORT value

plot(Tab1$HEIGHT,Tab1$WEIGHT,
     pch=16,
     col=factor(Tab1$REPORT),
     xlab='Altura',
     ylab='Peso',
     main='Figura 1')
hh <- c(1.55, 1.60,1.65, 1.70, 1.75, 1.80, 1.85, 1.90, 1.95)
lines(hh, 22.5 * hh^2)

# Changing of colors 

colores <- c("orange", "black", "green","blue")
plot(Tab1$HEIGHT,Tab1$WEIGHT,
     pch=16,
     col = colores[factor(Tab1$CLASSIFICATION,levels=c("Bajo Peso", "Normal","Sobrepeso","Obeso"))])
# Adding legend r
legend("topleft", 
       legend = c("Bajo Peso", "Normal","Sobrepeso","Obeso"),
       lwd = 16,
       col = colores)
hh <- c(1.55, 1.60,1.65, 1.70, 1.75, 1.80, 1.85, 1.90, 1.95)
lines(hh, 22.5 * hh^2)

# More information in https://r-coder.com/leyenda-r/



library(ISLR2)

head(Wage,10)

head(Default,10)

head(USArrests)

library("car")

help(mtcars)
mtcars
summary(mtcars$mpg)
# Exploring data
mtcars[mtcars$mpg==33.90, ]
mtcars[mtcars$mpg==33.90,2]
mtcars[mtcars$mpg==33.90,2:4]
mtcars[mtcars$mpg>22.80,]
mtcars[(mtcars$mpg>22.80)&(mtcars$hp>65),] 
mtcars[(mtcars$mpg>22.80)|(mtcars$hp>65),] 


Carbon <- read.table("http://stat4ds.rwth-aachen.de/data/Carbon.dat", header=TRUE)
head(Carbon, 3) # head(Carbon, n) shows n observations at top of data file
Carbon


Carbon <- read.table("http://stat4ds.rwth-aachen.de/data/Carbon.dat", header=TRUE)
# header=TRUE if variable names are at top of data file
head(Carbon, 3) # head(Carbon, n) shows n observations at top of data file
summary(Carbon$CO2) # 1st Qu = lower quartile, 3rd Qu = upper quartile
boxplot(Carbon$CO2, xlab="CO2 values", horizontal=TRUE)
Carbon[Carbon$CO2>9,] 



Crime <- read.table("http://stat4ds.rwth-aachen.de/data/Murder2.dat", header=TRUE)
boxplot(Crime$murder ~ Crime$nation, xlab="Murder rate", horizontal=TRUE)
tapply(Crime$murder, Crime$nation, summary)


PID <- read.table("http://stat4ds.rwth-aachen.de/data/PartyID.dat", header=TRUE)
PID
table(PID$race, PID$id) # Contingency Table 
prop.table(table(PID$race, PID$id), margin=1) # For margin=1, proportions

mosaicplot(table(PID$race, PID$id))
