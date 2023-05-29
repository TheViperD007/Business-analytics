X<-c(28, 35, 42, 90, 70, 56, 75, 66, 30, 89, 75, 64, 81, 69, 55, 83, 72, 68, 73, 16)
X
hist(X)
h <- hist(X,xlim = c(0,100))
text(h$mids,h$counts,labels=h$counts, adj=c(0.5, -0.5))

breaks <- seq(0, 100.0, by=20)
breaks
freq <- table(cut(X, breaks, right=TRUE)) 
freq

# N�mero de elementos  length(X)
A=c(1,2,4)
cumsum(A)

Table_freq <- cbind(freq, freq/length(X))

Table_freq

Table_freq <- cbind(freq, freq/length(X),cumsum(freq/length(X)))

Table_freq

colnames(Table_freq)<-c("Frequency","Density","Cumulative Density")

Table_freq



X1 <- c(1,3,5,5,6,7,8,10,12)
mean(X1)
median(X1)

X2 <- c(1,3,5,5,6,7,8,9,10,12)
mean(X2)
median(X2)

X3 <- c( 1,3,5,5,6,7,8,9,10,12,100)
mean(X3)
median(X3)



X
breaks

h <- hist(X,breaks,col='gray',border="blue",ylim=c(0,10))


h <- hist(X,breaks,freq=FALSE,col='gray',border="blue")
lines(density(X),col="red",lwd=2)

qqnorm(X, pch=19,col = "red") 
qqline(X,col="blue",lwd=3)

summary(X)

boxplot(X, horizontal=TRUE)

# tres graficos en una plantilla

par(mfrow = c(1, 3)) 

# Gr�fico 1
hist(X,breaks,freq=FALSE,col='gray',border="blue")
lines(density(X),col="red",lwd=2)


# Gr�fico 2
qqnorm(X, pch=19,col = "red") 
qqline(X,col="blue",lwd=3)

# Gr�fico 3
boxplot(X, horizontal=FALSE,main="Box plot")


# Creating a data frame
df = data.frame(
  "Category" = c("A", "A", "B","A", "B","B","B","A","A","B"),
  "Gender" = c("Male", "Female", "Male","Female", "Male","Female", "Male","Male","Female","Female")
)

df
df$Gender

# En python .. pd.crosstab()

# Creating contingency table from data using table()
conTable = table(df)
conTable



addmargins(conTable)

prop.table(conTable)

prop.table(conTable,margin = 1)  # proporci�n por fila

prop.table(conTable,margin = 2)  # proporci�n por columna

df1 = data.frame(
  "Category" = c("A", "A", "B","A"),
  "Gender" = c("Male", "Female", "Female","Female")
)


conTable1 = table(df1)
conTable1
conTable
# Proportional table--
round(conTable1/conTable,2)

# Percentaje table
100*round(conTable1/conTable,4)

# Grouping data

WEIGHT <- c(60, 82, 57, 90, 95, 72)
HEIGHT <- c(1.75, 1.80, 1.65, 1.90, 1.74, 1.91)
BMI <- WEIGHT/HEIGHT^2

Tab1=cbind(WEIGHT,HEIGHT,BMI)  
rownames(Tab1)<-c('Carlos','Pedro','Maria','Claudia','Luis','Manuel')  
Tab1=data.frame(Tab1)
Tab1
Tab1$REPORT<-ifelse(Tab1$BMI<25,'Bueno','Malo')
Tab1$CLASSIFICATION <- ifelse(Tab1$BMI < 18.5 , "Bajo Peso", ifelse(Tab1$BMI < 24.9, "Normal", ifelse(Tab1$BMI<29.9,"Sobrepeso","Obeso")))
Tab1

plot(Tab1$WEIGHT, Tab1$HEIGHT, pch=16,col='blue',xlab='Peso',ylab='Altura',main='Figura 1')
abline(v=80,col="red", lty=2)
abline(h=1.75,col="red", lty=2)


plot(Tab1$HEIGHT,Tab1$WEIGHT,pch=16,col=factor(Tab1$REPORT),xlab='Altura',ylab='Peso',main='Figura 1')
hh <- c(1.65, 1.70, 1.75, 1.80, 1.85, 1.90)
legend(x = "topleft",legend = c("Bueno", "Malo"),fill=factor(Tab1$REPORT))
lines(hh, 22.5 * hh^2)

# 22.5 es el IMC ideal

library(psych)

summary(mtcars)
describe(mtcars)



s  = describe(mtcars)
class(s)

s["gear","max"]
s["qsec","trimmed"]
s[c("disp","wt","carb"),c("min","max","sd","skew")]


round(mean(mtcars$mpg, trim=0.1),2)


library(vcd)
data(Arthritis)

tab1 = table(Arthritis$Improved)
tab1

tab1=round(prop.table(tab1),4)
tab1
prop.table(tab1)*100

head(Arthritis)

table(Arthritis$Improved,Arthritis$Sex)

xtabs(~Treatment+Improved,data=Arthritis)

prop.table(xtabs(~Treatment+Improved,data=Arthritis))
prop.table(xtabs(~Treatment+Improved,data=Arthritis),1)
prop.table(xtabs(~Treatment+Improved,data=Arthritis),2)
prop.table(xtabs(~Improved+Sex,data=Arthritis),2)
tab2=xtabs(~Treatment+Improved,data=Arthritis)
margin.table(tab2,1)
margin.table(tab2,2)
addmargins(tab2,1)
addmargins(tab2,2)

#1,2 -> rows, columns
tab3 = xtabs(~Treatment+Sex+Improved , data=Arthritis)
tab3
ftable(tab3)
margin.table(tab3,1)
margin.table(tab3,2)
margin.table(tab3,c(1,3))
ftable(prop.table(tab3,c(1,3)))

#https://www.kaggle.com/datasets/uciml/adult-census-income
#https://www.kaggle.com/datasets/uciml/adult-census-income

setwd("E:/Statistical_Learning_V4")
d = read.csv("census_income.csv", stringsAsFactors = F)
head(d)

describe(d[,c(1,3,5,11:13)])


# Tabla de frecuencias en variables categoricas

for(i in 1:ncol(d)){
  if(class(d[,i])=="character"){
    print(paste("Summary for ",names(d)[i]));
    print(table(d[,i]))
  }
  
}

t=table(d$education,d$Y)
round(prop.table(t,1),2)


# Creo una funci�n para determinar outliers
outlier.limits = function(x){
  x.q1 = quantile(x)[2]
  x.q3 = quantile(x)[4]
  x.iqr=IQR(x)
  x.lmin= x.q1-(1.5*x.iqr)
  x.lmax= x.q3+(1.5*x.iqr)
  limits = c(x.lmin,x.lmax)
  names(limits)=NULL
  return(limits)
}

boxplot(d$fnlwgt)


min(d$fnlwgt)

n1=outlier.limits(d$fnlwgt)
n1[2]

d$fnlwgt>n1[2]

sum(d$fnlwgt>n1[2])

n1[2]

table(d[d$fnlwgt>n1[2],"education.num"])


boxplot(d$education.num)

n1=outlier.limits(d$education.num)
n1

sum(d$education.num<n1[1])

d[d$education.num<n1[1],"education.num"]


sum(d$education.num<n1[1] | d$education.num>n1[2])

d$education.num


