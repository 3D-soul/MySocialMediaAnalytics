## Matrices
m <- matrix(1:9, 3,3, byrow = TRUE)
m

library(MASS)
ginv(m) # inverse of m

eigen(m) # eigen values and vectors

svd(m)

rowMeans(m)
rowSums(m)
colMeans(m)
colSums(m)


## Lists
list.sample <- list(
    nums = seq.int(1,5),
    language = c("R", "Python", "Scala", "Ruby"),
    sin.func = sin
)
list.sample

list.sample$language
list.sample$sin.func(1.5708)


## DataFrame
df <- data.frame(
    name = c("John", "Dave", "Sally", "Darla"),
    age = c(28, 32, 18, 21),
    job = c("IT", "HR", "HR", "CS")
)

df
class(df) # check class
str(df)   # summary
rownames(df) # names of rows
colnames(df) # names of  cols
dim(df)   # dimension


## Functions
# Built-in func
sqrt(16)
mean(1:10)
sum(1:5)
sqrt(1:5)
runif(1:5)
rnorm(5)

# user-defined func
square <- function(num){
    return (num^2)
}

square(10)

environment(square)
formals(square)
body(square)

square(1:5)


## Controlling code flow
# looping
for (i in 1:5){
    cat(paste(i, " "))
}

sum = 0
for (i in 1:10){
    sum = sum + i
} 
sum

n = 0
while (n <= 5){
    cat(paste(n, " "))
    n <- n + 1
} 

i <- 1
repeat{
    cat(paste(i, " "))
    
    if (i >= 5){break}
    
    i = i + 1
}


# conditional constructs
num = 10
if (num == 10){
    cat(paste("number is 10"))}

num = 5
if (num == 10){
    cat("number is 10")
}else {
    cat("number is not 10")}

if (num == 10){
    cat("number is 10")
}else if (num == 5){
    cat("number is 5")
}else{cat("No match found")}

ifelse(num==10, "Number is 10", "Number is 5") # alternate to if and else if

for (num in c("5", "10", "15")){
    cat(
        switch (num,
            "5" = "five",
            "7" = "seven",
            "10" = "ten",
            "No match found"
        ), "\n"
    )
}


## Advanced Operations
mat <- matrix(1:16, 4, 4)
mat

apply(mat, 1, sum) # row sums
rowSums(mat)

apply(mat, 1, mean) # row means
rowMeans(mat)

apply(mat, 2, sum) # col sums
colSums(mat)

apply(mat, 2,mean) # col means
colMeans(mat)

apply(mat, 1, quantile, probs=c(0.25, 0.5, 0.75))
apply(mat, 2, quantile, prob=c(0.25, 0.5, 0.75))

l <- list(nums=1:10, even=seq(2,10,2), odd=seq(1,10,2))
l

lapply(l, sum)
sapply(l, sum)

lapply(l, mean)
typeof(lapply(l, mean))

sapply(l ,mean)
typeof(sapply(l, mean))


data <- 1:30
data

groups <- gl(3,10)
groups

tapply(data, groups, sum)

tapply(data, groups, sum, simplify=F)

list(rep(1,4), rep(2,3), rep(3,2), rep(4,1))
mapply(rep, 1:4, 4:1)


## Data Visulaization
data(iris)

# base plotting sys
boxplot(Sepal.Length~Species, data=iris, xlab = "Species",
        ylab = "sepal Length", main = "Iris Boxplot")

# lattice plotting sys
library(lattice)
bwplot(Sepal.Length~Species, data=iris, xlab="Species",
       ylab="Sepal Lenght", main="Iris Boxplot")

# ggplot plotting sys
library(ggplot2)
ggplot(data=iris, aes(x=Species, y=Sepal.Length)) +
    geom_boxplot(aes(fill=Species)) +
    ylab("Sepal Length") +
    ggtitle("Iris Boxplot") +
    stat_summary(fun=mean, geom="point", shape=5, size=4)+
    theme_bw()

## Others
installed.packages(lib.loc=)
library()

