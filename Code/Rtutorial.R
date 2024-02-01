################################################################################
# Introduction to R 
# Brian Vegetabile 
# 2016 UCI Statistics Bootcamp
################################################################################

setwd('/Users/bvegetabile/Dropbox/ucirvine/teaching/StatGradBootcamp/')

# Data Structures --------------------------------------------------------------

### Vectors

# Vectors are the most basic things that we will work with in R.  They are very
# useful for storing information, but they must be chosen correctly to meet the 
# needs of the analysis (mixed data, computational considerations, etc.)

# An Atomic Vector is a one dimensional data structure which contains elements of
# the same type.  
# - Integers
# - Doubles
# - Characters
# - Logical

# Some Examples of Good Vectors
ints <- c(1L, 2L) 
a <- 1 
b <- c(1, 2)
words <- c('yes', 'no', 'maybe')
truths <- c(TRUE, FALSE, TRUE)

weird.vector <- c(0,1,c(1,2,4), 5) # This gets unpacked into one vector

# We can find the types of a vectors
typeof(ints)
typeof(a)
typeof(b)
typeof(words)
typeof(truths)

# As well as test the types
is.double(a)
is.character(b)

# An example of a misspecifying a vector

bad.vector <- c(1, 'no')
# We see that R coerces the vector to a type `character' without informing you
# Always be careful when creating vectors that the type is what you intended

# Indexing of Vectors 
# - vector[0] returns the type
# - vector[loc] returns the value at the specified location, if there is no 
#               value at the location it returns NA

bad.vector[50]

# We also are able to quickly summarize data in a list
numbers <- c(1,3,2,5,2,6,2,6)
length(numbers)   # Number of Elements in a vector
mean(numbers)     # Mean of the Numbers
summary(numbers)  # Five-Number Summary of the Values

# What happens if we do summary on bad.vector?
mean(bad.vector)
summary(bad.vector)

# -> Notice that the first operation of mean on a vector of characters returned 
#    an error. 

### Lists

# Lists are more complicated than vectors and can store any type of data, 
# including other lists.  These will be useful for returning data in function
# or for passing information around in a compact fashion.  Those of you that 
# are familiar with python, you can think of these as dict objects in a sense.

good.list <- list(1 , 'no')
named.list <- list('five' = 1, 'six' = 2, 3,4,5)

list.of.list <- list(list(c(1,2,4,6)), list(c(4,5,2,6)))

list.of.list[[1]]
lapply(list.of.list[[1]], mean)

numVec <- 1:10


bball <- read.csv('./rData/baseball.dat.txt')
bball$PlayerID <- 1:nrow(bball)

PlayerInfo <- data.frame(PlayerID=bball$PlayerID,
                         Name = bball$Name,
                         Salary = bball$Salary)
HitData <- bball[,c(19, 2:13)]
write.csv(PlayerInfo, file = './rData/playerInfo.csv', row.names = F)
set.seed(1515)
observed.hit.info <- sample(c(TRUE, FALSE), nrow(HitData), prob=c(0.95, 0.05), replace = T)
write.csv(HitData[observed.hit.info,], file = './rData/performanceInfo.csv', row.names = F)


dat1 <- read.csv('./rData/playerInfo.csv')
dat2 <- read.csv('./rData/performanceInfo.csv')
head(merge(dat1, dat2, by = 1, all = T))

pairs(dat2)
