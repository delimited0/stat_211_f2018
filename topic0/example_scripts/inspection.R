# how to see what you're dealing with

# create some variables for demonstration purposes
x <- 15
classes <- c("STAT 211", "STAT 405", "STAT 616")
what_time <- function() {
  Sys.time()
}
judge <- USJudgeRatings

# see all environment variables
ls()

# see environment variables in a directory
# ls("testdir")

# print out these variables
x
classes
what_time
judge

# this also prints variables, exactly same output as before
print(x)
print(classes)
print(what_time)
print(judge)

# learn something about the variables, use str function
str(x)
str(classes)
str(what_time)
str(judge)

# look at variables using viewer
View(x)
View(classes)
View(what_time)
View(judge)

# look at sizes and lengths
length(x)
length(classes)
dim(judge)

# delete all environment variables
rm(list = ls())


