# interacting with the filesystem

# choose a working directory by clicking 
# session > Set Working Directory > Choose Directory ...

# or use shortcut ctrl + shift + h

# where are we
getwd()

# I want cars
data <- read.csv("mtcars.csv")

# that didn't work
# let's go somewhere else
# set the working directory with a command
setwd("testdir")

# now where are we
getwd()

# what's in here
dir()

# get our data
data <- read.csv("mtcars.csv")

# take a look
data




