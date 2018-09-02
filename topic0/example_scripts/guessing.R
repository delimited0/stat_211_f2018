# simulation example

# guessing on multiple choice exams
# the exam has 20 questions
# each question has five answer choices, one of which is correct
# you didn't study. resigned to your fate, you make a (uniform) random guess on every question
# you need a score of at least 13/20 to pass

# you are also trapped in a groundhog day type scenario, where you must relive 
# this awful test 1000 times

# store the probability of answering a question correctly in a variable
p <- .2

# simulate the result of taking the test once. represent as a vector with 1
# indicating you got the question right, 0 wrong
# use the sample function
test_result <- sample(c(1, 0), 20, TRUE, c(p, 1 - p))

# tally the number of correct questions
sum(test_result)

# did you pass?
sum(test_result) > 13

# now repeat this 1000 times
# use a for loop
# then do this again, but use the replicate function
scores <- c()
scores <- rep(NA, 1000)
for (i in 1:1000) {
  test_result <- sample(c(1, 0), 20, TRUE, c(p, 1 - p))
  s <- sum(test_result)
  # scores <- c(scores, s)
  scores[i] <- s
}

# summarize the results with the table function
table(scores)

# how often did you pass?

# by studying, you raise the probability of answering correctly. what does this
# probability need to be for you to pass more than half the time?

