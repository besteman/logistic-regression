# Title: Problem Set 3
# Author: Justin Besteman


# Clean up
rm(list = ls())


library(readr)
library(dplyr)
library(ROCR)

# Loading in Data
theData <- read_delim(
  "~/Desktop/code/topics/logistic-regression/PassFail.dat",
  " ",
  escape_double = FALSE,
  col_names = FALSE,
  col_types =
    cols(
      X1 = col_skip(),
      X10 = col_skip(),
      X12 = col_skip(),
      X14 = col_skip(),
      X2 = col_skip(),
      X4 = col_skip(),
      X6 = col_skip(),
      X8 = col_skip()
    ),
  na = "null",
  trim_ws = TRUE
)

# Making the theData into a data.frame
PassFail <- data.frame(theData)

# Renaming columns
colnames(PassFail) <- c("y", "x1", "x2", "x3", "x4", "x5", "x6")

# Number of Observation will hold the number of observation
numberOfObservation <-  nrow(PassFail)

print(numberOfObservation)

# df = data.frame(matrix(rnorm(20), nrow=10))

# randomSamplePassPail <-  sample_n(PassFail, numberOfObservation * .60)


set.seed(123321)

index <- c(1:numberOfObservation)

random6000 <- sample(index, numberOfObservation * .60)

trainingSamplePassFail <- PassFail[random6000 ,]

testData <-  PassFail[-random6000 ,]




model <- glm(y  ~ . , data = trainingSamplePassFail , family = binomial(link = "logit"))

print(model)



fitted.results <- predict(model, testData ,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)

misClasificError <- mean(fitted.results != testData$y)
print(paste('Accuracy',1-misClasificError))


p <- predict(model,testData, type="response")
pr <- prediction(p, testData$y)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")

pdf("rocr.pdf")

plot(prf)

dev.off()

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc
