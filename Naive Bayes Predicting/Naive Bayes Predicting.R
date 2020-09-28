setwd(" ")
install.packages("e1071")
library(e1071)

BalanceScale = read.csv('Balance_Scale.csv')
summary(BalanceScale)
head(BalanceScale)
nrow(BalanceScale[!complete.cases(BalanceScale), ])
nrow(BalanceScale)
levels(BalanceScale$classes)
summary(BalanceScale$classes)
class(BalanceScale$classes)
sample_size = floor(.7 *nrow(BalanceScale))
training_index = sample(nrow(BalanceScale), size = sample_size, replace = FALSE)
train = BalanceScale[training_index, ]
test = BalanceScale[-training_index, ]

Balance.model = naiveBayes(as.factor(classes) ~ . , data = train)
#Balance.model
Balance.predict = predict(Balance.model, test, type = 'class')
results = data.frame(actual = test[ , 'classes'], predicted = Balance.predict)
table(results)
nrow(results[results$predicted == results$actual, ]) / nrow(results)


abalone = read.csv('abalone.csv')
summary(abalone)
head(abalone)
class(abalone$Sex)

nrow(abalone[!complete.cases(abalone), ])
nrow(abalone)

a_sample_size = floor(.7 *nrow(abalone))
a_training_index = sample(nrow(abalone), size = a_sample_size, replace = FALSE)
a_train = abalone[a_training_index, ]
a_test = abalone[-a_training_index, ]

abalone.model = naiveBayes(as.factor(Sex) ~ . , data = a_train)
abalone.model
abalone.predict = predict(abalone.model, a_test, type = 'class')

abalone.results = data.frame(actual = a_test[ , 'Sex'], predicted = abalone.predict)
abalone.results
table(abalone.results)
nrow(abalone.results[abalone.results$predicted == abalone.results$actual, ]) / nrow(abalone.results)

aggregate(abalone[, -1], by = list(abalone$Sex), mean)



a_train = abalone[a_training_index, c('Sex','Diameter', 'Wholeweight')]
a_test = abalone[-a_training_index, c('Sex', 'Diameter', 'Wholeweight')]

abalone.model = naiveBayes(as.factor(Sex) ~ . , data = a_train)
abalone.model
abalone.predict = predict(abalone.model, a_test, type = 'class')

abalone.results = data.frame(actual = a_test[ , 'Sex'], predicted = abalone.predict)
#abalone.results
table(abalone.results)

