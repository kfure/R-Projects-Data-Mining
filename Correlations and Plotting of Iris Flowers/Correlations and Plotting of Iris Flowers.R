setwd(" ")
 save.image(file = 'Lab2_myFile.Rdata')
install.packages("VIM")
 library(VIM)
 install.packages("mice")
 library(mice)
library(datasets)
data(iris)
head(iris)
summary(iris)
nrow(iris)
md.pattern(iris)
nrow(iris[!complete.cases(iris),])
boxplot(Petal.Length~Species, data=iris, col="pink", main="Iris Species by Petal Length", ylab = "Petal Length", xlab = "Iris Species")

boxplot(Petal.Width~Species, data=iris, col="purple", main="Iris Species by Petal Width", ylab = "Petal Width", xlab = "Iris Species")

install.packages("vioplot")
library(vioplot)

v1 = iris$Petal.Length[iris$Species == "setosa"]
v2 = iris$Petal.Length[iris$Species == "versicolor"]
v3 = iris$Petal.Length[iris$Species == "virginica"]
vioplot(v1, v2, v3, names = c("Setosa", "Versicolor", "Virginica"), col=c("lightgreen", "lightblue", "palevioletred"), main = "Iris Species by Petal Length")

v1 = iris$Petal.Width[iris$Species == "setosa"]
v2 = iris$Petal.Width[iris$Species == "versicolor"]
v3 = iris$Petal.Width[iris$Species == "virginica"]

vioplot(v1, v2, v3, names = c("Setosa", "Versicolor", "Virginica"), col=c("lightgreen", "lightblue", "palevioletred"), main = "Iris Species by Petal Width")


v1 = iris$Sepal.Width[iris$Species == "setosa"]
v2 = iris$Sepal.Width[iris$Species == "versicolor"]
v3 = iris$Sepal.Width[iris$Species == "virginica"]

vioplot(v1, v2, v3, names = c("Setosa", "Versicolor", "Virginica"), col=c("lightgreen", "lightblue", "palevioletred"), main = "Iris Species by Sepal Width")

v1 = iris$Sepal.Length[iris$Species == "setosa"]
v2 = iris$Sepal.Length[iris$Species == "versicolor"]
v3 = iris$Sepal.Length[iris$Species == "virginica"]

vioplot(v1, v2, v3, names = c("Setosa", "Versicolor", "Virginica"), col=c("lightgreen", "lightblue", "palevioletred"), main = "Iris Species by Sepal Length")

plot(Petal.Length ~Petal.Width, data = iris, col="blue", main = "Scatter Plot of Petal Size",  ylab = "Petal Length", xlab = "Petal Width")

plot(Petal.Length ~Sepal.Length, data = iris, col="blue", main = "Scatter Plot of Petal to Sepal Length",  ylab = "Petal Length", xlab = "Sepal Length")

plot(Petal.Width ~Sepal.Width, data = iris, col="blue", main = "Scatter Plot of Petal to Sepal Width",  ylab = "Petal Width", xlab = "Sepal Width")

IrisSpecs = iris[ , 1:4]
cor(IrisSpecs)





