
if(!require(ggplot2)){
install.packages("ggplot2")
}
library(ggplot2)
        
 # animation of k-means
if(!require(animation)){
install.packages("animation")
 }
 library(animation) 
        
if(!require(fpc)){
install.packages("fpc")
}
library(fpc)
        
setwd(" ")
iris_full <- read.csv("iris.csv")
head(iris_full)
iris = iris_full[ , c('Petal.Length', 'Petal.Width')]
head(iris)
plot(iris)
normIt <- function(feature){
                normalized <- ((feature - min(feature)) / (max(feature) - min(feature)))
                return (normalized)
        }
nor_iris <- apply(iris[,c(1,2)], 2, FUN = normIt)
nor_iris <- as.data.frame(nor_iris)
head(nor_iris)
K5 <- kmeans(nor_iris, 5)
class(K5)
str(K5)
kmeans.totwithinss.k <- function(dataset, number_of_centers){
                km <- kmeans(dataset, number_of_centers)
                km$tot.withinss
        }
c2 = kmeans.totwithinss.k(nor_iris, 2)
c2    
c3 = kmeans.totwithinss.k(nor_iris, 3)
c3
c4 = kmeans.totwithinss.k(nor_iris, 4)
c4        
c5 = kmeans.totwithinss.k(nor_iris, 5)   
c5  
c6 = kmeans.totwithinss.k(nor_iris, 6)   
c6  

kmeans.distortion <- function(dataset, maxk){
                vec <- as.vector(1:maxk)
                vec[1:maxk] <- sapply(1:maxk, kmeans.totwithinss.k, dataset = dataset)
                return (vec)
        }
        
maxk <- 7
dis_vct <- kmeans.distortion(nor_iris, maxk)
#   Elbow Curve
plot(1:maxk,
      dis_vct,
      type = 'b',
      col = 'blue',
      xlab = "Number of cluster",
      ylab = "Distortion"
        )
  
num_cluster = 3
        
#animation of k-means
result <- kmeans.ani(nor_iris, num_cluster)
        
# result$centers contains average geo-location, which are the centers for each clusters.
# The second aggregate method counts the number of points in each cluster
centers <- as.data.frame(result$centers)
centers
counts <- aggregate(nor_iris, by = list(result$cluster), FUN = length)[,2]
counts
nor_iris$cluster <- result$cluster  
head(nor_iris)


# attach cluster label based on the distance from center of clusters to position(0,0)
# k-means is a clustering algorithm, assuming a isotropic data as input
ra <- as.data.frame(centers$Petal.Length^2 + centers$Petal.Width^2)
ra

ra$real <- rank(ra)  #sort of their distance from origin
        
nominal <- c()
for(i in (1:nrow(ra))){
nominal[i] <- i
 	}
ra$nominal <- nominal    #I think nominal is the row number?
        
# retag cluster number
result$real <- c()
result
for (i in (1:length(result$cluster))){
 	for(j in (1:4)){
 		if(result$cluster[i] == j){
			result$real[i] <- ra$real[j]
		}   
	 }              
}
result

# confusion matrix
table(result$real, nor_iris$cluster)    #reassinging cluster number based on distance from origina
        ############################################
        ###    just used to check performance, 
        ###    DO NOT take K-means as a classifier
        ############################################

head(nor_iris)

##############################################
###        Visualization by ggplot2        ###
##############################################

 # base layer
 plot.iris <- ggplot(data = nor_iris, aes(x = Petal.Length, y = Petal.Width, color = result$cluster))
 plot.iris
        
 # alpha: semi-transparent points
 plot.iris + geom_point(alpha = .25, size = 5) + 
                # cluster centers, colored black:
                geom_point(data = centers, aes(x = Petal.Length, y = Petal.Width), size = 5, color = 'black')  + 
                # cool colors for each cluster:
                scale_color_gradientn(colours = rainbow(num_cluster)) + 
                # add a title, align to the center
                theme(plot.title = element_text(hjust = 0.5)) +
                ggtitle("GGPlot of K-means clusters")




###########################################################
##########                DBSCAN                ###########
###########################################################

#   eps:     Reachability distance
#   MinPts:  Reachability minimum no. of points,
#   scale:   Scale the data if TRUE

ds <- dbscan(nor_iris[,c(1,2)], eps = .4, MinPts = 4, scale = TRUE, showplot = 1, seeds = TRUE, method = "hybrid")
ds
        
# triangle:            core points
# circle in color:     border points
# else:                outliers or noises
        
        
#   confusion matrix
table(ds$cluster, nor_iris$cluster)   #ds is only showing 2 clusters vs 3
        
 #   0 noises or outliers
 #   else: identified clusters
        
 #  attach if a obs is a core point or a outlier
ds$isseed
nor_iris$core <- ds$isseed
nor_iris$outliers <- ds$cluster
nor_iris$core
nor_iris$outliers
head(nor_iris)

##############################################
###        Visualization by ggplot2        ###
##############################################

 # base layer
plot.dbiris <- ggplot(data = nor_iris, aes(x = Petal.Length, y = Petal.Width, color = ds$cluster, shape = nor_iris$core ))
        
        
# alpha: semi-transparent points
plot.dbiris + geom_point(alpha = .25, size = 5) + 
                # outliers, shape outline black:
                geom_point(data = subset(nor_iris, nor_iris$outliers == 0), aes(x = Petal.Length, y = Petal.Width), size = 5, color = 'black', shape = 1)  + 
                # cool colors for each cluster:
                scale_color_gradientn(colours = rainbow(length(unique(ds$cluster)))) + 
                 # colour backgound as 'grey'
                theme(panel.background = element_rect(fill = 'grey', colour = 'black')) +
                # add a title, align to the center
                theme(plot.title = element_text(hjust = 0.5)) +
                ggtitle("GGPLOT DBSCAN") 
    
 g=ggplot(iris_full, aes(x=Petal.Length, y=Petal.Width, col = Species)) + geom_point()  
 plot(g)  
 g = g+ggtitle("Petal Clusters Predict Species")
 plot(g)
