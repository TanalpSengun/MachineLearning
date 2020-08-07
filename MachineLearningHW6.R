library(MASS)
library(mixtools)

#we always use MASS library in the labs.


#since our course code is 521 , all of the homeworks's seeds are 521.
set.seed(521)


# mean parameters
means_of_classes = matrix(c(+2.5, +2.5,     #the mean parameters are given
                            -2.5, +2.5,
                            -2.5, -2.5,
                            +2.5, -2.5,
                            0.0, 0.0),2,5)


# covariance parameters
covariances_of_classes = array(c(+0.8, -0.6, -0.6, +0.8, +0.8, +0.6, +0.6, +0.8,    #since the covariances are given
                                 +0.8, -0.6, -0.6, +0.8, +0.8, +0.6, +0.6, +0.8,
                                 +1.6, -0.0, -0.0, +1.6), c(2, 2, 5))


length1= 50; #for the first 4 length
length2= 100; #for the last length

# sample sizes

size_of_classes = c(length1,length1,length1,length1, #the first four has 50 
                    length2) #the last has 100.

# generate random samples
# function's usage example mvrnorm(n = 1, mu, Sigma, tol = 1e-06, empirical = FALSE)
# n= number fo samples,mu= means , Sigma = covariances , tolerance , logical )
points_first = mvrnorm(n = size_of_classes[1],  means_of_classes[,1], covariances_of_classes[,,1])
points_second= mvrnorm(n = size_of_classes[2],  means_of_classes[,2],  covariances_of_classes[,,2])
points_third= mvrnorm(n = size_of_classes[3], means_of_classes[,3], covariances_of_classes[,,3])
points_fourth = mvrnorm(n = size_of_classes[4],  means_of_classes[,4], covariances_of_classes[,,4])
points_fifth = mvrnorm(n = size_of_classes[5],  means_of_classes[,5],  covariances_of_classes[,,5])

# to gather all of them 
X = rbind(points_first, points_second, points_third, points_fourth, points_fifth)

# plot data points
plot(points_first[,1], points_first[,2], type = "p", pch = 19, col = "black", las = 1,
     xlim = c(-6, 6), ylim = c(-6, 6),
     xlab = "x1", ylab = "x2")

#with points we can plot the others.
points(points_second[,1], points_second[,2], type = "p", pch = 19, col = "black")
#plot second
points(points_third[,1], points_third[,2], type = "p", pch = 19, col = "black")
#plot third
points(points_fourth[,1], points_fourth[,2], type = "p", pch = 19, col = "black")
#plot fourth
points(points_fifth[,1], points_fifth[,2], type = "p", pch = 19, col = "black")
#plot fifth

number_of_points <- 300   # sum of the class sizes.
centeroids <<- NULL


#Setting clustering number.
K <- 5
N <- length(size_of_classes)


appointments <<- NULL #in the lab 11 this is the same as assignements variable.



#the first two iterations step

for(i in 1:2 ){
  
  if (is.null(centeroids) == 1) {
    #initializing centroids to K cluster with number of points with randomly
    #then we will change the center of them looking at the distances.
    centeroids = X[sample(1:number_of_points, K),]
  } 
  else {
    for (m in 1:K) {
      centeroids[m,] = colMeans(X[appointments == m,])
    }  
  }
  #these are all very similar from lab 11.
  
  temporary = as.matrix(dist(rbind(centeroids, X), method = "euclidean"))
  temporary = temporary[1:nrow(centeroids), (nrow(centeroids) + 1):(nrow(centeroids) + nrow(X))]
  appointments <<- sapply(1:ncol(temporary), function(h) {which.min(temporary[,h])})
  
}



#initialize em

# calculate prior probabilities like in the older homeworks
prior_probabilities <- sapply(X = 1:K, 
                              FUN = function(c) {mean(appointments == c)})


# in here we find sample covariances

sample_covariances<- 0*covariances_of_classes # create covariance of classes length zero matrix

indice<- 0
centeroids <- t(centeroids) #we have to transpose centroids.
data_dimension = 2 # our data has two dimension namely x1,x2
temporary_covariances<- matrix(0,4,K)  #create zero matrix for the covariances

for (i in 1:data_dimension){
  temp_data <- X[,i]
  for (j in 1:data_dimension){
    temp_data_2 <- X[,j]
    # we will look for the x1 x2 and mean difference from the centeroids with two for loop.
    indice <- indice+1 #to start with 1.
    
    temporary_covariances[indice,]<- sapply(X = 1:K, FUN = function(c) {mean((temp_data[appointments == c] - 
                                                                                centeroids[i,c])*(temp_data_2[appointments == c] - centeroids[j,c]))})
    
  }
}

for (i in 1:K){
  sample_covariances[,,i] <-matrix(temporary_covariances[,i],2,2)
}


#em algorithm

em_max_iteration <- 100

aim <- matrix(0,number_of_points,K) #one of K encoding same as the older homeworks.

for (d in 1:em_max_iteration){
  
  #E-step
  sum_of_aim <- matrix(0,sum(size_of_classes))
  for (i in 1:number_of_points){
    for ( j in 1:K){
      #looking at the distances again.
      distances=X[i,]- centeroids[,j]
      #those distances changes will help to correct the mean in the future.
      #corrected with every iteration
      aim[i,j] <- (2*pi*(det(sample_covariances[,,j]))^(-0.5))%*%
        exp((-0.5)*t(distances)%*% solve(sample_covariances[,,j])%*%(distances)) * prior_probabilities[j] 
      
      #corrected with every iteration
      sum_of_aim[i] = sum_of_aim[i]+aim[i,j]
    }
  }
  #as discussed in the lecture, this matrix will have data * cluster size places.
  H=matrix(0,number_of_points,K)
  
  #finding every points possibility for any group
  
  H[,1] <- aim[,1]/sum_of_aim
  H[,2] <- aim[,2]/sum_of_aim
  H[,3] <- aim[,3]/sum_of_aim
  H[,4] <- aim[,4]/sum_of_aim
  H[,5] <- aim[,5]/sum_of_aim
  
  
  #rowSums(H) Since it is the probability every row's summation should be add up to 1.
  
  #Same M-step discussed in the Lecture 21.
  for (g in 1:K){
    centeroids[,g] <- t(X)%*%H[,g]/sum(H[,g])
  }
  
  for (i in 1:K){
    
    class_total_var= matrix(0,  2,  2)
    #now we have to look for the distance for all points.
    for (j in 1:number_of_points){
      distance=X[j,]-centeroids[,i]
      
      #as discussed the distance in the lecture 21 I gave dist 
      class_total_var=class_total_var + H[j,i]*(distance)%*%t(distance)
      
    }
    #with dividing the class total var to sum of its members find the sample covariance.
    total_H_i = sum(H[,i])
    sample_covariances[,,i] = class_total_var/total_H_i
  }
  
  #prior probabilites are changing with the H's.
  prior_probabilities <- colSums(H)/sum(size_of_classes)  
}

#all of the data points probability should be the number of data points and I checked this
sum(H[,2])+ sum(H[,1])+ sum(H[,3])+ sum(H[,4])+ sum(H[,5]) #should be 300.
centeroids <- t(centeroids) #again transpose it for the dimensions


H_maximum_probability=max.col(H) #maximum probability class assigned to it. FOr example if class2 has a higher prob returns 2 fro that column

colors=c("orange","red", "green", "blue", "purple")  

plot(X[,1][H_maximum_probability==1], X[,2][H_maximum_probability==1], type = "p", pch = 19, col = colors[1], las = 1,
     xlim = c(-6, 6), ylim = c(-6, 6),  xlab = "x1", ylab = "x2")
#with points we can plot the others.
#plot 2
points(X[,1][H_maximum_probability==2], X[,2][H_maximum_probability==2], type = "p", pch = 19, col = colors[2])
#plot 3
points(X[,1][H_maximum_probability==3], X[,2][H_maximum_probability==3], type = "p", pch = 19, col = colors[3])
#plot 4
points(X[,1][H_maximum_probability==4], X[,2][H_maximum_probability==4], type = "p", pch = 19, col = colors[4])
#plot 5
points(X[,1][H_maximum_probability==5], X[,2][H_maximum_probability==5], type = "p", pch = 19, col = colors[5])



# print the centeroids to compare it with the homework.
# even though the order difference ??ay be unimportant I would like to 
#order them as in the homework given.

centeroid_as_homework = 0*centeroids

centeroid_as_homework[1,]=centeroids[4,];
centeroid_as_homework[2,]=centeroids[3,];
centeroid_as_homework[3,]=centeroids[2,];
centeroid_as_homework[4,]=centeroids[1,];
centeroid_as_homework[5,]=centeroids[5,];

centeroid_as_homework


#usage of ellipse 
#ellipse(mu, sigma, alpha = .05, npoints = 250, newplot = FALSE, draw = TRUE, ...)
#one must download the mixtools package to use the ellipses.

for(i in 1:K){
  #these plots are from the means of the classes
  # we use the lty=2 for the dashed line as aked in the homework.
  # our aim is to plot over the datapoints so we make newplot false .
  ellipse(means_of_classes[,i], covariances_of_classes[,,i], .05,  size_of_classes[i], newplot = FALSE, draw = TRUE, lty=2, lwd=2)
  #these plots are from the centeroids
  ellipse(centeroids[i,], sample_covariances[,,i], .05, size_of_classes[i], newplot = FALSE, draw = TRUE, lwd=2)
}








