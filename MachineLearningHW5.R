#Author : TANALP SENGUN
#COURSE : INTRODUCTION TO MACHINE LEARNING


# read data into memory
data_set <- read.csv("hw05_data_set.csv", header = TRUE)

# get x and y values
x_train <- data_set$x[1:100]
y_train <- data_set$y[1:100]
x_test <- data_set$x[101:133]
y_test <- data_set$y[101:133]

#the length of the train and the test data
N_train= length(x_train)
N_test= length(x_test)


#for the color of the data points I will plot in p=10
point_color = matrix(1:133, nrow = 1, ncol = 133)
point_color[1:100] = "blue";
point_color[101:133] = "red";

D <- 1

#all of the test and train data
x_all <- data_set$x[1:133]
y_all <- data_set$y[1:133]


# we are giving p=10 as the initation
P<-10

# create necessary data structures
node_indices <- list()
is_terminal <- c()
need_split <- c()


node_splits <- c()
node_mean <- c()

#we no longer need any class since now only have one class
# put all training instances into the root node
node_indices <- list(1:N_train)
is_terminal <- c(FALSE)
need_split <- c(TRUE)

predicted_values <- c()

for(k in 1:20){
  node_indices <- list()
  is_terminal <- c()
  need_split <- c()
  
  
  node_splits <- c()
  node_mean <- c()
  # put all training instances into the root node
  node_indices <- list(1:N_train)
  is_terminal <- c(FALSE)
  need_split <- c(TRUE)
  # learning algorithm
  while (1) {
    # find nodes that need splitting
    split_nodes <- which(need_split)
    # check whether we reach all terminal nodes
    if (length(split_nodes) == 0) {
      break
    }
    # find best split positions for all nodes
    for (split_node in split_nodes) {
      data_indices <- node_indices[[split_node]]
      need_split[split_node] <- FALSE
      # Since there is no more multiclass only class would be enough.
      
      node_mean[[split_node] ] <- mean(y_train[data_indices]) 
      # check whether node is pure
      
      #if data indice number smaller than 10 preprune it 
      if (length(data_indices) <= k) {
        is_terminal[split_node] <- TRUE
      }
      #-----------------------------------------------
      else {
        is_terminal[split_node] <- FALSE
        
        best_scores <- 0
        best_splits <- 0
        
        unique_values <- sort(unique(x_train[data_indices]))
        split_positions <- (unique_values[-1] + unique_values[-length(unique_values)]) / 2
        split_scores <- rep(0, length(split_positions))
        for (s in 1:length(split_positions)) {
          left_indices <- data_indices[which(x_train[data_indices] <= split_positions[s])]
          right_indices <- data_indices[which(x_train[data_indices] > split_positions[s])]
          
          # I have to change the error form to square error 
          split_scores[s] <- 1 / length(data_indices) * sum((y_train[left_indices] - mean(y_train[left_indices]))^2) +
            1 / length(data_indices) * sum((y_train[right_indices] - mean(y_train[right_indices]))^2)
        }
        
        best_scores<- min(split_scores)
        best_splits <- split_positions[which.min(split_scores)]
        
        # decide where to split on which feature
        #split_d <- which.min(best_scores)
        
        node_splits[split_node] <- best_splits
        
        # create left node using the selected split
        left_indices <- data_indices[which(x_train[data_indices] <= best_splits)]
        node_indices[[2 * split_node]] <- left_indices
        is_terminal[2 * split_node] <- FALSE
        need_split[2 * split_node] <- TRUE
        
        # create left node using the selected split
        right_indices <- data_indices[which(x_train[data_indices] > best_splits)]
        node_indices[[2 * split_node + 1]] <- right_indices
        is_terminal[2 * split_node + 1] <- FALSE
        need_split[2 * split_node + 1] <- TRUE
      }
    }
  }
  
  # traverse tree for test data points
  y_predicted <- c()
  for (i in 1:N_test) {
    index <- 1
    while (1) {
      if (is_terminal[index] == TRUE  || is.na(node_splits[index])) {
        y_predicted[i] <-node_mean[[index]]
        break
      } else {
        if (x_test[i] <= node_splits[index]) {
          index <- index * 2
        } else {
          index <- index * 2 + 1
        }
      }
    }
   
  }
 
  # if P=10 I would plot the graph so that it will be similar to the homework.
  if(k==10){
   
    plot_array = c();
    data_interval <- seq(from = 0, to = 60, by = 0.01)
    for(i in 1:length(data_interval) ){
      index <- 1
      while (1) {
        plot_array[i] <-node_mean[index]
        if (is_terminal[index] == TRUE  || is.na(node_splits[index])) {
          break
        } else {
          if (data_interval[i] <= node_splits[index]) {
            index <- index * 2
          } else {
            index <- index * 2 + 1
          }
        }
      }
    }
    
    plot(x_all,   y_all, type = "p", pch = 19, col=point_color , 
         ylim = c(-150, 100), xlim = c(0, 60),
         ylab = "y", xlab = "x", las = 1, main = sprintf("P = %g", k))
    lines(data_interval,plot_array);
    
 
    
  }
  
  
  error_func= (sum((y_predicted - y_test)^2)/N_test)^(1/2);
  
  predicted_values <- c(predicted_values,error_func);
  
  
}

sprintf("RMSE is  %g when P is 10" , predicted_values[10] );

# I took this from the last homework that I done, the plots are almost the same except the x's and y's which are 1:20
# and the predicted values.
plot(1:20,   predicted_values, type = "p", pch = 1, col="black" , 
     ylim = c(min(predicted_values),max(predicted_values)), xlim = c(1, 20),
     ylab = "RMSE", xlab = "P", las = 1  )
# I connect the dots with the lines.
lines(1:20,predicted_values,lty=1);







