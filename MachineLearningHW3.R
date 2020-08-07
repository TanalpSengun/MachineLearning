
# Author : Tanalp Sengun ( 600150)
# Course : INTRODUCTION TO MACHINE LEARNING (Fall 2018)
# by Mehmet Gonen
#  the lab5 and the book from Ethem Alpayd??n is used for the 
# homework. The lines are explained again in the report.

# reading the data to the memory

#headers are false for taking into account the first data .

data_set <- read.csv("hw03_data_set_images.csv", header = FALSE)

true_y <-  read.csv("hw03_data_set_labels.csv", header = FALSE)

#the datas are uploaded now its time to direct them to matrix
# get X and y values
X1 =as.matrix(data_set)
Y1 =as.matrix(true_y)


#seperating the train and test data for every class 
#since only the 25 of each class will be the train data , others will be tested.

trainA = X1[1:25,]
trainB = X1[40:64,]
trainC = X1[79:103,]
trainD = X1[118:142,]
trainE = X1[157:181,]

#here I gathered all train data to feed my system at once

trainTotal= rbind(trainA,trainB,trainC,trainD,trainE)


#since only the 14 of each class will be the test data, I will sepereate them.
testA = X1[26:39,]
testB = X1[65:78,]
testC= X1[104:117,]
testD = X1[143:156,]
testE = X1[182:195,]

#here I gathered all test data to test my data at once

testTotal= rbind(testA,testB,testC,testD,testE);


#now I have to make the same procedure to the labels
# I divide all of the label data into train and test again
# But the letters are not okey , I should turn them into numbers 
#defaultly the numbers are added as strings, so I change them to numeric.

Y_trainA = Y1[1:25,]
Y_trainA [1:25] = 1;
Y_trainA = as.numeric(Y_trainA);

Y_trainB = Y1[40:64,]
Y_trainB [1:25] = 2;
Y_trainB = as.numeric(Y_trainB);

Y_trainC = Y1[79:103,]
Y_trainC [1:25] = 3;
Y_trainC = as.numeric(Y_trainC);

Y_trainD = Y1[118:142,];
Y_trainD [1:25] = 4;
Y_trainD= as.numeric(Y_trainD);

Y_trainE = Y1[157:181,]
Y_trainE [1:25] = 5;
Y_trainE = as.numeric(Y_trainE);

#I create a matrix from zero and match the proper numbers to them.

Y_trainTotal = matrix(1:125, nrow = 125, ncol = 1)
Y_trainTotal[1:25]=Y_trainA;
Y_trainTotal[26:50]=Y_trainB;
Y_trainTotal[51:75]=Y_trainC;
Y_trainTotal[76:100]=Y_trainD;
Y_trainTotal[101:125]=Y_trainE;


#now I have to make the same procedure to the labels
# I divide all of the label data into train and test again
# But the letters are not okey , I should turn them into numbers 
#defaultly the numbers are added as strings, so I change them to numeric.


Y_testA = Y1[26:39,]
Y_testA [1:14] = 1;
Y_testA = as.numeric(Y_testA);

Y_testB = Y1[65:78,]
Y_testB [1:14] = 2;
Y_testB = as.numeric(Y_testB);

Y_testC= Y1[104:117,]
Y_testC [1:14] = 3;
Y_testC = as.numeric(Y_testC);

Y_testD = Y1[143:156,]
Y_testD [1:14] = 4;
Y_testD = as.numeric(Y_testD);

Y_testE = Y1[182:195,]
Y_testE [1:14] = 5;
Y_testE = as.numeric(Y_testE);

Y_testTotal = matrix(1:70, nrow = 70, ncol = 1)
Y_testTotal[1:14]=Y_testA;
Y_testTotal[15:28]=Y_testB;
Y_testTotal[29:42]=Y_testC;
Y_testTotal[43:56]=Y_testD;
Y_testTotal[57:70]=Y_testE;



# get number of samples and number of features
N <- length(Y_trainTotal) #125 sample
D <- ncol(trainTotal) #320 features

#number of classes 
K<- 5

# set learning parameters
eta <- 0.005 # learning speed
epsilon <- 1e-3 # minimum value for the wanted error
H <- 20   #hidden node
max_iteration <- 200 # maximum number of iteration.

set.seed(521)

#initialize the weigths

W <- matrix(runif((D + 1) * H, min = -0.01, max = 0.01), D + 1, H)
v <- matrix(runif((H + 1)*K, min = -0.01, max = 0.01),H+1,K)

# define the sigmoid function
sigmoid <- function(a) {
  return (1 / (1 + exp(-a)))
}

# one-of-K-encoding
R_t <- matrix(0, N, K)
R_t[cbind(1:N, Y_trainTotal)] <- 1


# define the softmax function
softmax <- function(trainTotal, W) {
  scores <- cbind(1, trainTotal) %*% W
  scores <- exp(scores - matrix(apply(scores, MARGIN = 2, FUN = max), nrow = nrow(scores), ncol = ncol(scores), byrow = FALSE))
  scores <- scores / matrix(rowSums(scores), nrow(scores), ncol(scores), byrow = FALSE)
  return (scores)
}

# define the safelog function in case we use it
safelog <- function(x) {
  return (log(x + 1e-100))
}


Z <- sigmoid(cbind(1, trainTotal) %*% W)
y_predicted <- softmax(Z,v)
objective_values <- -sum(R_t * safelog(y_predicted))


# learn W and v using gradient descent and online learning
iteration <- 1
while (1) {
  v <- v + eta * t(cbind(1,Z)) %*% (R_t - y_predicted)
  W <- W + eta * (t(cbind(1, trainTotal)) %*% (((R_t - y_predicted) %*% t(v[2:21,])) * Z * (1 - Z) ))
  
  
  # calculate hidden nodesdim()
  Z <- sigmoid(cbind(1, trainTotal) %*% W)
  # calculate output node
  y_predicted <- softmax(Z,v)
  objective_values <- c(objective_values, -sum(R_t * safelog(y_predicted) ))
  
 
  if (abs(objective_values[iteration + 1] - objective_values[iteration]) < epsilon | iteration >= max_iteration) {
    break
  }
  
  iteration <- iteration + 1
}
#print(W)
#print(v)

# plot objective function during iterations
plot(1:(iteration + 1), objective_values,
     type = "l", lwd = 2, las = 1,
     xlab = "Iteration", ylab = "Error")

# calculate confusion matrix for the train data
y_prediction <- apply(y_predicted, 1, which.max)
confusion_matrix <- table(y_prediction, Y_trainTotal)
print(confusion_matrix)

Z <- sigmoid(cbind(1, testTotal) %*% W)
y_predicted_test <- softmax(Z,v)
# calculate confusion matrix for the train data
y_predicted_testConf <- apply(y_predicted_test, 1, which.max)
confusion_matrix <- table(y_predicted_testConf, Y_testTotal)
print(confusion_matrix)

