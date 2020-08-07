

#this is the safer version of log in case of the threat log0.
safe_log <- function(x1) {
  return (log(x1 + 1e-100))
}

# reading the data to the memory

  #headers are false for taking into account the first data .

data_set <- read.csv("hw02_data_set_images.csv", header = FALSE)

true_y <-  read.csv("hw02_data_set_labels.csv", header = FALSE)

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



# get number of classes and number of samples
classNum <- 5 # number of classes
N <- length(Y_trainTotal) #number of samples

# one-of-K-encoding
# I created a zero matrix with size NxK.
# I fill it with the prior prepared Y_traintotal.
R_t <- matrix(0, N, classNum)
R_t[cbind(1:N, Y_trainTotal)] <- 1


# define the sigmoid function with 3 variables
# I use the equations 10.44 and 10.45 when I am preparing the fundamental of the codes.
#10.37 from book --- sigmoid function given

sigmoid <- function(trainTotal, W, w0) {
  
  scores <- cbind(trainTotal, 1) %*% rbind(W, w0)
  
  return (1 / (1 + exp(-scores)))
  
}  

# define the gradient functions
# I use the equations 10.44 and 10.45 when I am preparing the fundamental of the codes.
# following code is exactly same mathematic applied from the 10.44
gradient_W <- function(trainTotal, R_t, Y_predicted) {
  return (sapply(X = 1:ncol(R_t), function(b) -colSums(matrix((R_t[,b] - Y_predicted[,b])*(1-Y_predicted[,b])*Y_predicted[,b],
                                                      nrow = nrow(trainTotal), ncol = ncol(trainTotal), byrow = FALSE) *trainTotal ))
  )
  
  
}

# following code is exactly same mathematic applied from the 10.45
gradient_w0 <- function(R_t, Y_predicted) {
  return (-colSums((R_t - Y_predicted)*(1-Y_predicted)*Y_predicted))
}

# set learning parameters ( those are given in the lecture)
eta <- 0.01
epsilon <- 1e-3

# randomly initalize W and w0
#wth the following functions I assigned the initial weights to W and W0 and those will be change
# with time by the iterations.

set.seed(521)
W <- matrix(runif(ncol(trainTotal) * classNum, min = -0.01, max = +0.01), ncol(trainTotal), classNum)
w0 <- runif(classNum, min = -0.001, max = 0.001)

# learn W and w0 using gradient descent
# the error function is again from the book 10.45
iteration <- 1
obj_val <- c()
while (1) {
  Y_predicted <- sigmoid(trainTotal, W, w0)
  
  
  obj_val <-  c(obj_val, (1/2)*sum((R_t -Y_predicted )^2))
  
  W_past <- W
  
  w0_past <- w0
  
  W <- W - eta * gradient_W(trainTotal, R_t, Y_predicted)
  w0 <- w0 - eta * gradient_w0(R_t, Y_predicted)
  
  if (sqrt(sum((w0 - w0_past)^2) + sum((W - W_past)^2)) < epsilon) {
    break
  }
  
  iteration <- iteration +1
}
#print(W)
#print(w0)

# plot objective function during iterations
plot(1:iteration, obj_val,
     type = "l", lwd = 2, las = 1,
     xlab = "Iteration", ylab = "Error")

# calculate confusion matrix for the train data
y_predicted <- apply(Y_predicted, 1, which.max)
confusion_matrix <- table(y_predicted, Y_trainTotal)
print(confusion_matrix)


# calculate confusion matrix for the test data
y_predictedTest <- sigmoid (testTotal, W, w0)

y_predicted2 <- apply(y_predictedTest, 1, which.max)
confusion_matrix <- table(y_predicted2, Y_testTotal)
print(confusion_matrix)


