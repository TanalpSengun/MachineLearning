library(MASS)

#importing the dataset
dataX = read.csv("hw01_data_set_images.csv", header = FALSE)
trueLabel = read.csv("hw01_data_set_labels.csv", header = FALSE)

#taking the csv file and make it a matrix 
matrixData =as.matrix(dataX)
priorProb =as.matrix(trueLabel)

#prior probability
priorA = length(trueLabel[trueLabel=="A"]) 
priorB = length(trueLabel[trueLabel=="B"])
priorC = length(trueLabel[trueLabel=="C"])
priorD = length(trueLabel[trueLabel=="D"])
priorE = length(trueLabel[trueLabel=="E"])

#seperating the train and test data for every class

trainA = matrixData[1:25,]
testA = matrixData[26:39,]

trainB = matrixData[40:64,]
testB = matrixData[65:78,]

trainC = matrixData[79:103,]
testC= matrixData[104:117,]

trainD = matrixData[118:142,]
testD = matrixData[143:156,]

trainE = matrixData[157:181,]
testE = matrixData[182:195,]

#to be sure the view of the results

View(dataSet)
matrixData[1,]


#calculating the pcds 

pcdA=colSums(trainA, 25)/25
pcdB=colSums(trainB, 25)/25
pcdC=colSums(trainC, 25)/25
pcdD=colSums(trainD, 25)/25
pcdE=colSums(trainE, 25)/25



#for the logarithym problem define my own log


safelog <- function(x) {
  return (log(x + 1e-100))
}


#the first one (a) is the matrix , the second one (b) is the pcd ,the third one (c) is prior probability (class of "A")

scoreFunc <- function(a,b,c) 
{
  return(sum(a * safelog(b) + (1 - a) * safelog(1-b))+log(c)) 
}


#to decide which one has the most fitting one
#decider looking at each scores

scoreFuncDecider <- function(a) 
{
  a1 = scoreFunc(a,pcdA,priorA)
  b1 = scoreFunc(a,pcdB,priorB)
  c1 = scoreFunc(a,pcdC,priorC)
  d1 = scoreFunc(a,pcdD,priorD)
  e1 = scoreFunc(a,pcdE,priorE)
  
  endResult=""
  maxInt = toString(max(a1,b1,c1,d1,e1))
  if(maxInt==a1) {endResult="A"}
  else if(maxInt==b1) {endResult="B"}
  else if(maxInt==c1) {endResult="C"}
  else if(maxInt==d1) {endResult="D"}
  else if(maxInt==e1) {endResult="E"}
  
  return(endResult)
 
}

#initialize the number of each class

numA=0;
numB=0;
numC=0;
numD=0;
numE=0;


y_predict=0
#for the prediction

printResults  <- function(a,c){ 
for (i in 1:c) {
  #print(scoreFuncDecider(a[i,]))
  if(scoreFuncDecider(a[i,])=="A") {numA=numA+1}
  if(scoreFuncDecider(a[i,])=="B") {numB=numB+1}
  if(scoreFuncDecider(a[i,])=="C") {numC=numC+1}
  if(scoreFuncDecider(a[i,])=="D") {numD=numD+1}
  if(scoreFuncDecider(a[i,])=="E") {numE=numE+1}
  
}

  y_predict= cbind(numA,numB,numC,numD,numE)
  return(y_predict)
}

y_TRAIN = cbind(printResults(trainA,25),printResults(trainB,25),
                   printResults(trainC,25),printResults(trainD,25),printResults(trainE,25))
dim(y_TRAIN)<-c(5,5)

y_TEST = cbind(printResults(testA,14),printResults(testB,14),
               printResults(testC,14),printResults(testD,14),printResults(testE,14))

dim(y_TEST)<-c(5,5)
print(y_TRAIN)
print(y_TEST)







