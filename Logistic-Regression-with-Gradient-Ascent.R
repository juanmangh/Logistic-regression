#Creation of random variables following a normal distribution.
set.seed(12345)
X0 <- rnorm(1000,0.5,1.2)
X1 <- rnorm(1000,1.1,1.2)
summary(X0)
summary(X1)

plot(density(X0),main="Comparison of Density of Samples")
lines(density(X1))

#Initial Beta values
B0 <- 0.3
B1 <- 1.7

#logit(P) Slide 15
logitP0 <- B0 + B1*X0
logitP1 <- B0 + B1*X1

#Slide 14, P=f(logit(P))=1(1+e^-logit(P))
P0 = 1/(1+exp(-logitP0))
P1= 1/(1+exp(-logitP1))
summary(P0)
summary(P1)
#xaxiz- po p1 -----------------------------------------
boxplot(P0,P1,main="Box plot of Probailities")

#Assigning binary values to Y0 and Y1.
Y0 <- ifelse(runif(1,0,1) <= P0, 1, 0)
Y1 <- ifelse(runif(1,0,1) <= P1, 1, 0)
plot(X0,Y0, main="Binary Responses of X0 - Random")
plot(X1,Y1, main="Binary Responses of X1 - Random")

#Assigning binary values to Y0 and Y1.
Y0Mean <- ifelse(P0 > mean(P0), 1, 0)
Y1Mean <- ifelse(P1 > mean(P1), 1, 0)
plot(X0,Y0Mean, main="Binary Responses of X0 - Mean")
plot(X1,Y1Mean, main="Binary Responses of X1 - Mean")

#Assigning binary values to Y0 and Y1.
Y0half <- ifelse(P0 > 0.5, 1, 0)
Y1half <- ifelse(P1 > 0.5, 1, 0)
plot(X0,Y0half, main="Binary Responses of X0 - Half")
plot(X1,Y1half, main="Binary Responses of X1 - Half")


#Merging the data for following points.
mergedX <- c(X0,X1)
mergedP <- c(P0,P1)
mergedY <- c(Y0,Y1)
plot(mergedX,mergedY,main="Merged Binary Respones -Random")
mergedYMean <- c(Y0Mean,Y1Mean)
mergedYHalf <- c(Y0half,Y1half)
plot(mergedX,mergedYMean,main="Merged Binary Respones - Mean")
plot(mergedX,mergedYHalf,main="Merged Binary Respones - Half")


#Logisitc Regression Model
listCost <- c() #store the costs to verify the convergence of the function
listNormGradient <- c()
gamma <-0.001 #Depends on random sample, number of random samples and the computation power
iteration <- 0 #variable to store number of iterations
oldCost <- 0 #variable to store the previous cost
currentCost <- 0 #variable to store the current cost
#LOOP - until the current cost converges (current cost is lower or equal than the old cost)
while(abs(oldCost)>=abs(currentCost)){
  Phat <- 1/(1+exp(-B0 -B1*mergedX))#slide 29
  #Gradient is calcualte derivating the cost function
  gradientB0 <- sum(mergedY-Phat)
  gradientB1 <- sum(mergedX*(mergedY-Phat))
  #----------------------------
  #As mentioned in the assignment, gamma is tuned based on gradient divided by its norm
  normGradient <- sqrt(gradientB0^2 + gradientB1^2)
  normGradientB0 <- gradientB0/normGradient
  normGradientB1 <- gradientB1/normGradient
  #------------------------------
  #Estimation of B0 and B1 according to gamma factor
  B0 <- B0 + gamma*normGradientB0
  B1 <- B1 + gamma*normGradientB1
  #----------------------------
  #computation of Cost - slide 28
  cost <- sum(mergedY*(B0 +B1*mergedX))-sum(log(1+exp(B0 +B1*mergedX)))
  #---------------------------
  #code for detecting the convergence
  listCost <- c(listCost,cost)
  listNormGradient <- c( listNormGradient,normGradient)
  if(iteration==0){
    currentCost <- cost
    oldCost <- cost
  }
  else{
    oldCost <- currentCost
    currentCost <- cost
  }
  iteration <- iteration+1
}
print(B0)
print(B1)
print(iteration)
plot(listCost,main="Cost as a function of the iterations")
plot(listNormGradient,main="Norm of the Gradient as a function of the iterations")

#Plot of our Logistic Regression Model
t=seq(-10,10,0.01)
y=1/(1+exp(-B0-B1*t))
plot(mergedX,mergedY)
lines(t,y,type="l", xlab="x", ylab="y",col="red")

#Classifier
classifier <- function(t){
  val <- 1/(1+exp(-B0-B1*t))
  
  if((val) > 0.5){
    return (1)
  }
  else{
    return (0)
  }
}
predictedValues <- lapply(mergedX,classifier)
count <- 0
for(i in 1:2000){
  if(mergedY[i]!=predictedValues[i]){
    count <- count+1
  }
}
#Count of Misclassified samples
print(count)

#BAYES
#P(Y=1|sample=0) = P(sample=0|Y=1)*P(Y=1)/P(sample=0)
#P(Y=1|sample=1) = P(sample=1|Y=1)*P(Y=1)/P(sample=1)
tableY <- table(mergedY)
#P(Y=1) = number of 1 responses /  total number of responses
ProbY1 <- tableY[names(tableY)==1]/length(mergedY)
#code to get data for below steps
tablem0Y <- table(mergedY[1:1000])
tablem1Y <- table(mergedY[1001:2000])
#P(sample=0|Y=1) = number of 1 responses in first 1000 samples (X0) / total number of 1 responses in all the samples
Probm0y1 <- tablem0Y[names(tablem0Y)==1]/tableY[names(tableY)==1]
#P(sample=1|Y=1) = number of 1 responses in next 1000 samples (X1) / total number of 1 responses in all the samples
Probm1y1 <- tablem1Y[names(tablem1Y)==1]/tableY[names(tableY)==1]
#Prm0(Y=1)
PY1S0 <- Probm0y1*ProbY1/0.5
#Prm1(Y=1)
PY1S1 <- Probm1y1*ProbY1/0.5
print(PY1S0)
print(PY1S1)

