#Question 1


```{r}
N <- 30000 # number of iterations
X <- rep(NA,N) # initialize the condition
k <- 4 # degree of freedom
for (i in 1:N)
{
  Y <- 10*runif(1) # uniform deviates from 0 to 10
  P <- runif(1) # uniform deviates from 0 to 1
  if (P<=Y/4*exp(-Y/2)) 	
    X[i]=Y #Accept Y if the random selected variable P is smaller or equal to the f(Y)
}
index <- which(X!="NA")
X <- X[index] # remove the rejcted value/remove the NA in X

length(X)/N # acceptance rate

#This is the theoretical value of pdf of Chi-square distribution with degree 4
x<-seq(0,10,by=0.5)#Set value of x with each gap 0.5
y<-x/4*exp(-x/2)#Calculate the corresponding theoretical value

hist(X,breaks=30,probability=TRUE)#plot the histogram with 30 bars
lines(x,y)#superimposing the theoretical value as a line on the histogram
```

```{r}
lr <- diff(X) #X is the observations from the above code
#Calculate the mean and the standard deviation using the function mean() and sd()
mlr <- mean(lr)
stdlr <- sd(lr)
#install.packages("moments")
library(moments)
JBstat <- rep(NA,10000)
for (i in 1:10000){
  lrth <- rnorm(length(lr),mean=mlr,sd=stdlr)
  JBstat[i] <- length(lr)*(skewness(lrth)^2+(kurtosis(lrth)-3)^2/4)/6
}
hist(JBstat, breaks=50, probability=TRUE)

F10 <- ecdf(JBstat)
summary(F10)

plot(F10, verticals = TRUE, do.points = FALSE)
length(lr)*(skewness(lr)^2+(kurtosis(lr)-3)^2/4)/6
```
###########################################################################################

#Question 2

```{r}
library(expm)
PMatrix1 <- matrix(rep(0,9),nrow=3,ncol=3)

PMatrix1[1,1] <- 0.5
PMatrix1[2,2] <- 0.3
PMatrix1[3,3] <- 0.5

PMatrix1[1,2] <- 0.5
PMatrix1[2,1] <- 0.35
PMatrix1[2,3] <- 0.35
PMatrix1[3,2] <- 0.5

PMatrix1
PMatrix1 %^% 2
PMatrix1 %^% 200
```


```{r}
# Consider 3 states: 0,1,2 in this simulations

freq <- rep(0,3) # initialize the frequency
# Set the initial conditions, it can be any kinds:(0,0), (0,1), (1,0) ,(1,1) and will not influence the final result
X1 <- rep(0,2) 

Niter <- 100000 # number of iterations
Kstates <- rep(0,Niter) # vector of states according to iterations

#According to my transition matrix, At state 0 and 2, the probability of remain unchanged is 0.5. At state 1, the probability of remain unchanged is 0.3.
for (i in 1:Niter) {
  # select one of the position from X1 and change the number on its position. In other words, change 1 to 0 or change 0 to 1.
  k1 <- sample(2, size = 1)
  #Two conditions for different remaining probabilities.
  if (sum(X1)==1)
    if (runif(1)<0.7){X1[k1] <- -X1[k1]+1} else {X1[k1] <-X1[k1]}
  else
    if (runif(1)<1/2){X1[k1] <- -X1[k1]+1} else {X1[k1] <-X1[k1]}
  # sum(X1) is the states of each iteration(after each moving) and put it into the corresponding position in Kstates, forming the state against each time of action
  Kstates[i] <- sum(X1)
}

# Compute the frequency of each state(0, 1, 2) appeared in the vector Kstates.
for (j in 1:3) {
  freq[j] <- length(which(Kstates==3-j))/Niter
}

# Comparisons: Frequency vs equilibrium probability

# Theoretical equilibrium distribution
pi <- c(7/24,5/12,7/24)
freq
pi
state = c(1:3)

plot(state,freq,xlab="state",ylab="Pr(state)")
lines(state,pi,type="h")
```

###########################################################################################

#Question 3
```{r}
#Number of iterations
n3 <- 10000 
#Number of N, meaning N Chi-Square Distribution sum up
N3 <- 1000
#Degree of freedom, same as the first question
k <- 4

# Initialize the condition
Chi1 <- rep(NA,n3)

# The idea of this loop is to generate the random numbers from 0 to infinity with probability under the Chi-Squared Distribution. Therefore, the first step is to randomly select a probability and then get the corresponding random number using the function 'qchisq'. Finally sum up all the numbers(N3 in total) and finish this single loop.
for (i in 1:n3) {
  prob3 <- runif(N3)
  x3 <- qchisq(prob3, df=4,lower.tail = TRUE)
  Chi1[i] <- sum(x3)
} 

#Histogram of the summation with 50 bars
hist(Chi1,50,probability='TRUE')

#Normalize the result using the fomula (Z-N*mean)/sqrt(N*Var)
Normal1 = (Chi1-N3*k)/sqrt(N3*k*2)
hist(Normal1,50,probability='TRUE')
#Create an interval from -4 to 4 by 0.1 gap. The interval can be any length but better to be symmetric. Finally, superimposing the Standard Normal Distribution curve, which is N(0,1) curve on the histogram.
x4 <- seq(-4, 4, 0.1)
lines(x4, dnorm(x4, 0, 1))

library(tseries)
jarque.bera.test(Normal1)
```