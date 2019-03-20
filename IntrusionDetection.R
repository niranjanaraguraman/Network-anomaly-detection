                    #Om Muruga
#Intrusion Anomaly Detection by cascading k-means and c4.5 decision tree algorithm

#to calculate distance between two datapoints a,b
distance <- function(a,b)
{
  n <- ncol(a)
  s <- 0
  for(i in 1:(n-1)) 
  {
    s <- s + ((a[1,i]-b[1,i])**2)
  }
  return(s**0.5)
}

#to search if k is in a 
searche <- function(k,a)
{
  n <- length(a)
  for(i in 1:n) 
  {
    if(a[i] == k)
    {
      return(TRUE)
    }  
  }
  return(FALSE)
}

intrusionDetection<- function(k,data)  
{
  count=0 #to calculate accuracy
  n <- nrow(data)
  c <- ncol(data)
  test <- data[round(2*n/3):n,] #test set
  train <- data[1:round(n*(2/3)),]  #training set
  data <- data[1:round(n*(2/3)),1:40] #training set without class labels(for clustering)
  n <-nrow(data)
  c <- ncol(data)
  centroids <- matrix(0,k,c)
  
  ic <- array(0,k)
  for(i in 1:k) 
  {
    r <- sample(1:n,1)                        #selection of centroids
    while(searche(r,ic) == TRUE) 
    {
      r <- sample(1:n,1)
    }
    ic[i] <- r
    if(i>1)
    {
      #initial random centroids
      centroids <- rbind(centroids[1:(i-1),],data[r,])
    }
    else 
    {
      centroids <- rbind(data[r,])
      
    }
  }
  cluster <- array(0,n)

  #adding a new column cluster to the data
  data <- cbind(data,cluster)
  t <- 0
  while(TRUE) 
  {
    t <- t+1  #no of iterations
    print(t)
    for(i in 1:n) 
    {
      dc <- array(0,k)
      #print(dc)
      for(j in 1:k)
      {
        dc[j] <- distance(data[i,],centroids[j,])
      }
      #cat("dc :",dc)
      #cat("\nn: ",i)
      data[i,ncol(data)] <- which.min(dc)
    }
    
    cnt <- 0
    
    for(i in 1:k)
    {
      temp <- data[data$cluster == i,]
      for(j in 1:c) 
      {
        m <- mean(temp[,j])
        if(m == centroids[i,j])
        {
          cnt <- cnt+1
        }
        centroids[i,j] <- m
      }
    }
    if(cnt >= (k*c)-c)  #loop stopping criteria
    {
      break
    }
  }
  cat("t: ",t)
  for(i in 1:k) 
  {
    #cat("Cluster ",i,":\n")
    #print(data[data$cluster == i,1:c])
  }
  
  ############# Test data classification using c50
  ## first finding its cluster,then find decision tree within that cluster
  for(i in 1:nrow(test)) 
  {
    dc <- array(0,k)
    print(dc)
    for(j in 1:k)
    {
      dc[j] <- distance(test[i,],centroids[j,])
    }
    min_val=which.min(dc)

    class <- c(train[,41])
    data1 <- cbind(data,class)
    data1 <- data1[data1$cluster == k,]
#        print(data[,])
    class <- data1[,42]
    data1 <- cbind(data1[,1:40],class)
    data1$class <- as.factor(data1$class)
    str(data1$class)
    library("C50")
    treeModel <- C5.0(x = data1[,-41], y = data1$class) #appyling c50 algorithm
    plot(treeModel)
    c <- predict.C5.0(treeModel,test[i,1:40],type = "class")
    cat("predicted: ",c," Actual:  ",test[i,41],"\n")
    
    if(c==test[i,41]) #for calculating accuracy
    {
      count=count+1
    }
  }
  acc=(count/nrow(data))*100
  cat("Accuracy:",acc)
  
  return(data)
}

 
data <- read.csv(file.choose())
data <- data[,1:41]
k <- 3
data <- intrusionDetection(3,data)
