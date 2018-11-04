library(pdist)

iris<-read.csv("iris.data", header=FALSE)
irisnew<-iris[3:4]
colnames(irisnew)<-c("x","y")

chooseInitialCentroids<-function(dataset,k){
  init<-dataset[sample(nrow(dataset), k),]
  return(init)
}


K_Means<-function(dataset, newmatrix, k){
  #Select Random rows for clusters
  oldCentroids<-newmatrix
  initialDist<-pdist(dataset, newmatrix)
  initialDist<-as.matrix(initialDist)
  classified<-classify(dataset, initialDist)
  # Find mean based on clusters to get centroids
  newCentroids<-NULL
  for(i in 1:k){
    newCentroids<-append(newCentroids, (cbind(with(classified, mean(x[class==i])), with(classified, mean(y[class==i])))))
  }
  # matrix of new centroids found by taking mean
  output <- matrix(unlist(newCentroids), nrow =  k, byrow = TRUE)
  if(!is.null(oldCentroids) & all(oldCentroids==output)){
    return(output)
  }
  return(K_Means(dataset, output, k))
}

#Func to find lowest and classify the dataset
classify<-function(dataset, distancemat){
  lowest<-apply(distancemat, 1, function(x) which(x==min(x)))
  dataset$class<-lowest
  return(dataset)
}
k=3
means<-K_Means(irisnew, chooseInitialCentroids(irisnew, k), k)
stored<-classify(irisnew, as.matrix(pdist(irisnew, means)))
