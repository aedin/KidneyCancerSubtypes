
clearCode34<-function(dataDir=sigModelPath) {  
  library(pamr)
  #data(xn)
  #data(classes)
  load(file.path(dataDir,"data/xn.rda"))
  load(file.path(dataDir,"data/classes.rda"))
  
  trainSet <- list(x=scale(xn), y=classes, geneid=rownames(xn), genenames=rownames(xn))
  boxplot(xn)
  mytrain <-pamr.train(trainSet)
  class(mytrain)<-c(class(mytrain), "clearCode34")
  return(mytrain)
}

logCenter<-function(df, xMedian=NULL){
  #log2 and median center ( normalized data)
  df<-log2(df+1)
  if (is.null(xMedian)) xMedian=apply(df,1, median)
  if (length(xMedian)!=nrow(df)) stop("scaling factor xMedian incorrect length")
  df <-sweep(df,1, xMedian, FUN="-")
  return(df)
}

predict.clearCode34<-function(model=clearCode34(), newdata, logcenter=TRUE, xMedian=NULL, boxplot=FALSE){
  if(logcenter) newdata<-logCenter(newdata, xMedian)
  if (boxplot) boxplot(newdata)
  pred<-pamr.predict(model,scale(newdata),0)
  posteriors<-pamr.predict(model,scale(newdata),0,type="posterior")
  names(pred)= rownames(posteriors)
  res<-cbind(data.frame(posteriors), prediction=as.character(pred))
  
  return(res)
}

