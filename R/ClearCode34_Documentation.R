


clearCode34<-function()
  {  library(pamr)
  data(xn)
  data(classes)
  data(test)
  trainSet <- list(x=scale(xn), y=classes, geneid=rownames(xn), genenames=rownames(xn))
  mytrain <-pamr.train(trainSet)
}

predict.clearCode34<-function(){
  sum(rownames(trainSet$x)==rownames(test))
  #boxplot(scale(data.frame(trainSet$x,test)))
  pamr.predict(mytrain,scale(test),0)
  table(pamr.predict(mytrain,scale(test),0))
  pamr.predict(mytrain,scale(test),0,type="posterior")
}


