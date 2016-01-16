#A general pattern of a metaheuristic method
#(C)Jaroslaw Arabas, ALHE, 2012
#To define the METHOD completely the user must 
#code the procedures of selection, model update, and variation.
#Proper execution of the metaheuristic method needs 
#a list of start points, an evaluation method
#an initialization procedure, and a termination condition

############################################################




#### TO BE DEFINED BY THE USER

#selection of a LIST of points from the history
#to be defined
selection<-function(history, model)
{
  #select a number of points from the history using the 
  #method's parameters and the current state of the model
  curr<-history[[length(history)]]
  
  if ((model$best$quality < curr$quality) || (runif(1, 0, 1) < accept(model, curr)))
    selectedPoints = curr
  else
    selectedPoints = model$best
  return(selectedPoints)
}

#update of a model based on a LIST of points
#to be defined
modelUpdate<-function(selectedPoints, oldModel)
{
  #take a look at the list of selectedPoints and 
  #on the current state of the model, update it 
  #and then return
  newModel = list(best = selectedPoints, temp = processTemp(oldModel), tempStart = oldModel$tempStart, step = oldModel$step + 1)
  return (newModel)
}

#generation of a LIST of new points
#to be defined
variation<-function(selectedPoints, model)
{
  #generate the list of newPoints and then  
  newPoints<-list()
  newPoints[[1]]<-list(coordinates = rnorm(length(model$best$coordinates), model$best$coordinates, 1))
  return (newPoints)
}

#####  THE METAHEURISTIC "ENGINE"

#An aggregated operator takes the list of historical points anf the model
#and generates the list of new points
#A "side effect" is the model update
aggregatedOperator<-function(history, oldModel)
{
  
  selectedPoints<-selection(history, oldModel)
  newModel<-modelUpdate(selectedPoints, oldModel)
  newPoints<-variation(selectedPoints, newModel)
  return (list(newPoints=newPoints,newModel=newModel))
}

#The main loop of a metaheuristic.
#The user must define a LIST of start points,
#a termination condition, an initialization procedure
#and an evaluation procedure.
#The result is the history of the run
metaheuristicRun<-function(initialization, startPoints, termination, evaluation)
{
  history<-initialization(startPoints)
  history<-evaluateList(history, evaluation)
  model<-initModel(history)
  while (!termination(history,model))
  {
    aa<-aggregatedOperator(history, model)
    aa$newPoints<-evaluateList(aa$newPoints, evaluation)
    history<-historyPush(history,aa$newPoints)
    model<-aa$newModel
  }
  #return(c(model$step, model$best))
  #write.csv(history, file="test.csv")
  
  
  #write.csv(df, 'csv.csv')
  return (list (history = history, best = model$best))
}

#push a LIST of points into the history
historyPush<-function(oldHistory, newPoints)
{
  newHistory<-c(oldHistory,newPoints)
  return (newHistory)
}
#read a LIST of points pushed recently into the history
historyPop<-function(history, number)
{
  stop=length(history)
  start=max(stop-number+1,1)
  return(history[start:stop])
}

#evaluate a LIST of points
evaluateList<-function(points,evaluation)
{
  for (i in 1:length(points)) {
    points[[i]]$quality<-evaluation(points[[i]]$coordinates)
  }
  return (points) 
}

initialization<-function(startPoints) {
  points<-list()
  for (i in 1:length(startPoints))
    points[[i]]<-list(coordinates = startPoints[[i]])
  return (points)
}

initModel<-function(startPoints) {
  model<-list(best = startPoints[[which.max(lapply(startPoints, function(x) x$quality))]], tempStart=10^10, step = 1)
  model$temp<-model$tempStart
  return (model)
}

termination<-function(history,model) {
  return (model$temp < 10^-100)
}

#####################################################################
# Parameter function
#####################################################################

accept<- function (model, curr) {
  return (exp(-(model$best$quality - curr$quality) / model$temp))
}

processTemp<-function(model) {
 return (model$tempStart * (0.9^model$step))
  #return (model$tempStart / log(model$step))
  }

#####################################################################
# Functions to optimize
#####################################################################

square<-function(coordinates) {
  return (-coordinates[[1]] * coordinates[[1]] + 10)
}

himmel<-function(coordinates) {
  x <- coordinates[[1]]
  y <- coordinates[[2]]
  wynik<-((x^2+y-11)^2 + (x+y^2-7)^2)
  return(-wynik)
}

shekel<-function(coordinates) {
  xx <- coordinates
  m  <- 10
  b <- 0.1 * c(1, 2, 2, 4, 4, 6, 3, 7, 5, 5)
  C <- c(4.0, 1.0, 8.0, 6.0, 3.0, 2.0, 5.0, 8.0, 6.0, 7.0,
         4.0, 1.0, 8.0, 6.0, 7.0, 9.0, 3.0, 1.0, 2.0, 3.0,
         4.0, 1.0, 8.0, 6.0, 3.0, 2.0, 5.0, 8.0, 6.0, 7.0,
         4.0, 1.0, 8.0, 6.0, 7.0, 9.0, 3.0, 1.0, 2.0, 3.0)
  C <- matrix(C, 4, 10, byrow=TRUE)
  Ct <- t(C)
  
  xxmat <- matrix(rep(xx,times=m), m, 4, byrow=TRUE)
  inner <- rowSums((xxmat-Ct[,1:4])^2)
  
  outer <- sum(1/(inner+b))
  
  y <- outer
  return(y)
}

######################################################################
# start()
######################################################################
chart<-function(){
  returnVal<-metaheuristicRun(initialization, list(c(0,0,89,100)), termination, himmel)
  history <- returnVal$history
  best<-returnVal$best
  
  coordinates1 <- unlist(lapply(history, function(x) x$coordinates[[1]]))
  coordinates2 <- unlist(lapply(history, function(x) x$coordinates[[2]]))
  #quality <- unlist(lapply(history, function(x) x$quality))
  df <- data.frame(coordinates1, coordinates2)
  library(plotly)
  p<-plot_ly(df, x=coordinates1, y=coordinates2, name="Himmel", mode="markers", marker=list(color="red"))
  add_trace(x=best$coordinates[[1]], y=best$coordinates[[2]], name="Best", marker = list(color="blue"))
  add_trace(x=history[[1]]$coordinates[[1]], y=history[[1]]$coordinates[[2]], name="First", marker = list(color="orange"))
}

best<-function() {
  bestList<-list()
  for (k in 1:20) {
    message(sprintf("coto"))
    returnVal<-metaheuristicRun(initialization, list(c(-1000,5,2,1)), termination, shekel)
    bestList[[length(bestList) + 1]] <- returnVal$best
  }
  
  x <- unlist(lapply(bestList, function(x) x$coordinates[[1]]))
  y <- unlist(lapply(bestList, function(x) x$coordinates[[2]]))
  z <- unlist(lapply(bestList, function(x) x$coordinates[[3]]))
  s <- unlist(lapply(bestList, function(x) x$coordinates[[4]]))
  value <- unlist(lapply(bestList, function(x) x$quality))
  df<-data.frame(x,y,z,s,value)
  write.xlsx(df, file = "shekel3.xlsx", sheetName = "temp10e3")
}

metaheuristicRun(initialization, list(c(4,5,2,1)), termination, shekel)
####  THAT'S ALL FOLKS