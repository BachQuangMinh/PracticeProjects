MAPE<-function(actual,predict){
  mape=mean(abs((actual-predict)/actual))
  return(mape)
}