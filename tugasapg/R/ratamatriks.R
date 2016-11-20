ratamatriks<-function(data){
  rt<-matrix(ncol=ncol(data))
  for(i in 1:ncol(data)){
    rt[i]<-matrix(c(ratakita(data,i)))
  }
  return(rt)
}
