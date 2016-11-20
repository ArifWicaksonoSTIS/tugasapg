matrikskov<-function(data){
  if(!is.matrix(data)){
    data2<-as.matrix(data)
    matrikskov(data2)
  }
  else{

    #Xbar
    rt<- matrix(data=ratamatriks(data),nrow=10,ncol=3,byrow = TRUE)

    #X jadi X-Xbar
    data1 = data-rt

    #tranpose X'
    data3 = t(data1)

    #Perkalian matriks SS=X'X dan kov=SS/nrow(data)-1
    data4 = (data3 %*% data1)/(nrow(data1)-1)

    return(data4)
  }
}
