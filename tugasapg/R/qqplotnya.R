qqplotnya<-function(data){
  if(!is.matrix(data)){
    data2<-as.matrix(data)
    qqplotnya(data2)
  }
  else{

    #xbar
    rt<- t(ratamatriks(data))
    #inv.matrkov
    sinv=solve(matrikskov(data))
    #xj
    xj<-t(data)


    dj2=matrix(nrow = nrow(data),ncol = 1)
    for (j in 1:ncol(xj)){
      #xj-xbar
      xj[,j] = xj[,j]-rt

      #dj^2
      dj2[j] = t(xj[,j])%*%sinv%*%xj[,j]

    }
    #pengurutan nilai
    dj2s=as.matrix(sort(dj2[,1]))

    #mencari quartil
    qj<-matrix(nrow = nrow(data),ncol = 1)
    for (j in 1:nrow(data)){
      qj[j] = stats::qchisq((j-0.5)/nrow(data),df=ncol(data)-1)
    }

    #penggambaran plot
    graphics::plot(qj,dj2s)

  }
}
