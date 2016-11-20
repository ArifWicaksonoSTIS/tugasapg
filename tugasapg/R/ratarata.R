ratakita <- function(data,z){
  x <- sum(data[,z])/nrow(data)
  return(x)
}
