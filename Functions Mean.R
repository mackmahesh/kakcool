findc <- function(y, remover = TRUE)
{
  nc <- ncol(y)
  for(i in 1:nc)
    {
    nc[i] <- mean(y[,i],na.rm = remover)
    
    }
  print(nc)
}args