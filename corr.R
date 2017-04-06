corr <- function(directorio ="C:/Users/J. Ricardo Munguía/Documents/specdata",horizonte = 0){
  correlacion <- c()
  cont <- 0
  setwd(directorio)
  #______________________________________________-
  for(i in 1:332){
    if(nchar(i)==1){                # un solo digito
      c <- read.csv(paste0("00",i,".csv"))
    }
    if(nchar(i)==2){                  # dos digitos
      gato <- paste0("0",i)
      c <- read.csv(paste0("0",i,".csv"))
    }
    if(nchar(i)==3){                  # tres digitos
      gato <- i
      c <- read.csv(paste0(i,".csv"))
    }
    
    
    
    x <- data.matrix(c)               #la convierto en matrix
    com <- x[complete.cases(x),]      # "despejo"casos completos, uso complete.cases en lugar de la funcion anterior :v
    n <- nrow(com)                    # cuento los casos
    if (n>horizonte){               
      correlacion <- c(correlacion, cor(com[,2],com[,3]))
    }
  }
  correlacion 
}
