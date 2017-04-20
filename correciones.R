mediacontaminante <- function(directorio = "C:/Users/J. Ricardo Munguía/Documents/specdata", contaminante, dir){
#____________________________
    almacen <- c()
  di <- directorio
setwd(di)
#_____________________________

for(i in dir){
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

  x <- data.matrix(c)             # la convierto en matriz
  p <- x
  #__________________________________________
  
  if (contaminante == "sulfate"){
    almacen <- c(almacen,c$sulfate)
  } else if (contaminante == "nitrate"){
    almacen <- c(almacen,c$nitrate)
  } 
}
promedio <- mean(almacen, na.rm = T)
promedio

}
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
completos <- function(directorio ="C:/Users/J. Ricardo Munguía/Documents/specdata",id){
  almacen <- c()
  setwd(directorio)
  #______________________________________________-
  for(i in id){
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
    
    
    
    x <- data.matrix(c)             # la convierto en matriz
    x[is.na(x)] <- 0                # los NA los convierto en 0's
    conta <- 0
    for(g in 1:nrow(c)){
      if(x[g,2] > 0 ){
        conta<- conta + 1   
      }  
    }
    almacen <- c(almacen, conta)    # almacenó en una variable el elemento del vector que quiero
  }
  
  nobs <- almacen
  #__________________aclarar si con indices
  data.frame(id,nobs)
}
