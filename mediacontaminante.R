mediacontaminante <- function(directorio = "C:/Users/J. Ricardo Munguía/Documents/specdata", contaminante, dir){
#____________________________
    almacen <- 0
  di <- directorio
setwd(di)
#_____________________________
  if(contaminante == "sulfate"){
  co <- 2
  }
  
  if(contaminante == "nitrate"){
  co <- 3
  }
#_________________________________

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
  x[is.na(x)] <- 0                # los NA los convierto en 0's
  p <-colMeans(x)                  # sumo los valores de las columnas para crear un vector
  almacen <- almacen + p[co]      # almacenó en una variable el elemento del vector que quiero
}

almacen
}
