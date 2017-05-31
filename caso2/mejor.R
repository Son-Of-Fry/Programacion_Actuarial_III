mejor <- function(estado="AL",resultado="falla"){
  datos <- read.csv("outcome-of-care-measures.csv",colClasses = "character")
  #checador1
  {
  edo <- FALSE
  if(isTRUE(match(estado,datos$State)>0)){edo <- TRUE}
  if(edo== FALSE){stop("Estado inválido")}
  nf <-nrow(datos)
  co<-0
}
  #checador2
  {
  if(resultado== "ataque"){co <- 11}
  if (resultado== "falla"){co <- 17}
  if (resultado== "neumonia"){co <- 23}
  if (co <1){stop("resultado inválido")}
  }
  #codigo 
  {
  datos[,co] <- suppressWarnings(as.numeric(matrix(datos[,co],nf,1)))  #los convierto en NA desde una matriz de nf filas por 1 columna
   b <- datos[grep(estado,datos$State),]        #relaciono y extraigo una matrix de los X numero de hospitales en cada fila por Y numero de columas de los datos originales
   cuak <- b[order(b[,co], b[,2], na.last=NA),] #extraigo una matrix y la reordeno deacuerdo a las muertes(columna r) y su nombre del hospital la muevo a la columna 2
  }
   cuak[1,2] # pido el primer lugar y su dato derecho, el nombre
  
   }


