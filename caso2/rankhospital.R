rankhospital <- function(estado, resultado, num) {
  x <- vector("numeric")
  y <- vector("numeric")
  
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


#extractor de datos
{    xl <- 0
    for (i in 1:nf) {
      if (datos[i,7] == estado) {
        xl <- length(x) + 1
        length(x) <- xl
        length(y) <- xl
        x[xl] <- datos[i,2]
        y[xl] <- datos[i,co]
      }
    }
    
  y<-suppressWarnings(as.numeric(y))    
  a<- y[1:length(x)]
  rar<-data.frame(cbind(x,a))
  a<-as.numeric(matrix(rar[,2]))
  rar[,2] <- suppressWarnings(a)
  cuak <- rar[order(rar[,2], rar[,1], na.last=NA),]
  
  if(num == "mejor"){num<- 1}
  if(num == "peor"){num <- nrow(cuak)}
  
  sa<-cuak[num,1]
  as.character(sa)
}
}
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
rankcompletos <- function(resultado="ataque", num) {
  
  datos <- read.csv("outcome-of-care-measures.csv",colClasses = "character")
  vaca <- datos[,7]
  nom <-levels(factor(vaca))
  bingo <- length(levels(factor(vaca)))
  hospital <- c()
  
  for(lex in 1:(bingo)){
    estado <- nom[lex]
    
    x <- vector("numeric")
    y <- vector("numeric")
    #checador1
    {
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
    #extractor de datos
    {    xl <- 0
      for (i in 1:nf) {
        if (datos[i,7] == estado) {
          xl <- length(x) + 1
          length(x) <- xl
          length(y) <- xl
          x[xl] <- datos[i,2]
          y[xl] <- datos[i,co]
        }
      }
      
      y<-suppressWarnings(as.numeric(y))    
      a<- y[1:length(x)]
      rar<-data.frame(cbind(x,a))
      a<-as.numeric(matrix(rar[,2]))
      rar[,2] <- suppressWarnings(a)
      cuak <- rar[order(rar[,2], rar[,1], na.last=NA),]
      if(num == "mejor"){num<- 1}
      sa<-cuak[num,1]
      if(num == "peor"){  sa<-cuak[nrow(cuak),1]}
      
      hospital <- c(hospital,as.character(sa))
    }
    
    
  }
  state <- nom
  data.frame(hospital,state)
}
