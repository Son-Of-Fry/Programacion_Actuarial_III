# NOTA!!! tener instalado la libreria RCurl y XML para los URL
Football <- function(busqueda){
  library("XML")
    library("RCurl")
  if(isTRUE(match(busqueda, c("AFC","NFC")) >0)){
    li <- busqueda
    a<-0
    b<-0
    link <- "http://www.espn.com/nfl/standings"
    urlass<- getURL(link)
    doc <- htmlTreeParse(urlass, useInternalNodes = T)
    liga <- xpathApply(doc,"//h2", xmlValue)
    liga <- liga[1:2]
    
    
    if(li == "AFC"){
      print(liga[1])
      divis <- 0:3
      
      division <- xpathApply(doc,"//th", xmlValue)
      datos <- xpathApply(doc,"//td", xmlValue)
      
      a<-1
      b<-13
      for(u in divis){
        x<- 1 + (52*u)
        y <- 13+ (52*u)
        A1<-rbind(division[a:b],datos[(x):(y)],datos[(x+13):(y+13)],datos[(x+26):(y+26)],datos[(x+39):(y+39)])
        a<-a+13
        b<-b+13
        colnames(A1) <- A1[1,]
        A1 <- data.frame(A1)
        abs<-0
        print(A1[2:5,])
        
      }                                
      
    }
    if(li == "NFC"){
      print(liga[2])
      divis <- 0:3
      
      division <- xpathApply(doc,"//th", xmlValue)
      datos <- xpathApply(doc,"//td", xmlValue)
      
      a<-53
      b<-65
      for(u in 0:3){
        x<- 209 + (52*u)
        y <- 221+ (52*u)
        A1<-rbind(division[a:b],datos[(x):(y)],datos[(x+13):(y+13)],datos[(x+26):(y+26)],datos[(x+39):(y+39)])
        a<-a+13
        b<-b+13
        colnames(A1) <- A1[1,]
        A1 <- data.frame(A1)
        abs<-0
        print(A1[2:5,])
      }                                
      
    }
    
    
  }
  
  {link <- "http://www.espn.com/nfl/standings"
    urlass<- link
    doc <- htmlTreeParse(urlass, useInternalNodes = T)}
  if(busqueda == "NFL"){
    li <- "NFL"
    a<-0
    b<-0
    print("National Football League")
    link <- "http://www.espn.com/nfl/standings"
    urlass<- getURL(link)
    doc <- htmlTreeParse(urlass, useInternalNodes = T)
    liga <- xpathApply(doc,"//h2", xmlValue)
    liga <- liga[1:2]
    
    
    print(liga[1])
    divis <- 0:3
    
    division <- xpathApply(doc,"//th", xmlValue)
    datos <- xpathApply(doc,"//td", xmlValue)
    
    a<-1
    b<-13
    for(u in divis){
      x<- 1 + (52*u)
      y <- 13+ (52*u)
      A1<-rbind(division[a:b],datos[(x):(y)],datos[(x+13):(y+13)],datos[(x+26):(y+26)],datos[(x+39):(y+39)])
      a<-a+13
      b<-b+13
      colnames(A1) <- A1[1,]
      A1 <- data.frame(A1)
      print(A1[2:5,])
      
    }                                
    
    
    print(liga[2])
    
    
    division <- xpathApply(doc,"//th", xmlValue)
    datos <- xpathApply(doc,"//td", xmlValue)
    
    a<-53
    b<-65
    for(u in divis){
      x<- 209 + (52*u)
      y <- 221+ (52*u)
      A1<-rbind(division[a:b],datos[(x):(y)],datos[(x+13):(y+13)],datos[(x+26):(y+26)],datos[(x+39):(y+39)])
      a<-a+13
      b<-b+13
      colnames(A1) <- A1[1,]
      A1 <- data.frame(A1)
      print(A1[2:5,])
      
    }                                
    
    
  }
  
  {division <- xpathApply(doc,"//th", xmlValue)
    division <- c(division[1], division[14], division[27], division[40],division[53],division[66],division[79],division[92])
    abs<- match(busqueda,division)}
  if(isTRUE(abs > 0)){ 
    div <- busqueda
    
    link <- "http://www.espn.com/nfl/standings"
    urlass<- getURL(link)
    doc <- htmlTreeParse(urlass, useInternalNodes = T)
    liga <- xpathApply(doc,"//h2", xmlValue)
    liga <- liga[1:2]
    division <- xpathApply(doc,"//th", xmlValue)
    division <- c(division[1], division[14], division[27], division[40],division[53],division[66],division[79],division[92])
    datos <- xpathApply(doc,"//td", xmlValue)
    codigo<- match(div,division)
    
    
    #Este AM
    equipo <- c()
    w <- c()
    L <- c()
    Ta <- c()
    w <- c()
    PCT <- c()
    Home<- c()
    Road <- c()
    Div<- c()
    conf<- c()
    PF<- c()
    PA<- c()
    Dife<- c()
    STRK<- c()
    
    
    if(codigo == 1){i<-1}
    if(codigo == 2){i<-53}
    if(codigo == 3){i<-105}
    if(codigo == 4){i<-157}
    if(codigo == 5){i<-209}
    if(codigo == 6){i<-261}
    if(codigo == 7){i<-313}
    if(codigo == 8){i<-365}
    
    for(i in seq(i,(i+39), by =13)) {
      equipo <- c(equipo, datos[i])
      w <- c(w,datos[i+1])
      L <- c(L,datos[i+2])
      Ta <- c(Ta,datos[i+3])
      PCT <- c(PCT, datos[i+4])
      Home<- c(Home, datos[i+5])
      Road <- c(Road, datos[i+6])
      Div<- c(Div, datos[i+7])
      conf<- c(conf, datos[i+8])
      PF<- c(PF, datos[i+9])
      PA<- c(PA, datos[i+10])
      Dife<- c(Dife, datos[i+11])
      STRK<- c(STRK, datos[i+12])
    }
    sasd<- cbind(equipo,w,L,Ta,PCT,Home,Road,Div,conf,PF,PA,Dife,STRK)
    
    xas<- data.frame(sasd)
    
    print(div)
    print(xas)
  }
  {equipos <- xpathApply(doc,"//abbr", xmlValue)
    equipos <-substr(equipos,nchar(equipos)-1,nchar(equipos))
    bus <-substr(busqueda,nchar(busqueda)-1,nchar(busqueda))
    wcs<- match(bus,equipos)}
  if(isTRUE(wcs > 0)){
    equ <- busqueda
    a<-0
    b<-0
    print("National Football League")
    link <- "http://www.espn.com/nfl/standings"
    urlass<- getURL(link)
    doc <- htmlTreeParse(urlass, useInternalNodes = T)
    liga <- xpathApply(doc,"//h2", xmlValue)
    liga <- liga[1:2]
    
    
    print(liga[1])
    divis <- 0:3
    
    division <- xpathApply(doc,"//th", xmlValue)
    datos <- xpathApply(doc,"//td", xmlValue)
    division2 <- c(division[1], division[14], division[27], division[40],division[53],division[66],division[79],division[92])
    
    blas <- xpathApply(doc,"//abbr", xmlValue)
    sdeb <-substr(blas,nchar(blas)-1,nchar(blas))
    d2 <-substr(equ,nchar(equ)-1,nchar(equ))
    wcs<- match(d2,sdeb)
    codigo <- wcs
    if(equ =="BUF"){codigo<-3}
    if(equ =="DEN"){codigo<-15}  
    if(equ =="DAL"){codigo<-17}
    ss<- division2[ceiling(codigo/4)]
    
    
    if(ceiling(codigo/4)==1){a<-1;b<-13}
    if(ceiling(codigo/4)==2){a<-14;b<-26}
    if(ceiling(codigo/4)==3){a<-27;b<-39}
    if(ceiling(codigo/4)==4){a<-40;b<-52}
    if(ceiling(codigo/4)==5){a<-53;b<-65}
    if(ceiling(codigo/4)==6){a<-66;b<-78}
    if(ceiling(codigo/4)==7){a<-79;b<-91}
    if(ceiling(codigo/4)==8){a<-92;b<-104}
    
    
    n<-codigo
    {x<-1+(n*13)-13;y<-13+(n*13)-13}
    
    if(isTRUE(match(n,seq(1,32,4))>0)){lugar <-"1° puesto de la division"}
    if(isTRUE(match(n,seq(2,32,4))>0)){lugar <-"2° puesto de la division"}
    if(isTRUE(match(n,seq(3,32,4))>0)){lugar <-"3° puesto de la division"}
    if(isTRUE(match(n,seq(4,32,4))>0)){lugar <-"4° puesto de la division"}
    
    A1<-rbind(division[a:b],datos[(x):(y)])
    colnames(A1) <- A1[1,]
    
    print(lugar)
    A1 <- data.frame(A1)
    print(A1[2,])
    
    
  }
  
}


#Ejemplos
Football("NFL") #da Toda la Liga
Football("AFC") #da la conferencia AFC o NFC
Football("NFC West") #da La division 
Football("BAL") #da el equipo con su posicion
