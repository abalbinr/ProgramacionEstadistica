crearMalla = function(n){
  x = seq(0,n,1)
  y = x 
  
  malla = expand.grid(x,y) 
  malla = expand.grid(colx = x, coly = y) 
  
  x11()
  plot(malla,asp = 1)

}

crearDfMalla = function(n){
  
  X0 = vector(mode = "numeric", length = n*n)
  X1 = vector(mode = "numeric", length = n*n)
  Y0 = vector(mode = "numeric", length = n*n)
  Y1 = vector(mode = "numeric", length = n*n)
  
  izq = vector(mode = "numeric", length = n*n)
  aba = vector(mode = "numeric", length = n*n)
  der = vector(mode = "numeric", length = n*n)
  arr = vector(mode = "numeric", length = n*n)
  
  Vizq = vector(mode = "numeric", length = n*n)
  Vaba = vector(mode = "numeric", length = n*n)
  Vder = vector(mode = "numeric", length = n*n)
  Varr = vector(mode = "numeric", length = n*n)
  
  total = vector(mode = "numeric", length = n*n)
  
  df = data.frame(X0,X1,Y0,Y1,izq,aba,der,arr,Vizq,Vaba,Vder,Varr,total)
  
  #acá ingresamos las coordenadas de todos los cuadrados de la malla
  cuadrado = 1
  for (i in 1:n) { #columnas
    for (j in 1:n) { #cuadros en columna i
      df[cuadrado,]$X0 = i-1
      df[cuadrado,]$X1 = i
      df[cuadrado,]$Y0 = j-1
      df[cuadrado,]$Y1 = j
      
      cuadrado = cuadrado + 1
    }
  }
  
  #acá ingresamos los vecinos de todos los cuadrados de la malla
  ##acá creamos el marco porque son especiales
  marco = vector(mode = "numeric",length = 4*n - 4)
  
  indice = 1
  #para lado izquierdo
  for (i in 1:n) {
    marco[indice] = i
    indice = indice + 1
  }
  
  #para lado arriba
  col = 1
  while( col <= n ){
    i = col*n
    if (!(is.element(el = i,set = marco))) {
      marco[indice] = i
      indice = indice + 1
    }
    col = col + 1
  }
  
  #para lado derecho
  for (i in ((n*n):((n-1)*n+1))) {
    if (!(is.element(el = i,set = marco))){
      marco[indice] = i
      indice = indice + 1
    }
  }
  
  #para lado abajo
  col = n
  while( col > 0 ){
    i = (col-1)*n+1
    if (!(is.element(el = i,set = marco))) {
      marco[indice] = i
      indice = indice + 1
    }
    col = col - 1
  }
  
  
  ##Hasta acá se llena el marco

  for (i in 1:(n*n)) {
    if (is.element(el = i, set = marco)) {
      
      #vecino izquierdo
      vecino = i-n
      if (vecino <= 0) {
        df[i,]$Vizq = 0
      }else{
        df[i,]$Vizq = vecino
      }   
      #vecino abajo
      vecino = i-1
      if (vecino %% n == 0) {
        df[i,]$Vaba = 0
      }else{
        df[i,]$Vaba = vecino
      }
      #vecino derecha
      vecino = i+n
      if (vecino > n*n) {
        df[i,]$Vder = 0
      }else{
        df[i,]$Vder = vecino
      }
      #vecino arriba
      vecino = i+1
      if (i %% n == 0) {
        df[i,]$Varr = 0
      }else{
        df[i,]$Varr = vecino
      }
    }else{
      df[i,]$Vizq = i-n
      df[i,]$Vaba = i-1
      df[i,]$Vder = i+n
      df[i,]$Varr = i+1
    }
  }
  #Hasta acá se llenan los vecinos
  
  
  return(df)
  
}

#lado: izq, aba, der, arr.
#dfAnt: data.frame viejo
actualizarDf = function(dfAnt, cuadrado, lado){
  dfAnt[lado][cuadrado,] = 1
  dfAnt[cuadrado,]$total = dfAnt[cuadrado,]$izq + dfAnt[cuadrado,]$aba + dfAnt[cuadrado,]$der + dfAnt[cuadrado,]$arr
  if (lado == "izq") {
    vecino = dfAnt[cuadrado,]$Vizq
    if (vecino != 0) {
      dfAnt[vecino,]$der = 1
    }  
  }else if (lado == "aba") {
    vecino = dfAnt[cuadrado,]$Vaba
    if (vecino != 0) {
      dfAnt[vecino,]$arr = 1
    }
    
  }else if (lado == "der") {
    vecino = dfAnt[cuadrado,]$Vder
    if (vecino != 0) {
      dfAnt[vecino,]$izq = 1
    }
    
  }else{
    vecino = dfAnt[cuadrado,]$Varr
    if (vecino != 0) {
      dfAnt[vecino,]$aba = 1
    }
    
  }
  if (vecino != 0) {
    dfAnt[vecino,]$total = dfAnt[vecino,]$izq + dfAnt[vecino,]$aba + dfAnt[vecino,]$der + dfAnt[vecino,]$arr
  }
  
  return(dfAnt)
}

mallaLlena = function(df){
  cuadrosLlenos = which(df$total %in% 4)
  if (length(cuadrosLlenos) == dim(df)[1]) {
    return(TRUE)
  }else{
    return(FALSE)
  }
}



trazarUsuario = function(){
  
  distancia = function(x1,y1,x2,y2) {
    valor = sqrt((x2-x1)^2+(y2-y1)^2)
    return(valor)
  }
  
  ##Repite hasta que un segmento sea rayado
  repeat{ 
    coordenadas = locator(1)
    
    x = coordenadas$x
    y = coordenadas$y
    
    x0 = floor(x) 
    y0 = floor(y)
    
    x1 = ceiling(x)
    y1 = ceiling(y)
    
    ##Condicional de rayar solo dentro de la malla
    if((x0 >= 0 && x0 < n) && (x1 > 0 && x1 <= n) && (y0 >= 0 && y0 < n) && (y1 > 0 && y1 <= n)){
      abajo = distancia(x,y,x,y0)
      derecha = distancia(x,y,x1,y)
      arriba = distancia(x,y,x,y1)
      izquierda = distancia(x,y,x0,y)
      
      todasDist = c(abajo,derecha,arriba,izquierda)
      minimo = min(todasDist)
      
      cuadrado = n*(x1-1)+y1
      
      ##Condicional de rayar solo si está cerca de una linea
      if (minimo <= 0.3){
        
        ##Condicionales de rayar el lado minimo solo si no está rayado
        if (minimo == izquierda && df[cuadrado,]$izq == 0) {
          segments(x0,y0,x0,y1, col = "blue", lwd = 3)
          df <<- actualizarDf(df, cuadrado,"izq")
          vecino = df[cuadrado,]$Vizq
          break
        }else if (minimo == abajo && df[cuadrado,]$aba == 0){
          segments(x0,y0,x1,y0, col = "blue", lwd = 3)
          df <<- actualizarDf(df, cuadrado,"aba")
          vecino = df[cuadrado,]$Vaba
          break
        }else if (minimo == derecha && df[cuadrado,]$der == 0){
          segments(x1,y0,x1,y1, col = "blue", lwd = 3)
          df <<- actualizarDf(df, cuadrado,"der")
          vecino = df[cuadrado,]$Vder
          break
        }else if (minimo == arriba && df[cuadrado,]$arr == 0) {
          segments(x0,y1,x1,y1, col = "blue", lwd = 3)
          df <<- actualizarDf(df, cuadrado,"arr")
          vecino = df[cuadrado,]$Varr
          break
        }
      }
    }
  }
  
  ##Solo llega hasta aqui si algun segmento fue rayado
  
  decision = 0
  if (df[cuadrado,]$total == 4) {
    segments(x0,y0,x1,y1, col = "blue", lwd = 3)
    cuadrosUsu <<- cuadrosUsu + 1
    decision = 1
  }
  
  if (vecino != 0) {
    if(df[vecino,]$total == 4){
      segments(df[vecino,]$X0,df[vecino,]$Y0,df[vecino,]$X1,df[vecino,]$Y1, col = "blue", lwd = 3)
      cuadrosUsu <<- cuadrosUsu + 1
      decision = 1
    }
  }
  
  if (decision == 1){
    return(TRUE)
  }else{
    return(FALSE)
  }
  
}


trazarComputador = function(){
  
  trazar = function(cuadrado,lado){
    if (lado == "izq") {
      x0 = df[cuadrado,]$X0
      y0 = df[cuadrado,]$Y0
      y1 = df[cuadrado,]$Y1
      segments(x0,y0,x0,y1, col = "red", lwd = 3)
    } else if (lado == "aba") {
      x0 = df[cuadrado,]$X0
      x1 = df[cuadrado,]$X1
      y0 = df[cuadrado,]$Y0
      segments(x0,y0,x1,y0, col = "red", lwd = 3)
    } else if (lado == "der") {
      x1 = df[cuadrado,]$X1
      y0 = df[cuadrado,]$Y0
      y1 = df[cuadrado,]$Y1
      segments(x1,y0,x1,y1, col = "red", lwd = 3)
    } else if (lado == "arr") {
      x0 = df[cuadrado,]$X0
      x1 = df[cuadrado,]$X1
      y1 = df[cuadrado,]$Y1
      segments(x0,y1,x1,y1, col = "red", lwd = 3)
    }else{
      x0 = df[cuadrado,]$X0
      x1 = df[cuadrado,]$X1
      y0 = df[cuadrado,]$Y0
      y1 = df[cuadrado,]$Y1
      segments(x0,y0,x1,y1, col = "red", lwd = 3)
    }
  }
  
  mejor2 = function(dfOriginal,cuadrados2){
    
    cuadrosCompletos = function(dfModificado){
      cuadros = 0
      
      #Repite hasta que no pueda llenar mas cuadros con esa opcion
      repeat{
        cuadrado3 = match(3,dfModificado$total)
        
        if(is.na(cuadrado3)){
          break
        }
        
        if (dfModificado[cuadrado3,]$izq == 0) {
          dfModificado = actualizarDf(dfModificado,cuadrado3,"izq")
          vecino = dfModificado[cuadrado3,]$Vizq
        } else if (dfModificado[cuadrado3,]$aba == 0) {
          dfModificado = actualizarDf(dfModificado,cuadrado3,"aba")
          vecino = dfModificado[cuadrado3,]$Vaba
        } else if (dfModificado[cuadrado3,]$der == 0) {
          dfModificado = actualizarDf(dfModificado,cuadrado3,"der")
          vecino = dfModificado[cuadrado3,]$Vder
        }else{
          dfModificado = actualizarDf(dfModificado,cuadrado3,"arr")
          vecino = dfModificado[cuadrado3,]$Varr
        }
        
        cuadros = cuadros + 1
        if (vecino != 0) {
          if(dfModificado[vecino,]$total == 4){
            cuadros = cuadros + 1
          }
        }
      }
      
      return(cuadros)
    }
    
    #Valores mas alla del extremo para poder ser modificados con seguridad
    mejorCuadrado = 0
    mejorLado = ""
    implicacion = (n*n) + 1
    
    #Revisaremos cada cuadrado para saber cual es la mejor opcion 
    for (cuadrado in cuadrados2) {
      
      #Todos son if porque queremos saber tambien cuál será el mejor lado a rayar
      if (dfOriginal[cuadrado,]$izq == 0) {
        dfModificado = actualizarDf(dfOriginal,cuadrado,"izq")
        cuadrosTotales = cuadrosCompletos(dfModificado)
        
        if(cuadrosTotales < implicacion){
          implicacion = cuadrosTotales
          mejorCuadrado = cuadrado
          mejorLado = "izq"
        }
      }
      if (dfOriginal[cuadrado,]$aba == 0) {
        dfModificado = actualizarDf(dfOriginal,cuadrado,"aba")
        cuadrosTotales = cuadrosCompletos(dfModificado)
        
        if(cuadrosTotales < implicacion){
          implicacion = cuadrosTotales
          mejorCuadrado = cuadrado
          mejorLado = "aba"
        }
      } 
      if (dfOriginal[cuadrado,]$der == 0) {
        dfModificado = actualizarDf(dfOriginal,cuadrado,"der")
        cuadrosTotales = cuadrosCompletos(dfModificado)
        
        if(cuadrosTotales < implicacion){
          implicacion = cuadrosTotales
          mejorCuadrado = cuadrado
          mejorLado = "der"
        }
      }
      if (dfOriginal[cuadrado,]$arr == 0){
        dfModificado = actualizarDf(dfOriginal,cuadrado,"arr")
        cuadrosTotales = cuadrosCompletos(dfModificado)
        
        if(cuadrosTotales < implicacion){
          implicacion = cuadrosTotales
          mejorCuadrado = cuadrado
          mejorLado = "arr"
        }
      }
    }
    
    #Si dos o mas cuadrados son optimos devuelve el primero que se encontró
    return(list(mejorCuadrado = mejorCuadrado,mejorLado = mejorLado))
  }
  
  
  #esto nos regresa el número del cuadrado que tiene tres segmentos rayados
  cuadrado = match(3,df$total)
  if (!is.na(cuadrado)) {
    if (df[cuadrado,]$izq == 0) {
      trazar(cuadrado,"izq")
      df <<- actualizarDf(df,cuadrado,"izq")
      vecino = df[cuadrado,]$Vizq
    } else if (df[cuadrado,]$aba == 0) {
      trazar(cuadrado,"aba")
      df <<- actualizarDf(df,cuadrado,"aba")
      vecino = df[cuadrado,]$Vaba
    } else if (df[cuadrado,]$der == 0) {
      trazar(cuadrado,"der")
      df <<- actualizarDf(df,cuadrado,"der")
      vecino = df[cuadrado,]$Vder
    }else{
      trazar(cuadrado,"arr")
      df <<- actualizarDf(df,cuadrado,"arr")
      vecino = df[cuadrado,]$Varr
    }
    trazar(cuadrado,"diagonal")
    cuadrosCompu <<- cuadrosCompu + 1
    if (vecino != 0) {
      if(df[vecino,]$total == 4){
        trazar(vecino,"diagonal")
        cuadrosCompu <<- cuadrosCompu + 1
      }
    }
    return(TRUE) #retorna true ya que completó un cuadro
  }
  
  cuadrados0 = which(df$total %in% 0)
  cuadrados1 = which(df$total %in% 1)
  cuadrados = c(cuadrados0,cuadrados1)
  
  tamanno = length(cuadrados)
  indices = sample(1:tamanno,tamanno,replace=FALSE)
  
  for (i in indices) {
    cuadrado = cuadrados[i]
    if (df[cuadrado,]$izq == 0){
      vecino = df[cuadrado,]$Vizq
      if (vecino != 0) {
        if (df[vecino,]$total < 2) {
          trazar(cuadrado,"izq")
          df <<- actualizarDf(df,cuadrado,"izq")
          return(FALSE)
        }
      }else{
        trazar(cuadrado,"izq")
        df <<- actualizarDf(df,cuadrado,"izq")
        return(FALSE)
      }
    }
    if (df[cuadrado,]$aba == 0){
      vecino = df[cuadrado,]$Vaba
      if (vecino != 0) {
        if (df[vecino,]$total < 2) {
          trazar(cuadrado,"aba")
          df <<- actualizarDf(df,cuadrado,"aba")
          return(FALSE)
        }
      }else{
        trazar(cuadrado,"aba")
        df <<- actualizarDf(df,cuadrado,"aba")
        return(FALSE)
      }
    }
    if (df[cuadrado,]$der == 0) {
      vecino = df[cuadrado,]$Vder
      if (vecino != 0) {
        if (df[vecino,]$total < 2) {
          trazar(cuadrado,"der")
          df <<- actualizarDf(df,cuadrado,"der")
          return(FALSE)
        }
      }else{
        trazar(cuadrado,"der")
        df <<- actualizarDf(df,cuadrado,"der")
        return(FALSE)
      }
    }
    if(df[cuadrado,]$arr == 0){
      vecino = df[cuadrado,]$Var
      if (vecino != 0) {
        if (df[vecino,]$total < 2) {
          trazar(cuadrado,"arr")
          df <<- actualizarDf(df,cuadrado,"arr")
          return(FALSE)
        }
      }else{
        trazar(cuadrado,"arr")
        df <<- actualizarDf(df,cuadrado,"arr")
        return(FALSE)
      }
    }
  }

  #El computador escoge el cuadro de 2 segmentos que implique menos daño para él
  cuadrados2 = which(df$total %in% 2)
  listaMejor2 = mejor2(df,cuadrados2)
  
  cuadrado = listaMejor2$mejorCuadrado
  lado = listaMejor2$mejorLado
  

  if(lado == "izq"){
    trazar(cuadrado,"izq")
    df <<- actualizarDf(df,cuadrado,"izq")
  }else if(lado == "aba"){
    trazar(cuadrado,"aba")
    df <<- actualizarDf(df,cuadrado,"aba")
  }else if(lado == "der"){
    trazar(cuadrado,"der")
    df <<- actualizarDf(df,cuadrado,"der")
  }else{
    trazar(cuadrado,"arr")
    df <<- actualizarDf(df,cuadrado,"arr")
  }
  return(FALSE) 
  
}

#ACÁ INICIA EL MAIN
#Para crear malla nxn
n <<- readline(prompt="Ingrese la dimensión de la matriz cuadrada: ")
n = as.integer(n)

#jugador = 0 : computador
#jugador = 1 : usuario
jugador = readline(prompt="Ingrese 0 si quiere que inicie el computador, sino 1: ")
jugador = as.integer(jugador)

crearMalla(n)
df = crearDfMalla(n)

cuadrosCompu = 0
cuadrosUsu = 0

while(!mallaLlena(df)){
  #turno de usuario
  if (jugador == 1) {
    #usuario traza linea
    completo = trazarUsuario()
    #si completa un cuadrado
    if (completo) {
      jugador = 1
    }
    #si no completa un cuadrado
    else{
      jugador = 0
      
    }
    
  }
  #turno de computador
  else{ 
    #computador traza linea
    completo = trazarComputador()
    #si completa un cuadro
    if (completo) {
      jugador = 0
    }
    #si no completa un cuadro
    else{
      jugador = 1
    }
  }
  
}

print(paste("El computador obtuvo: ",cuadrosCompu))
print(paste("Tú obtuviste: ",cuadrosUsu))
if (cuadrosUsu > cuadrosCompu) {
  print("GANASTE")
}else if (cuadrosUsu < cuadrosCompu) {
  print("PERDISTE")
}else{
  print("EMPATARON")
}
