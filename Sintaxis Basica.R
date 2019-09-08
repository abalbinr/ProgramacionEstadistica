#CLASES

##character (alfanumérico).
##numeric (reales).
##integer (enteros).
##complex (complejos).
##logical (lógicos TRUE/FALSE)

character1 <- "a"
character2 <- "augusto"
numeric1 <- 20 #Lo guarda como un numero real y no entero
numeric2 <- 20.2
integer1 <- 20L #Se pone L para especificar que es entero
complex1 <- 20 + 1i

infinito <- Inf #Representa al infinito positvo
valorNA <- NA #Representa un valor faltante 
valorNAN <- NaN #Representa un valor no definido 
valorNULL <- NULL #Representa un valor vacio





#ATRIBUTOS

##class(X) #Para sacer que clase es X
##names(X) #Nombre de cada elemento
##dimnames(X) #Nombres de las filas y columnas de una matriz (Clase lista)
##dim(X) #Dimensiones de una matriz
##length(X) # Cantidad de elementos de cualquier objeto
##attributes(X) #Atributos de un objeto





#VECTORES
##Los elementos del vector tiene que ser de la misma clase
##La indexación comienza en 1

secuencia <- 1:5 # a:b crea una secuencia integer entre a y b inclusives

concatenar <- c(1,2,3,4,5) #La funcion c(...) sirve para concatenar varios valores
vector1 <- c(0.5,0.6,0.1,0.2) #Vector numeric
vector2 <- 3:7 #Vector integer
vector3 <- c(TRUE,FALSE,NA) #Vector logical
vector4 <- c("Hola","asd","a") #Vector character
vector4 <- c(vector4,"nuevo") #Agregar elementos
vector4[4] <- "Reemplazo" #Reemplzar un valor 


vacioNumeric <- numeric(0) #Vector vacio numeric
vacioLogical <- vector() #Vector vacio logical

vectorCeros1 <- vector("numeric",length = 10) #Vector que contiene 10 ceros de clase numeric
vectorCeros2 <- numeric(10) #Otra forma de hacer lo de arriba





##########################¿Como hacer para cambiar valores de un vector?

vector5 <- c(1.7, "a")  #Vector character (Coversion implicita)
vector6 <- c(3, TRUE, FALSE)  #Vector numeric (Coversion implicita)

vector7 <- 0:6 #Vector Integer
vector8 <- as.numeric(vector7) #Vector numeric (Coversion explicita)
vector9 <- as.character(vector7) #Vector character (Coversion explicita)
vector10 <- as.logical(vector7) #Vector logical (Coversion explicita)
vector11 <- as.complex(vector7) #Vector complex (Coversion explicita)

vector12 <- 3:5
names(vector12) #Vacio porque cada elemento del vector no tiene nombre

names(vector12) <- c("ele1","ele2","ele3") #Asigna un nombre a cada elemento
names(vector12) 

vector13 <- c("ele1" = 3,"ele2" = 4,"ele3" = 5) #Otra forma de asignar nombres
names(vector13) 

vector14 <- c("a", "b", "c", "c", "d", "a") 
vector14[2] #Devuelve el elemento del vector
vector14[2:4] #Devuelve los elementos inclusives

vector14 > "a" #Hace la comparacion elemento por elemento
vector14[vector14 > "a"] #Solo devuelve los elementos que cumplen la comparción





#MATRICES
##Los elementos de una matriz tiene que ser de la misma clase

matriz1 <- matrix(nrow = 2, ncol = 3) #Matriz de NA de 2x3
matriz2 <- matrix(1:6,nrow = 2, ncol = 3) 
matriz3 <- 1:10 #Comienza como un vector
dim(matriz3) #Dimensiones nulas porque es un vector
dim(matriz3) <- c(2,5) #Convertir un vector a una matriz

x <- 1:3
y <- 10:12
matriz4 <- cbind(x,y) #Las pega como columnas
matriz5 <- rbind(x,y) #Las pega como filas

atrib1 <- attributes(matriz4)
atrib2 <- attributes(matriz5)

matriz6 <- matrix(1:6, nrow = 2, ncol = 3)
dimnames(matriz6) <- list(c("a", "b"),c("c", "d","e")) #Asignar nombres a las "dimensiones" (fila,columna)
matriz6

matriz7 <- matrix(6:1, 2,3)
matriz7[3] #Devuelve el elemento en la posicion 3, una matriz se lee lineal, de abajo luego derecha
matriz7[1, 2] #Otra forma de hacer lo de arriba, pero aqui es fila X columna
matriz7[1,] #Toda la primera fila
matriz7[,2] #Toda la segunda columna

elemento <- matriz7[1, 2, drop = TRUE] # Elemento en esa posición (Equivalente a matriz7[1, 2]) (Es como [[]] de una lista)
matriz8 <-  matriz7[1, 2, drop = FALSE] #Devuelve el elemento en esa posicion pero como clase matrix (Es como [] de una lista)

matriz9 <- matrix(6:1, 2,3)
matriz9[1,, drop = TRUE] #Vector con los elementos de la primera fila (Equivalente a matriz7[1,])  (Es como [[]] de una lista)
matriz9[1,, drop = FALSE] #Matriz con los elementos de la primera fila (Es como [] de una lista)

matriz10<- matrix(c("a","b","c",4,5,6), 2,3)





#LISTAS
##En una lista si se puede guardar diferentes tipos de elementos
##Los indices comienzan desde 1
##El [] devuelve un objeto de la misma clase que el original
##El [[]] devuelve el elemento exacto 

lista1 <- list(1,"a",TRUE,1+4i)
lista2 <- lista1[1] #Devuelve una lista con el primer elemento
elem1 <- lista1[[1]] #Devuelve el primer elemento como tal

lista3 <- list(1,2,3,"a","b")
lista4 <- lista3[5]
elem5 <- lista3[[5]]

lista5 <- list(a = 1, b = 2, c = 3)
names(lista5)
lista5["a"]
lista5[["a"]]


lista6 <- list(nom1 = 6:1, nom2 = 0.6)
lista6[1] #Clase list, tambien coge el nombre
lista6[[1]] #solo coge los elementos como tal

lista6[2]
lista6["nom2"] #Forma de sacar elementos con la misma clase dependiendo del nombre

lista6[[2]]
lista6[["nom2"]] #Forma de sacar elementos como tal dependiendo del nombre
lista6$nom2 #Otra forma de hacerlo ( Es equivalente a [[]] )


lista7 <- list(6:1 , 0.6 , "hello")
lista7[c(1,3)] #Forma de sacar elementos en posiciones no consecutivas como clase lista

lista8 <- list(a = list(10, 12, 14), b = c(3.14, 2.81))
lista8[[c(1,3)]] #Coge el elemento 1 de la lista8 y luego saca el elemento 3 de a
lista8[[1]][[3]] #Equivalente a lo de arriba

lista8[[c(2,1)]]





#FACTORES
##Se usan para representar variables categóricas.

factor1 <- factor(c("si", "si", "no", "si", "no"))
levels(factor1) #Devuelve los valores unicos, donde el orden importa
as.numeric(factor1) #Devuelve la representacion numerica teniendo en cuenta el orden de levels
unclass(factor1) #Devuelve algunos atributos


factor2 <- factor(c("si", "si", "no", "si", "no"))
factor2 <- factor(factor2, levels = c("si","no")) #Se está poniendo un orden deseado para los levels
levels(factor2)
as.numeric(factor1) #A pesar de tener el mismo orden que el factor1, tienen diferentes representaciones numericas
unclass(factor2)





#VALORES FALTANTES (NA) Y VALORES NO DEFINIDOS(NaN)
##Un valor NaN es NA pero no al contrario.

prueba1 <- c(1, 2, NaN, NA, 4)
is.na(prueba1)
is.nan(prueba1)





#BASE DE DATOS (DATA FRAME)
##Cada columna tiene que tener la misma cantidad de elementos
##Tambien pasa lo mismo con [] y [[]]
##La indexacion es por columnas

frame1 <- data.frame(foo = 1:4, bar = c(T, T, F, F)) #Cada atributo es una columna con su respectivo nombre

nrow(frame1)
ncol(frame1)
dim(frame1)
names(frame1)
attributes(frame1)

frame1[1] #Columna1
frame1$foo

frame1[2] #Columna2
frame1$bar





#OPERADORES LOGICOS

## | (or)
## & (and)
## ! (negacion)


######################################### ¿Cual es la diferencia entre & y && ?
##Deferencia entre & y &&
((-2:2) >= 0) & ((-2:2) <= 0) #Hace una verificacion 1 a 1 

((-2:2) >= 0) && ((-2:2) <= 0) #Hace una verificacion grobal





#CONDICIONALES

##Forma 1
x <- 11
if (x > 10) {
  y <- 100
} else if(x==10) {
  y <- 10
} else{
  y <- 0
}


##Forma 2
y <- if (x > 10) {
  100
} else if(x==10) {
  10
} else{
  0
}





#CICLO FOR

suma <-  0
for (i in 1:10){
  suma <-  suma + i
}


letras <- c("a", "b", "c", "d")
for (i in seq_along(letras)){ #seq_along(letras) sirve para crear un vector con los indices de letras
  print (letras[i])
}

for (letra in letras) { #Otra forma de hacerlo
  print(letra)
}





#CICLO WHILE

conteo <- 0
while (conteo < 10) {
  print(conteo)
  conteo <- conteo + 1
}

z <- 5
while (z >= 3 && z <= 10){
  print(z)
  moneda <- rbinom(1, 1, 0.5)
  if (moneda == 1){
    z <- z + 1
  } else {
    z <- z - 1
  }
}





#CICLO REPEAT
##La única forma de salir del repeat es mediante el break.

contador <- 1
repeat{
  if(contador==10){
    break
  }
  print(contador)
  contador <- contador + 1

}





#CALCULAR EFICIENCIA
##Con la funcion system.time()

##Prueba 1
n <- 100000
vect <- numeric(0)
system.time(for(i in 1:n) vect <- c(vect, i*2))

##Prueba 2
n <- 100000
vect <- numeric(n)
system.time(for(i in 1:n) vect[i] <- i*2)





#FUNCIONES
##Pueden tener valores opcionales (Valores por defecto)

funcion1 <- function(a, b = 1, c = 2, d = NULL) {
  return(a+b+c)
}

valor <-  funcion1(1,3)

formals(funcion1) #Para saber que parametros tiene una funcion
