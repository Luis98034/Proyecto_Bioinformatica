
### Segunda parte para desarrollar los ciclos que estaremos manejando para cada mosca ? 


### Qué datos necesitamos considerar para calcular el intervalo postmorten ? 

# Ciclo de vida de las moscas: tiempo de desarollo en horas 
# Temperatura por día; estación metereologica más cercana al sitio de hallazgo
# Instar en el que se encontró a la mosca; huevo, larva1, larva2,larva3, pupa  
# Temperatura umbral minima de desarrollo de cada especie de mosca 


### ¿Cuales son las moscas de importancia forense en Quéretaro? 

#   Phormia regina 
#   Chrysomyia rufifaciens 
#   Lucilia eximia 
#   Sarcophaga crassipalpis 
#   Musca domestica
#   Lucilia sericata 
#   Cochliomyia macellaria


# La formula para calcular los ADD/ADH 
#   (Tmax+Tmin/2)- UTI 

# Donde
#   Tmax; temperatura maxima de ese día 
#   Tmin; temperatura minima del mismo día 
#   UTI; umbral termico inferior o temperatura minima de desarrollo de la mosca 



# El valor de entrada puede ser un csv con la información de la
# temperatura max y min por día, empezando por el día mas reciente

mosquita_bd <- read.csv("02_Raw_data/dias_temp.csv", header = T) 
View(mosquita_bd) # Coenzando con una base de prueba 

# Debemos aplicar la formula para que calcule la temperatura promedio de cada día 
# y ademas que añada una columna para el promedio de ese día y una para ADD/ADH

# Podemos usar un ciclo for para que calcule los valores que nos faltan 
t_min_desarollo <- as.numeric(readline(prompt = "¿Cuál es la temperatura minima de desarrollo del especimen encontrado?:"))
promedio <- c() # vector vacio para agregar los valores que sacamos de temperatura promedio

for ( i in 1:nrow(mosquita_bd)) {
  promedio[i] <- (mosquita_bd[i, 2] + mosquita_bd[i,3])/2
  #print(promedio) # Al quitar el gato "#" nos damos cuenta que SÍ corre
}
promedio
# Agregamos el vector "promedio" a la base de datos inicial 
mosquita_bd$promedio <- promedio
View(mosquita_bd)



# Haremos el mismo procedimiento pero para determinar el ADD/ADH  
add_adh <- c()  # vector vacio para agregar el ADD/ADH

for ( i in 1:nrow(mosquita_bd)) {
  add_adh[i] <- mosquita_bd[i, 4] - t_min_desarollo
}
add_adh # se guardaron todos los valores 
mosquita_bd$add_adh <- add_adh
View(mosquita_bd) ## hasta aquí todo bien 


# Ya con la tabla completada falta sumar los días hasta que  
# alcancemos el add/adh requerido para el instar que nos estan indicando 
# que se encontró de la especie de la mosca 

# Para ello necesitamos preguntar el instar en el que se encontró al espécimen  

instar <- readline(prompt = "Cuál es el instar en el que se encontró a tu especie? Escribe justo como se te indica; huevo, L1 (larva primer instar),L2(larva segundo instar), L3(larva tercer instar), pupa o adulto:")


# y preguntar la especie de la mosca 
especie_mosca <- readline(prompt ="Escribe la especie a la que pertenece tu espécimen, ejemplo; Sarcophaga crassipalpis, Musca domestica, Lucilia sericata, Cochliomyia macellaria:")

# Necesitamos especificar las horas de cada instar que forman parte del ciclo
# de la especie de interés   



# si se cumple el instar en el que se encontro a tu especimen y tambien es 
# de cierta especie, entonces tomara el valor de ADD que se conoce ese instar de
# la especie que te interesa 
if( instar == "L1" && especie_mosca ==  "Sarcophaga crassipalpis"){

}

# ADD para cada instar de la especie de interes "Sarcophaga crassipalpis" 
P_instar <- 24.4 
S_instar <- 55.8
T_instar <- 106.6
Pupa <- 355.8
Adulto <- 698.6 

suma_add <- 0 # Requerimos de un vector donde se vaya sumando el ADD
indice <- 0 # es como una variable contadora para que sume uno por uno los valores de 
# add de la tabla que se ingresó 
while( suma_add < P_instar & indice <length( mosquita_bd$add_adh)) {
  indice <- indice + 1 
  suma_add <- suma_add + mosquita_bd$add_adh[indice] 
} # Se supone que el while va dentro del primer condicional "if" pero no estoy segura si esto funciona bien 


print(paste("El ADD/ADH resultante es:", suma_add))# Para que nos indique si se sumo 
# como debe de ser el add 


##Selección de los valores predeterminados##########

#Las siguientes son moscas de importancia forensce
Especies_mosca <- c ("Sarcophaga crassipalpis", 
                     "Musca domestica", "Lucilia sericata", "Cochliomyia macellaria")
#Cada una de estas moscas tiene diferentes valores de grados día en cada estado de desarrollo
#además de diferentes valores humbrales de desarrollo

#Temp_desarrollo <- 12
#P_instar <- 18.67
#S_instar <- 53.78
#T_instar <- 91.56
#Pupa <- 176.89
#Adulto <- 334.44

#Se puede generar condicionales para cargar solo los valores de cada mosca si es el 
#caso de tenerla, o también se puede tener una opción para que en caso de que no se tenga
#las moscas preseleccionadas, se puedan agregar los parametros

if(Mosca == "Musca domestica"){
  Temp_desarrollo <- 12
  P_instar <- 18.67
  S_instar <- 53.78
  T_instar <- 91.56
  Pupa <- 176.89
  Adulto <- 334.44
}else if(Mosca == "Lucilia sericata"){
  Temp_desarrollo <- 10.4
  P_instar <- 8.8
  S_instar <- 22.7
  T_instar <- 56.5
  Pupa <- 102.4
  Adulto <- 221.2
}else if (Mosca == "Cochliomyia macellaria"){
  Temp_desarrollo <- 10
  P_instar <- 22.24
  S_instar <- 44.48
  T_instar <- 78.03
  Pupa <- 161.64
  Adulto <- 294.5
}else if (Mosca == "Sarcophaga crassipalpis"){
  Temp_desarrollo <- 9.31
  P_instar <- 24.4
  S_instar <- 55.8
  T_instar <- 106.6
  Pupa <- 355.8
  Adulto <- 698.6 
} else{ #Si no hay ninguna de las opciones anteriores entonces corre los siguiente y añade los valores
  Temp_desarrollo <- as.numeric(readline(prompt = "¿Cuál es la temperatura minima de desarrollo del especimen encontrado?"))
  P_instar <- as.numeric(readline(prompt = "ADD primer instar"))
  S_instar <- as.numeric(readline(prompt = "ADD segundo instar"))
  T_instar <- as.numeric(readline(prompt = "ADD tercer instar"))
  Pupa <- as.numeric(readline(prompt = "ADD Pupa"))
  Adulto <- as.numeric(readline(prompt = "ADD Adulto"))
}

#Una vez dados los valores de cada mosca podríamos comenzar a calcular los grados días 
#necesarios para que se llegue a ese estado de desarrollo














