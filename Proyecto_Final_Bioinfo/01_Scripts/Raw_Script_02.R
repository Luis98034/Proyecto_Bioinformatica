
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
instar <- readline(prompt("Cuál es el instar en el que se encontró a tu especie? 
                             Escribe justo como se te indica; 
                             huevo, 
                             L1 (larva primer instar),
                             L2(larva segundo instar), 
                             L3(larva tercer instar), pupa o adulto:"))


# y preguntar la especie de la mosca 
especie_mosca <- readline(prompt("Escribe la especie a la que pertenece tu espécimen, ejemplo; 
Sarcophaga crassipalpis, 
Musca domestica, 
Lucilia sericata, 
Cochliomyia macellaria:"))

# Necesitamos especificar las horas de cada instar que forman parte del ciclo
# de la especie de interés   



if( instar == "L1" | especie_mosca ==  "Sarcophaga crassipalpis"){
}

P_instar <- 24.4
S_instar <- 55.8
T_instar <- 106.6
Pupa <- 355.8
Adulto <- 698.6 

suma_add <- 0 
indice <- 0
while( suma_add >= P_instar & indice <length( mosquita_bd$add_adh)) {
  indice <- indice + 1 
}



 




