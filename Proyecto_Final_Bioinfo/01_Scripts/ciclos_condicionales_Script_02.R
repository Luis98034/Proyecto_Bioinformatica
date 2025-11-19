
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

Temperaturas_prueba <- sample(3:28, size = 365, replace = TRUE)# Vector con temperaturas 
                                                          #aleatorias entre 3 y 28 grados
add_acumulado <- 0 #Aquí se va guardando 
add_diario <- 0 #Aquí se aloja el add diario de cada ciclo
dias_transcurridos <- 0 #Para contar los dias que pasan, o sea con cada ciclo pase lo que pase
                      #se cuenta un día
pupa_prueba <- 355
for (dia in 1:length(Temperaturas_prueba)) {#de nuestra vareable dia toma los valores de la temperatura de uno en unp
  if (add_acumulado <= pupa_prueba) {#siempre y cuando el add acumula sea menor que el valor del add de la pupa corre lo siguiente
    add_diario <- Temperaturas_prueba[dia] - 12# El add diario lo calcula restando el valor de la temperatura menos el valor humbral, la desventaja es que tiene valores negativos
    add_acumulado <- add_acumulado + add_diario#Aqui sobre escribe el acumulado sumandole el add diario que se calcula dependiendo del valor del dia
    dias_transcurridos <- dias_transcurridos + 1# cada que corra el ciclo for sin importar el valo del add diario suma uno haciendo que se cuenten todos los días
  } 
}
#El ciclo se detiene solo si el valor del add acumulado supera el de la pupa o si se acaban
#los valores dentro del vector del valor de las temperaturas, por lo que el siguiente
#codigo nos dice si es necesario tomar más temperaturas
if (add_acumulado<pupa_prueba){ #En cado de que despues del ciclo for el add caumulado siga siendo menor
    paste(print("hay que ir más atras"))#dime que es necesario ir más atras en el tiempo
}


dias_transcurridos
add_acumulado




add_solve <- function (nombre_mosca, estado_desarrollo, vector_temperaturas){#Se necesita el nombre de la mosca, el estado en el que se encuentra y las temperaturas de los días
  Especies_mosca <- c ("Sarcophaga crassipalpis", 
                       "Musca domestica", "Lucilia sericata", "Cochliomyia macellaria")#Estas son las moscas aceptadas en el programa
  Especies_mosca_abreviado <- c ("S. crassipalpis", 
                       "M. domestica", "L. sericata", "C. macellaria")#así se deben de ingresar
posible_estado_desarrollo <- c("primer instar", "segundo instar", "tercer instar",
                               "pupa", "adulto")
  #Primero determinar los valores estandar para cada mosca
  if(nombre_mosca == "M. domestica"){#Valores de add para musca domestica
    Temp_desarrollo <- 12
    P_instar <- 18.67
    S_instar <- 53.78
    T_instar <- 91.56
    Pupa <- 176.89
    Adulto <- 334.44
  }else if(nombre_mosca == "L. sericata"){#valores de add para L sericata
    Temp_desarrollo <- 10.4
    P_instar <- 8.8
    S_instar <- 22.7
    T_instar <- 56.5
    Pupa <- 102.4
    Adulto <- 221.2
  }else if (nombre_mosca == "C. macellaria"){#valores de add para C. macellaria
    Temp_desarrollo <- 10
    P_instar <- 22.24
    S_instar <- 44.48
    T_instar <- 78.03
    Pupa <- 161.64
    Adulto <- 294.5
  }else if (nombre_mosca == "S. crassipalpis"){#Valores para S. crassipalpis
    Temp_desarrollo <- 9.31
    P_instar <- 24.4
    S_instar <- 55.8
    T_instar <- 106.6
    Pupa <- 355.8
    Adulto <- 698.6 
  } else{ #Si no hay ninguna de las opciones anteriores entonces corre los siguiente y el usuario puede añadir sus propios valores añade los valores
    Temp_desarrollo <- as.numeric(readline(prompt = "¿Cuál es la temperatura minima de desarrollo del especimen encontrado?"))
    P_instar <- as.numeric(readline(prompt = "ADD primer instar"))
    S_instar <- as.numeric(readline(prompt = "ADD segundo instar"))
    T_instar <- as.numeric(readline(prompt = "ADD tercer instar"))
    Pupa <- as.numeric(readline(prompt = "ADD Pupa"))
    Adulto <- as.numeric(readline(prompt = "ADD Adulto"))
  }
#Ahora es necesario calcular los add acumulados, los siguientes vectores se utilizan en todos
add_acumulado <- 0 #Aquí se va guardando 
add_diario <- 0 #Aquí se aloja el add diario de cada ciclo
dias_transcurridos <- 0 #Contador para los dias reales transcurridos
####
#para los estados de primer instar#
####
if(estado_desarrollo=="primer instar"){
  for (dia in 1:length(vector_temperaturas)) {#de nuestra vareable dia toma los valores de la temperatura de uno en unp
    if (add_acumulado<=P_instar) {#siempre y cuando el add acumula sea menor que el valor del add de la pupa corre lo siguiente
      add_diario <- vector_temperaturas[dia] - Temp_desarrollo# El add diario lo calcula restando el valor de la temperatura menos el valor humbral, la desventaja es que tiene valores negativos
      if(add_diario<=0){#Con esto se evida que se tomen valores negativos para la suma
        add_diario <- 0# si hay un valor negativo en el add diario se le asigna un valor de 0
      }
      add_acumulado <- add_acumulado + add_diario#Aqui sobre escribe el acumulado sumandole el add diario que se calcula dependiendo del valor del dia
      dias_transcurridos <- dias_transcurridos + 1# cada que corra el ciclo for sin importar el valo del add diario suma uno haciendo que se cuenten todos los días
    } 
  }
  
  if(add_acumulado>=P_instar){
    cat("Con un add acumulado de", add_acumulado, "pasaron un total de",#Si se llego al objetivo del add, me da un mensaje de que así fue
        dias_transcurridos, "días desde el deceso hasta el descubrimiento del cuerpo")
  }else {cat("El add acumulado fue de", add_acumulado, "pasaron un total de", dias_transcurridos,
             " dias sin embargo no se llego al total de grados día para llegar al estado de pupa",
             "por lo que se tienen que tomar más días")}# de no llegar al objetivo se especifica que se tienen que tomar más días
  
}
####
#para los estados de segundo instar#
####
if(estado_desarrollo=="segundo instar"){
  for (dia in 1:length(vector_temperaturas)) {#de nuestra vareable dia toma los valores de la temperatura de uno en unp
    if (add_acumulado<=S_instar) {#siempre y cuando el add acumula sea menor que el valor del add de la pupa corre lo siguiente
      add_diario <- vector_temperaturas[dia] - Temp_desarrollo# El add diario lo calcula restando el valor de la temperatura menos el valor humbral, la desventaja es que tiene valores negativos
      if(add_diario<=0){#Con esto se evida que se tomen valores negativos para la suma
        add_diario <- 0# si hay un valor negativo en el add diario se le asigna un valor de 0
      }
      add_acumulado <- add_acumulado + add_diario#Aqui sobre escribe el acumulado sumandole el add diario que se calcula dependiendo del valor del dia
      dias_transcurridos <- dias_transcurridos + 1# cada que corra el ciclo for sin importar el valo del add diario suma uno haciendo que se cuenten todos los días
    } 
  }
  
  if(add_acumulado>=S_instar){
    cat("Con un add acumulado de", add_acumulado, "pasaron un total de",#Si se llego al objetivo del add, me da un mensaje de que así fue
        dias_transcurridos, "días desde el deceso hasta el descubrimiento del cuerpo")
  }else {cat("El add acumulado fue de", add_acumulado, "pasaron un total de", dias_transcurridos,
             " dias sin embargo no se llego al total de grados día para llegar al estado de pupa",
             "por lo que se tienen que tomar más días")}# de no llegar al objetivo se especifica que se tienen que tomar más días
  
}
####
#para los estados de tercer instar#
####
if(estado_desarrollo=="tercer instar"){
  for (dia in 1:length(vector_temperaturas)) {#de nuestra vareable dia toma los valores de la temperatura de uno en unp
    if (add_acumulado<=T_instar) {#siempre y cuando el add acumula sea menor que el valor del add de la pupa corre lo siguiente
      add_diario <- vector_temperaturas[dia] - Temp_desarrollo# El add diario lo calcula restando el valor de la temperatura menos el valor humbral, la desventaja es que tiene valores negativos
      if(add_diario<=0){#Con esto se evida que se tomen valores negativos para la suma
        add_diario <- 0# si hay un valor negativo en el add diario se le asigna un valor de 0
      }
      add_acumulado <- add_acumulado + add_diario#Aqui sobre escribe el acumulado sumandole el add diario que se calcula dependiendo del valor del dia
      dias_transcurridos <- dias_transcurridos + 1# cada que corra el ciclo for sin importar el valo del add diario suma uno haciendo que se cuenten todos los días
    } 
  }
  
  if(add_acumulado>=T_instar){
    cat("Con un add acumulado de", add_acumulado, "pasaron un total de",#Si se llego al objetivo del add, me da un mensaje de que así fue
        dias_transcurridos, "días desde el deceso hasta el descubrimiento del cuerpo")
  }else {cat("El add acumulado fue de", add_acumulado, "pasaron un total de", dias_transcurridos,
             " dias sin embargo no se llego al total de grados día para llegar al estado de pupa",
             "por lo que se tienen que tomar más días")}# de no llegar al objetivo se especifica que se tienen que tomar más días
  
}
###
#Para los estados de pupa#
###
if(estado_desarrollo=="pupa"){
  for (dia in 1:length(vector_temperaturas)) {#de nuestra vareable dia toma los valores de la temperatura de uno en unp
    if (add_acumulado<=Pupa) {#siempre y cuando el add acumula sea menor que el valor del add de la pupa corre lo siguiente
      add_diario <- vector_temperaturas[dia] - Temp_desarrollo# El add diario lo calcula restando el valor de la temperatura menos el valor humbral, la desventaja es que tiene valores negativos
      if(add_diario<=0){ #Con esto se evida que se tomen valores negativos para la suma
        add_diario <- 0 # si hay un valor negativo en el add diario se le asigna un valor de 0
      }
      add_acumulado <- add_acumulado + add_diario#Aqui sobre escribe el acumulado sumandole el add diario que se calcula dependiendo del valor del dia
      dias_transcurridos <- dias_transcurridos + 1# cada que corra el ciclo for sin importar el valo del add diario suma uno haciendo que se cuenten todos los días
    } 
  }
  
  if(add_acumulado>=Pupa){
  cat("Con un add acumulado de", add_acumulado, "pasaron un total de",#Si se llego al objetivo del add, me da un mensaje de que así fue
      dias_transcurridos, "días desde el deceso hasta el descubrimiento del cuerpo")
  }else {cat("El add acumulado fue de", add_acumulado, "pasaron un total de", dias_transcurridos,
             "sin embargo no se llego al total de grados día para llegar al estado de pupa",
             "por lo que se tienen que tomar más días")}# de no llegar al objetivo se especifica que se tienen que tomar más días
  
}
###
#Para los estados de pupa#
###
if(estado_desarrollo=="adulto"){
  for (dia in 1:length(vector_temperaturas)) {#de nuestra vareable dia toma los valores de la temperatura de uno en unp
    if (add_acumulado<=Adulto) {#siempre y cuando el add acumula sea menor que el valor del add de la pupa corre lo siguiente
      add_diario <- vector_temperaturas[dia] - Temp_desarrollo# El add diario lo calcula restando el valor de la temperatura menos el valor humbral, la desventaja es que tiene valores negativos
      if(add_diario<=0){#Con esto se evida que se tomen valores negativos para la suma
        add_diario <- 0# si hay un valor negativo en el add diario se le asigna un valor de 0
      }
      add_acumulado <- add_acumulado + add_diario#Aqui sobre escribe el acumulado sumandole el add diario que se calcula dependiendo del valor del dia
      dias_transcurridos <- dias_transcurridos + 1# cada que corra el ciclo for sin importar el valo del add diario suma uno haciendo que se cuenten todos los días
    } 
  }
  
  if(add_acumulado>=Adulto){#Si se llego al objetivo del add, me da un mensaje de que así fue
    cat("Con un add acumulado de", add_acumulado, "pasaron un total de",
        dias_transcurridos, "días desde el deceso hasta el descubrimiento del cuerpo")
  }else {cat("El add acumulado fue de", add_acumulado, "pasaron un total de", dias_transcurridos,
             "sin embargo no se llego al total de grados día para llegar al estado de pupa",
             "por lo que se tienen que tomar más días")}# de no llegar al objetivo se especifica que se tienen que tomar más días
  
}
  
}












