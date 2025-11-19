
#### Funcion/es  para determinar el intervalo post morten del espécimen 


# PRIMERA FUNCIÓN ; para calcular a partir de un vector 

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



# SEGUNDA FUNCIÓN; para calcular a partir de un archivo cvs 






