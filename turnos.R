# Este se basa en dar prioridad al docente mas ocupado para tomar los turnos menos cubiertos
# No funciona con todas la mayoría de las matrices de disponibilidad :(
# Pero con algunas si...

# Lo raro es que si todos los docentes pueden en todos los turnos (p = 1)
# Hay combinaciones de #docentes y #turnos que fallan siempre
# Pero más raro es que para esa combinación de #turnos #docente, hay soluciones posibles para p < 1
# O sea que el algoritmo tiene algo muy mal pero no encontré que es

# Creo que es un tema de en qué orden se van "inutilizando" o "consumiendo" los slots.
# Y de que hay maneras de desperdiciar menos dispoonibilidad.
# Por ejemplo, en lo siguiente con p = 1 (100% determinista) el algoritmo seguramente camine mal y desperdicie cosas:

# [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10]
# [1,]    1    1    1    0    0    0    0    0    0     0
# [2,]    1    1    1    0    0    0    0    0    0     0
# [3,]    1    1    1    0    0    0    0    0    0     0
# [4,]    1    1    1    0    0    0    0    0    0     0
# [5,]    1    1    1    0    0    0    0    0    0     0
# [6,]    0    0    0    1    1    1    0    0    0     0
# [7,]    0    0    0    1    1    1    0    0    0     0
# [8,]    0    0    0    1    1    1    0    0    0     0
# [9,]    0    0    0    1    1    1    0    0    0     0
# [10,]    0    0    0    1    1    1    0    0    0     0
# [11,]    0    0    0    0    0    0    1    1    1     0
# [12,]    0    0    0    0    0    0    1    1    1     0
# [13,]    0    0    0    0    0    0    1    1    1     0
# [14,]    0    0    0    0    0    0    1    1    1     0
# [15,]    0    0    0    0    0    0    1    1    1     0
# [16,]    0    0    0    0    0    0    0    0    0     1
# [17,]    0    0    0    0    0    0    0    0    0     1

# Ahora intenté randomizar cuál docente/turno se elige si hay dos que da igual (misma disp/cobertura)
# Descubrí un "bug" en sample() porque si length(x)==1 entonces samplea de 1:x (una estupidez, maldito R)
resamp <- function(x,...){if(length(x)==1) x else sample(x,...)} 
# y....? violá! funcionó de una... ¿habrá sido solo lo del sample?
# Porque ahora lo resuelve casi siempre con p = 1

# Antes de ponerle sample no funcionaba con p = 1, ahora si... (a veces)
# Entonces hay una forma mejor de ir asignando turnos que sin el sample (digamos mejor que la "caminata" que hace el algoritmo por defecto)
# Y seguramente debe haber otra mejor aún que la "random"
# Ni idea cuál

# Debe tener que ver con eso de que cuando se completa un turno con los ayudantes que necesita
# se impide que otros ayudantes puedan trabajar ese turno y quizás hacer eso ingénuamente es la muerte

# Padría hacer machine learning con esto (?)

# Probando con p = 0.7
# ...y? funciona con "~la mitad" de las matrices de disponibilidad/turnos que pasan mi test
# Eso no quita que falta encontrar un algoritmo mejor
# Pero tampoco dice que es malo, porque no sé cómo demostrar que una matriz disp/turnos es "resolvible" o no

# Otra cosa que se me ocurre es que cuando calculo el turno menos cubierto la primera vez es fácil elegir
# Pero cuando hay varios turnos asignados, entonces imagino que quizás los turnos "menos cubiertos" pueden ser los más asignados
# ¿Hay una razón por la que esto pueda joder? No sé
# Pero un turno con 1/3 asignaciones y N cobertura, debería tener menos prioridad que uno con 0/3 asignaciones y la misma cobertura
# Así que tendría sentido. Además tengo que aclararle al código que no haga esa corrección para turnos/docentes ya completos.
# Hice una prueba haciendo lo mismo para los docentes que para los turnos:
#   Control: si pongo p = 1 con la estrategia original tengo ~75% bien.
#   Experimento: genial! ahora resuelve el 100% de los casos con una corrida con 78 pruebas.

# La "caminata" del código es mejor implementando esa idea.
# Sigue haciendo ese sampling ingénuo cuando los docentes/turnos son igualmente ocupados/cubiertos. Eso no mejoró.
# Ahora debería probar con p < 1.
# Prueba con p = 0.7 resuelve: 100 %
# Prueba con p = 0.5 resuelve: 65 %
# Prueba con p = 0.3 resuelve: 0.35 %
#  Jaja tardó muchiiisimo más en ejecutarse, entonces hay matrices más complicadas que otras para resolver (lógicamente)
#  Pero si p es lo suficientemente grande (o sea, que la gente está bastante disponible) entonces el coso este puede funcionar bien
#  Si p es baja, este código tarda mucho porque es bastante tonto.

# Conclusión, con p = 0.7 este código funciona porque suertudo.

# Hay que definir jj la primera vez
jj = c()
cantidad_tests = 100

for(i in 1:cantidad_tests){
  # Esta cosa loopea hasta que genere algo que pudo resolver (no es muy inteligente)
  loop = TRUE
  j = 0
  
  
  
  while (loop) {
    {# Setup
      
      turnos <- 3*5  # hay 3 "slots" por dia en la semana posibles donde podría haber un TP
      
      cant_turnos_tp <- 10  # turnos de tp que se reparten entre esos slots
      
      
      ayudantes_por_turno <- 5  # personas por turno
      turnos_a_cubrir <-cant_turnos_tp * ayudantes_por_turno  # = shifts a cubrir
      
      
      turnos_por_ayudante <- 3  # turnos por persona que podemos pagar/pedir
      cant_ayudantes <- ceiling(turnos_a_cubrir / turnos_por_ayudante) # debe haber al menos x personas
      
      setup <- TRUE
      while(setup){
        # Intentar hacer una matriz de horarios TPs y disponibilidades
        # que al menos tenga suficiente disponibilidad para cada día que hay TP
        # Supongo que no asegura que se pueda resolver
        
        # Disponibilidad
        p <- 0.5
        avail <- matrix(data = rbinom(turnos * cant_ayudantes, 1, p), nrow = cant_ayudantes, ncol = turnos)
        # Turnos disponibles
        turnos_tp <- resamp(c(rep(1,cant_turnos_tp), rep(0,turnos-cant_turnos_tp)))
        
        # Reducir disponibilidad según los turnos que hay para cubrir
        avail_bup <- avail[,turnos_tp == T]
        
        # Contar
        sum_cobertura_turnos <- apply(avail, 2, sum)  # Cuantos ayudantes pueden cubrir cada turno
        if(min(sum_cobertura_turnos) < ayudantes_por_turno){
          print('No hay suficientes ayudantes para uno de los días')
          setup <- TRUE} else {setup <- FALSE}
        
        sum_disp_docente <- apply(avail, 1, sum)  # Cuantos turnos puede cubrir cada ayudante
        if(min(sum_disp_docente) < turnos_por_ayudante){
          print('Un ayudante no puede cumplir con el mínimo de turnos')
          setup <- TRUE} else {setup <- FALSE}
        
      }
    }
    
    
    
    
    {
      # (Re)iniciar
      
      # Matriz de disponibilidad
      avail <- avail_bup
      avail
      
      # ¿Y ahora?
      assigned <- matrix(rep(0, length(avail)), nrow = nrow(avail), ncol = ncol(avail))
    }
    
    
    
    for(i in 1:turnos_a_cubrir){
      sum_cobertura_turnos <- apply(avail, 2, sum)
      print(paste("Cobertura de turnos:", paste(sum_cobertura_turnos, collapse = ' ')))
      sum_disp_docente <- apply(avail, 1, sum)
      print(paste("Disponibilidad docente:", paste(sum_disp_docente, collapse = ' ')))
      
      # Buscar al docente más ocupado
      sum_disp_docente <- apply(avail, 1, sum)
      sum_docente_asignado <- apply(assigned, 1, sum)
      sum_disp_docente2 <- (sum_disp_docente + sum_docente_asignado)*(sum_disp_docente > 0)  # opcion 2: corregir coverage por docentes ya asignados excepto cuando coverage == 0
      mas_ocupado <- resamp(which(sum_disp_docente2 %in% min(sum_disp_docente2[sum_disp_docente2 > 0])), 1)
      
      # Buscar al turno menos cubierto de los que ese docente puede cubrir
      sum_cobertura_turnos <- apply(avail, 2, sum) * avail[mas_ocupado,]  # cobertura_turnos_del_ayudante_mas_ocupado
      sum_turnos_cubiertos <- apply(assigned, 2, sum)
      sum_cobertura_turnos2 <- (apply(avail, 2, sum) + sum_turnos_cubiertos) * avail[mas_ocupado,]  # opcion 2: corregir coverage por slots ya asignados
      sum_cobertura_turnos2 <- sum_cobertura_turnos2 * (sum_cobertura_turnos > 0)  # excepto cuando coverage == 0
      menos_cubierto <- resamp(which(sum_cobertura_turnos2 %in% min(sum_cobertura_turnos2[sum_cobertura_turnos2 > 0])), 1)  # turno del docente mas ocupado con menos cobertura
      
      # Asignación
      print(paste("Asignando docente", mas_ocupado, "al turno", menos_cubierto, sep = ' '))
      avail[mas_ocupado,menos_cubierto] <- 0
      assigned[mas_ocupado, menos_cubierto] <- 1
      
      # Ver si hay algún docente con suficientes turnos y volarlo de la lista
      sum_docente_asignado <- apply(assigned, 1, sum)
      docentes_completos <- which(sum_docente_asignado %in% turnos_por_ayudante)
      if(length(docentes_completos)!=0) avail[docentes_completos,] <- 0  # Volar los docente/s ocupados
      
      # Ver si hay algún turno cubierto y volarlo de la lista
      sum_turnos_cubiertos <- apply(assigned, 2, sum)
      turnos_completos <- which(sum_turnos_cubiertos >= ayudantes_por_turno)
      if(length(turnos_completos)!=0) avail[,turnos_completos] <- 0  # Volar los turnos/s completos
      
      if(sum(avail) == 0 & sum(assigned) != turnos_a_cubrir) {
        print("Error: zero availability but assignment is incomplete")
        break
      }
      
      avail
    }
    
    if(i == turnos_a_cubrir){
      avail
      avail_bup * assigned
      
      sum_assign_docente <- apply(assigned, 1, sum)
      sum_assign_docente
      sum_cobertura_turnos <- apply(assigned, 2, sum)
      sum_cobertura_turnos
      
      loop = FALSE
    }
    
    j = j + 1
  }
  
  # Y esto es solo para ir contando  j
  jj <<- c(j, jj)
}

jj
length(jj)/sum(jj)  # proporcion de 'inicializaciones' que pudo resolver
