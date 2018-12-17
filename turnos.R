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

# Esta cosa loopea hasta que genere algo que pudo resolver (no es muy inteligente)
loop = TRUE
j = 0

# Hay que definir jj la primera vez y dsp comentar la definición
# jj = c()

while (loop) {
  {# Setup
    setup <- TRUE
    
    turnos <- 3*5  # hay 3 "slots" por dia en la semana posibles donde podría haber un TP
    
    cant_turnos_tp <- 10  # turnos de tp que se reparten entre esos slots
    
    
    ayudantes_por_turno <- 5  # personas por turno
    turnos_a_cubrir <-cant_turnos_tp * ayudantes_por_turno  # = 12 shifts a cubrir
    
    
    turnos_por_ayudante <- 3  # turnos por persona que podemos pagar/pedir
    cant_ayudantes <- ceiling(turnos_a_cubrir / turnos_por_ayudante) # debe haber al menos 4 personas
    
    while(setup){
      # Disponibilidad
      p <- 0.7
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
    mas_ocupado <- resamp(which(sum_disp_docente %in% min(sum_disp_docente[sum_disp_docente > 0])), 1)
    # Buscar al turno menos cubierto de los que ese docente puede cubrir
    sum_cobertura_turnos <- apply(avail, 2, sum)
    asd <- sum_cobertura_turnos * avail[mas_ocupado,]  # cobertura_turnos_del_ayudante_mas_ocupado
    menos_cubierto <- resamp(which(asd %in% min(asd[asd > 0])), 1)  # turno del docente mas ocupado con menos cobertura
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

# Y esto es solo para ir contando
j
jj <- c(j, jj)
length(jj)/sum(jj)  # proporcion de 'inicializaciones' que pudo resolver