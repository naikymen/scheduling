# Este se basa en dar prioridad al docente mas libre para tomar los turnos mejor cubiertos
# Es mucho peor que el otro
# Creo que pasa porque cuando un turno queda cubierto, se impide "aprovechar" a los docentes con menor disponibilidad
# Entonces pasa que de repente hay docentes que solo pueden ir a 1 turno de los que todavía tienen lugar

resamp <- function(x,...){if(length(x)==1) x else sample(x,...)} 

loop = TRUE
j = 0

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
    
    # Buscar al docente más libre
    mas_libre <- resamp(which(sum_disp_docente %in% max(sum_disp_docente)), 1)
    # Buscar al turno más cubierto de los que ese docente puede cubrir
    sub_cobertura <- sum_cobertura_turnos * avail[mas_ocupado,]  # cobertura_turnos_del_ayudante_mas_ocupado
    mejor_cubierto <- resamp(which(sub_cobertura %in% max(sub_cobertura)), 1)  # turno del docente mas ocupado con menos cobertura
    
    # Asignación
    print(paste("Asignando docente", mas_libre, "al turno", mejor_cubierto, sep = ' '))
    avail[mas_libre, mejor_cubierto] <- 0
    assigned[mas_libre, mejor_cubierto] <- 1
    print(assigned)
    
    
    
    # Ver si hay algún docente con suficientes turnos y volarlo de la lista
    sum_docente_asignado <- apply(assigned, 1, sum)
    docentes_completos <- which(sum_docente_asignado %in% turnos_por_ayudante)
    if(length(docentes_completos)!=0) avail[docentes_completos,] <- 0  # Volar a los docente/s
    print(c(sum_docente_asignado, sum(sum_docente_asignado)))
    
    # Ver si hay algún turno cubierto y volarlo de la lista
    sum_turnos_cubiertos <- apply(assigned, 2, sum)
    turnos_completos <- which(sum_turnos_cubiertos >= ayudantes_por_turno)
    if(length(turnos_completos)!=0) avail[,turnos_completos] <- 0  # Volar a los docente/s
    print(c(sum_turnos_cubiertos, sum(sum_turnos_cubiertos)))
    
    if(sum(avail) == 0 & i != turnos_a_cubrir) {
      print("Error: zero availability but loop is unfinished")
      print(i)
      break
    }
  }
  
  if(sum(assigned) == turnos_a_cubrir){
    print(avail_bup)
    print(avail_bup * assigned)
    
    sum_assign_docente <- apply(assigned, 1, sum)
    print(paste("Asignaciones docentes:", paste(sum_assign_docente, collapse = ' ')))
    sum_cobertura_turnos <- apply(assigned, 2, sum)
    print(paste("Cobertura de turnos:", paste(sum_cobertura_turnos, collapse = ' ')))
    
    print("Totalidad de turnos asignados")
    loop = FALSE
  }
  
  j = j + 1
  
}

j