import numpy as np
import random

###############################################################################
####### Creo una matriz de disponibilidad docente al azar para testear
###############################################################################
N_turnos=10
N_docentes=15
N_turnos_por_docente=4
N_docentes_por_turno=6

turnos=np.zeros(N_turnos)
docentes=np.ones(N_docentes*N_turnos).reshape(N_turnos,N_docentes)

for docente in range(N_docentes):
    turnos_libres=4+np.random.randint(1,3)
    horarios=[]
    for i in range(N_turnos):
        horarios.append(i)
    random.shuffle(horarios)
    for i in range(N_turnos-turnos_libres):
        docentes[horarios[i],docente]=0
###############################################################################
####### Función que asigna los docentes
###############################################################################
def designacion(docentes,N_turnos_por_docente):
    N_docentes=len(docentes[0,:])
    N_turnos=len(docentes[:,0])
    N_docentes_por_turno=N_turnos_por_docente*N_docentes/N_turnos
    docentes_matriz=docentes.copy()    
    asignacion_turnos=np.ones(N_turnos)*N_docentes_por_turno
    asignacion_docente=np.ones(N_docentes)*N_turnos_por_docente
    asignacion=np.zeros(N_docentes*N_turnos).reshape(N_turnos,N_docentes)
    for i in range(N_docentes*N_turnos_por_docente):          
        disp_turnos=docentes_matriz.sum(axis=1)/asignacion_turnos
        disp_docentes=docentes_matriz.sum(axis=0)/asignacion_docente
        docente_ocupado=np.nanargmin(disp_docentes)
        turnos_docente_ocupado=disp_turnos*docentes_matriz[:,docente_ocupado]
        turno_asignado=np.nanargmin(abs(turnos_docente_ocupado-np.nanmin(turnos_docente_ocupado[np.nonzero(turnos_docente_ocupado)])))
        
        asignacion[turno_asignado,docente_ocupado]+=1
        docentes_matriz[turno_asignado,docente_ocupado]=0
        asignacion_turnos[turno_asignado]-=1
        asignacion_docente[docente_ocupado]-=1

    if sum(asignacion_turnos<0)>0:
        key="Falta(n) "+str(int(np.max(asignacion_turnos)))+" docente(s) que quiera(n) el turno "+str(int(np.argmax(asignacion_turnos)))
    else:
        key="Todo joya"
    print(key)
    return(asignacion,asignacion_turnos,asignacion_docente)
###############################################################################
####### Aplico la función
###############################################################################
asignacion,asignacion_turnos,asignacion_docente=designacion(docentes,4)