
#genera los numeros aleatorios
ale=sample(1:10,10,replace=F)
# se separan en  2 vectores "x" y "y"
vx=ale[1:5]
vy=ale[6:10]
#presenta los numeros en una matriz
aux=c(vx,vy)
dim(aux)<-c(5,2)
print("Generados aleatoriamente")
print(aux)
#funcion que determina la primera fila multiplicados dan 360 y las segunda sumados dan 36
bolas(vx,vy)




#funcion para comprobar si la suma es igual a 36 y la multiplicación a 360 devuelve 1 caso contrario 0

comprobar<- function(vx, vy){
  sum=vy[1]+vy[2]+vy[3]+vy[4]+vy[5]
  if(sum==36){
    mult=vx[1]*vx[2]*vx[3]*vx[4]*vx[5]
    if(mult==360){
      return(1)
    }
  }
  return(0)
}

#funcion que realiza el torneo , devuelve un solo vector con los genes mutados 
#toma en cuenta que los numeros deben ser menores y divisibles para 360 para colocarlo en X
# para colocar en Y deben ser mayores sin importar si son divisibles o no para 360

mutacion<- function(vx, vy){
  for (i in 1:5) {
    c=i+1
    if(i==5){
      c=1
    }
    if (360%%vx[i]==0) {
      if (360%%vy[c]==0) {
        if (vy[c]<vx[i]) {
          aux=vx[i]
          vx[i]=vy[c]
          vy[c]=aux
        }
      }
    }else{
      aux=vx[i]
      vx[i]=vy[c]
      vy[c]=aux
    }
  }
 return(c(vx,vy[2:5],vy[1]))
}



bolas<- function(vx, vy){
  cont=0
  ban=comprobar(vx,vy)
  if (ban!=1){
    #solo se rompera este while cuando ban=1 que seria cuando haya encontrado la solucion
    while(ban!=1){
      #llamada de la funcion mutación
      aux= mutacion(vx,vy)
      vx=aux[1:5]
      vy=aux[6:10]
      ban=comprobar(vx,vy)
      aux=c(vx,vy)
      dim(aux)<-c(5,2)
      print("muta los dos especimenes")
      print(aux)
      if (ban!=1){
        
        #aqui se intercambia los genes para nuevo x(primero dex y 4 de y) y nuevo y(primero de y y 4 de x)
        aux=c(vx[1],vy[2:5])
        vy=c(vy[1],vx[2:5])
        vx=aux
        aux=c(vx,vy)
        dim(aux)<-c(5,2)
        print("se combinan los dos especimenes")
        print(aux)
        cont=cont +1
      }
      ban=comprobar(vx,vy)
    }
  }
  aux=c(vx,vy)
  dim(aux)<-c(5,2)
  
  print(cat("la SOLUCION es en ",cont," Iteraciones"))
  return(aux)
}
