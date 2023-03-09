testotal=function(comparador,datos){
  
  for (i in 1:nrow(comparador)){
    
   for(j in 1:ncol(datos)){
     
     if(comparador[i,1]==colnames(datos[,j])){

       media=comparador[i,2]
       desviacion=comparador[i,3]
       numero=comparador[i,4]
       testm(media,desviacion,numero,datos[,j])
       }
 
  }
 }
}

#Test para comparar dos grupos de valores
#media= valor medio con el que se quiere comparar
#desviacion= desviación estándar de los valores utilizados para obtener la media anterior
#numero= numero de datos utilizados para calcular la media y desviación estándar anteriores
#datos= matriz de datos que se quiere comparar con los valores anteriores

testm = function(media,desviacion,numero,datos){
  library(dplyr)
  
  for (i in 1:ncol(datos)){
  # Si el número de datos a comparar proviene de un número mayor o igual a 30 en ambos casos entonces
  
  datos2=filter(datos,abs(datos[[i]])<mean(na.exclude(datos[[i]]))+2*sd(na.exclude(datos[[i]])) & abs(datos[[i]])>mean(na.exclude(datos[[i]]))-2*sd(na.exclude(datos[[i]])))
  datos2=data.matrix(datos2[,i])
  datos2=na.omit(datos2)

  
  
  
  
  print(colnames(datos[,i]))
  if(numero>=30 && length(datos2)>=30){
    print(paste0("Como n1 y n2 son >=30 se utiliza un t-test normal, n inicial= ",numero, " n de datos= ",length(datos2)))
    tcal=(abs(media-mean(datos2))/sqrt((desviacion^2/numero)+((sd(datos2))^2/length(datos2))))
    df=numero+length(datos2)-2
    ttab=abs(qt(0.05/2,data.matrix(df)))
    if(tcal<=ttab){
      print(paste0("media datos matriz= ",mean(datos2)))
      print(paste0("desviación estándar datos matriz= ",sd(datos2)))
      print(paste0("tcal= ",tcal))
      print(paste0("grados de libertad matriz(df)= ",df))
      print(paste0("ttab= ",ttab))
      print(paste0("las medias son estadísticamente IGUALES pues ",tcal," <= ",ttab))
      print("------------------------------------------------------------")
    } else{
      print(paste0("media datos matriz= ",mean(datos2)))
      print(paste0("desviación estándar datos matriz= ",sd(datos2)))
      print(paste0("tcal= ",tcal))
      print(paste0("grados de libertad matriz(df)= ",df))
      print(paste0("ttab= ",ttab))
      print(paste0("las medias son estadísticamente DIFERENTES pues ",tcal," > ",ttab))
      print("------------------------------------------------------------")
    }
  }
  
  
  #Si el número de datos a comparar es en algunos de los casos <30 entonces
  #Hacemos un test-F para comprobar si las S1^2=S2^2
  #Recuerda que en el numerador va el valor más grande de S, que los grados de libertad son
  # el número de datos usados para obtener esas S menos uno (n-1) y que en la formula qf primero
  # se pone el valor de n de la S en el numerador(más grande) y luego se pone el valor de n de la
  # S en el denominador (más pequeña)
  
  if(numero < 30 | length(datos2) < 30){
    
    print(paste0("Como n1 o n2 son < 30 se calcula el F-test, n inicial= ",numero," n de datos = ",length(datos2)))
    Fcal=max(desviacion^2,(sd(datos2))^2)/min(desviacion^2,(sd(datos2))^2)
    
    if(max(desviacion^2,(sd(na.exclude(datos2)))^2)==(desviacion^2)){
      Ftabu=qf(0.975,data.matrix(numero),length(datos2))
    } else{
      Ftabu=qf(0.975,length(datos2),data.matrix(numero))
    }
    
    if(Fcal<=Ftabu){
      print(paste0("El test F demuestra que las desviaciones estándar son IGUALES pues ",Fcal," <= ",Ftabu))
      print("Se utiliza el t-test de Student modificado con agrupación de las desviaciones estándar")
      desp=sqrt((((numero-1)*desviacion^2)+((length(datos2)-1))*(sd(datos2))^2)/(numero+length(datos2)-2))
      tcal=((abs(media-mean(datos2))/desp)*sqrt((numero*length(datos2))/(numero+length(datos2))))
      df=numero+length(datos2)-2
      ttab=abs(qt(0.05/2,data.matrix(df)))
      if(tcal<=ttab){
        
        print(paste0("media datos matriz= ",mean(datos2)))
        print(paste0("desviación estándar datos matriz= ",sd(datos2)))
        print(paste0("tcal= ",tcal))
        print(paste0("grados de libertad matriz(df)= ",df))
        print(paste0("ttab= ",ttab))
        print(paste0("las medias son estadísticamente IGUALES pues ",tcal," <= ",ttab))
        print("------------------------------------------------------------")
      } else{
        
        print(paste0("media datos matriz= ",mean(datos2)))
        print(paste0("desviación estándar datos matriz= ",sd(datos2)))
        print(paste0("tcal= ",tcal))
        print(paste0("grados de libertad matriz(df)= ",df))
        print(paste0("ttab= ",ttab))
        print(paste0("las medias son estadísticamente DIFERENTES pues ",tcal," > ",ttab))
        print("------------------------------------------------------------")
        
      }
      
    } else {
      print(paste0("El test F demuestra que las desviaciones estándar son DIFERENTES pues ",Fcal," > ",Ftabu))
      print("Se utiliza el t-test de Cochran")
      tcal=((abs(media-mean(datos2)))/sqrt((desviacion^2/numero)+((sd(datos2))^2/length(datos2))))
      t1=abs(qt(0.05/2,data.matrix(numero)-1))
      t2=abs(qt(0.05/2,length(datos2)-1))
      ttab=((t1*(desviacion^2/numero))+t2*((sd(datos2))^2/length(datos2)))/((desviacion^2/numero)+((sd(datos2))^2/length(datos2)))
      df=numero+length(datos2)-2
      if(tcal<=ttab){

        print(paste0("media datos matriz= ",mean(datos2)))
        print(paste0("desviación estándar datos matriz= ",sd(datos2)))
        print(paste0("tcal= ",tcal))
        print(paste0("grados de libertad matriz(df)= ",df))
        print(paste0("ttab= ",ttab))
        print(paste0("las medias son estadísticamente IGUALES pues ",tcal," <= ",ttab))
        print("------------------------------------------------------------")
      }else{


        
        print(paste0("media datos matriz= ",mean(datos2)))
        print(paste0("desviación estándar datos matriz= ",sd(datos2)))
        print(paste0("tcal= ",tcal))
        print(paste0("grados de libertad matriz(df)= ",df))
        print(paste0("ttab= ",ttab))
        print(paste0("las medias son estadísticamente DIFERENTES pues ",tcal," > ",ttab))
        print("------------------------------------------------------------")
      }
      
    }
    
  }
  

  }
 
}