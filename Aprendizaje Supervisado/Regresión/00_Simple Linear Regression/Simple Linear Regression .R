#Regresion Lineal Simple, para calcular la cantidad de grasa en personas con una edad especIfica.

#==================== 1.Importar Data Set ==================== 
grasas <- read.table("http://www.uam.es/joser.berrendero/datos/EdadPesoGrasas.txt", header = TRUE)

#==================== 2.Corelacion lineal de datos ==================== 
cor(grasas)    #Ver la corelacion numericamente
pairs(grasas)  #Ver la corelacion graficamente

#==================== 3.Calculo de regresion ==================== 
regresion <- lm(grasas ~ edad, data = grasas) 
summary(regresion) #Estadisticas descriptivas de la regresion

#==================== 4.Generar grafico ==================== 
plot(grasas$edad, grasas$grasas, xlab = "Edad", ylab = "Grasas") #Genera grafico
abline(regresion, col = "green") #Genera linea de regresion

#==================== 5.Generar predicciones ==================== 
nuevas.edades <- data.frame(edad = seq(30, 50)) #Armamos un data set con las edades que queremos predecir
prediccion <- predict(regresion, nuevas.edades) #Realizamos la rediccion  

#==================== *Alternativa para generar prediccione*  ==================== 
#Nota este paso puede sustituirse por el anterior, es solamente otra forma de hacerlo.
predict(regresion, nuevas.edades, level = 0.95, interval = "prediction") #Genera la prediccion con un 95% de seguridad, para ello genera la predicci??n, el valor m??nimo y m??ximo que podr??a tomar la predicci??n
boxplot(nuevas.edades) #Diagrama de caja para ver si existen valores atipicos que afecten el minimo y maximo

#==================== 6.Generar predicciones ==================== 
resultado <- data.frame(nuevas.edades,prediccion) #Se crea data frame para las edades deseadas y prediccion respectiva para cada una
names(resultado) <- c("Edades a predecir", "prediccion") #Se agregan nombres
