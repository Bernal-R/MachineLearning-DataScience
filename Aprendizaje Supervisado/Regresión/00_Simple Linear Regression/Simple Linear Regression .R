#Regresion Lineal Simple, para calcular la cantidad de grasa en personas con una edad espec??fica.

#==================== 1.Importar Data Set ==================== 
grasas <- read.table("http://www.uam.es/joser.berrendero/datos/EdadPesoGrasas.txt", header = TRUE)

#==================== 2.Corelaci??n lineal de datos ==================== 
cor(grasas)    #Ver la corelaci??n nume??ricamente
pairs(grasas)  #Ver la corelaci??n gr??ficamente

#==================== 3.Calculo de regresion ==================== 
regresion <- lm(grasas ~ edad, data = grasas) 
summary(regresion) #Estad??sticas descriptivas de la regresi??n

#==================== 4.Generar gr??fico ==================== 
plot(grasas$edad, grasas$grasas, xlab = "Edad", ylab = "Grasas") #Genera gr??fico
abline(regresion, col = "green") #Genera linea de regresi??n

#==================== 5.Generar predicciones ==================== 
nuevas.edades <- data.frame(edad = seq(30, 50)) #Armamos un data set con las edades que queremos predecir
prediccion <- predict(regresion, nuevas.edades) #Realizamos la redicci??n  

#==================== *Alternativa para generar prediccione*  ==================== 
#Nota este paso puede sustituirse por el anterior, es solamente otra forma de hacerlo.
predict(regresion, nuevas.edades, level = 0.95, interval = "prediction") #Genera la predicci??n con un 95% de seguridad, para ello genera la predicci??n, el valor m??nimo y m??ximo que podr??a tomar la predicci??n
boxplot(nuevas.edades) #Diagrama de caja para ver si existen valores at??picos que afecten el m??nimo y m??ximo

#==================== 6.Generar predicciones ==================== 
resultado <- data.frame(nuevas.edades,prediccion) #Se crea data frame para las edades deseadas y prediccion respectiva para cada una
names(resultado) <- c("Edades a predecir", "prediccion") #Se agregan nombres
