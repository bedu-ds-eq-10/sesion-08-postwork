# Accedemos a la base de datos a traves de la función read.csv(). Llamamos a esta base "df". 

df <- read.csv("https://raw.githubusercontent.com/beduExpert/Programacion-R-Santander-2022/main/Sesion-08/Postwork/inseguridad_alimentaria_bedu.csv")

class(df) # Qué tipo de objeto estamos trabajando: data.frame
str(df) # El data frame cuenta con 40 809 observaciones de 10 variables. 
dim(df) # dimensión: 40809 filas, 10 columnas.
View(df) # Visualizamos los datos: observamos datos NA, int, num, etc. 

# Debemos definir adecuadamente cada tipo de variable. Esto lo realizamos mediante la 
# función factor().

summary(df)

df$nse5f <- factor(df$nse5f, levels = 1:5,
                   labels = c("Bajo", "Medio bajo", "Medio", "Medio alto", "Alto"),
                   ordered = TRUE)

df$area <- factor(df$area, levels = 0:1,
                  labels = c("Zona urbana", "Zona rural"))
              
df$refin <- factor(df$refin, levels = 0:1,
                   labels = c("no", "sí"))

df$sexojef<- factor(df$sexojef, levels = 0:1,
                    labels = c("Hombre", "Mujer"))

df$IA<- factor(df$IA, levels = 0:1,
               labels = c("No presenta IA", "Presenta IA"))

summary(df) # observamos, la redifinición de las variables. 

# Descripciónde las variables: 
#nse5f (Nivel socieconómico del hogar): 1 "Bajo", 2 "Medio bajo", 3 "Medio", 4 "Medio alto", 5 "Alto"
#area (Zona geográfica): 0 "Zona urbana", 1 "Zona rural"
#numpeho (Número de persona en el hogar)
#refin (Recursos financieros distintos al ingreso laboral): 0 "no", 1 "sí"
#edadjef (Edad del jefe/a de familia)
#sexoje (Sexo del jefe/a de familia): 0 "Hombre", 1 "Mujer"
#añosedu (Años de educación del jefe de familia)
#ln_als (Logarítmo natural del gasto en alimentos saludables)
#ln_alns (Logarítmo natural del gasto en alimentos no saludables)
#IA (Inseguridad alimentaria en el hogar): 0 "No presenta IA", 1 "Presenta IA"

# Palteamos algunas hipotesis a responder:

# El gasto de alimentos saludables está relacionado positivamente con el nivel socioeconomico. 
# El gasto de alimentos no saludables está relacionado positivamente con el nivel socioeconomico. 
# El gasto de alimentos saludabes está relacionado positivamente con los años de educación del jefe/jefa de familia. 
# El gasto de alimentos no saludabes está relacionado positivamente con los años de educación del jefe/jefa de familia. 
# La inseguridad alimentaria se relaciona positivamente con Ln_als y Ln_alns.  
# El area (rural y urbana) se relaciona positivamente con Ln_als y Ln_alns. 


#Analisis Grafico de los datos categoricos. 

plot(x = df$nse5f, y = df$IA)# Relacion nse vs IA. Observamos que mientras más bajo es el nivel socioeconomico 
# es más notorio la presencia de AI.
plot(x = df$IA, y = df$numpeho) # Observamos la relación entre IA y Numero de personas en el hogar. Box plot. 
# 
plot(x = df$IA, y = df$edadjef)# Relación entre edadjef y IA. Box plot. 

plot(x = df$nse5f, y = df$area, col = c("#99cc99", "#cc9999"))# Relacion nse vs area. Observamos que 
# el nivel socioeconomico es menor para zonas rurales. 

#tab_1 <- table(df$numpeho, df$IA)
#tab_2 <- prop.table(tab_1)
#barplot(tab_2) # Numero de integrantes de la familia vs IA

tabla_1 <- table(df$nse5f, df$IA)
tabla_1
plot(tabla_1, col = c("red", "blue")) # Observamos que parece haber una relación entre 
# el nivel socioeconomico y la inseguridad alimenticia. 

chisq.test(tabla_1) #Con un Chi-squared alto y un p-value menor que 0.05, rechazamos la hipotesis nula 
# y concluimos que NSE e IA tienen una relación significativa. 


tabla_3 <- table(df$numpeho, df$IA)
tabla_3
plot(tabla_3) # Observamos que parece haber una relación entre el numero de personas en el hogar 
# y la presencia de IA. 

chisq.test(tabla_3) #Con un Chi-squared alto y un p-value menor que 0.05, rechazamos la hipotesis nula 
# y concluimos que NUMERO DE PERSONAS e IA tienen una relación significativa. 

tabla_4 <- table(df$edadjef, df$IA)
tabla_4
plot(tabla_4) # La grafica de barras no parece dar señas de una relación entre la edad del jefe/jefa de 
#familia y la presencia de IA. 

chisq.test(tabla_3) # la prueba chisq no es clara en este punto. 


tabla_5 <- table(df$nse5f, df$area)
tabla_5
plot(tabla_5) # La grafica señala una relación entre el nivel socioeconomico y el area. Mientras más bajo es
#el nivel socioeconomico más concentración de población hay en zonas rurales. 

chisq.test(tabla_5) #Con un Chi-squared alto y un p-value menor que 0.05, rechazamos la hipotesis nula 
# y concluimos que EL NSE y  el AREA tienen una relación significativa. 

library(ggplot2)
library(dplyr)
theme_set(theme_bw())

#El gasto de alimentos saludables está relacionado positivamente con el nivel socioeconomico. 
ggplot(data = df) +
  geom_histogram(mapping = aes(x = df$ln_als, fill = df$nse5f),
                 colour = "black") +
  scale_fill_viridis_d() +
  facet_wrap(~ nse5f) #Observamos que las distribucione son similares, y que el ingreso 
#bajo presenta ln_als más bajo que los niveles socioeconomicos altos. 

ggplot(data = df) +
  geom_freqpoly(mapping = aes(x = df$ln_als, y = ..density.., colour = factor(df$nse5f),),
                binwidth = 10,
                size = 0.8) #Lo anterior es más claro en la siguiente grafica. 

# Conclusion: mientras más alto es el nivel socioeconomico, más gasto en alimentos saludables se realiza en el hogar. 



#El gasto de alimentos no saludables está relacionado positivamente con el nivel socioeconomico. 
ggplot(data = df) +
  geom_histogram(mapping = aes(x = df$ln_alns, fill = df$nse5f),
                 colour = "black") +
  scale_fill_viridis_d() +
  facet_wrap(~ nse5f) #Observamos que las distribucione son similares, y que el ingreso 
#bajo presenta ln_alns más bajo que los niveles socioeconomicos altos. 

ggplot(data = df) +
  geom_freqpoly(mapping = aes(x = df$ln_alns, y = ..density.., colour = factor(df$nse5f),),
                binwidth = 10,
                size = 0.8) #Lo anterior es más claro en la siguiente grafica. 

# Conclusion: mientras más bajo es el nivel socioeconomico, más gasto en alimentos saludables se realiza en el hogar. 


# El gasto de alimentos saludabes está relacionado positivamente con los años de educación del jefe/jefa de familia. 

ggplot(data = df) +
  geom_histogram(mapping = aes(x = df$ln_als, fill = df$nse5f),
                 colour = "black") +
  scale_fill_viridis_d() +
  facet_wrap(~ añosedu) #Observamos que las distribucione son similares. 

ggplot(data = df) +
  geom_freqpoly(mapping = aes(x = df$ln_als, y = ..density.., colour = factor(df$añosedu),),
                binwidth = 10,
                size = 0.8) #Lo anterior es más claro en la siguiente grafica. 

# Conclusion: mientras más alto es el numero de años de estudio más gasto en alimentos saludables se realiza en el hogar. 

# El gasto de alimentos no saludabes está relacionado positivamente con los años de educación del jefe/jefa de familia. 

ggplot(data = df) +
  geom_histogram(mapping = aes(x = df$ln_als, fill = df$nse5f),
                 colour = "black") +
  scale_fill_viridis_d() +
  facet_wrap(~ añosedu) #Observamos que las distribucione son similares. 

ggplot(data = df) +
  geom_freqpoly(mapping = aes(x = df$ln_als, y = ..density.., colour = factor(df$añosedu),),
                binwidth = 10,
                size = 0.8) #Lo anterior es más claro en la siguiente grafica. 

# Conclusion: mientras más alto es el numero de años de estudio más gasto en alimentos saludables se realiza en el hogar. 


ggplot(data = df) +
  geom_histogram(mapping = aes(x = df$ln_alns, fill = df$nse5f),
                 colour = "black") +
  scale_fill_viridis_d() +
  facet_wrap(~ añosedu) #Observamos que las distribucione son similares. 

ggplot(data = df) +
  geom_freqpoly(mapping = aes(x = df$ln_alns, y = ..density.., colour = factor(df$añosedu),),
                binwidth = 10,
                size = 0.8) #Lo anterior es más claro en la siguiente grafica. 
# Conclusion: mientras más bajo es el numero de años de estudio más gasto en alimentos no saludables se realiza en el hogar. Nota: Los graficos no 
# son lo suficientemente claros. 


# La inseguridad alimentaria se relaciona positivamente con Ln_als y Ln_alns. 

ggplot(data = df) +
  geom_histogram(mapping = aes(x = df$ln_als, fill = df$IA),
                 colour = "black") +
  scale_fill_viridis_d() +
  facet_wrap(~ nse5f) #Observamos que las distribucione son similares. 
# Observamos que mientras mayor sea el gasto en alimentos saludables mayor es la ausencia de inseguridad 
# alimenticia. 

ggplot(data = df) +
  geom_histogram(mapping = aes(x = df$ln_alns, fill = df$IA),
                 colour = "black") +
  scale_fill_viridis_d() +
  facet_wrap(~ nse5f) #Observamos que las distribucione son similares. 
# Observamos que mientras mayor sea el gasto en alimentos no saludables mayor es la presencia de inseguridad 
# alimenticia.

# El area (rural y urbana) se relaciona positivamente con Ln_als y Ln_alns. 

ggplot(data = df) +
  geom_histogram(mapping = aes(x = df$ln_als, fill = df$area),
                 colour = "black") +
  scale_fill_viridis_d() +
  facet_wrap(~ area) #Observamos que las distribucione son similares. Es muy notorio
# el hecho de que en las zonas urbanas se realiza más gasto en alimentos saludables que en 
# las zonas rurales. 

ggplot(data = df) +
  geom_freqpoly(mapping = aes(x = df$ln_als, y = ..density.., colour = factor(df$area),),
                binwidth = 10,
                size = 0.8) # Es esta grafica es más notoria la conclusion anterior. 

ggplot(data = df) +
  geom_freqpoly(mapping = aes(x = df$ln_alns, y = ..density.., colour = factor(df$area),),
                binwidth = 10,
                size = 0.8) # Es esta grafica es más notoria la conclusion anterior. 


#Conclusion: En las area urbanas se presenta un mayor gasto de alimentos saludables. Las area rurales 
# gastan más en alimentos no saludbles. 


# Modelo predictivo

#ggplot(df, aes(x= df$nse5f, y=df$ln_als, color=df$nse5f)) + 
#  geom_point() + theme_light()

mod <- lm(df$ln_als ~ df$nse5f, data=df)
summary(mod)


#