
# Postwork sesión 8. Análisis de la Inseguridad Alimentaria en México
"
#### OBJETIVO

- Realizar un análisis estadístico completo de un caso 
- Publicar en un repositorio de Github el análisis y el código empleado 

#### REQUISITOS

- Haber realizado los works y postworks previos 
- Tener una cuenta en Github o en RStudioCloud

#### DESARROLLO

Un centro de salud nutricional está interesado en analizar estadísticamente y probabilísticamente los 
patrones de gasto en alimentos saludables y no saludables en los hogares mexicanos con base en su nivel 
socioeconómico, en sí el hogar tiene recursos financieros extra al ingreso y en sí presenta o no inseguridad alimentaria.
Además, está interesado en un modelo que le permita identificar los determinantes socioeconómicos de la inseguridad alimentaria.

La base de datos es un extracto de la Encuesta Nacional de Salud y Nutrición (2012) levantada por el Instituto
Nacional de Salud Pública en México. La mayoría de las personas afirman que los hogares con menor nivel socioeconómico
tienden a gastar más en productos no saludables que las personas con mayores niveles socioeconómicos y que esto, 
entre otros determinantes, lleva a que un hogar presente cierta inseguridad alimentaria.

La base de datos contiene las siguientes variables:
- nse5f (Nivel socieconómico del hogar): 1 'Bajo', 2 'Medio bajo', 3 'Medio', 4 'Medio alto', 5 'Alto'
- area (Zona geográfica): 0 'Zona urbana', 1 'Zona rural'
- numpeho (Número de persona en el hogar)
- refin (Recursos financieros distintos al ingreso laboral): 0 'no', 1 'sí'
- edadjef (Edad del jefe/a de familia)
- sexoje (Sexo del jefe/a de familia): 0 'Hombre', 1 'Mujer'
- añosedu (Años de educación del jefe de familia)
- ln_als (Logarítmo natural del gasto en alimentos saludables)
- ln_alns (Logarítmo natural del gasto en alimentos no saludables)
- IA (Inseguridad alimentaria en el hogar): 0 'No presenta IA'', 1 'Presenta IA' "

"NOTA: Todo tu planteamiento deberá estár correctamente desarrollado y deberás analizar e interpretar todos 
tus resultados para poder dar una conclusión final al problema planteado."


"1) Plantea el problema del caso"
"----OBJETIVOS----
+ Analizar estadistica y probabilisticamnte los patrones de gasto en alimentos saludables y no saludables en los hogares mexicanos
con base en

    - El nivel socieconomico
    - Los recursos financieros extra al ingreso
    - La presencia de inseguridad alimentaria o no.

+ Elaborar un modelo que permita identificar los determinantes socioeconómicos de la inseguridad alimentaria, especificamente
validar la hipotesis  que la mayoría de las personas afirman que los hogares con menor nivel socioeconómico
tienden a gastar más en productos no saludables que las personas con mayores niveles socioeconómicos y que esto, 
entre otros determinantes, lleva a que un hogar presente cierta inseguridad alimentaria"

"2) Realiza un análisis descriptivo de la información"
install.packages("ggplot2")
library(ggplot2)

df.s8 <- read.csv("https://raw.githubusercontent.com/beduExpert/Programacion-R-Santander-2022/main/Sesion-08/Postwork/inseguridad_alimentaria_bedu.csv")
#Se valida la cantidad de registros y numero de variables
str(df.s8) # ---> se cuentan con 40809 registros de 10 variables
# Se toma un extracto de los primeros datos del dataframe
head(df.s8)
# Se utiliza Summary para ver el resumen de algunos datos y algunos estadisticos
summary(df.s8)
# En este se observa que los siguienes campos tienen datos NA --> ln_als, ln_alns, edadjef y sexojef
sum(complete.cases(df.s8)) #---> La cantidad de datos son casi la mitad de los que originalmente trae
df.s8.clean <- df.s8[complete.cases(df.s8),]
str(df.s8.clean)
summary(df.s8.clean)

#----- LIMPIEZA DE DATOS ---------
df.s8.clean$nse5f <- factor(df.s8.clean$nse5f,
                            levels = 1:5,
                            labels = c("Bajo", "Medio bajo", "Medio", "Medio alto", "Alto"),
                            ordered = TRUE)

df.s8.clean$area <- factor(df.s8.clean$area,
                           levels = 0:1,
                           labels = c("Zona urbana", "Zona rural"),
                           ordered = TRUE)
df.s8.clean$refin <- factor(df.s8.clean$refin,
                            levels = 0:1,
                            labels = c("no", "sí"))
df.s8.clean$sexojef <- factor(df.s8.clean$sexojef,
                              levels = 0:1,
                              labels = c("Hombre", "Mujer"))
df.s8.clean$IA <- factor(df.s8.clean$IA,
                         levels = 0:1,
                         labels = c("No presenta IA", "Presenta IA"))

summary(df.s8.clean)


" -----> Marco <------ "
#Algunas conclusiones importantes:

hist(df$numpeho) # Número de personas en el hogar: En Promedio es de 4 tomando la Mediana como medida central
hist(df$edadjef) # Edad del jefe/a de familia: En Promedio es de 46 tomando la Mediana como medida central
hist(df$añosedu) # Años de educación del jefe de familia: En Promedio son 10.9 años
hist(df$ln_als)  # Logarítmo natural del gasto en alimentos saludables: En Promedio es 6.192 el gasto
hist(df$ln_alns) # Logarítmo natural del gasto en alimentos no saludables: En Promedio es 4.119 el gasto 
Marcos says:#Respecto del Nivel Socioeconómico
(table(df$nse5f,df$area))
plot(df$nse5f,df$area)   # Las zonas rurales tienen un Nivel socieconómico más Bajo en comparación con las Zonas Urbanas
(table(df$nse5f,df$numpeho))
plot(df$nse5f,df$numpeho)# Independientemente del Nivel Socioeconómico del Hogar, en general los núcleos familiares oscilan de 3 a 5 personas
(table(df$nse5f,df$refin))
plot(df$nse5f,df$refin)  ## Hogares con un Nivel Socioeconómico bajo tienden a obtener otros recursos financieros diferentes al ingreso laboral
(table(df$nse5f,df$edadjef))
plot(df$nse5f,df$edadjef)## En Hogares con un Nivel Socioeconómico Alto se es Jefe de Familia por un menor tiempo en comparación con uno bajo
(table(df$nse5f,df$sexoje))
plot(df$nse5f,df$sexoje) # Independientemente del Nivel Socioeconómico, los Jefes de Familia son Hombres en 80% de los casos
(table(df$nse5f,df$añosedu))
plot(df$nse5f,df$añosedu)# La educación influye para tener un Nivel Socioeconómico Alto en el Hogar
(table(df$nse5f,df$ln_als))
plot(df$nse5f,df$ln_als) # Los Hogares con un Nivel Socioeconómico Alto gastan más en alimentos saludables que un hogar con nivel bajo
(table(df$nse5f,df$ln_alns))
plot(df$nse5f,df$ln_alns)# Los Hogares con un Nivel Socioeconómico Alto también gastan más en alimentos no saludables
(table(df$nse5f,df$IA))
plot(df$nse5f,df$IA)     # Los Hogares con un Nivel Socioeconómico Bajo presentan Inseguridad alimentaria en el hogar 
Marcos says:#Respecto del Área
(table(df$area,df$nse5f))
plot(df$area,df$nse5f)   # En las zonas urbanas es posible tener un Nivel socieconómico más Alto
(table(df$area,df$numpeho))
plot(df$area,df$numpeho) # Tanto en Zonas rurales como urbanas los núcleos familiares oscilan de 3 a 5 personas
(table(df$area,df$refin))
plot(df$area,df$refin)   # El 80% de la población solo obtiene recursos de su ingreso laboral y en zonas rurales obtienen ligeramente recursos adicionales de otras fuentes
(table(df$area,df$edadjef))
plot(df$area,df$edadjef) ## En zonas rurales se es Jefe de Familia por más tiempo
(table(df$area,df$sexoje))
plot(df$area,df$sexoje)  # En zonas urbanas es mayor el número de mujeres que son jefas de familia en comparación con zonas rurales
(table(df$area,df$añosedu))
plot(df$area,df$añosedu) # En zonas urbanas hay más posibilidades de continuar con estudios educativos
(table(df$area,df$ln_als))
plot(df$area,df$ln_als)  # En zonas urbanas gastan más en alimentos saludables en comparación con una zona rural
(table(df$area,df$ln_alns))
plot(df$area,df$ln_alns) # En zonas urbanas también gastan más en alimentos no saludables en comparación con una zona rural
(table(df$area,df$IA))
plot(df$area,df$IA)      # La zonas rurales por lo general presentan Inseguridad alimentaria en el hogar 

#Respecto del Número de Personas del Hogar
(table(df$refin,df$nse5f))
plot(df$refin,df$nse5f)   # En las zonas urbanas es posible tener un Nivel socieconómico más Alto
(table(df$refin,df$area))
plot(df$refin,df$area) # Tanto en Zonas rurales como urbanas los núcleos familiares oscilan de 3 a 5 personas
(table(df$refin,df$numpeho))
plot(df$refin,df$numpeho)   # El 80% de la población solo obtiene recursos de su ingreso laboral y en zonas rurales obtienen ligeramente recursos adicionales de otras fuentes
(table(df$refin,df$edadjef))
plot(df$refin,df$edadjef) ## En zonas rurales se es Jefe de Familia por más tiempo
(table(df$refin,df$sexoje))
plot(df$refin,df$sexoje)  # En zonas urbanas es mayor el número de mujeres que son jefas de familia en comparación con zonas rurales
(table(df$refin,df$añosedu))
plot(df$refin,df$añosedu) # En zonas urbanas hay más posibilidades de continuar con estudios educativos
(table(df$refin,df$ln_als))
plot(df$refin,df$ln_als)  # En zonas urbanas gastan más en alimentos saludables en comparación con una zona rural
(table(df$refin,df$ln_alns))
plot(df$refin,df$ln_alns) # En zonas urbanas también gastan más en alimentos no saludables en comparación con una zona rural
(table(df$refin,df$IA))
plot(df$refin,df$IA)      # La zonas rurales por lo general presentan Inseguridad alimentaria en el hogar 


"3) Calcula probabilidades que nos permitan entender el problema en México"

"4) Plantea hipótesis estadísticas y concluye sobre ellas para entender el problema en México"

"5) Estima un modelo de regresión, lineal o logístico, para identificiar los determinanres de la inseguridad alimentaria en México"

"6) Escribe tu análisis en un archivo README.MD y tu código en un script de R y publica ambos en un repositorio de Github."




