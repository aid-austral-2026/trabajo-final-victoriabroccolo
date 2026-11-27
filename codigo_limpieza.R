
# MCD Austral - AID
# TP Final Broccolo

################################
# 1. INTRODUCCIÓN 
################################

# El Servicio Meteorológico Nacional publica diferentes mediciones que se realizan de temperaturas, viento, humedad, 
# precipitaciones, entre otras, en diferentes lugares del país. Se pueden visualizar estas mediciones en la página web
# oficial (https://datos.gob.ar/dataset/?_tags_limit=0).

# En el presente trabajo, se utilizaron los datos publicados sobre mediciones efectuadas en diferentes puntos del país
# en el último año, con el objetivo de analizar las diferencias de temperaturas en distintas partes de Argentina.

################################
# 2. MATERIALES Y MÉTODOS
################################

# 1. Importación de datos

library(readr)

datos_met <- read_csv("Datos crudos/servicio_meteorologico.csv",
  locale = locale(encoding = "ISO-8859-1"))

View(datos_met)

# 2. Limpieza

names(datos_met) # Para ver los nombres de los encabezados

names(datos_met)[1] <- "Estacion" # Quitar tilde

unique(datos_met$`Valor Medio de`) # Ver contenido dentro de la columna

# Reemplazar los nombres de las variables con caracteres extraños

library(dplyr)

datos_met <- datos_met %>%
  mutate(`Valor Medio de` = recode(
      `Valor Medio de`,
      "Temperatura m\u0087xima (grados C)" = "Temperatura maxima (grados C)",
      "Temperatura m\u0092nima (grados C)" = "Temperatura minima (grados C)",
      "Precipitaci\u0097n (mm)" = "Precipitacion (mm)",
      "Frecuencia de d\u0092as con Precipitaci\u0097n superior a 0.1 mm" = 
        "Frecuencia de dias con Precipitacion superior a 0.1 mm"))

# Pasar los meses a una columna

library(tidyr)

datos_met_long <- datos_met %>%
  pivot_longer(
    cols = Ene:Dic,
    names_to = "Mes",
    values_to = "Valor")

# 3. Análisis y tratamiento de datos faltantes

datos_met_long <- datos_met_long %>%
  mutate(Valor = na_if(Valor, "S/D"))

library(naniar)

#Cantidad de casos completos
sum(complete.cases(datos_met_long)) # 6717 casos completos de 7104

#Proporción de casos completos
prop_complete_case(datos_met_long) # 0.9455236

#Proporción de casos perdidos
prop_miss_case(datos_met_long) # 0.05447635

#heatmap: % de datos faltantes según Mes y Variable

na_heatmap <- datos_met_long %>%
  group_by(`Valor Medio de`, Mes) %>%
  summarise(porcentaje_na = mean(is.na(Valor)) * 100,
    .groups = "drop")

na_heatmap$Mes <- factor(
  na_heatmap$Mes,
  levels = c("Ene","Feb","Mar","Abr","May","Jun",
    "Jul","Ago","Sep","Oct","Nov","Dic"))

library(ggplot2)

ggplot(na_heatmap, 
       aes(x = Mes, 
           y = `Valor Medio de`, 
           fill = porcentaje_na)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c(
    name = "% NA") +
  labs(title = "Porcentaje de datos faltantes", 
       x = "Mes", 
       y = "Variable meteorológica") +
  theme_minimal()

# El análisis de datos faltantes mostró que la ausencia de información se concentra principalmente en las variables
# Velocidad del Viento, Precipitación y Frecuencia de días con precipitación. La inspección de la base reveló que 
# gran parte de estos faltantes corresponden a estaciones que no reportan dichas variables durante todo el período 
# considerado, lo que sugiere un patrón estructural más que faltantes aleatorios. Dado que el objetivo del trabajo es 
# descriptivo y exploratorio, se decidió conservar los valores faltantes como "NA" y excluirlos de los cálculos correspondientes, 
# evitando la imputación de información potencialmente sesgada.

################################
# 3. ANÁLISIS DESCRIPTIVO Y RESULTADOS
################################

class(datos_met)
summary(datos_met_long)

# 1. Descripción del dataset y métricas estadísticas

# El conjunto de datos se encuentra conformado por 74 estaciones de servicio metereológico de Argentina, las cuales están
# analizadas por 8 variables numéricas. Cada una de las mismas tiene valores para los 12 meses del año, excepto por aquellas con
# valores faltantes previamente mencionados.

# Para las variables del dataset, se analizan las métricas estadísticas descriptivas.

library(tidyverse)
library(pastecs)

str(datos_met_long) # La columna Valor es de tipo character (chr), no numérica. Por lo tanto convertimos la columna en numérica
                      #para poder calcular medidas estadísticas.

datos_met_long$Valor <- as.numeric(datos_met_long$Valor) 

# Resumen descriptivo por variable meteorológica:

resumen <- datos_met_long %>%
  group_by(`Valor Medio de`) %>%
  summarise(
    n = sum(!is.na(Valor)),
    faltantes = sum(is.na(Valor)),
    minimo = min(Valor, na.rm = TRUE),
    q1 = quantile(Valor, 0.25, na.rm = TRUE),
    mediana = median(Valor, na.rm = TRUE),
    media = mean(Valor, na.rm = TRUE),
    q3 = quantile(Valor, 0.75, na.rm = TRUE),
    maximo = max(Valor, na.rm = TRUE),
    sd = sd(Valor, na.rm = TRUE))

knitr::kable(resumen, digits = 2)

# |Valor Medio de                                         |   n| faltantes| minimo|   q1| mediana| media|    q3| maximo|    sd|
#   |:------------------------------------------------------|---:|---------:|------:|----:|-------:|-----:|-----:|------:|-----:|
#   |Frecuencia de dias con Precipitacion superior a 0.1 mm | 809|        79|    0.0|  4.9|    7.10|  6.76|   8.7|   16.3|  2.73|
#   |Humedad relativa (%)                                   | 826|        62|   30.1| 62.0|   70.20| 67.95|  76.3|   88.1| 10.88|
#   |Nubosidad total (octavos)                              | 876|        12|    1.4|  3.4|    3.70|  3.85|   4.1|    7.5|  0.81|
#   |Precipitacion (mm)                                     | 809|        79|    0.0| 23.9|   59.30| 69.33| 109.6|  237.0| 51.06|
#   |Temperatura (grados C)                                 | 887|         1|  -20.4| 10.7|   15.90| 14.92|  21.5|   27.9|  8.46|
#   |Temperatura maxima (grados C)                          | 888|         0|  -17.1| 17.4|   22.60| 21.33|  28.1|   35.1|  9.14|
#   |Temperatura minima (grados C)                          | 866|        22|  -24.5|  4.9|   10.30|  9.29|  15.3|   22.3|  8.09|
#   |Velocidad del Viento (km/h)                            | 756|       132|    3.0|  8.6|   11.35| 12.78|  15.1|   34.8|  6.04|

# A partir de las medidas descriptivas, se observa que la precipitación es la variable con mayor amplitud de
# valores (0 a 237 mm) y mayor dispersión (SD = 51.06 mm), lo que evidencia una importante heterogeneidad entre estaciones
# y períodos del año. Entre las variables térmicas, la temperatura máxima presenta el rango más amplio, con valores 
# comprendidos entre −17.1 °C y 35.1 °C. Asimismo, la humedad relativa exhibe una variabilidad moderada, reflejada en un 
# rango intercuartílico de 14.3 puntos porcentuales. Por otra parte, la nubosidad total es la variable más estable, 
# con el menor desvío estándar (0.81 octavos) y un rango intercuartílico reducido. Respecto a la calidad de los datos, 
# como ya se mencionó anteriormente, la velocidad del viento es la variable que presenta la mayor cantidad de observaciones
# faltantes (132 datos), seguida por la precipitación y la frecuencia de días con precipitación (79 datos cada una). 
# Finalmente, en la mayoría de las variables la media y la mediana presentan valores cercanos, lo que sugiere distribuciones
# relativamente simétricas; no obstante, la precipitación muestra una diferencia más marcada entre ambas medidas,
# indicando una posible asimetría positiva asociada a episodios de lluvia de gran magnitud.

# 2. Boxplot con la distribución de las variables metereológicas

# Se realizan los boxplots para ver descriptivamente las variables y detectar outliers. Sin embargo, es difícil comparar las 
# variables porque poseen distintas magnitudes y unidades. Esto genera que, por ejemplo, la variable precipitación domine el 
# gráfico por su escala; y que variables como frecuencia de días con precipitación y nubosidad queden con boxplots muy estrechos,
# como puede evidenciarse a continuación:

# Crear etiquetas más cortas para el gráfico
datos_box <- datos_met_long %>%
  mutate(
    Variable = recode(
      `Valor Medio de`,
      "Frecuencia de dias con Precipitacion superior a 0.1 mm" =
        "Frecuencia de\ndias con\nprecipitacion",
      "Humedad relativa (%)" =
        "Humedad\nrelativa (%)",
      "Nubosidad total (octavos)" =
        "Nubosidad\ntotal\n(octavos)",
      "Precipitacion (mm)" =
        "Precipitacion\n(mm)",
      "Temperatura (grados C)" =
        "Temperatura\nmedia (°C)",
      "Temperatura maxima (grados C)" =
        "Temperatura\nmaxima (°C)",
      "Temperatura minima (grados C)" =
        "Temperatura\nminima (°C)",
      "Velocidad del Viento (km/h)" =
        "Velocidad del\nviento (km/h)"))

ggplot(datos_box,
       aes(x = Variable, y = Valor,
           fill = Variable)) +
  geom_boxplot(
    alpha = 0.8,
    outlier.alpha = 0.5) +
  scale_fill_brewer(palette = "Set3") +
  labs(
    title = "Distribución de las variables meteorológicas",
    subtitle = "Estaciones meteorológicas de Argentina",
    x = "Variable meteorológica",
    y = "Valor observado") +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 16,
                              face = "bold",
                              hjust = 0.5),
    plot.subtitle = element_text(size = 12,
                                 hjust = 0.5),
    axis.text.x = element_text(size = 10,
                               lineheight = 0.9))

# 3. Matriz de correlaciones

# Se realiza una matriz de correlaciones donde, en la diagonal, se ven las variables con su histograma y curva de densidad; 
# por debajo, los gráficos de dispersión; y por encima, los coeficientes de correlación.

datos_cor <- datos_met_long %>%
  pivot_wider(names_from = `Valor Medio de`,
              values_from = Valor)

glimpse(datos_cor)

datos_numericos <- datos_cor %>%
  select(where(is.numeric)) # seleccionar solo variables numéricas.

cor(datos_numericos, 
    use = "pairwise.complete.obs") 

# Se calcula los coeficientes de correlación entre variables.
# Para cada par de variables, se usa únicamente las observaciones disponibles para ese par.

library(psych)

pairs.panels(datos_numericos,
             method = "pearson",   # Método de correlación
             hist.col = "#00AFBB",
             density = TRUE,        # Muestra los gráficos de densidad
             ellipses = FALSE)      # Si es T muestra elipses de correlación

# En base a la matriz de correlaciones, se puede concluir que las variables térmicas tienen una correlación positiva
# muy fuerte, lo que indica que están muy correlacionadas. Otra relación muy positiva es la de Precipitación y 
# frecuencia de días con precipitación, lo cual es lógico porque cuando llueve más, suele haber más días con lluvia.
# Existen otras variables con correlaciones moderadas, como nubosidad con temperatura (-0.55), y precipitación con humedad 
# relativa (0.40), entre otras. Además, hay correlaciones muy mínimas, lo que indica muy poca relación entre las variables, como
# es el caso de humedad relativa con temperatura mínima (0.02).

# 4. Identificación de estaciones con valores extremos

# A continuación, se realizó una identificación de las estaciones con valores extremos, junto con el mes en que se indentificaron. 
# Si bien este trabajo se centra en analizar la variable temparatura de las bases, también se incorporaron más variables en el
# siguiente análisis a modo informativo.

# Estaciones con temperaturas máximas más altas

datos_cor %>%
  arrange(desc(`Temperatura maxima (grados C)`)) %>%
  select(Estacion,
         Mes, 
         `Temperatura maxima (grados C)`) %>%
  head(10)

# Puede observarse que las bases con temperaturas máximas más altas son Las Lomitas, La Rioja Aero, San Juan Aero, Catamarca Aero,
# Santiago del Estero Aero, y Tinogasta, en los meses de diciembre y enero. Las Lomitas, provincia de Formosa, presentó la 
# temperatura máxima más alta del último año (35.1 grados C). Todas las bases mencionadas se encuentran en las provincias de Formosa,
# Catamarca, La Rioja, San Juan y Santiagio del Estero, las cuales son todas provincias del Norte Argentino (noreste y noroeste), 
# salvo por San Juan que pertenece a la región de Cuyo. Dichas provincias son conocidas por la presencia de altas temperaturas, 
# sobretodo en verano, coincidente con los meses en que se detectaron los picos máximos del año.

# Estaciones con temperaturas mínimas más bajas

datos_cor %>%
  arrange(`Temperatura minima (grados C)`) %>%
  select(Estacion, 
         Mes, 
         `Temperatura minima (grados C)`) %>%
  head(10)

# Puede observarse que las bases que presentan las temperaturas mínimas más bajas son la base Belgrano II en los meses de 
# abril a mayo, los cuales corresponden a las estaciones de otoño-invierno, y la base Marambio también en los meses invernales.
# Estos resultado son esperables, ya que estas dos bases están ubicadas en la Antartida, el punto más al sur del país. Belgrano II
# fue la base que presentó la temperatura mínima más baja en el último año (-24.5 grados C).

# Estaciones con mayores precipitaciones

datos_cor %>%
  arrange(desc(`Precipitacion (mm)`)) %>%
  select(Estacion, 
         Mes, 
         `Precipitacion (mm)`) %>%
  head(10)

# Puede observarse que las bases del país con mayores precipitaciones son Iguazu Aero, Tucuman Aero, Posadas Aero, Corrientes Aero,
# Paso de los libres Aero, Resistencia Aero, Tartagal Aero, y Salta Aero. Dichas bases se encuentran en las provincias argentinas
# de Misiones, Tucuman, Corrientes, Chaco y Salta, las cuales son todas provincias del norte del país (tanto noreste como noroeste).
# La mayoría de los valores identificados se detectaron en el mes de abril. Sin embargo, la base con mayores precipitaciones del
# último año fue Iguazu Aero, provincia de Misiones, en el mes de octubre (237 mm). Otros meses en los cuales se detectó un máximo
# de precipitaciones fueron enero y mayo. Es decir, las provincias con mayores precipitaciones en Argentina son aquellas de la 
# región del norte, en las estaciones de verano, otoño y primavera, y se destaca la ausencia de precipitaciones en los meses
# invernales.

# Estaciones con mayor humedad

datos_cor %>%
  arrange(desc(`Humedad relativa (%)`)) %>%
  select(Estacion,
         Mes,
         `Humedad relativa (%)`) %>%
  head(10)

# Puede observarse que las estaciones con mayor humedad fueron Iguazu Aero, Rio Grande B.A., Punta Indio B.A., Tartagal Aero, y
# Dolores Aero. Estas bases se encuentran en las provincias de Misiones, Tierra del Fuego, Buenos Aires, y Salta, lo cual indica
# que la mayor concentración de humedad está distribuida por el norte, centro y sur del país. La base Iguazu Aero, provincia de
# Misiones, fue la que presentó el máximo de humedad relativa en el último año (88.1%), en el mes de junio. Sin enmabrgo, las bases
# con más registros máximos de humedad relativa fueron Rio Grande B.A., provicia de Tierra del Fuego, y Punta Indio B.A., 
# provincia de Buenos Aires. Los meses en que se detectaron los máximos registros de humedad fueron de abril a julio, es decir, 
# los meses pertenecientes a las estaciones otoño-invierno.

# Estaciones con mayor nubosidad

datos_cor %>%
  arrange(desc(`Nubosidad total (octavos)`)) %>%
  select(Estacion,
         Mes,
         `Nubosidad total (octavos)`) %>%
  head(10)

# Puede observarse que la principal base con mayor nubosidad total fue Orcadas, seguida de Marambio, las cuales se encuentran
# en la Antartida. La base Orcadas detectó el máximo registro de nubosidad total en el mes de enero (7.5 octavos). Otros meses 
# en los cuales se identificaron valores máximos fueron febrero, marzo, abril, mayo, septiembre, octubre, noviembre y diciembre.
# Es decir, las bases del punto más sur del país, la Antartida, registraron la mayor nubosidad en verano, otoño y primavera, 
# destacando la presencia de nubosisdad en los meses invernales.

# 5. Georeferencia

# Con el objetivo de incorporar una dimensión espacial al análisis, se construyó una tabla de coordenadas 
# geográficas (latitud y longitud) para cada estación meteorológica. Posteriormente, esta información fue 
# integrada a la base de datos original mediante el identificador de estación, permitiendo representar 
# espacialmente las variables climáticas.

library(readxl)

estaciones_smn <- read_excel("Datos crudos/estaciones_smn.xlsx")

estaciones <- datos_met %>%
  distinct(Estacion)

estaciones_name <- estaciones_smn %>%
  distinct(NOMBRE)

setdiff(estaciones$Estacion, estaciones_smn$NOMBRE) # Ver si hay diferencias entre los nombres de las estaciones

datos_met_long <- datos_met_long %>% # Se cambian los nombres de las estaciones para que coincidan con el dataset original.
  mutate(Estacion = recode(Estacion, 
                         "LA QUIACA OBS." = "LA QUIACA OBSERVATORIO",
                         "VILLA MARIA DEL RIO SECO" = "VILLA DE MARIA DEL RIO SECO",
                         "PILAR OBS." = "PILAR OBSERVATORIO",
                         "BUENOS AIRES" = "BUENOS AIRES OBSERVATORIO",
                         "PEHUAJÃ\u0093 AERO" = "PEHUAJO AERO",
                         "SAN MIGUEL" = "MARIANO MORENO AERO"))

estaciones_smn_filtrado <- estaciones_smn %>%
  filter(NOMBRE %in% datos_met_long$Estacion) # Se dejan solo las estaciones que tengo en el dataset original.

nrow(estaciones_smn_filtrado)

# A continuación, se corrobora que no haya diferencias entre los nombres de las estaciones en los dos datasets.

setdiff(datos_met_long$Estacion, estaciones_smn_filtrado$NOMBRE)
setdiff(estaciones_smn_filtrado$NOMBRE, datos_met_long$Estacion)

names(estaciones_smn_filtrado)

estaciones_smn_filtrado <- estaciones_smn_filtrado %>%
  rename(LAT_MIN = `...4`, LON_MIN = `...6`) # Se cambia el nombre de las columnas de minutos que vienen mal del excel.

estaciones_smn_filtrado <- estaciones_smn_filtrado %>% # Se transforman las columnas a numéricas.
  mutate(LATITUD = as.numeric(LATITUD), 
         LAT_MIN = as.numeric(LAT_MIN), 
         LONGITUD = as.numeric(LONGITUD),
         LON_MIN = as.numeric(LON_MIN))

# A continuación, se calculan las coordenadas decimales

estaciones_smn_filtrado <- estaciones_smn_filtrado %>%
  mutate(
    lat = ifelse(
      LATITUD < 0,
      LATITUD - LAT_MIN/60,
      LATITUD + LAT_MIN/60),
    lon = ifelse(
      LONGITUD < 0,
      LONGITUD - LON_MIN/60,
      LONGITUD + LON_MIN/60))

# Se unen los dos datasets. Específicamente, se añaden al dataset original "datos_met_long" las colunmas que contienen las
# coordenadas de las estaciones metereológicas (provincia, latitud y longitud). Los últimos datos viene del dataset obtenido 
# del Servicio Metereológico Nacional.

datos_geo <- datos_met_long %>%
  left_join(estaciones_smn_filtrado %>% 
              select(NOMBRE, PROVINCIA, lat, lon),
            by = c("Estacion" = "NOMBRE"))

# Verificación de la integración de ambos datasets.

sum(is.na(datos_geo$lat))
sum(is.na(datos_geo$lon))
length(unique(datos_geo$Estacion))

# En resumen, se incorporó información geográfica proveniente del catálogo oficial de estaciones meteorológicas 
# del Servicio Meteorológico Nacional. Fue necesario realizar cambios de los nombres en ambos datasets
# para asegurar la correcta coincidencia entre las estaciones. Posteriormente, las coordenadas geográficas expresadas en grados 
# y minutos fueron transformadas a coordenadas decimales y vinculadas con la base meteorológica mediante una unión por nombre de 
# estación como clave. Como resultado, se logró georreferenciar la totalidad de las 74 estaciones analizadas.

# A continuación, se muestra un mapa de control para verificar visualmente a las estaciones:

library(leaflet)

datos_geo %>%
  distinct(Estacion, lat, lon) %>%
  leaflet() %>%
  addTiles() %>%
  addCircleMarkers(
    lng = ~lon,
    lat = ~lat,
    popup = ~Estacion,
    radius = 5)

# Mapas y gráficos de temperatura 

# 1. Bases metereológicas con temperaturas promedio

# Se filtra solo la variable que se quiere analizar. En este caso, temperatura media. Además se realizan mapas y gráficos para
# cuatros meses claves en las distintas estaciones del año: enero para verano, abril para otoño, julio para invierno, y 
# octubre para primvera. El objetivo es indentificar cómo se comporta la temperatura media en cada estación del año, para las
# distintas estaciones metereológicas del país.

datos_temp <- datos_geo %>%
  filter(`Valor Medio de` == "Temperatura (grados C)")

datos_temp_ene <- datos_geo %>%
  filter(`Valor Medio de` == "Temperatura (grados C)",
         Mes == "Ene")

datos_temp_abr <- datos_geo %>%
  filter(`Valor Medio de` == "Temperatura (grados C)",
         Mes == "Abr")

datos_temp_jul <- datos_geo %>%
  filter(`Valor Medio de` == "Temperatura (grados C)",
         Mes == "Jul")

datos_temp_oct <- datos_geo %>%
  filter(`Valor Medio de` == "Temperatura (grados C)",
         Mes == "Oct")

# 2. Bases metereológicas con temperaturas altas

# 3. Bases metereológicas con temperaturas bajas


################################
# 4. CONCLUSIONES
################################


