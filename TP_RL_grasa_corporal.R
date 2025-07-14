################################################################################
# Exploración del dataset Obesidad25                                           #
# Objetivo: describir la muestra y explorar relaciones con el % de grasa       #
#           corporal (variable grc).                                         #
################################################################################

# INSTALACION DE LIBRERIAS
#-------------------------------------------------------------------------------

# Librerías necesarias
libs <- c("readxl", "ggplot2", "dplyr", "tidyr", "ggcorrplot", "GGally", "scales",
          "kableExtra", "summarytools", "formattable", "gt", "car", "broom",
          "MASS", "ggfortify", "glue", "tibble")

# Instalar si faltan
install_if_missing <- function(pkg) {
  if (!require(pkg, character.only = TRUE)) install.packages(pkg)
}
invisible(lapply(libs, install_if_missing))

# Cargar
invisible(lapply(libs, library, character.only = TRUE))



#-------------------------------------------------------------------------------
# 2. ANÁLISIS EXPLORATORIO DE DATOS (EDA)
#-------------------------------------------------------------------------------



# 2.1. Carga y examinación los datos
#-------------------------------------------------------------------------------

# Ver descripción de variables
datos <- read_excel("obesidad25.xlsx")

# Vista preliminar
kable(head(datos, 6), digits = 2, caption = "Primeras 6 observaciones") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F)

# Nombres de variables
names(datos)    

# Descripción general de las variables
summary(datos)

# Resumen del DataFrame exportable
dfSummary(datos) %>% 
  print(method = "browser")

# visión estructural del dataset
str(datos)    



# 2.2. Verificación de NAs y duplicados
#-------------------------------------------------------------------------------

# Contar cuántos valores NA hay por variable
colSums(is.na(datos))

# Eliminar filas con NA para trabajar solo con datos completos
datos_completos <- datos %>% drop_na()

# Verificacion datos duplicados
sum(duplicated(datos))        

# dimensiones luego de limpieza
dim(datos_completos)      



# 2.3. Gráficos univariados
#-------------------------------------------------------------------------------

# Histograma de cada variable numérica
datos %>% 
  dplyr::select(where(is.numeric)) %>% 
  pivot_longer(cols = everything(), names_to = "variable", values_to = "valor") %>%
  drop_na() %>%
  ggplot(aes(x = valor)) +
  geom_histogram(bins = 30, fill = "#b2d8b2", color = "darkgreen") +
  facet_wrap(~ variable, scales = "free", ncol = 3) +
  labs(title = "Distribución de variables numéricas",
       x = "Valor", y = "Frecuencia")



# 2.4. Outliers
#-------------------------------------------------------------------------------

# Boxplots por variable
datos %>%
  dplyr::select(where(is.numeric)) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "valor") %>%
  drop_na(valor) %>%
  ggplot(aes(x = variable, y = valor)) +
  geom_boxplot(fill = "darkorange", color = "black") +
  coord_flip() +
  labs(title = "Boxplots de variables numéricas",
       x = "Variable", y = "Valor")


# 1. Función para contar outliers por variable usando regla de Tukey
contar_outliers <- function(x) {
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  sum(x < (q1 - 1.5 * iqr) | x > (q3 + 1.5 * iqr), na.rm = TRUE)
}


# 2. Aplicar la función a todas las variables numéricas
outliers_por_variable <- sapply(dplyr::select(datos_completos, where(is.numeric)), contar_outliers)
print(outliers_por_variable)


# 3. Identificar los valores atípicos

# Mostrar los valores outliers por variable
outliers_valores <- lapply(dplyr::select(datos_completos, where(is.numeric)), function(x) {
  boxplot.stats(x)$out
})

# Crear data.frame limpio, sin rownames
outliers_resumen <- data.frame(
  Variable = names(outliers_valores),
  Valores_Atipicos = sapply(outliers_valores, function(x) paste(round(x, 2), collapse = " - ")),
  row.names = NULL
)

# Imprimir tabla
outliers_resumen %>%
  kable(caption = "Valores atípicos detectados por variable", align = "l") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F)


# 4. Análisis de valores atípicos en variable: altura

# Mostrar filas donde altura == 73.75
datos_completos %>% filter(altura ==  73.75)

summary(datos_completos$altura)

ggplot(datos_completos, aes(x = altura)) +
  geom_histogram(bins = 30, fill = "darkorange", color = "black") +
  labs(title = "Histograma de Altura",
       x = "Altura (cm)",
       y = "Frecuencia")

datos_sin_outliers <- datos_completos %>% filter(altura != 73.75)

summary(datos_sin_outliers)


# 5. Análisis de valores atípicos en variable: pecho - cadera - muslo - rodilla

# Función para obtener índices de outliers con IQR
get_outlier_index <- function(vec) {
  q1 <- quantile(vec, 0.25, na.rm = TRUE)
  q3 <- quantile(vec, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  which(vec < (q1 - 1.5 * iqr) | vec > (q3 + 1.5 * iqr))
}

# Obtener índices
out_pecho <- get_outlier_index(datos_sin_outliers$pecho)
out_cadera <- get_outlier_index(datos_sin_outliers$cadera)
out_muslo <- get_outlier_index(datos_sin_outliers$muslo)
out_rodilla <- get_outlier_index(datos_sin_outliers$rodilla)

# Unir todos los índices únicos
outliers_indices <- unique(c(out_pecho, out_cadera, out_muslo, out_rodilla))

# Filtrar casos con outliers
personas_outliers <- datos_sin_outliers[outliers_indices, 
                                        c("abdomen", "pecho", "cadera", "muslo", "rodilla", "grc")]

# Ordenar por % grc descendente
personas_outliers <- personas_outliers[order(-personas_outliers$grc), ]

# Ver tabla
formattable(personas_outliers, 
            list(
              grc = color_tile("white", "tomato"),
              abdomen = color_tile("white", "lightblue")
            ))

# Gráfico
ggplot(personas_outliers, aes(x = abdomen, y = grc)) +
  geom_point(color = "darkred", size = 3) +
  labs(title = "% GRC vs Abdomen en personas con outliers",
       x = "Circunferencia abdomen (cm)",
       y = "% Grasa corporal") +
  theme_minimal()


# 6. Análisis de valores atípicos en variable: abdomen

# Detección y análisis de outliers en abdomen usando regla de Tukey
q1 <- quantile(datos_completos$abdomen, 0.25, na.rm = TRUE)
q3 <- quantile(datos_completos$abdomen, 0.75, na.rm = TRUE)
iqr <- q3 - q1
lim_inf <- q1 - 1.5 * iqr
lim_sup <- q3 + 1.5 * iqr

# Filtrar datos sin outliers en abdomen
datos_sin_outliers_abdomen <- datos_sin_outliers %>%
  filter(abdomen >= lim_inf & abdomen <= lim_sup)

# Mostrar filas con outliers para abdomen
outliers_abdomen <- datos_completos %>%
  filter(abdomen < lim_inf | abdomen > lim_sup)

# Crear una tabla formateada para mostrar los casos con outliers en abdomen
outliers_abdomen %>%
  gt() %>%
  tab_header(
    title = "Casos con valores atípicos en la variable abdomen"
  ) %>%
  fmt_number(
    columns = everything(),
    decimals = 1
  )

# Ajustar modelo con y sin outlier de abdomen (pero solo como experimento)
modelo_todos <- lm(grc ~ abdomen, data = datos_sin_outliers)
modelo_sin_outliers <- lm(grc ~ abdomen, data = datos_sin_outliers_abdomen)
summary(modelo_sin_outliers)

# Resumen de modelos
cat("### Modelo con todos los datos ###\n")
print(summary(modelo_todos))

cat("\n### Modelo sin outliers en abdomen ###\n")
print(summary(modelo_sin_outliers))

# Crear los datasets con etiquetas
datos_completos$modelo <- "Con outliers"
datos_sin_outliers$modelo <- "Sin outliers"

# Visualización con leyenda
ggplot() +
  geom_point(data = datos_completos, aes(x = abdomen, y = grc), alpha = 0.5, color = "grey") +
  geom_point(data = datos_sin_outliers, aes(x = abdomen, y = grc), alpha = 0.8, color = "#69b3a2") +
  geom_smooth(data = datos_completos, aes(x = abdomen, y = grc, color = modelo, linetype = modelo),
              method = "lm", se = FALSE) +
  geom_smooth(data = datos_sin_outliers, aes(x = abdomen, y = grc, color = modelo, linetype = modelo),
              method = "lm", se = FALSE) +
  scale_color_manual(values = c("Con outliers" = "blue", "Sin outliers" = "darkred")) +
  scale_linetype_manual(values = c("Con outliers" = "solid", "Sin outliers" = "dashed")) +
  labs(title = "Comparación del modelo lineal con y sin outliers en abdomen",
       x = "Abdomen (cm)", y = "% de grasa corporal",
       color = "Modelo", linetype = "Modelo") +
  theme_minimal()



# 2.5. Análisis bivariado y correlaciones
#-------------------------------------------------------------------------------

# Matriz de correlación
cor_matrix <- cor(dplyr::select(datos_sin_outliers, where(is.numeric)), use = "complete.obs")

# Mapa de correlación
ggcorrplot(cor_matrix,
           type = "lower",
           lab = TRUE,
           lab_size = 3,
           tl.cex = 10,
           title = "Matriz de correlación entre variables numéricas")

# Ordenar correlaciones de grc con otras
cor_con_grc <- sort(cor_matrix["grc", ], decreasing = TRUE)

# Transformar vector en data frame
cor_tabla <- enframe(cor_con_grc, name = "Variable", value = "Correlación")

# Mostrar tabla con formato gt
cor_tabla %>%
  gt() %>%
  tab_header(
    title = "Correlación con % de grasa corporal (grc)"
  ) %>%
  fmt_number(columns = "Correlación", decimals = 3) %>%
  cols_label(
    Variable = "Variable",
    Correlación = "Correlación"
  )

# Seleccionar las 3 variables más correlacionadas (excepto grc)
top_vars <- names(sort(cor_con_grc[-1], decreasing = TRUE))[1:3]

# Crear scatterplots
datos_sin_outliers %>%
  dplyr::select(grc, all_of(top_vars)) %>%
  pivot_longer(cols = -grc, names_to = "variable", values_to = "valor") %>%
  ggplot(aes(x = valor, y = grc)) +
  geom_point(alpha = 0.6, color = "orange") +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  facet_wrap(~ variable, scales = "free_x") +
  labs(title = "Relación entre grc y las variables más correlacionadas",
       x = "Valor de variable predictora", y = "% de grasa corporal")

# Crear una variable categórica por terciles del % de grasa corporal (grc)
# Esto permite clasificar a los individuos en grupos de bajo, medio y alto % de grasa,
# útil para análisis comparativos y visualizaciones con color por grupo
datos_sin_outliers <- datos_sin_outliers %>%
  mutate(grc_cat = cut(grc,
                       breaks = quantile(grc, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE),
                       labels = c("Bajo", "Medio", "Alto"),
                       include.lowest = TRUE))

# Gráfico de dispersión de grc contra las 3 variables más correlacionadas
GGally::ggpairs(
  datos_sin_outliers,
  columns = c("grc", top_vars),  # top_vars ya está definido con las 3 más correlacionadas
  mapping = aes(color = grc_cat),
  title = "Relaciones entre grc y variables predictoras (color por nivel de grasa)"
)

# Histograma + línea de densidad para grc
ggplot(datos_sin_outliers, aes(x = grc)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "#69b3a2", color = "darkgreen") +
  geom_density(color = "darkred", linewidth = 1) +
  labs(title = "Distribución y densidad del % de grasa corporal", 
       x = "grc", y = "Densidad")



#-------------------------------------------------------------------------------
# 3. MODELO 1: REGRESIÓN LINEAL SIMPLE
#-------------------------------------------------------------------------------



# 3.1. Seleccion de la variable predictora
#-------------------------------------------------------------------------------

# Se selecciona 'abdomen' por ser la variable más correlacionada con 'grc'
variable_predictora <- "abdomen"


# 3.2. Ajuste del modelo lineal simple
#-------------------------------------------------------------------------------

modelo_rls <- lm(grc ~ abdomen, data = datos_sin_outliers)
summary(modelo_rls)


# 3.3. Interpretación del modelo
#-------------------------------------------------------------------------------

coef(modelo_rls)


# 3.4. Visualización del ajuste
#-------------------------------------------------------------------------------

ggplot(datos_sin_outliers, aes(x = abdomen, y = grc)) +
  geom_point(alpha = 0.6, color = "darkorange") +
  geom_smooth(method = "lm", se = TRUE, color = "darkred", fill = "blue", alpha = 0.3) +
  labs(title = "Ajuste del modelo lineal simple: grc ~ abdomen",
       x = "Abdomen (cm)", y = "% de grasa corporal")



#-------------------------------------------------------------------------------
# 4. MODELO 2: REGRESIÓN LINEAL MÚLTIPLE CON TODAS LAS VARIABLES NUMERICAS
#-------------------------------------------------------------------------------


# 4.1. Ajuste del modelo múltiple
#-------------------------------------------------------------------------------

# Regresión lineal múltiple: grc ~ peso, altura, cuello, pecho, abdomen, cadera, muslo, rodilla
# Se relacionan todas las variales con la varible objetivo 'grc'

mod_full <- lm(grc ~peso+altura+cuello+pecho+abdomen+cadera+muslo+rodilla, data = datos_sin_outliers)


# 4.2. Evaluación global del modelo
#-------------------------------------------------------------------------------

summary(mod_full)

# Interpretamos el modelo
coef(mod_full)


# 4.4. Análisis de significancia individual de variables 
#-------------------------------------------------------------------------------

# Ecuación del modelo ajustado
summary(mod_full)$coefficients


# 4.5.Diagnóstico de multicolinealidad
#-------------------------------------------------------------------------------
vif(mod_full)



#-------------------------------------------------------------------------------
# 5.COMPARACIÓN Y SELECCIÓN DE MODELOS DE REGRESIÓN
#-------------------------------------------------------------------------------



# 5.1. Modelo con selección automática de variables
#-------------------------------------------------------------------------------

modelo_completo <- lm(grc ~ peso + altura + cuello + pecho + abdomen + cadera + muslo + rodilla,
                      data = datos_sin_outliers)

mod_automatico <- stepAIC(modelo_completo, direction = "both",trace = FALSE)

# Evaluacióm global del modelo
summary(mod_automatico)
vif(mod_automatico)


# 5.2. Modelo con selección manual
#-------------------------------------------------------------------------------

mod_manual <- lm(grc ~ abdomen+cuello, data = datos_sin_outliers)

# Evaluación global del modelo
summary(mod_manual)
vif(mod_manual)



#-------------------------------------------------------------------------------
# 6. EVALUACIÓN DEL MODELO SELECCIONADO
#-------------------------------------------------------------------------------



# 6.1. Verificación de supuestos del modelo lineal
#-------------------------------------------------------------------------------


# Gráfico de residuos del modelo ajustado

autoplot(mod_automatico, 
         which = 1:4, 
         colour = '#69b3a2', 
         smooth.colour = "red",
         smooth.linetype = "dashed",
         size = 1)

tidy(mod_automatico) %>%
  gt() %>%
  tab_header(title = "Coeficientes del modelo automático") %>%
  fmt_number(columns = c(estimate, std.error, statistic, p.value), decimals = 3) %>%
  cols_label(
    term = "Variable",
    estimate = "Estimación",
    std.error = "Error Est.",
    statistic = "Valor t",
    p.value = "Valor p"
  )

# Matriz de correlación entre variables seleccionadas - Ver qué variables quedaron
names(mod_automatico$coefficients)

# Filtrar solo esas columnas
vars_seleccionadas <- names(mod_automatico$coefficients)[-1]
cor_select <- cor(datos_sin_outliers[, vars_seleccionadas])

# Visualizar correlaciones
ggcorrplot(cor_select, lab = TRUE, type = "lower", title = "Correlación entre variables seleccionadas")



# 6.2. Detección de observaciones influyentes
#-------------------------------------------------------------------------------

# Ajustar el modelo
mod_auto <- lm(grc ~ altura + cuello + abdomen + cadera, data = datos_sin_outliers)

# Calcular leverage, residuos, y Distancia de Cook
hii <- hatvalues(mod_auto)
ri <- rstudent(mod_auto)
CDi <- cooks.distance(mod_auto)

# Umbral de influencia
umbral <- 4 / nrow(datos_sin_outliers)

# Gráfico de Distancia de Cook con ggplot2
cook_df <- data.frame(
  obs = 1:length(CDi),
  CD = CDi,
  influente = CDi > umbral
)

ggplot(cook_df, aes(x = obs, y = CD, color = influente)) +
  geom_segment(aes(xend = obs, yend = 0), linewidth = 0.8) +
  scale_color_manual(values = c("FALSE" = "red", "TRUE" = "darkgreen")) +
  geom_hline(yintercept = umbral, linetype = "dashed", color = "darkblue") +
  labs(title = "Distancia de Cook",
       x = "Observación", y = "Cook's Distance", color = "Influyente") +
  theme_minimal()

# Identificar observaciones influyentes
influyentes <- which(CDi > umbral)

# Histograma de leverage
leverage_df <- data.frame(leverage = hii)
umbral_leverage <- 2 * length(coef(mod_auto)) / nrow(datos_sin_outliers)

ggplot(leverage_df, aes(x = leverage)) +
  geom_histogram(binwidth = 0.01, fill = "steelblue", color = "white") +
  geom_vline(xintercept = umbral_leverage, color = "darkred", linetype = "dashed", size = 1) +
  annotate("text", x = umbral_leverage, y = Inf,
           label = paste0("Umbral: ", round(umbral_leverage, 3)),
           vjust = 2, hjust = 0, color = "darkred", angle = 90, size = 3.5) +
  labs(title = "Distribución del leverage",
       x = "Leverage (hii)", y = "Frecuencia") +
  theme_minimal()

# Detectar observaciones con leverage alto
leverage_alto <- which(hii > umbral_leverage)

# Intersección
interseccion <- intersect(influyentes, leverage_alto)

# Mostrar observaciones 
cols_num <- datos_sin_outliers %>% dplyr::select(where(is.numeric)) %>% names()

datos_sin_outliers %>%
  slice(interseccion) %>%
  mutate(Nro_Obs = as.integer(interseccion)) %>%
  dplyr::select(Nro_Obs, everything()) %>%
  gt() %>%
  tab_header(
    title = "Observaciones influyentes con leverage alto"
  ) %>%
  fmt_number(
    columns = c("Nro_Obs"),
    decimals = 0
  ) %>%
  fmt_number(
    columns = setdiff(cols_num, "Nro_Obs"),
    decimals = 1
  ) %>%
  cols_label(
    Nro_Obs = "Nro Obs",
    grc = "% Grasa",
    peso = "Peso (kg)",
    altura = "Altura (cm)",
    cuello = "Cuello (cm)",
    pecho = "Pecho (cm)",
    abdomen = "Abdomen (cm)",
    cadera = "Cadera (cm)",
    muslo = "Muslo (cm)",
    rodilla = "Rodilla (cm)"
  ) %>%
  tab_style(
    style = cell_borders(sides = "all", color = "gray"),
    locations = cells_body()
  )

# Armar el texto formateado
resumen <- glue("
📊 Resumen de observaciones influyentes
────────────────────────────────────────────
✔ Observaciones influyentes por Cook's Distance: {length(influyentes)}
✔ Observaciones con leverage alto: {length(leverage_alto)}
✔ Observaciones que cumplen ambos criterios: {length(interseccion)}

🟢 IDs influyentes por Cook (no leverage alto):
{paste(setdiff(influyentes, leverage_alto), collapse = ', ')}

🔵 IDs con leverage alto (no influyentes por Cook):
{paste(setdiff(leverage_alto, influyentes), collapse = ', ')}

🟡 IDs en la intersección (Cook + leverage alto):
{paste(interseccion, collapse = ', ')}
")

# Mostrar todo junto
cat(resumen)


# 6.3. Predicción para nuevos casos
#-------------------------------------------------------------------------------

nuevos_casos <- data.frame(
  altura = c(165, 175, 185), cuello = c(35, 40, 45),
  abdomen = c(85, 100, 115),
  cadera = c(90, 100, 110)
)

# Intervalo de confianza para la media
predict(mod_automatico, newdata = nuevos_casos, interval = "confidence")

# Intervalo de predicción para casos individuales
predict(mod_automatico, newdata = nuevos_casos, interval = "prediction")