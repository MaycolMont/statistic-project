library(dplyr)
library(ggplot2)
library(reshape2)
library(dplyr)
library(moments)

# Carga de datos de la hoja de Excel
df <- read_excel("data/DATOS ESTADISTICA.xlsx", sheet="FORMATO 1")

# ============= AGREGACIÓN DE DATOS =================

#eliminar columna juego que funciona como índice
df_agg <- df_agg %>% select(-Juego)

df_agg <- df_agg %>% 
  rename(
    Condicion_de_la_torre = `Condicion de la torre`,
    Numero_de_jugadores   = `Numero de jugadores`
  )


df_agg <- df %>%
  group_by(Juego, `Condicion de la torre`, `Numero de jugadores`) %>%
  summarise(
    Tiempo_promedio   = mean(Segundos, na.rm = TRUE),
    Tiempo_mediana    = median(Segundos, na.rm = TRUE),
    Tiempo_sd         = sd(Segundos, na.rm = TRUE),
    Tiempo_min        = min(Segundos, na.rm = TRUE),
    Tiempo_max        = max(Segundos, na.rm = TRUE),
    Intentos_totales  = n(),
    
    # variables categóricas resumidas
    Dominante_prop    = mean(`Mano Utilizada` == "dominante"),
    NoDominante_prop  = mean(`Mano Utilizada` == "no dominante"),
    
    Dominante_n       = sum(`Mano Utilizada` == "dominante"),
    NoDominante_n     = sum(`Mano Utilizada` == "no dominante")
  ) %>%
  ungroup()

# Seleccionar solo variables numéricas relevantes
df_num <- df_agg %>% 
  select(
    Numero_de_jugadores,
    Tiempo_promedio,
    Tiempo_mediana,
    Tiempo_sd,
    Tiempo_min,
    Tiempo_max,
    Intentos_totales,
    Dominante_prop,
    NoDominante_prop,
    Dominante_n,
    NoDominante_n
  )

# ================ EXPLORACIÓN DE DATOS ====================
# Datos númericos
resumen_numerico <- df_num %>%
  summarise(
    across(everything(),
           list(
             media = mean,
             mediana = median,
             sd = sd,
             min = min,
             max = max,
             iqr = IQR
           ),
           na.rm = TRUE)
  )

forma_numerica <- df_num %>%
  summarise(
    across(everything(),
           list(
             skewness = skewness,
             kurtosis = kurtosis
           ),
           na.rm = TRUE)
  )

for (v in names(df_num)) {
  print(
    ggplot(df_agg, aes(x = .data[[v]])) +
      geom_histogram(bins = 30, fill = "skyblue", color = "black") +
      theme_minimal() +
      labs(title = paste("Histograma de", v), x = v, y = "Frecuencia")
  )
}

# VARIABLES CATEGÓRICAS
cat_vars <- df_agg %>% select(where(is.character))

for (v in names(cat_vars)) {
  cat("\n===== Variable:", v, "=====\n")
  print(table(df_agg[[v]]))
  print(prop.table(table(df_agg[[v]])))
}

for (v in names(cat_vars)) {
  print(
    ggplot(df_agg, aes(x = .data[[v]])) +
      geom_bar(fill = "orange", color = "black") +
      theme_minimal() +
      labs(title = paste("Frecuencias de", v), x = v, y = "Cuenta")
  )
}

# ================ CORRELACIÓN DE DATOS ===================

# Matriz de correlación
corr_matrix <- cor(df_num, use = "complete.obs")

# Pasar a formato largo para ggplot
corr_melt <- melt(corr_matrix)

# Heatmap
ggplot(corr_melt, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(label = round(value, 2)), color = "white", size = 3) +
  scale_fill_gradient2(
    low = "blue",
    high = "red",
    mid = "white",
    midpoint = 0,
    limit = c(-1, 1)
  ) +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    title = "Matriz de correlación de las variables agregadas",
    x = "",
    y = ""
  )

# 1. Tiempo_promedio vs Tiempo_mediana
ggplot(df_agg, aes(x = Tiempo_promedio, y = Tiempo_mediana)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Promedio vs Mediana")

# 2. Tiempo_sd vs Tiempo_max
ggplot(df_agg, aes(x = Tiempo_sd, y = Tiempo_max)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Desviación estándar vs Tiempo máximo")

# 3. Intentos_totales vs Tiempo_promedio
ggplot(df_agg, aes(x = Intentos_totales, y = Tiempo_promedio)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Intentos totales vs Tiempo promedio")

# 4. Numero_de_jugadores vs Dominante_prop
ggplot(df_agg, aes(x = `Numero de jugadores`, y = Dominante_prop)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Nº de jugadores vs Proporción de mano dominante")

# 5. Tiempo_min vs Tiempo_mediana
ggplot(df_agg, aes(x = Tiempo_min, y = Tiempo_mediana)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Tiempo mínimo vs Tiempo mediana")

# 6. Dominante_n vs Intentos_totales
ggplot(df_agg, aes(x = Dominante_n, y = Intentos_totales)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Intentos totales vs Nº de veces usando mano dominante")
