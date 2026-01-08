# =============================================================================
# Análisis de Tiempos de Respuesta - Variables Dicotómicas
# =============================================================================

# Cargar librerías necesarias
if (!require("tidyverse")) install.packages("tidyverse", dependencies = TRUE)
if (!require("ggplot2")) install.packages("ggplot2", dependencies = TRUE)
if (!require("gridExtra")) install.packages("gridExtra", dependencies = TRUE)
if (!require("scales")) install.packages("scales", dependencies = TRUE)

library(tidyverse)
library(ggplot2)
library(gridExtra)
library(scales)

# Cargar datos
datos <- read.csv("mapa-eh2-rt-anon.csv", stringsAsFactors = FALSE)

# Identificar variables dicotómicas de tiempo de respuesta
# Variables que empiezan con "dt" son dicotómicas
vars_dicotomicas <- c("dt02_gend", "dt05_inte", "dt39_pact")

# Verificar que las variables existen
vars_existentes <- vars_dicotomicas[vars_dicotomicas %in% names(datos)]
cat("Variables dicotómicas encontradas:", paste(vars_existentes, collapse = ", "), "\n\n")

# Verificación de unidades: mostrar algunos ejemplos para confirmar si son ms o seg
cat("=== VERIFICACIÓN DE UNIDADES ===\n")
cat("Mostrando algunos valores de ejemplo para verificar unidades:\n")
for (var in vars_existentes) {
  valores <- as.numeric(datos[[var]])
  valores <- valores[!is.na(valores) & valores > 0]
  if (length(valores) > 0) {
    ejemplos <- head(sort(valores), 5)
    cat("\n", var, ":\n")
    cat("  Ejemplos (5 menores):", paste(round(ejemplos, 2), collapse = ", "), "\n")
    cat("  Si son milisegundos:", paste(round(ejemplos/1000, 2), "seg", collapse = ", "), "\n")
    cat("  Si son segundos:", paste(round(ejemplos/60, 2), "min", collapse = ", "), "\n")
    cat("  Mediana:", round(median(valores), 2), "\n")
    if (median(valores) < 10000) {
      cat("  -> Parece que son MILISEGUNDOS (mediana =", round(median(valores)/1000, 2), "seg)\n")
    } else {
      cat("  -> Podrían ser SEGUNDOS (mediana =", round(median(valores)/60, 2), "min)\n")
    }
  }
}
cat("\n")

# Función para calcular estadísticas descriptivas
calcular_estadisticas <- function(x, nombre_var) {
  x_limpio <- as.numeric(x)
  x_limpio <- x_limpio[!is.na(x_limpio) & x_limpio > 0]  # Eliminar NAs y valores no válidos
  
  if (length(x_limpio) == 0) {
    return(data.frame(
      variable = nombre_var,
      n = 0,
      media = NA,
      sd = NA,
      mediana = NA,
      q25 = NA,
      q75 = NA,
      iqr = NA,
      min = NA,
      max = NA
    ))
  }
  
  data.frame(
    variable = nombre_var,
    n = length(x_limpio),
    media = mean(x_limpio),
    sd = sd(x_limpio),
    mediana = median(x_limpio),
    q25 = quantile(x_limpio, 0.25),
    q75 = quantile(x_limpio, 0.75),
    iqr = IQR(x_limpio),
    min = min(x_limpio),
    max = max(x_limpio)
  )
}

# Calcular estadísticas para cada variable dicotómica
estadisticas_individuales <- map_dfr(vars_existentes, function(var) {
  calcular_estadisticas(datos[[var]], var)
})

# Imprimir estadísticas individuales
cat("=== ESTADÍSTICAS POR VARIABLE ===\n")
print(estadisticas_individuales)
cat("\n")

# Combinar todos los tiempos de respuesta de variables dicotómicas
todos_tiempos <- datos %>%
  select(all_of(vars_existentes)) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "tiempo") %>%
  mutate(tiempo = as.numeric(tiempo)) %>%
  filter(!is.na(tiempo) & tiempo > 0)

# Estadísticas combinadas
estadisticas_combinadas <- calcular_estadisticas(todos_tiempos$tiempo, "Todas las dicotómicas")
cat("=== ESTADÍSTICAS COMBINADAS (Todas las variables dicotómicas) ===\n")
print(estadisticas_combinadas)
cat("\n")

# Función para crear gráfico combinado (histograma + boxplot con puntos)
crear_grafico_combinado <- function(data, var_name, titulo) {
  # Preparar datos
  data_limpio <- data %>%
    filter(!is.na(tiempo) & tiempo > 0)
  
  if (nrow(data_limpio) == 0) {
    return(NULL)
  }
  
  # Calcular estadísticas para el gráfico
  stats <- calcular_estadisticas(data_limpio$tiempo, var_name)
  
  # Crear histograma
  hist_plot <- ggplot(data_limpio, aes(x = tiempo)) +
    geom_histogram(bins = 30, fill = "#4A90E2", color = "white", alpha = 0.7) +
    geom_vline(xintercept = stats$media, color = "#E74C3C", linetype = "dashed", linewidth = 1.2) +
    geom_vline(xintercept = stats$mediana, color = "#2ECC71", linetype = "dashed", linewidth = 1.2) +
    annotate("text", x = stats$media, y = Inf, label = "Media", 
             color = "#E74C3C", vjust = 1.5, hjust = 0.5, size = 3, fontface = "bold") +
    annotate("text", x = stats$mediana, y = Inf, label = "Mediana", 
             color = "#2ECC71", vjust = 1.5, hjust = 0.5, size = 3, fontface = "bold") +
    labs(
      title = paste0("Histograma: ", titulo),
      x = "Tiempo de respuesta (milisegundos)",
      y = "Frecuencia"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 11, face = "bold", hjust = 0.5),
      axis.text = element_text(size = 9),
      axis.title = element_text(size = 10)
    ) +
    scale_x_continuous(labels = comma)
  
  # Crear boxplot con puntos individuales
  box_plot <- ggplot(data_limpio, aes(x = "", y = tiempo)) +
    geom_boxplot(
      fill = "#3498DB",
      color = "#2C3E50",
      alpha = 0.7,
      outlier.shape = NA  # Ocultar outliers del boxplot para mostrarlos como puntos
    ) +
    geom_jitter(
      width = 0.2,
      height = 0,
      alpha = 0.4,
      color = "#34495E",
      size = 0.8
    ) +
    geom_point(
      data = data.frame(x = "", y = stats$mediana),
      aes(x = x, y = y),
      color = "#2ECC71",
      size = 3,
      shape = 18
    ) +
    labs(
      title = paste0("Boxplot con puntos: ", titulo),
      x = "",
      y = "Tiempo de respuesta (ms)"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 11, face = "bold", hjust = 0.5),
      axis.text.x = element_blank(),
      axis.text.y = element_text(size = 9),
      axis.title = element_text(size = 10),
      axis.ticks.x = element_blank()
    ) +
    scale_y_continuous(labels = comma)
  
  # Crear tabla de estadísticas
  stats_text <- paste0(
    "n = ", stats$n, "\n",
    "Media = ", round(stats$media, 2), " ms\n",
    "SD = ", round(stats$sd, 2), " ms\n",
    "Mediana = ", round(stats$mediana, 2), " ms\n",
    "IQR = ", round(stats$iqr, 2), " ms\n",
    "Min = ", round(stats$min, 2), " ms\n",
    "Max = ", round(stats$max, 2), " ms"
  )
  
  stats_table <- ggplot() +
    annotate(
      "text",
      x = 0.5,
      y = 0.5,
      label = stats_text,
      size = 3.5,
      hjust = 0.5,
      vjust = 0.5,
      family = "mono"
    ) +
    theme_void() +
    labs(title = "Estadísticas Descriptivas") +
    theme(
      plot.title = element_text(size = 11, face = "bold", hjust = 0.5, margin = margin(b = 10))
    )
  
  # Combinar gráficos
  grid.arrange(
    hist_plot,
    box_plot,
    stats_table,
    ncol = 3,
    widths = c(2, 2, 1.2),
    top = paste0("Análisis de Tiempos de Respuesta: ", titulo)
  )
}

# Crear gráficos individuales para cada variable dicotómica
cat("Generando gráficos individuales...\n\n")

for (var in vars_existentes) {
  # Preparar datos para esta variable
  datos_var <- datos %>%
    select(all_of(var)) %>%
    rename(tiempo = all_of(var)) %>%
    mutate(variable = var)
  
  # Nombre más legible
  nombre_legible <- case_when(
    var == "dt02_gend" ~ "Género (dt02_gend)",
    var == "dt05_inte" ~ "Internet (dt05_inte)",
    var == "dt39_pact" ~ "Pacto (dt39_pact)",
    TRUE ~ var
  )
  
  # Crear y guardar gráfico
  png(
    filename = paste0("grafico_", var, ".png"),
    width = 14,
    height = 5,
    units = "in",
    res = 300
  )
  
  crear_grafico_combinado(datos_var, var, nombre_legible)
  
  dev.off()
  
  cat("Gráfico guardado: grafico_", var, ".png\n", sep = "")
}

# Crear gráfico combinado para todas las variables dicotómicas juntas
cat("\nGenerando gráfico combinado (todas las variables)...\n\n")

png(
  filename = "grafico_todas_dicotomicas.png",
  width = 14,
  height = 5,
  units = "in",
  res = 300
)

crear_grafico_combinado(todos_tiempos, "Todas", "Todas las Variables Dicotómicas")

dev.off()

cat("Gráfico guardado: grafico_todas_dicotomicas.png\n\n")

# Crear gráfico comparativo: todas las variables en un solo panel
cat("Generando gráfico comparativo...\n\n")

# Boxplot comparativo
boxplot_comparativo <- ggplot(todos_tiempos, aes(x = variable, y = tiempo, fill = variable)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.3, size = 0.6, color = "#34495E") +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Comparación de Tiempos de Respuesta entre Variables Dicotómicas",
    x = "Variable",
    y = "Tiempo de respuesta (ms)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5, margin = margin(b = 15)),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    legend.position = "none"
  ) +
  scale_y_continuous(labels = comma)

# Guardar gráfico comparativo
png(
  filename = "grafico_comparativo_dicotomicas.png",
  width = 10,
  height = 6,
  units = "in",
  res = 300
)

print(boxplot_comparativo)
dev.off()

cat("Gráfico guardado: grafico_comparativo_dicotomicas.png\n\n")

cat("=== ANÁLISIS COMPLETADO ===\n")
cat("Revisa los gráficos generados para identificar posibles outliers.\n")
