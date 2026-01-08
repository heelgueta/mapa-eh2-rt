# =============================================================================
# Análisis de Proporciones del Tiempo Total - Variables Likert
# Control de Variabilidad Intra-Sujeto
# =============================================================================

# Cargar librerías necesarias
if (!require("tidyverse")) install.packages("tidyverse", dependencies = TRUE)
if (!require("ggplot2")) install.packages("ggplot2", dependencies = TRUE)
if (!require("gridExtra")) install.packages("gridExtra", dependencies = TRUE)
if (!require("scales")) install.packages("scales", dependencies = TRUE)
if (!require("viridis")) install.packages("viridis", dependencies = TRUE)

library(tidyverse)
library(ggplot2)
library(gridExtra)
library(scales)
library(viridis)

# Cargar datos
datos <- read.csv("mapa-eh2-rt-anon.csv", stringsAsFactors = FALSE)

# Identificar variables Likert de tiempo de respuesta
vars_likert <- c(
  "lt06_awa1", "lt07_awa2", "lt08_awa3",
  "lt09_tru1", "lt10_tru2", "lt11_tru3",
  "lt12_pbe1", "lt13_pbe2", "lt14_pbe3",
  "lt15_sef1", "lt16_sef2", "lt17_sef3",
  "lt18_exp1", "lt19_exp2", "lt20_exp3",
  "lt21_sup1", "lt22_sup2", "lt23_sup3",
  "lt24_opp1", "lt25_opp2", "lt26_opp3",
  "lt27_loc1", "lt28_loc2", "lt29_loc3",
  "lt30_reg1", "lt31_reg2", "lt32_reg3",
  "lt33_amb1", "lt34_amb2", "lt35_amb3",
  "lt36_eco1", "lt37_eco2", "lt38_eco3"
)

# Verificar que las variables existen
vars_existentes <- vars_likert[vars_likert %in% names(datos)]
cat("=== ANÁLISIS DE PROPORCIONES DEL TIEMPO TOTAL ===\n")
cat("Variables Likert encontradas:", length(vars_existentes), "de", length(vars_likert), "\n\n")

# Verificar que existe tott
if (!"tott" %in% names(datos)) {
  stop("La variable 'tott' (tiempo total) no se encuentra en el dataset")
}

# Crear ID de persona
datos$persona_id <- 1:nrow(datos)

# Calcular proporciones del tiempo total para cada item Likert
cat("Calculando proporciones del tiempo total por persona...\n\n")

# Preparar datos: calcular porcentaje del tiempo total para cada variable Likert
datos_proporciones <- datos %>%
  select(persona_id, tott, all_of(vars_existentes)) %>%
  mutate(tott = as.numeric(tott)) %>%
  # Convertir a formato largo
  pivot_longer(
    cols = all_of(vars_existentes),
    names_to = "variable",
    values_to = "tiempo"
  ) %>%
  mutate(
    tiempo = as.numeric(tiempo),
    # Calcular porcentaje del tiempo total
    porcentaje = ifelse(
      !is.na(tott) & tott > 0 & !is.na(tiempo) & tiempo > 0,
      (tiempo / tott) * 100,
      NA
    )
  ) %>%
  filter(!is.na(porcentaje) & porcentaje > 0)

cat("Total de observaciones con proporciones válidas:", nrow(datos_proporciones), "\n")
cat("Número de personas:", length(unique(datos_proporciones$persona_id)), "\n\n")

# Función para calcular estadísticas descriptivas
calcular_estadisticas <- function(x, nombre_var) {
  x_limpio <- x[!is.na(x) & x > 0]
  
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

# Función para identificar outliers usando IQR
identificar_outliers <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR_val <- IQR(x, na.rm = TRUE)
  limite_inferior <- Q1 - 1.5 * IQR_val
  limite_superior <- Q3 + 1.5 * IQR_val
  return(x >= limite_inferior & x <= limite_superior)
}

# Calcular estadísticas para cada variable Likert (en porcentajes)
cat("Calculando estadísticas por variable...\n")
estadisticas_individuales <- map_dfr(vars_existentes, function(var) {
  datos_var <- datos_proporciones %>%
    filter(variable == var)
  calcular_estadisticas(datos_var$porcentaje, var)
})

# Imprimir estadísticas individuales (resumen)
cat("=== ESTADÍSTICAS POR VARIABLE (Porcentaje del tiempo total) ===\n")
print(estadisticas_individuales %>% 
  select(variable, n, media, mediana, sd, min, max) %>%
  arrange(media))
cat("\n")

# Guardar estadísticas completas
write.csv(
  estadisticas_individuales,
  file = "estadisticas_proporciones_likert_individuales.csv",
  row.names = FALSE
)
cat("Estadísticas guardadas: estadisticas_proporciones_likert_individuales.csv\n\n")

# Estadísticas combinadas
estadisticas_combinadas <- calcular_estadisticas(datos_proporciones$porcentaje, "Todas las Likert")
cat("=== ESTADÍSTICAS COMBINADAS (Todas las variables Likert) ===\n")
print(estadisticas_combinadas)
cat("\n")

# Función para crear gráfico combinado (histograma + boxplot con puntos)
crear_grafico_combinado <- function(data, var_name, titulo) {
  # Preparar datos
  data_limpio <- data %>%
    filter(!is.na(porcentaje) & porcentaje > 0)
  
  if (nrow(data_limpio) == 0) {
    return(NULL)
  }
  
  # Calcular estadísticas para el gráfico
  stats <- calcular_estadisticas(data_limpio$porcentaje, var_name)
  
  # Crear histograma
  hist_plot <- ggplot(data_limpio, aes(x = porcentaje)) +
    geom_histogram(bins = 30, fill = "#9B59B6", color = "white", alpha = 0.7) +
    geom_vline(xintercept = stats$media, color = "#E74C3C", linetype = "dashed", linewidth = 1.2) +
    geom_vline(xintercept = stats$mediana, color = "#2ECC71", linetype = "dashed", linewidth = 1.2) +
    annotate("text", x = stats$media, y = Inf, label = "Media", 
             color = "#E74C3C", vjust = 1.5, hjust = 0.5, size = 3, fontface = "bold") +
    annotate("text", x = stats$mediana, y = Inf, label = "Mediana", 
             color = "#2ECC71", vjust = 1.5, hjust = 0.5, size = 3, fontface = "bold") +
    labs(
      title = paste0("Histograma: ", titulo),
      x = "Porcentaje del tiempo total (%)",
      y = "Frecuencia"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 11, face = "bold", hjust = 0.5),
      axis.text = element_text(size = 9),
      axis.title = element_text(size = 10)
    ) +
    scale_x_continuous(labels = function(x) paste0(x, "%"))
  
  # Crear boxplot con puntos individuales
  box_plot <- ggplot(data_limpio, aes(x = "", y = porcentaje)) +
    geom_boxplot(
      fill = "#9B59B6",
      color = "#2C3E50",
      alpha = 0.7,
      outlier.shape = NA
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
      y = "Porcentaje del tiempo total (%)"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 11, face = "bold", hjust = 0.5),
      axis.text.x = element_blank(),
      axis.text.y = element_text(size = 9),
      axis.title = element_text(size = 10),
      axis.ticks.x = element_blank()
    ) +
    scale_y_continuous(labels = function(x) paste0(x, "%"))
  
  # Crear tabla de estadísticas
  stats_text <- paste0(
    "n = ", stats$n, "\n",
    "Media = ", round(stats$media, 3), " %\n",
    "SD = ", round(stats$sd, 3), " %\n",
    "Mediana = ", round(stats$mediana, 3), " %\n",
    "IQR = ", round(stats$iqr, 3), " %\n",
    "Min = ", round(stats$min, 3), " %\n",
    "Max = ", round(stats$max, 3), " %"
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
    top = paste0("Análisis de Proporciones del Tiempo Total: ", titulo)
  )
}

# Crear gráfico combinado para todas las variables Likert juntas
cat("Generando gráfico combinado (todas las variables Likert)...\n\n")

png(
  filename = "grafico_proporciones_todas_likert.png",
  width = 14,
  height = 5,
  units = "in",
  res = 300
)

crear_grafico_combinado(datos_proporciones, "Todas", "Todas las Variables Likert")

dev.off()

cat("Gráfico guardado: grafico_proporciones_todas_likert.png\n\n")

# Versión sin outliers del gráfico combinado
cat("Generando gráfico combinado SIN OUTLIERS...\n\n")

# Identificar outliers en todos los porcentajes combinados
datos_proporciones_sin_outliers_combinado <- datos_proporciones %>%
  filter(identificar_outliers(porcentaje))

png(
  filename = "grafico_proporciones_todas_likert_sin_outliers.png",
  width = 14,
  height = 5,
  units = "in",
  res = 300
)

crear_grafico_combinado(datos_proporciones_sin_outliers_combinado, "Todas", "Todas las Variables Likert (SIN OUTLIERS)")

dev.off()

cat("Gráfico guardado: grafico_proporciones_todas_likert_sin_outliers.png\n\n")

# Crear gráfico comparativo: todas las variables en un solo panel
cat("Generando gráfico comparativo...\n\n")

# Boxplot comparativo
boxplot_comparativo <- ggplot(datos_proporciones, aes(x = variable, y = porcentaje, fill = variable)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.2, size = 0.4, color = "#34495E") +
  scale_fill_viridis_d(option = "plasma", guide = "none") +
  labs(
    title = "Comparación de Proporciones del Tiempo Total entre Variables Likert",
    subtitle = paste("Total de", length(vars_existentes), "variables - Control de variabilidad intra-sujeto"),
    x = "Variable",
    y = "Porcentaje del tiempo total (%)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5, margin = margin(b = 10)),
    plot.subtitle = element_text(size = 11, hjust = 0.5, margin = margin(b = 15)),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 8),
    axis.text.y = element_text(size = 9),
    axis.title = element_text(size = 12),
    legend.position = "none"
  ) +
  scale_y_continuous(labels = function(x) paste0(x, "%"))

# Guardar gráfico comparativo
png(
  filename = "grafico_proporciones_comparativo_likert.png",
  width = 16,
  height = 8,
  units = "in",
  res = 300
)

print(boxplot_comparativo)
dev.off()

cat("Gráfico guardado: grafico_proporciones_comparativo_likert.png\n\n")

# Versión sin outliers del gráfico comparativo
cat("Generando gráfico comparativo SIN OUTLIERS...\n\n")

# Identificar outliers por variable
datos_proporciones_sin_outliers <- datos_proporciones %>%
  group_by(variable) %>%
  mutate(es_outlier = !identificar_outliers(porcentaje)) %>%
  ungroup() %>%
  filter(!es_outlier)

boxplot_comparativo_sin_outliers <- ggplot(datos_proporciones_sin_outliers, aes(x = variable, y = porcentaje, fill = variable)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.2, size = 0.4, color = "#34495E") +
  scale_fill_viridis_d(option = "plasma", guide = "none") +
  labs(
    title = "Comparación de Proporciones del Tiempo Total entre Variables Likert",
    subtitle = paste("SIN OUTLIERS - Total de", length(vars_existentes), "variables - Control de variabilidad intra-sujeto"),
    x = "Variable",
    y = "Porcentaje del tiempo total (%)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5, margin = margin(b = 10)),
    plot.subtitle = element_text(size = 11, hjust = 0.5, margin = margin(b = 15)),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 8),
    axis.text.y = element_text(size = 9),
    axis.title = element_text(size = 12),
    legend.position = "none"
  ) +
  scale_y_continuous(labels = function(x) paste0(x, "%"))

png(
  filename = "grafico_proporciones_comparativo_likert_sin_outliers.png",
  width = 16,
  height = 8,
  units = "in",
  res = 300
)

print(boxplot_comparativo_sin_outliers)
dev.off()

cat("Gráfico guardado: grafico_proporciones_comparativo_likert_sin_outliers.png\n\n")

# Versión ordenada por mediana del gráfico comparativo sin outliers
cat("Generando gráfico comparativo SIN OUTLIERS ordenado por mediana...\n\n")

# Calcular medianas por variable para ordenar
medianas_por_variable <- datos_proporciones_sin_outliers %>%
  group_by(variable) %>%
  summarise(mediana = median(porcentaje), .groups = "drop") %>%
  arrange(mediana)

# Crear factor ordenado por mediana
datos_proporciones_sin_outliers_ordenado <- datos_proporciones_sin_outliers %>%
  mutate(
    variable_ordenada = factor(variable, levels = medianas_por_variable$variable)
  )

boxplot_comparativo_sin_outliers_ordenado <- ggplot(
  datos_proporciones_sin_outliers_ordenado, 
  aes(x = variable_ordenada, y = porcentaje, fill = variable_ordenada)
) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.2, size = 0.4, color = "#34495E") +
  scale_fill_viridis_d(option = "plasma", guide = "none") +
  labs(
    title = "Comparación de Proporciones del Tiempo Total entre Variables Likert",
    subtitle = paste("SIN OUTLIERS - Ordenado por mediana - Total de", length(vars_existentes), "variables - Control de variabilidad intra-sujeto"),
    x = "Variable (ordenadas por mediana)",
    y = "Porcentaje del tiempo total (%)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5, margin = margin(b = 10)),
    plot.subtitle = element_text(size = 11, hjust = 0.5, margin = margin(b = 15)),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 8),
    axis.text.y = element_text(size = 9),
    axis.title = element_text(size = 12),
    legend.position = "none"
  ) +
  scale_y_continuous(labels = function(x) paste0(x, "%"))

png(
  filename = "grafico_proporciones_comparativo_likert_sin_outliers_ordenado_mediana.png",
  width = 16,
  height = 8,
  units = "in",
  res = 300
)

print(boxplot_comparativo_sin_outliers_ordenado)
dev.off()

cat("Gráfico guardado: grafico_proporciones_comparativo_likert_sin_outliers_ordenado_mediana.png\n\n")

# Crear gráfico de distribución de medias por variable
cat("Generando gráfico de distribución de medias...\n\n")

medias_plot <- estadisticas_individuales %>%
  ggplot(aes(x = reorder(variable, media), y = media)) +
  geom_bar(stat = "identity", fill = "#9B59B6", alpha = 0.8) +
  geom_errorbar(
    aes(ymin = media - sd, ymax = media + sd),
    width = 0.3,
    color = "#2C3E50",
    linewidth = 0.5
  ) +
  labs(
    title = "Media de Proporciones del Tiempo Total por Variable Likert",
    subtitle = "Ordenadas de menor a mayor porcentaje promedio - Control de variabilidad intra-sujeto",
    x = "Variable",
    y = "Porcentaje promedio del tiempo total (%)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5, margin = margin(b = 10)),
    plot.subtitle = element_text(size = 11, hjust = 0.5, margin = margin(b = 15)),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 8),
    axis.text.y = element_text(size = 9),
    axis.title = element_text(size = 12)
  ) +
  scale_y_continuous(labels = function(x) paste0(x, "%"))

png(
  filename = "grafico_proporciones_barras_medias_likert.png",
  width = 16,
  height = 8,
  units = "in",
  res = 300
)

print(medias_plot)
dev.off()

cat("Gráfico guardado: grafico_proporciones_barras_medias_likert.png\n\n")

# Agrupar variables por constructo
datos_proporciones_constructo <- datos_proporciones %>%
  mutate(
    constructo = case_when(
      str_detect(variable, "awa") ~ "Awareness",
      str_detect(variable, "tru") ~ "Trust",
      str_detect(variable, "pbe") ~ "Perceived Benefits",
      str_detect(variable, "sef") ~ "Self-Efficacy",
      str_detect(variable, "exp") ~ "Expectativas de Concreción",
      str_detect(variable, "sup") ~ "Support",
      str_detect(variable, "opp") ~ "Opposition",
      str_detect(variable, "loc") ~ "Local Identity",
      str_detect(variable, "reg") ~ "Regional Identity",
      str_detect(variable, "amb") ~ "Actitudes Pro Ambientales",
      str_detect(variable, "eco") ~ "Ecocentrismo",
      TRUE ~ "Otro"
    )
  )

# Estadísticas por constructo
estadisticas_constructo <- datos_proporciones_constructo %>%
  group_by(constructo) %>%
  summarise(
    n = n(),
    media = mean(porcentaje),
    mediana = median(porcentaje),
    sd = sd(porcentaje),
    .groups = "drop"
  ) %>%
  arrange(media)

cat("=== ESTADÍSTICAS POR CONSTRUCTO (Proporciones) ===\n")
print(estadisticas_constructo)
cat("\n")

# Gráfico comparativo por constructo
constructo_plot <- datos_proporciones_constructo %>%
  ggplot(aes(x = reorder(constructo, porcentaje, median), y = porcentaje, fill = constructo)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.3, size = 0.5, color = "#34495E") +
  scale_fill_viridis_d(option = "plasma", guide = "none") +
  labs(
    title = "Comparación de Proporciones del Tiempo Total por Constructo",
    subtitle = "Agrupación de variables Likert por constructo teórico - Control de variabilidad intra-sujeto",
    x = "Constructo",
    y = "Porcentaje del tiempo total (%)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5, margin = margin(b = 10)),
    plot.subtitle = element_text(size = 11, hjust = 0.5, margin = margin(b = 15)),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 9),
    axis.title = element_text(size = 12),
    legend.position = "none"
  ) +
  scale_y_continuous(labels = function(x) paste0(x, "%"))

png(
  filename = "grafico_proporciones_constructos_likert.png",
  width = 12,
  height = 7,
  units = "in",
  res = 300
)

print(constructo_plot)
dev.off()

cat("Gráfico guardado: grafico_proporciones_constructos_likert.png\n\n")

# Versión sin outliers del gráfico por constructo
cat("Generando gráfico por constructo SIN OUTLIERS...\n\n")

datos_proporciones_constructo_sin_outliers <- datos_proporciones_constructo %>%
  group_by(constructo) %>%
  mutate(es_outlier = !identificar_outliers(porcentaje)) %>%
  ungroup() %>%
  filter(!es_outlier)

constructo_plot_sin_outliers <- datos_proporciones_constructo_sin_outliers %>%
  ggplot(aes(x = reorder(constructo, porcentaje, median), y = porcentaje, fill = constructo)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.3, size = 0.5, color = "#34495E") +
  scale_fill_viridis_d(option = "plasma", guide = "none") +
  labs(
    title = "Comparación de Proporciones del Tiempo Total por Constructo",
    subtitle = "SIN OUTLIERS - Agrupación de variables Likert por constructo teórico - Control de variabilidad intra-sujeto",
    x = "Constructo",
    y = "Porcentaje del tiempo total (%)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5, margin = margin(b = 10)),
    plot.subtitle = element_text(size = 11, hjust = 0.5, margin = margin(b = 15)),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 9),
    axis.title = element_text(size = 12),
    legend.position = "none"
  ) +
  scale_y_continuous(labels = function(x) paste0(x, "%"))

png(
  filename = "grafico_proporciones_constructos_likert_sin_outliers.png",
  width = 12,
  height = 7,
  units = "in",
  res = 300
)

print(constructo_plot_sin_outliers)
dev.off()

cat("Gráfico guardado: grafico_proporciones_constructos_likert_sin_outliers.png\n\n")

cat("=== ANÁLISIS DE PROPORCIONES COMPLETADO ===\n")
cat("Este análisis controla la variabilidad intra-sujeto normalizando por el tiempo total.\n")
cat("Total de variables Likert analizadas:", length(vars_existentes), "\n")
