# =============================================================================
# Análisis de Tiempos de Respuesta - Variables Likert
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
# Variables que empiezan con "lt" son Likert
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
cat("Variables Likert encontradas:", length(vars_existentes), "de", length(vars_likert), "\n")
cat("Variables:", paste(vars_existentes, collapse = ", "), "\n\n")

# Verificación de unidades: mostrar algunos ejemplos para confirmar si son ms o seg
cat("=== VERIFICACIÓN DE UNIDADES ===\n")
cat("Mostrando algunos valores de ejemplo para verificar unidades:\n")
# Muestra solo las primeras 3 variables para no saturar
for (var in head(vars_existentes, 3)) {
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

# Función para identificar outliers usando IQR
identificar_outliers <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR_val <- IQR(x, na.rm = TRUE)
  limite_inferior <- Q1 - 1.5 * IQR_val
  limite_superior <- Q3 + 1.5 * IQR_val
  return(x >= limite_inferior & x <= limite_superior)
}

# Calcular estadísticas para cada variable Likert
cat("Calculando estadísticas por variable...\n")
estadisticas_individuales <- map_dfr(vars_existentes, function(var) {
  calcular_estadisticas(datos[[var]], var)
})

# Imprimir estadísticas individuales (resumen)
cat("=== ESTADÍSTICAS POR VARIABLE (Resumen) ===\n")
print(estadisticas_individuales %>% 
  select(variable, n, media, mediana, sd, min, max) %>%
  arrange(media))
cat("\n")

# Guardar estadísticas completas
write.csv(
  estadisticas_individuales,
  file = "estadisticas_likert_individuales.csv",
  row.names = FALSE
)
cat("Estadísticas guardadas: estadisticas_likert_individuales.csv\n\n")

# Combinar todos los tiempos de respuesta de variables Likert
todos_tiempos <- datos %>%
  select(all_of(vars_existentes)) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "tiempo") %>%
  mutate(tiempo = as.numeric(tiempo)) %>%
  filter(!is.na(tiempo) & tiempo > 0)

# Estadísticas combinadas
estadisticas_combinadas <- calcular_estadisticas(todos_tiempos$tiempo, "Todas las Likert")
cat("=== ESTADÍSTICAS COMBINADAS (Todas las variables Likert) ===\n")
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
    geom_histogram(bins = 30, fill = "#9B59B6", color = "white", alpha = 0.7) +
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
      fill = "#9B59B6",
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

# Crear gráfico combinado para todas las variables Likert juntas
cat("Generando gráfico combinado (todas las variables Likert)...\n\n")

png(
  filename = "grafico_todas_likert.png",
  width = 14,
  height = 5,
  units = "in",
  res = 300
)

crear_grafico_combinado(todos_tiempos, "Todas", "Todas las Variables Likert")

dev.off()

cat("Gráfico guardado: grafico_todas_likert.png\n\n")

# Versión sin outliers del gráfico combinado
cat("Generando gráfico combinado SIN OUTLIERS...\n\n")

# Identificar outliers en todos los tiempos combinados
todos_tiempos_sin_outliers_combinado <- todos_tiempos %>%
  filter(identificar_outliers(tiempo))

png(
  filename = "grafico_todas_likert_sin_outliers.png",
  width = 14,
  height = 5,
  units = "in",
  res = 300
)

crear_grafico_combinado(todos_tiempos_sin_outliers_combinado, "Todas", "Todas las Variables Likert (SIN OUTLIERS)")

dev.off()

cat("Gráfico guardado: grafico_todas_likert_sin_outliers.png\n\n")

# Crear gráfico comparativo: todas las variables en un solo panel
cat("Generando gráfico comparativo...\n\n")

# Boxplot comparativo (con rotación de etiquetas para que se vean)
boxplot_comparativo <- ggplot(todos_tiempos, aes(x = variable, y = tiempo, fill = variable)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.2, size = 0.4, color = "#34495E") +
  scale_fill_viridis_d(option = "plasma", guide = "none") +
  labs(
    title = "Comparación de Tiempos de Respuesta entre Variables Likert",
    subtitle = paste("Total de", length(vars_existentes), "variables"),
    x = "Variable",
    y = "Tiempo de respuesta (ms)"
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
  scale_y_continuous(labels = comma)

# Guardar gráfico comparativo
png(
  filename = "grafico_comparativo_likert.png",
  width = 16,
  height = 8,
  units = "in",
  res = 300
)

print(boxplot_comparativo)
dev.off()

cat("Gráfico guardado: grafico_comparativo_likert.png\n\n")

# Versión sin outliers del gráfico comparativo
cat("Generando gráfico comparativo SIN OUTLIERS...\n\n")

# Identificar outliers por variable
todos_tiempos_sin_outliers <- todos_tiempos %>%
  group_by(variable) %>%
  mutate(es_outlier = !identificar_outliers(tiempo)) %>%
  ungroup() %>%
  filter(!es_outlier)

boxplot_comparativo_sin_outliers <- ggplot(todos_tiempos_sin_outliers, aes(x = variable, y = tiempo, fill = variable)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.2, size = 0.4, color = "#34495E") +
  scale_fill_viridis_d(option = "plasma", guide = "none") +
  labs(
    title = "Comparación de Tiempos de Respuesta entre Variables Likert",
    subtitle = paste("SIN OUTLIERS - Total de", length(vars_existentes), "variables"),
    x = "Variable",
    y = "Tiempo de respuesta (ms)"
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
  scale_y_continuous(labels = comma)

png(
  filename = "grafico_comparativo_likert_sin_outliers.png",
  width = 16,
  height = 8,
  units = "in",
  res = 300
)

print(boxplot_comparativo_sin_outliers)
dev.off()

cat("Gráfico guardado: grafico_comparativo_likert_sin_outliers.png\n\n")

# Versión ordenada por mediana del gráfico comparativo sin outliers
cat("Generando gráfico comparativo SIN OUTLIERS ordenado por mediana...\n\n")

# Calcular medianas por variable para ordenar
medianas_por_variable <- todos_tiempos_sin_outliers %>%
  group_by(variable) %>%
  summarise(mediana = median(tiempo), .groups = "drop") %>%
  arrange(mediana)

# Crear factor ordenado por mediana
todos_tiempos_sin_outliers_ordenado <- todos_tiempos_sin_outliers %>%
  mutate(
    variable_ordenada = factor(variable, levels = medianas_por_variable$variable)
  )

boxplot_comparativo_sin_outliers_ordenado <- ggplot(
  todos_tiempos_sin_outliers_ordenado, 
  aes(x = variable_ordenada, y = tiempo, fill = variable_ordenada)
) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.2, size = 0.4, color = "#34495E") +
  scale_fill_viridis_d(option = "plasma", guide = "none") +
  labs(
    title = "Comparación de Tiempos de Respuesta entre Variables Likert",
    subtitle = paste("SIN OUTLIERS - Ordenado por mediana - Total de", length(vars_existentes), "variables"),
    x = "Variable (ordenadas por mediana)",
    y = "Tiempo de respuesta (ms)"
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
  scale_y_continuous(labels = comma)

png(
  filename = "grafico_comparativo_likert_sin_outliers_ordenado_mediana.png",
  width = 16,
  height = 8,
  units = "in",
  res = 300
)

print(boxplot_comparativo_sin_outliers_ordenado)
dev.off()

cat("Gráfico guardado: grafico_comparativo_likert_sin_outliers_ordenado_mediana.png\n\n")

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
  geom_hline(
    yintercept = mean(estadisticas_individuales$media, na.rm = TRUE),
    color = "#E74C3C",
    linetype = "dashed",
    linewidth = 1
  ) +
  labs(
    title = "Media de Tiempos de Respuesta por Variable Likert",
    subtitle = "Ordenadas de menor a mayor tiempo promedio",
    x = "Variable",
    y = "Tiempo promedio (ms)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5, margin = margin(b = 10)),
    plot.subtitle = element_text(size = 11, hjust = 0.5, margin = margin(b = 15)),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 8),
    axis.text.y = element_text(size = 9),
    axis.title = element_text(size = 12)
  ) +
  scale_y_continuous(labels = comma)

png(
  filename = "barras_medias_likert.png",
  width = 16,
  height = 8,
  units = "in",
  res = 300
)

print(medias_plot)
dev.off()

cat("Gráfico guardado: barras_medias_likert.png\n\n")

# Agrupar variables por constructo (si es posible identificar patrones)
# Extraer el constructo de cada variable (awa, tru, pbe, sef, exp, sup, opp, loc, reg, amb, eco)
estadisticas_individuales <- estadisticas_individuales %>%
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
estadisticas_constructo <- todos_tiempos %>%
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
  ) %>%
  group_by(constructo) %>%
  summarise(
    n = n(),
    media = mean(tiempo),
    mediana = median(tiempo),
    sd = sd(tiempo),
    .groups = "drop"
  ) %>%
  arrange(media)

cat("=== ESTADÍSTICAS POR CONSTRUCTO ===\n")
print(estadisticas_constructo)
cat("\n")

# Gráfico comparativo por constructo
constructo_plot <- todos_tiempos %>%
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
  ) %>%
  ggplot(aes(x = reorder(constructo, tiempo, median), y = tiempo, fill = constructo)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.3, size = 0.5, color = "#34495E") +
  scale_fill_viridis_d(option = "plasma", guide = "none") +
  labs(
    title = "Comparación de Tiempos de Respuesta por Constructo",
    subtitle = "Agrupación de variables Likert por constructo teórico",
    x = "Constructo",
    y = "Tiempo de respuesta (ms)"
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
  scale_y_continuous(labels = comma)

png(
  filename = "grafico_constructos_likert.png",
  width = 12,
  height = 7,
  units = "in",
  res = 300
)

print(constructo_plot)
dev.off()

cat("Gráfico guardado: grafico_constructos_likert.png\n\n")

# Versión sin outliers del gráfico por constructo
cat("Generando gráfico por constructo SIN OUTLIERS...\n\n")

todos_tiempos_constructo_sin_outliers <- todos_tiempos %>%
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
  ) %>%
  group_by(constructo) %>%
  mutate(es_outlier = !identificar_outliers(tiempo)) %>%
  ungroup() %>%
  filter(!es_outlier)

constructo_plot_sin_outliers <- todos_tiempos_constructo_sin_outliers %>%
  ggplot(aes(x = reorder(constructo, tiempo, median), y = tiempo, fill = constructo)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.3, size = 0.5, color = "#34495E") +
  scale_fill_viridis_d(option = "plasma", guide = "none") +
  labs(
    title = "Comparación de Tiempos de Respuesta por Constructo",
    subtitle = "SIN OUTLIERS - Agrupación de variables Likert por constructo teórico",
    x = "Constructo",
    y = "Tiempo de respuesta (ms)"
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
  scale_y_continuous(labels = comma)

png(
  filename = "grafico_constructos_likert_sin_outliers.png",
  width = 12,
  height = 7,
  units = "in",
  res = 300
)

print(constructo_plot_sin_outliers)
dev.off()

cat("Gráfico guardado: grafico_constructos_likert_sin_outliers.png\n\n")

cat("=== ANÁLISIS COMPLETADO ===\n")
cat("Revisa los gráficos generados para identificar posibles outliers.\n")
cat("Total de variables Likert analizadas:", length(vars_existentes), "\n")
