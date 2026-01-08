# Paradata y Procesos de Respuesta en Cuestionarios Digitales

Análisis exploratorio de tiempos de respuesta por ítem en un estudio piloto sobre actitudes hacia el hidrógeno verde en Magallanes, Chile.

## 📋 Contenido

Este repositorio contiene:

- **Datos**: `mapa-eh2-rt-anon.csv` - Datos anonimizados de tiempos de respuesta
- **Scripts de análisis R**:
  - `analisis_tiempos_dicotomicos.R` - Análisis de ítems dicotómicos
  - `analisis_tiempos_likert.R` - Análisis de ítems Likert
  - `analisis_tiempos_open.R` - Análisis de ítems abiertos
  - `analisis_tiempo_total.R` - Análisis del tiempo total de la encuesta
  - `analisis_por_persona_dicotomicos.R` - Análisis individual por persona
  - `analisis_proporciones_likert.R` - Análisis de proporciones del tiempo total (control intra-sujeto)
- **Presentación**: `presentacion.html` - Presentación académica interactiva

## 🚀 Uso

### Ver la presentación

Abre `presentacion.html` en tu navegador. La presentación es interactiva:

- **Flechas** ← → o **Espacio** para avanzar/retroceder
- **Home/End** para ir al inicio/final
- **Swipe** en dispositivos móviles

### Ejecutar los análisis

1. Asegúrate de tener R instalado
2. Instala las librerías necesarias (se instalan automáticamente si no están disponibles)
3. Ejecuta los scripts en R o RStudio:

```r
source("analisis_tiempos_dicotomicos.R")
source("analisis_tiempos_likert.R")
source("analisis_tiempos_open.R")
source("analisis_tiempo_total.R")
source("analisis_proporciones_likert.R")
```

Los scripts generarán:
- Gráficos PNG de alta resolución
- Archivos CSV con estadísticas descriptivas

## 📊 Estructura del análisis

### Tipos de ítems analizados

- **Ítems dicotómicos** (dt): Respuestas binarias (sí/no)
- **Ítems Likert** (lt): Escalas de acuerdo (33 ítems agrupados en 11 constructos)
- **Ítems abiertos** (ot): Respuestas de texto libre

### Análisis realizados

1. **Estadísticas descriptivas**: Media, mediana, SD, IQR, min, max por variable
2. **Visualizaciones**: Histogramas, boxplots, gráficos comparativos
3. **Análisis de outliers**: Identificación y versiones sin outliers
4. **Análisis por constructo**: Agrupación teórica de ítems Likert
5. **Control intra-sujeto**: Proporciones del tiempo total para normalizar diferencias individuales

## 🎯 Objetivo

Explorar el potencial metodológico del uso de paradata (tiempos de respuesta por ítem) en la medición de actitudes, problematizando la interacción entre participante e instrumento.

## 📝 Notas metodológicas

- Estudio de carácter **exploratorio y piloto**
- Muestra pequeña
- Aplicación presencial mediante dispositivos móviles
- Registro automático de eventos de interacción con resolución temporal a nivel de ítem

## 📄 Licencia

Este trabajo es parte de una investigación académica. Los datos están anonimizados.

## 👤 Autor

**Herman Elgueta Sepúlveda**  
Departamento de Psicología  
Universidad de Magallanes

---

*Para más detalles, ver la presentación en `presentacion.html`*
