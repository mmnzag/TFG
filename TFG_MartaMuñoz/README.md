# Trabajo de Fin de Grado: Integración de Análisis Formal de Conceptos y Procesamiento de Lenguaje Natural para el Estudio de Dinámicas Sociales en Reddit

**Autora:** Marta Muñoz Aguilar  
**Tutores:** Dr. Ángel Mora Bonilla y Dr. Domingo López Rodríguez  

## Descripción
Este repositorio contiene el código fuente, los *notebooks* computacionales y la aplicación interactiva desarrollados para el Trabajo de Fin de Grado. El proyecto integra técnicas de Análisis Formal de Conceptos (FCA) y Procesamiento de Lenguaje Natural (NLP) aplicadas a conjuntos de datos extraídos de Reddit y Kaggle.

## Estructura del Directorio
El proyecto está organizado de la siguiente manera para facilitar su evaluación y reproducibilidad:

*   **`app.R`**: Código principal de la aplicación web interactiva desarrollada con R Shiny. Contiene tanto la definición de la interfaz de usuario (UI) como la lógica del servidor (Server).
*   **Archivos `.qmd`**: *Notebooks* de Quarto que contienen el código del análisis exploratorio, la limpieza de datos y el análisis matemático paso a paso.
*   **`_quarto.yml`**: Archivo de configuración global del proyecto Quarto.
*   **`data/`**: Directorio que incluye los conjuntos de datos necesarios:
    *   Archivos fuente originales (descargas de la API de Reddit y Kaggle).
    *   Archivos procesados (`.rds`) generados a partir de los `.qmd` y listos para ser consumidos por la aplicación interactiva.
*   **Carpetas de imágenes (`images/`)**: Recursos gráficos estáticos utilizados por la aplicación Shiny y los *notebooks*.

## Requisitos y Dependencias
Para ejecutar el código y la aplicación es necesario contar con [R](https://cran.r-project.org/) y [RStudio](https://posit.co/download/rstudio-desktop/). 

El entorno requiere la instalación de las siguientes librerías, categorizadas según su función en el proyecto:

* **Interfaz Web (Shiny):** `shiny`, `shinydashboard`, `shinyWidgets`, `DT`
* **Manipulación de Datos:** `dplyr`, `tidyr`, `tibble`, `purrr`, `stringr`, `rlang`, `glue`
* **Análisis de Grafos y Redes:** `igraph`, `tidygraph`, `visNetwork`
* **Procesamiento de Lenguaje Natural (NLP):** `syuzhet`, `quanteda`, `topicmodels`
* **Análisis Formal de Conceptos:** `fcaR`
* **Visualización Estática e Interactiva:** `ggplot2`, `ggraph`, `plotly`, `viridis`, `png`, `grid`
* **Peticiones API:** `httr2`

### Instalación rápida de dependencias
Para instalar todas las librerías necesarias de una sola vez, puede ejecutar el siguiente comando en la consola de R:

```r
paquetes_necesarios <- c("shiny", "shinydashboard", "ggplot2", "dplyr", "tidygraph", 
                         "ggraph", "viridis", "png", "grid", "fcaR", "DT", 
                         "visNetwork", "httr2", "plotly", "shinyWidgets", "glue", 
                         "rlang", "stringr", "syuzhet", "quanteda", "topicmodels", 
                         "igraph", "purrr", "tibble", "tidyr")

paquetes_a_instalar <- paquetes_necesarios[!(paquetes_necesarios %in% installed.packages()[,"Package"])]
if(length(paquetes_a_instalar)) install.packages(paquetes_a_instalar)
```

## Instrucciones de Ejecución

### 1. Despliegue de la Aplicación Interactiva (Evaluación Rápida)
La herramienta analítica está diseñada para ejecutarse sin necesidad de compilar los datos previamente, ya que los ficheros `.rds` procesados se incluyen en la carpeta `data/`.
1. Abrir el archivo `app.R` en RStudio.
2. Hacer clic en el botón **"Run App"** situado en la parte superior derecha del editor de código.

### 2. Reproducibilidad del Análisis (Archivos Quarto)
Para auditar la limpieza de datos y el desarrollo metodológico paso a paso:
1. Abrir cualquiera de los archivos `.qmd` en RStudio.
2. Ejecutar los bloques de código (*chunks*) secuencialmente o utilizar el botón **"Render"** para compilar el documento completo. Los *notebooks* leerán automáticamente los archivos fuente situados en la carpeta de datos.

## Notas Adicionales
*   Se han excluido de este directorio las carpetas temporales de caché y los archivos compilados de la memoria (`_book/`) para mantener el entorno de desarrollo limpio. La memoria del proyecto se entrega en formato PDF de manera independiente.