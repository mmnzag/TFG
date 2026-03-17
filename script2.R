library(igraph)

library(RedditExtractoR)
library(tidyverse)
library(igraph)
library(ggraph)

# -------------------------------------------------------------------------
# PASO 1: DESCARGA DE DATOS (RedditExtractoR)
# -------------------------------------------------------------------------
cat("1. Buscando hilos en Reddit...\n")

# Buscamos hilos recientes sobre "AI" en el subreddit de tecnología
# Limitamos a 'period = "month"' para que sean recientes
hilos <- find_thread_urls(
  keywords = "AI", 
  subreddit = "technology", 
  sort_by = "comments", 
  period = "month"
)

# Para este ejemplo, cogemos SOLO el primer hilo (el que tiene más comentarios)
# para no saturar tu ordenador.
url_objetivo <- hilos$url[1] 
titulo_hilo <- hilos$title[1]

cat(paste("2. Descargando comentarios del hilo:", titulo_hilo, "\n"))
cat("   (Esto puede tardar unos segundos dependiendo de la cantidad de comentarios...)\n")

contenido <- get_thread_content(url_objetivo)
comentarios <- contenido$comments

# -------------------------------------------------------------------------
# PASO 2: LIMPIEZA Y CREACIÓN DE RELACIONES (Who talks to whom?)
# -------------------------------------------------------------------------
# Reddit nos da la estructura, pero necesitamos cruzar los datos para saber
# a qué USUARIO responde cada comentario.
# -------------------------------------------------------------------------
# PASO 2: LIMPIEZA Y CREACIÓN DE RELACIONES (Versión por Estructura)
# -------------------------------------------------------------------------

# 1. Nos quedamos con lo que tienes disponible
datos_limpios <- comentarios |>
  select(author, comment_id) |>
  filter(author != "[deleted]")

# 2. Creamos la columna "padre" manipulando el texto del "comment_id"
# Lógica: Si mi ID es "1_5_2", le quito el último trozo ("_2") y mi padre es "1_5"
datos_con_padre <- datos_limpios %>%
  # Quitamos todo desde el último guion bajo hasta el final
  mutate(target_id = sub("_[^_]+$", "", comment_id)) %>%
  # Si el ID no tenía guiones (ej. "1"), al quitarle el guion se queda igual.
  # Esos son comentarios raíz (no responden a otro usuario, sino al post), los quitamos.
  filter(target_id != comment_id)

# 3. Cruzamos los datos
enlaces <- datos_con_padre %>%
  # Buscamos al autor cuyo 'comment_id' coincida con mi 'target_id'
  left_join(datos_limpios, by = c("target_id" = "comment_id"), suffix = c("_replier", "_target")) %>%
  select(from = author_replier, to = author_target) %>%
  drop_na() %>%
  filter(from != to)

cat(paste("3. Se han encontrado", nrow(enlaces), "interacciones entre usuarios.\n"))

# -------------------------------------------------------------------------
# PASO 3: CREACIÓN DEL GRAFO (igraph puro)
# -------------------------------------------------------------------------
# Asumimos que ya tienes el dataframe 'enlaces' creado en el paso anterior

# 1. Crear el objeto grafo
g <- graph_from_data_frame(enlaces, directed = TRUE)

# 2. Simplificar: Nos quedamos con nodos que tengan más de 1 conexión
# (Para evitar que el dibujo sea una nube de puntos gigante)
g_simple <- induced_subgraph(g, vids = which(degree(g) > 1))

# -------------------------------------------------------------------------
# PASO 4: VISUALIZACIÓN CON PLOT BASE (Solo igraph)
# -------------------------------------------------------------------------

# PREPARACIÓN ESTÉTICA (igraph base es feo por defecto, vamos a tunearlo)

# A) Calculamos el "Grado" (importancia) para el tamaño de los nodos
deg <- degree(g_simple, mode = "in")

# B) Definimos etiquetas: Solo ponemos nombre si el usuario es "importante" (grado > 3)
# Si no, ponemos NA para que no salga texto
V(g_simple)$label <- ifelse(deg > 3, V(g_simple)$name, NA) 

# C) Color de los nodos (basado en el grado)
# Usamos una paleta de colores básica de R
colores <- colorRampPalette(c("lightblue", "orange", "red"))(max(deg)+1)
V(g_simple)$color <- colores[deg + 1]

# D) Dibujamos
# par(mar=...) reduce los márgenes para aprovechar la pantalla
par(mar = c(0,0,2,0)) 

plot(g_simple,
     layout = layout_with_fr,    # Algoritmo de fuerza (Fruchterman-Reingold)
     vertex.size = deg * 5,    # Tamaño del nodo proporcional a importancia
     vertex.label.cex = 0.8,     # Tamaño de la letra
     vertex.label.color = "black", 
     edge.arrow.size = 0.4,      # Flechas pequeñitas
     edge.color = "gray80",      # Enlaces en gris suave
     main = "Red de discusión (Visualización nativa igraph)"
)
