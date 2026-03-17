# -------------------------------------------------------------------------
# PASO 0: INSTALACIÓN Y CARGA DE PAQUETES
# -------------------------------------------------------------------------
# Descomenta la siguiente línea si no tienes los paquetes instalados:
# install.packages(c("RedditExtractoR", "tidyverse", "igraph", "ggraph"))

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
# PASO 3: CREACIÓN DEL GRAFO (igraph)
# -------------------------------------------------------------------------

# Creamos el objeto grafo
g <- graph_from_data_frame(enlaces, directed = TRUE)

# SIMPLIFICACIÓN PARA VISUALIZACIÓN:
# Si el grafo es gigante, nos quedamos solo con el componente gigante o los nodos más conectados
# para que el dibujo se entienda. Aquí filtramos por "grado" (mínimo 2 conexiones)
g_simple <- induced_subgraph(g, vids = which(degree(g) > 1))

# Añadimos métrica de centralidad (Tamaño del nodo = Importancia)
V(g_simple)$size <- degree(g_simple, mode = "in") # Cuanta gente le responde

# -------------------------------------------------------------------------
# PASO 4: VISUALIZACIÓN (ggraph)
# -------------------------------------------------------------------------
cat("4. Generando visualización...\n")

ggraph(g_simple, layout = "fr") + # Layout Fruchterman-Reingold (fuerza)
  geom_edge_link(alpha = 0.4, 
                 arrow = arrow(length = unit(2, 'mm')), 
                 color = "gray60") +
  geom_node_point(aes(size = size, color = size), show.legend = FALSE) +
  # Solo ponemos etiquetas a los nodos muy importantes para no llenar todo de texto
  geom_node_text(aes(label = ifelse(size > 5, name, "")), 
                 repel = TRUE, size = 3, color = "black") +
  scale_color_viridis_c(option = "plasma") +
  theme_void() +
  labs(
    title = paste("Red de discusión en: r/technology"),
    subtitle = str_trunc(titulo_hilo, 60), # Cortar título si es muy largo
    caption = paste("Nodos:", vcount(g_simple), "| Aristas:", ecount(g_simple))
  )
