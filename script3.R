library(igraph)
library(syuzhet)
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
# PASO EXTRA: ANÁLISIS DE SENTIMIENTOS
# -------------------------------------------------------------------------
cat("Calculando emociones (esto puede tardar un poco)...\n")

# Obtenemos la matriz de emociones para cada comentario
# (Esto analiza la columna 'comment' fila por fila)
emociones <- get_nrc_sentiment(comentarios$comment)

# Unimos las emociones al dataframe original
datos_emocionales <- bind_cols(comentarios, emociones)

# AHORA: Agrupamos por AUTOR para saber el "humor promedio" de cada usuario.
# Calculamos un "Score General": (Positivo + Alegría) - (Negativo + Ira)
perfil_usuarios <- datos_emocionales %>%
  group_by(author) %>%
  summarise(
    # Calculamos la media de positividad vs negatividad
    score_humor = mean(positive - negative),
    # Opcional: ¿Qué tan enfadado suele estar este usuario?
    score_ira = mean(anger),
    # Guardamos el número de comentarios para filtrar después
    n_comentarios = n()
  )

head(perfil_usuarios)
# -------------------------------------------------------------------------
# PASO 3 MODIFICADO: GRAFO DE EMOCIONES
# -------------------------------------------------------------------------

# 1. Creamos el grafo (asumiendo que ya tienes 'enlaces' del paso anterior)
g <- graph_from_data_frame(enlaces, directed = TRUE)

# 2. Simplificamos (usuarios conectados)
g_simple <- induced_subgraph(g, vids = which(degree(g) > 1))

# 3. Cruzamos los datos del grafo con los datos de emociones
# El orden de los nodos en igraph es fijo, así que usamos match() para asignar correctamente
nombres_nodos <- V(g_simple)$name

# Buscamos el score de cada usuario en nuestra tabla de perfiles
valores_humor <- perfil_usuarios$score_humor[match(nombres_nodos, perfil_usuarios$author)]

# Asignamos ese valor al grafo
V(g_simple)$humor <- valores_humor

# Si hay NAs (gente que no se pudo analizar), ponemos 0 (neutro)
V(g_simple)$humor[is.na(V(g_simple)$humor)] <- 0

# -------------------------------------------------------------------------
# PASO 4 MODIFICADO: VISUALIZACIÓN EMOCIONAL
# -------------------------------------------------------------------------

# Definimos una paleta de colores semántica:
# Rojo = Negativo/Tóxico
# Gris = Neutro
# Verde/Azul = Positivo

# Creamos una función para asignar color según el score
get_color <- function(score) {
  if (score < -0.5) return("firebrick")   # Muy negativo
  if (score < 0)    return("salmon")      # Algo negativo
  if (score == 0)   return("gray80")      # Neutro
  if (score > 0.5)  return("forestgreen") # Muy positivo
  return("lightgreen")                    # Algo positivo
}

# Aplicamos los colores a cada nodo
V(g_simple)$color <- sapply(V(g_simple)$humor, get_color)

# Dibujamos
par(mar=c(0,0,2,0))
plot(g_simple,
     layout = layout_with_fr,
     vertex.size = degree(g_simple, mode="in") * 8, # Tamaño = Popularidad
     vertex.label = NA,           # Quitamos etiquetas para ver mejor los colores
     edge.arrow.size = 0.3,
     edge.color = "gray90",
     main = "Mapa de Calor Emocional de la Discusión"
)
legend("bottomleft", legend=c("Positivo", "Neutro", "Negativo"), 
       col=c("forestgreen", "gray80", "firebrick"), pch=19, pt.cex=1.5)