library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(tidygraph)
library(ggraph)
library(viridis)
library(png)
library(grid)
library(fcaR)
library(DT)
library(visNetwork)
library(httr2)
library(plotly)
library(shinyWidgets)
library(glue)
library(rlang)

plot_png <- function(filename) {
  path <- file.path("images", filename)
  validate(need(file.exists(path), paste("No se encontró el archivo:", path)))
  img <- readPNG(path)
  grid.newpage()
  grid.raster(img)
}

sent_graficos <- list(
  clasico = c(
    "Sentimiento vs popularidad" = "sa_plot_relacion_sentimiento_popularidad.png",
    "Evolución temporal" = "sa_plot_evo_temp.png",
    "Micro-contextos" = "sa_plot_micro_contextos.png",
    "Cámara de eco" = "sa_plot_camara_eco.png"
  ),
  ia = c(
    "Sentimiento vs popularidad" = "sa_plot_relacion_sentimiento_popularidad_ia.png",
    "Evolución temporal" = "sa_grafico_temporal_ia.png",
    "Micro-contextos" = "sa_plot_micro_contextos_ia.png",
    "Cámara de eco" = "sa_plot_camara_eco_ia.png"
  ),
  comparativa = c(
    "Correlación valencia NRC vs IA" = "sa_plot_grafico_correlacion_valencia_nrc_ia.png",
    "Perfil emocional global" = "sa_plot_perfil_emocional_global.png"
  )
)

topic_graficos <- list(
  clasico = c(
    "Optimización de k (tuning)" = "tm_plot_tuning.png",
    "Prevalencia e impacto" = "tm_plot_prevalencia_impacto.png",
    "Top términos" = "tm_plot_top_terms.png",
    "Red de coocurrencia" = "tm_plot_red_coocurrencia.png"
  ),
  ia = c(
    "Prevalencia e impacto" = "tm_plot_prevalencia_impacto_ia.png",
    "Top términos" = "tm_plot_top_terms_ia.png",
    "Red de coocurrencia" = "tm_plot_red_coocurrencia_ia.png"
  )
)

ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "TFG - Análisis Reddit"),
  dashboardSidebar(
    sidebarMenu(
      id = "menu",
      menuItem("Análisis de redes sociales", tabName = "sna", icon = icon("project-diagram")),
      menuItem("Análisis de sentimiento", tabName = "sentimiento", icon = icon("face-smile")),
      menuItem("Topic modeling", tabName = "topic", icon = icon("comments")),
      menuItem("FCA", tabName = "fca", icon = icon("sitemap"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "sna",
        fluidRow(
          box(
            width = 3,
            title = "Opciones",
            status = "primary",
            solidHeader = TRUE,
            selectInput(
              "sna_grafico",
              "Gráfico",
              choices = c(
                "Red interactiva" = "interactive",
                "Red de interacciones" = "sna_grafo_limpio.png",
                "Distribución de grado" = "sna_distribucion_grado.png",
                "Distribución de métricas" = "sna_distribucion_metrica.png",
                "Comunidades (Louvain)" = "sna_louvain.png",
                "Comunidades (Walktrap)" = "sna_walktrap.png",
                "Mapa de roles" = "sna_mapa_roles.png",
                "Red egocéntrica" = "sna_ego_red.png"
              ),
              selected = "interactive"
            ),
            conditionalPanel(
              condition = "input.sna_grafico == 'interactive'",
              sliderInput("sna_umbral_nodos", "Umbral nodos (grado)", min = 0, max = 100, value = 10, step = 1),
              sliderInput("sna_umbral_aristas", "Umbral aristas (weight)", min = 1, max = 50, value = 1, step = 1),
              numericInput("sna_seed", "Seed", value = 123, min = 1, step = 1),
              actionButton("sna_run", "Generar red", class = "btn-primary")
            )
          ),
          box(
            width = 9,
            title = "Visualización",
            status = "primary",
            solidHeader = TRUE,
            uiOutput("sna_plot_ui"),
            br(),
            uiOutput("sna_node_details")
          )
        )
      ),
      tabItem(
        tabName = "sentimiento",
        fluidRow(
          box(
            width = 3,
            title = "Opciones",
            status = "primary",
            solidHeader = TRUE,
            radioButtons(
              "sent_metodo",
              "Metodología",
              choices = c(
                "Algoritmo clásico (NRC)" = "clasico",
                "Inteligencia Artificial (LLM)" = "ia",
                "Comparativa NRC vs IA" = "comparativa"
              ),
              selected = "clasico"
            ),
            selectInput("sent_grafico", "Gráfico", choices = sent_graficos$clasico)
          ),
          box(
            width = 9,
            title = "Visualización",
            status = "primary",
            solidHeader = TRUE,
            uiOutput("sent_titulo"),
            imageOutput("sent_plot", height = 650)
          )
        )
      ),
      tabItem(
        tabName = "topic",
        fluidRow(
          box(
            width = 3,
            title = "Opciones",
            status = "primary",
            solidHeader = TRUE,
            radioButtons(
              "topic_metodo",
              "Metodología",
              choices = c(
                "Algoritmo clásico (LDA)" = "clasico",
                "Inteligencia Artificial (LLM)" = "ia"
              ),
              selected = "clasico"
            ),
            selectInput("topic_grafico", "Gráfico", choices = topic_graficos$clasico)
          ),
          box(
            width = 9,
            title = "Visualización",
            status = "primary",
            solidHeader = TRUE,
            uiOutput("topic_titulo"),
            imageOutput("topic_plot", height = 650)
          )
        )
      ),
      tabItem(
        tabName = "fca",
        fluidRow(
          box(
            width = 3,
            title = "Opciones",
            status = "primary",
            solidHeader = TRUE,
            helpText("Secciones: Atributos y Conceptos (usar pestañas a la derecha)."),
            radioButtons(
              "fca_metodo",
              "Contexto FCA",
              choices = c(
                "Algoritmo clásico (NRC / datos clásicos)" = "clasico",
                "Inteligencia Artificial (IA / datos IA)" = "ia"
              ),
              selected = "ia"
            ),
            actionButton("fca_run", "Calcular FCA", class = "btn-primary"),
            br(), br(),
            conditionalPanel(
              condition = "input.fca_tabs == 'conceptos'",
              numericInput("fca_min_attributes", "Min atributos por concepto", value = 3, min = 1, step = 1),
              numericInput("fca_min_comments", "Min comentarios (soporte)", value = 5, min = 1, step = 1),
              numericInput("fca_top_n", "Top N conceptos", value = 20, min = 1, step = 1),
              uiOutput("fca_attr_selector"),
              downloadButton("fca_download", "Exportar conceptos")
            ),
            conditionalPanel(
              condition = "input.fca_tabs == 'atributos'",
              uiOutput("fca_attr_single")
            ),
            conditionalPanel(
              condition = "input.fca_tabs == 'ia'",
              radioButtons("fca_ia_type", "Interpretar como", choices = c("Concepto" = "concepto", "Implicación" = "implicacion"), selected = "concepto"),
              uiOutput("fca_ia_item_selector"),
              actionButton("fca_ia_interpret", "Interpretar con IA", class = "btn-success"),
              br(), br(),
              helpText("Se requiere un servidor Ollama local en http://localhost:11434.")
            ),
            br(), br(),
            p("Análisis de Conceptos Formales integrando red, sentimiento y temas (como en fca.qmd).")
          ),
          box(
            width = 9,
            title = "Visualización",
            status = "primary",
            solidHeader = TRUE,
            uiOutput("fca_plot_ui")
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  # Función para comunicarse con Ollama
  explain_with_ollama <- function(prompt) {
    tryCatch({
      resp <- httr2::request("http://localhost:11434/api/chat") |>
        httr2::req_body_json(list(
          model = "qwen2.5",
          messages = list(list(role = "user", content = prompt)),
          stream = FALSE,
          options = list(temperature = 0.30)
        )) |>
        httr2::req_timeout(120) |>
        httr2::req_perform()
      parsed <- httr2::resp_body_json(resp)
      if (!is.null(parsed$message$content)) parsed$message$content else "Sin respuesta de Ollama."
    }, error = function(e) {
      paste("Error IA:", conditionMessage(e))
    })
  }
  
  observeEvent(input$sent_metodo, {
    opciones <- sent_graficos[[input$sent_metodo]]
    updateSelectInput(
      session, 
      "sent_grafico", 
      choices = opciones,
      selected = opciones[1] # Forzamos a que se seleccione el primero
    )
  }, ignoreInit = TRUE)
  
  observeEvent(input$topic_metodo, {
    opciones <- topic_graficos[[input$topic_metodo]]
    updateSelectInput(
      session, 
      "topic_grafico", 
      choices = opciones,
      selected = opciones[1] # Forzamos a que se seleccione el primero
    )
  }, ignoreInit = TRUE)
  
  observeEvent(input$fca_metodo, {
    fca_net_positions(NULL)
  }, ignoreInit = TRUE)
  
  output$sent_titulo <- renderUI({
    metodo <- switch(
      input$sent_metodo,
      clasico = "Algoritmo clásico (NRC / Syuzhet)",
      ia = "Inteligencia Artificial (LLM / Ollama)",
      comparativa = "Comparativa entre metodologías"
    )
    tags$p(tags$strong(metodo), style = "color: #555; margin-bottom: 12px;")
  })
  
  output$topic_titulo <- renderUI({
    metodo <- switch(
      input$topic_metodo,
      clasico = "Algoritmo clásico (LDA / topicmodels)",
      ia = "Inteligencia Artificial (LLM / Ollama)"
    )
    tags$p(tags$strong(metodo), style = "color: #555; margin-bottom: 12px;")
  })
  
  # --- Renderizado nativo de imágenes ---
  output$sent_plot <- renderImage({
    req(input$sent_grafico)
    ruta <- file.path("images", input$sent_grafico)
    
    validate(need(file.exists(ruta), paste("No se encontró la imagen en la carpeta 'images':", input$sent_grafico)))
    
    list(
      src = ruta,
      contentType = "image/png",
      width = "100%", # Ajusta la imagen al ancho del panel
      height = "auto",
      alt = "Gráfico de sentimiento"
    )
  }, deleteFile = FALSE) # deleteFile = FALSE es crucial para que no borre tu foto original
  
  output$topic_plot <- renderImage({
    req(input$topic_grafico)
    ruta <- file.path("images", input$topic_grafico)
    
    validate(need(file.exists(ruta), paste("No se encontró la imagen en la carpeta 'images':", input$topic_grafico)))
    
    list(
      src = ruta,
      contentType = "image/png",
      width = "100%",
      height = "auto",
      alt = "Gráfico de temas"
    )
  }, deleteFile = FALSE)
  
  # --- SNA (sna.qmd) ---
  grafo_reddit <- reactiveVal(NULL)
  
  load_grafo_reddit <- function() {
    if (!is.null(grafo_reddit())) return(invisible(TRUE))
    
    data_sna <- readRDS("data/data_sna.rds")
    data_sna <- dplyr::count(data_sna, from, to, name = "weight")
    
    # 1. Creamos el grafo y calculamos las métricas
    g <- tidygraph::as_tbl_graph(data_sna, directed = TRUE) |>
      tidygraph::activate("nodes") |>
      dplyr::mutate(
        degree = tidygraph::centrality_degree(mode = "all"),
        betweenness = tidygraph::centrality_betweenness(directed = TRUE),
        closeness = tidygraph::centrality_closeness(mode = "all"),
        pagerank = tidygraph::centrality_pagerank(),
        eigenvector = tidygraph::centrality_eigen()
      )
    
    # 2. Le pegamos los roles de tu archivo tabla_roles.rds
    if (file.exists("data/tabla_roles.rds")) {
      tabla_roles <- readRDS("data/tabla_roles.rds")
      
      g <- g |>
        tidygraph::activate("nodes") |>
        # Unimos usando el nombre del usuario
        dplyr::left_join(dplyr::select(tabla_roles, name, Rol), by = "name")
    } else {
      # Si por algún motivo no encuentra el archivo, creamos la columna vacía
      g <- g |> 
        tidygraph::activate("nodes") |> 
        dplyr::mutate(Rol = NA_character_)
    }
    
    grafo_reddit(g)
    invisible(TRUE)
  }
  
  generate_subgraph_advanced <- function(umbral_nodos, umbral_aristas, g) {
    g |>
      activate("edges") |>
      filter(.data$weight >= umbral_aristas) |>
      activate("nodes") |>
      mutate(temp_degree = centrality_degree(mode = "all")) |>
      filter(.data$temp_degree > umbral_nodos & .data$temp_degree > 0) |>
      filter(!node_is_isolated()) |>
      select(-.data$temp_degree)
  }
  
  plot_graph <- function(seed = 123, grafo) {
    set.seed(seed)
    layout <- create_layout(grafo, layout = "graphopt")
    
    ggraph(layout) +
      geom_edge_link(alpha = 0.1, color = "gray70") +
      geom_node_point(aes(size = .data$degree, color = .data$degree, alpha = 0.8), show.legend = TRUE) +
      geom_node_text(
        aes(label = ifelse(.data$degree > stats::quantile(.data$degree, 0.75), .data$name, NA)),
        repel = TRUE,
        size = 3,
        fontface = "bold",
        color = "black",
        bg.color = "white",
        bg.r = 0.15
      ) +
      scale_color_viridis_c(option = "plasma") +
      scale_size_continuous(range = c(1, 8)) +
      theme_graph() +
      labs(
        title = "Red de interacciones en Reddit",
        subtitle = "Nodos filtrados"
      )
  }
  
  # --- Variables reactivas para el nodo de SNA ---
  selected_sna_node <- reactiveVal(NULL)
  selected_sna_node_explanation <- reactiveVal(NULL)
  
  output$sna_plot_ui <- renderUI({
    if (identical(input$sna_grafico, "interactive")) {
      visNetworkOutput("sna_network", height = 650)
    } else {
      plotOutput("sna_plot_static", height = 650)
    }
  })

  output$sna_network <- renderVisNetwork({
    req(input$sna_run)
    load_grafo_reddit()
    g0 <- grafo_reddit()
    validate(need(!is.null(g0), "No se pudo cargar `data/data_sna.rds`."))
    
    g1 <- generate_subgraph_advanced(
      umbral_nodos = input$sna_umbral_nodos,
      umbral_aristas = input$sna_umbral_aristas,
      g = g0
    )
    
    nodes <- g1 |>
      activate("nodes") |>
      as_tibble()
    
    # Control de seguridad: Si los sliders filtran todo, avisamos al usuario
    validate(need(nrow(nodes) > 0, "El umbral es muy alto. No hay nodos para mostrar con estos filtros."))
    
    if (!"Rol" %in% names(nodes)) {
      nodes$Rol <- NA_character_
    }
    
    color_nodos <- if (isTRUE(input$sna_highlight_roles)) {
      purrr::map_chr(nodes$Rol, role_color)
    } else {
      "#8fb9d4"
    }
    
    nodes <- nodes |>
      mutate(
        id = name,
        label = ifelse(isTRUE(input$sna_show_labels), name, ""),
        title = paste0(
          "<b>", name, "</b><br>",
          "Grado: ", degree, "<br>",
          "Betweenness: ", round(betweenness, 3), "<br>",
          "Pagerank: ", round(pagerank, 4), "<br>",
          "Rol: ", ifelse(is.na(Rol), "N/A", as.character(Rol))
        ),
        value = pmax(degree, 1),
        color = color_nodos
      )
    
    edges <- g1 |>
      activate("edges") |>
      as_tibble() |>
      mutate(
        # MAGIA AQUÍ: Mapeamos el índice numérico al nombre real del nodo
        from = nodes$id[from],
        to = nodes$id[to],
        title = paste0("Peso: ", weight, "<br>De: ", from, "<br>Para: ", to),
        width = pmax(weight / max(weight, 1) * 5, 1)
      )
    
    # Forzamos que sean data.frames puros para evitar el error [object Object]
    nodes <- as.data.frame(nodes)
    edges <- as.data.frame(edges)
    
    visNetwork(nodes, edges, height = "650px") |>
      # 1. Calculamos el diseño en R y apagamos las físicas de rebote (red rígida)
      visIgraphLayout(layout = "layout_with_fr") |> 
      visNodes(shadow = list(enabled = TRUE, size = 20)) |>
      # 2. smooth = FALSE hace que las líneas sean rectas y pierdan la curva elástica
      visEdges(arrows = "to", smooth = FALSE, color = list(highlight = "#FF7034")) |>
      visOptions(
        highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE),
        nodesIdSelection = list(enabled = TRUE, useLabels = TRUE)
      ) |>
      visInteraction(navigationButtons = TRUE, zoomView = TRUE) |>
      visEvents(
        selectNode = "function(event) { if (event.nodes && event.nodes.length) { Shiny.setInputValue('sna_node', event.nodes[0], {priority:'event'}); } }"
      )
  })

  output$sna_plot_static <- renderPlot({
    plot_png(input$sna_grafico)
  })

  # Panel inteligente con detalles del nodo y explicación IA
  observeEvent(input$sna_node, {
    req(input$sna_node)
    load_grafo_reddit()
    node_name <- input$sna_node
    node_data <- grafo_reddit() |>
      activate("nodes") |>
      as_tibble() |>
      filter(name == node_name) |>
      slice(1)
    validate(need(nrow(node_data) == 1, "Nodo no encontrado."))

    selected_sna_node(node_data)
    selected_sna_node_explanation("Consultando Ollama...")
    
    tiene_rol <- "Rol" %in% names(node_data)
    role_label <- if (tiene_rol && !is.na(node_data[["Rol"]])) node_data[["Rol"]] else "N/A"

    prompt <- glue(
      "Eres un experto en análisis de redes sociales en Reddit.\n",
      "Contexto: análisis SNA con métricas topológicas y roles de usuarios.\n",
      "Nodo seleccionado: {node_data$name}\n",
      "Métricas: Grado={node_data$degree}, Betweenness={round(node_data$betweenness,3)}, Pagerank={round(node_data$pagerank,4)}\n",
      "Rol: {ifelse(is.na(node_data$Rol), 'N/A', node_data$Rol)}\n",
      "Describe brevemente: 1) Su posición en la red, 2) Importancia estratégica, 3) Rol probable en la comunidad.\n",
      "Sé conciso (3-5 líneas)."
    )

    selected_sna_node_explanation(explain_with_ollama(prompt))
  }, ignoreNULL = TRUE)

  output$sna_node_details <- renderUI({
    node <- selected_sna_node()
    explanation <- selected_sna_node_explanation()
    
    if (is.null(node)) {
      return(helpText("Haz clic en un nodo para ver detalles y análisis IA."))
    }
    
    box(
      title = paste("📊 Nodo:", node$name),
      status = "info",
      solidHeader = TRUE,
      width = NULL,
      HTML(paste0(
        "<b>Métricas de red:</b><br>",
        "• Grado (conexiones): ", node$degree, "<br>",
        "• Betweenness (intermediación): ", round(node$betweenness, 3), "<br>",
        "• Pagerank (influencia): ", round(node$pagerank, 4), "<br>",
        "• Rol en la red: ", ifelse(is.na(node$Rol), "N/A", node$Rol), "<br>",
        "<hr>",
        "<b>Análisis inteligente:</b><br>",
        explanation
      ))
    )
  })
  
  # --- FCA (fca.qmd) ---
  fca_resultados <- reactiveVal(NULL)
  fca_net_positions <- reactiveVal(NULL)
  
  build_matriz_fca <- function() {
    # 1. Cargar los datos y preparar cruces
    modelo_lda <- readRDS("data/modelo_lda.rds") 
    tabla_roles <- readRDS("data/tabla_roles.rds")
    data_texto <- readRDS("data/data_texto_procesado.rds")
    use_ia_data <- identical(input$fca_metodo, "ia")
    data_sentim <- if (use_ia_data) readRDS("data/data_sentim_ia.rds") else readRDS("data/data_sentim.rds")
    nombres_temas <- readRDS("data/nombres_temas.rds")
    df_temas <- as.data.frame(readRDS("data/df_temas_ia.rds"), stringsAsFactors = FALSE, check.names = FALSE)
    
    if ("name" %in% names(tabla_roles) && !"Usuario" %in% names(tabla_roles)) tabla_roles$Usuario <- tabla_roles$name
    if ("comunidad" %in% names(tabla_roles) && !"Comunidad" %in% names(tabla_roles)) tabla_roles$Comunidad <- as.character(tabla_roles$comunidad)
    
    df_integrado <- data_sentim |>
      inner_join(df_temas, by = "comment_id") |> 
      left_join(tabla_roles, by = c("author" = "name")) |>
      filter(!is.na(Rol))
    
    # 2. Detección automática de temas y comunidades
    nombres_temas_automaticos <- setdiff(names(df_temas), "comment_id")
    nombres_temas_limpios <- gsub("\\s+", "_", nombres_temas_automaticos)
    
    num_comunidades <- max(as.numeric(df_integrado$comunidad), na.rm = TRUE)
    comunidades_list <- as.character(1:num_comunidades)
    
    q1_influencia <- stats::quantile(df_integrado$pagerank, 0.25, na.rm = TRUE)
    q3_influencia <- stats::quantile(df_integrado$pagerank, 0.75, na.rm = TRUE)
    q1_intermediacion <- stats::quantile(df_integrado$betweenness, 0.25, na.rm = TRUE)
    q3_intermediacion <- stats::quantile(df_integrado$betweenness, 0.75, na.rm = TRUE)
    q1_engagement <- stats::quantile(df_integrado$score, 0.25, na.rm = TRUE)
    q3_engagement <- stats::quantile(df_integrado$score, 0.75, na.rm = TRUE)
    
    # 3. Creación DINÁMICA de columnas (Comunidades y Temas)
    mutaciones_comunidades <- purrr::map_dfc(comunidades_list, ~{
      col_name <- paste0("Autor_Comunidad_", .x)
      tibble::tibble(!!col_name := ifelse(df_integrado$comunidad == .x, 1, 0))
    })
    
    mutaciones_temas <- purrr::map_dfc(seq_along(nombres_temas_automaticos), ~{
      tema_original <- nombres_temas_automaticos[.x]
      tema_limpio <- nombres_temas_limpios[.x]
      col_name <- paste0("Tema_", tema_limpio)
      tibble::tibble(!!col_name := ifelse(df_integrado[[tema_original]] > 0.15, 1, 0))
    })
    
    # Evaluamos las condiciones especiales fuera del mutate principal (para evitar errores en R)
    valencia_act <- if (use_ia_data) df_integrado$valencia_ia else df_integrado$valencia
    confianza_act <- if (use_ia_data) (!is.na(df_integrado$emocion_ia) & df_integrado$emocion_ia == "Trust") else (df_integrado$trust > 0)
    anticipacion_act <- if (use_ia_data) (!is.na(df_integrado$emocion_ia) & df_integrado$emocion_ia == "Anticipation") else (df_integrado$anticipation > 0)
    miedo_act <- if (use_ia_data) (!is.na(df_integrado$emocion_ia) & df_integrado$emocion_ia == "Fear") else (df_integrado$fear > 0)
    ira_act <- if (use_ia_data) (!is.na(df_integrado$emocion_ia) & df_integrado$emocion_ia == "Anger") else (df_integrado$anger > 0)
    alegria_act <- if (use_ia_data) (!is.na(df_integrado$emocion_ia) & df_integrado$emocion_ia == "Joy") else (df_integrado$joy > 0)
    tristeza_act <- if (use_ia_data) (!is.na(df_integrado$emocion_ia) & df_integrado$emocion_ia == "Sadness") else (df_integrado$sadness > 0)
    sorpresa_act <- if (use_ia_data) (!is.na(df_integrado$emocion_ia) & df_integrado$emocion_ia == "Surprise") else (df_integrado$surprise > 0)
    asco_act <- if (use_ia_data) (!is.na(df_integrado$emocion_ia) & df_integrado$emocion_ia == "Disgust") else (df_integrado$disgust > 0)
    
    # 4. Construcción del Dataframe combinando base + bloques dinámicos
    df_fca <- df_integrado |>
      mutate(
        Autor_Alta_Influencia  = ifelse(pagerank >= q3_influencia, 1, 0),
        Autor_Media_Influencia = ifelse(pagerank > q1_influencia & pagerank < q3_influencia, 1, 0),
        Autor_Baja_Influencia  = ifelse(pagerank <= q1_influencia, 1, 0),
        
        Autor_Alto_Puente  = ifelse(betweenness >= q3_intermediacion, 1, 0),
        Autor_Medio_Puente = ifelse(betweenness > q1_intermediacion & betweenness < q3_intermediacion, 1, 0),
        Autor_Bajo_Puente  = ifelse(betweenness <= q1_intermediacion, 1, 0),
        
        Autor_Rol_Regular = ifelse(Rol == "Usuario Regular", 1, 0),
        Autor_Rol_Broker = ifelse(Rol == "Broker (Conector)", 1, 0),
        Autor_Rol_Autoridad = ifelse(Rol == "Autoridad (Referencia)", 1, 0),
        Autor_Rol_Hub = ifelse(Rol == "Hub (Difusor activo)", 1, 0),
        
        Coment_Alto_Impacto  = ifelse(score >= q3_engagement, 1, 0),
        Coment_Medio_Impacto = ifelse(score > q1_engagement & score < q3_engagement, 1, 0),
        Coment_Bajo_Impacto  = ifelse(score <= q1_engagement, 1, 0),
        
        Sent_Muy_Positivo = ifelse(valencia_act >= 3, 1, 0),
        Sent_Muy_Negativo = ifelse(valencia_act <= -3, 1, 0),
        Sent_Positivo     = ifelse(valencia_act >= 1 & valencia_act < 3, 1, 0),
        Sent_Negativo     = ifelse(valencia_act <= -1 & valencia_act > -3, 1, 0),
        Sent_Neutro       = ifelse(valencia_act > -1 & valencia_act < 1, 1, 0),
        
        Emocion_Confianza    = ifelse(confianza_act, 1, 0),
        Emocion_Anticipacion = ifelse(anticipacion_act, 1, 0),
        Emocion_Miedo        = ifelse(miedo_act, 1, 0),
        Emocion_Ira          = ifelse(ira_act, 1, 0),
        Emocion_Alegria      = ifelse(alegria_act, 1, 0),
        Emocion_Tristeza     = ifelse(tristeza_act, 1, 0),
        Emocion_Sorpresa     = ifelse(sorpresa_act, 1, 0),
        Emocion_Asco         = ifelse(asco_act, 1, 0)
      ) |>
      bind_cols(mutaciones_comunidades) |>
      bind_cols(mutaciones_temas)
    
    # 5. Selección final dinámica de columnas
    nombres_comunidades <- paste0("Autor_Comunidad_", comunidades_list)
    nombres_temas_atributos <- paste0("Tema_", nombres_temas_limpios)
    
    cols_a_seleccionar <- c(
      "Autor_Alta_Influencia", "Autor_Media_Influencia", "Autor_Baja_Influencia",
      "Autor_Alto_Puente", "Autor_Medio_Puente", "Autor_Bajo_Puente",
      nombres_comunidades,
      "Autor_Rol_Regular", "Autor_Rol_Broker", "Autor_Rol_Autoridad", "Autor_Rol_Hub",
      "Coment_Alto_Impacto", "Coment_Medio_Impacto", "Coment_Bajo_Impacto",
      "Sent_Muy_Positivo", "Sent_Muy_Negativo", "Sent_Positivo", "Sent_Negativo", "Sent_Neutro",
      "Emocion_Confianza", "Emocion_Anticipacion", "Emocion_Miedo", "Emocion_Ira",
      "Emocion_Alegria", "Emocion_Tristeza", "Emocion_Sorpresa", "Emocion_Asco",
      nombres_temas_atributos
    )
    
    matriz_fca_limpia <- df_fca |>
      select(all_of(cols_a_seleccionar))
    
    # 6. Procesamiento FCA
    matriz_fca_final <- as.matrix(matriz_fca_limpia)
    
    resumen_atributos <- data.frame(
      Atributo = colnames(matriz_fca_final),
      Frecuencia = colSums(matriz_fca_final),
      Porcentaje = round((colSums(matriz_fca_final) / nrow(matriz_fca_final)) * 100, 1)
    )
    
    fc <- FormalContext$new(matriz_fca_final)
    fc$clarify()
    fc$find_concepts()
    fc$find_implications()
    
    soportes <- fc$concepts$support()
    matriz_intensiones <- fc$concepts$intents()
    implicaciones <- capture.output(print(fc$implications))
    nombres_atributos_final <- colnames(matriz_fca_final)
    
    extraer_texto <- function(col) { paste(nombres_atributos_final[col > 0], collapse = ", ") }
    intensiones_texto <- apply(matriz_intensiones, 2, extraer_texto)
    
    tabla_conceptos <- data.frame(
      ID_Concepto = seq_along(soportes),
      Num_Comentarios = round(soportes * nrow(matriz_fca_final)),
      Atributos_Compartidos = intensiones_texto,
      stringsAsFactors = FALSE
    )
    tabla_conceptos <- tabla_conceptos[order(-tabla_conceptos$Num_Comentarios), ]
    
    list(
      resumen_atributos = resumen_atributos,
      tabla_conceptos = tabla_conceptos,
      fc = fc,
      concepts_intents = matriz_intensiones,
      implicaciones = implicaciones,
      soportes = soportes,
      matriz = matriz_fca_final
    )
  }
  
  observeEvent(input$fca_run, {
    tryCatch(
      {
        withProgress(message = "Calculando FCA...", {
          fca_resultados(build_matriz_fca())
        })
        showNotification("FCA calculado correctamente.", type = "message")
      },
      error = function(e) {
        showNotification(conditionMessage(e), type = "error", duration = NULL)
      }
    )
  })
  
  output$fca_plot_ui <- renderUI({
    # Tabbed view: Atributos | Conceptos
    tabsetPanel(
      id = "fca_tabs",
      tabPanel(
        title = "Atributos",
        value = "atributos",
        fluidRow(
          # Le damos el ancho completo (12) al gráfico
          column(12, plotlyOutput("fca_plot", height = 400)),
          
          # Le damos el ancho completo (12) a la tabla y añadimos un separador
          column(12, tags$hr(), DT::dataTableOutput("fca_attr_table"))
        ),
        br(),
        uiOutput("fca_attr_details")
      ),
      tabPanel(
        title = "Conceptos",
        value = "conceptos",
        fluidRow(
          # Le damos todo el ancho a la red interactiva y la ponemos arriba
          column(12, visNetworkOutput("fca_concepts_net", height = "500px")),
          
          # Ponemos la tabla debajo ocupando todo el ancho
          column(12, tags$hr(), DT::dataTableOutput("fca_concepts_table"))
        )
      ),
      tabPanel(
        title = "Interpretación IA",
        value = "ia",
        fluidRow(
          column(12, htmlOutput("fca_ia_result", container = tags$div))
        )
      )
    )
  })
  
  output$fca_attr_selector <- renderUI({
    res <- fca_resultados()
    if (is.null(res)) return(helpText("Pulsa 'Calcular FCA' para cargar atributos."))
    choices <- res$resumen_atributos$Atributo
    checkboxGroupInput("fca_atributos", "Filtrar por atributos (opcional)", choices = choices, selected = NULL)
  })
  
  output$fca_attr_single <- renderUI({
    res <- fca_resultados()
    if (is.null(res)) return(NULL)
    choices <- res$resumen_atributos$Atributo
    selectInput("fca_attr_single_sel", "Ver detalles del atributo", choices = c("(ninguno)" = "", choices), selected = "")
  })
  
  output$fca_ia_item_selector <- renderUI({
    res <- fca_resultados()
    if (is.null(res)) return(helpText("Pulsa 'Calcular FCA' para cargar los datos."))
    if (input$fca_ia_type == "concepto") {
      choices <- res$tabla_conceptos$ID_Concepto
      selectInput("fca_ia_item", "Seleccionar concepto", choices = setNames(choices, paste0("Concepto ", choices)), selected = choices[1])
    } else {
      imp <- res$implicaciones
      if (length(imp) == 0) {
        helpText("No se encontraron implicaciones.")
      } else {
        # use index because implication text can be long
        choices <- seq_along(imp)
        selectInput("fca_ia_item", "Seleccionar implicación", choices = setNames(choices, paste0("Implicación ", choices)), selected = choices[1])
      }
    }
  })
  
  interpret_ia <- eventReactive(input$fca_ia_interpret, {
    res <- fca_resultados()
    validate(need(!is.null(res), "Pulsa 'Calcular FCA' para cargar los datos."))
    type <- input$fca_ia_type
    item <- input$fca_ia_item
    if (is.null(item) || item == "") return("Selecciona un concepto o implicación.")
    
    method_label <- ifelse(input$fca_metodo == "ia", "IA", "clásico")
    if (type == "concepto") {
      idx <- as.integer(item)
      row <- res$tabla_conceptos[res$tabla_conceptos$ID_Concepto == idx, ]
      if (nrow(row) == 0) return("Concepto no encontrado.")
      prompt_text <- paste(
        "Eres un experto en análisis de redes sociales y minería de temas en Reddit.",
        paste("El contexto FCA se construyó usando datos de", method_label, "."),
        "Tienes un concepto formal que representa un grupo de comentarios con atributos compartidos.",
        "Interpreta este concepto en español de forma breve y profesional. Explica qué perfil de usuarios o comportamientos describe,",
        "qué significa en términos de sentimiento, temas y rol social, y por qué puede ser relevante.",
        "Concepto:", row$Atributos_Compartidos,
        sep = " \n"
      )
    } else {
      idx <- as.integer(item)
      imp <- res$implicaciones
      if (idx < 1 || idx > length(imp)) return("Implicación no encontrada.")
      prompt_text <- paste(
        "Eres un experto en lógica formal y análisis sociotécnico.",
        paste("El contexto FCA se construyó usando datos de", method_label, "."),
        "Tienes una implicación obtenida de un Análisis de Conceptos Formales (FCA) sobre datos de Reddit.",
        "Interpreta esta implicación en español de forma breve y profesional.",
        "Explica el significado social y la conexión entre los atributos.",
        "Implicación:", imp[idx],
        sep = " \n"
      )
    }
    
    tryCatch({
      resp <- request("http://localhost:11434/api/chat") |>
        req_body_json(list(
          model = "qwen2.5",
          messages = list(list(role = "user", content = prompt_text)),
          stream = FALSE,
          options = list(temperature = 0.35)
        )) |>
        req_timeout(300) |>
        req_perform()
      parsed <- resp_body_json(resp)
      if (!is.null(parsed$message$content)) {
        parsed$message$content
      } else {
        "No se recibió contenido de la IA."
      }
    }, error = function(e) {
      paste("Error al consultar la IA:", conditionMessage(e))
    })
  })
  
  output$fca_ia_result <- renderUI({
    req(input$fca_ia_interpret)
    result <- interpret_ia()
    if (is.null(result) || result == "") {
      helpText("Pulsa 'Interpretar con IA' para obtener una explicación.")
    } else {
      tagList(
        h4("Interpretación IA"),
        verbatimTextOutput("fca_ia_text")
      )
    }
  })
  
  output$fca_ia_text <- renderText({
    interpret_ia()
  })
  
  output$fca_plot <- renderPlotly({
    input$fca_run
    input$fca_tabs
    res <- fca_resultados()
    validate(need(!is.null(res), "Pulsa 'Calcular FCA' para generar los resultados."))

    top_attrs <- res$resumen_atributos[order(-res$resumen_atributos$Porcentaje), ]
    top_attrs <- utils::head(top_attrs, 15)

    p <- ggplot(top_attrs, aes(
      x = stats::reorder(.data$Atributo, .data$Porcentaje),
      y = .data$Porcentaje,
      text = paste0(
        "<b>", .data$Atributo, "</b><br>",
        "Frecuencia: ", .data$Frecuencia, "<br>",
        "Porcentaje: ", .data$Porcentaje, " %"
      )
    )) +
      geom_col(fill = "#4472C4", alpha = 0.8) +
      coord_flip() +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold", size = 13),
        axis.title = element_text(size = 11),
        axis.text = element_text(size = 10)
      ) +
      labs(
        title = "Frecuencia de atributos en el contexto FCA",
        subtitle = paste("Método:", ifelse(input$fca_metodo == "ia", "IA (Ollama)", "Clásico (NRC)")),
        x = NULL,
        y = "Porcentaje (%)"
      )

    plotly_config(ggplotly(p, tooltip = "text"))
  })
  
  output$fca_concepts_table <- DT::renderDataTable({
    input$fca_run
    input$fca_tabs
    res <- fca_resultados()
    validate(need(!is.null(res), "Pulsa 'Calcular FCA' para generar los resultados."))
    
    intents <- as.matrix(res$concepts_intents)
    nombres_atributos <- colnames(res$matriz)
    n_comentarios <- nrow(res$matriz)
    soportes_reales <- round(res$soportes * n_comentarios)
    
    # Filtrado por número de atributos y soporte
    min_attr <- as.integer(input$fca_min_attributes)
    min_comments <- as.integer(input$fca_min_comments)
    ids <- which(colSums(intents > 0) >= min_attr & soportes_reales >= min_comments)
    
    # Filtrado por atributos seleccionados (si aplica)
    sel_attrs <- input$fca_atributos
    if (!is.null(sel_attrs) && length(sel_attrs) > 0) {
      ids <- ids[sapply(ids, function(i) all(sel_attrs %in% nombres_atributos[intents[, i] > 0]))]
    }
    
    if (length(ids) == 0) return(datatable(data.frame(Message = "No hay conceptos con esos filtros"), options = list(dom = 't')))
    
    # Ordenar por soporte y limitar a top N
    ord <- ids[order(-soportes_reales[ids])]
    topn <- as.integer(input$fca_top_n)
    if (length(ord) > topn) ord <- ord[1:topn]
    
    df <- lapply(ord, function(i) {
      atributos_activos <- nombres_atributos[intents[, i] > 0]
      data.frame(
        ID_Concepto = i,
        Num_Comentarios = soportes_reales[i],
        Atributos_Compartidos = paste(atributos_activos, collapse = " + "),
        stringsAsFactors = FALSE
      )
    })
    df <- do.call(rbind, df)
    
    # Cambia la última línea del bloque por esta:
    datatable(df, options = list(pageLength = 5, autoWidth = TRUE, scrollX = TRUE), rownames = FALSE)  })
  
  output$fca_concepts_net <- renderVisNetwork({
    input$fca_run
    input$fca_tabs
    res <- fca_resultados()
    validate(need(!is.null(res), "Pulsa 'Calcular FCA' para generar los resultados."))
    
    intents <- as.matrix(res$concepts_intents)
    nombres_atributos <- colnames(res$matriz)
    n_comentarios <- nrow(res$matriz)
    soportes_reales <- round(res$soportes * n_comentarios)
    
    min_attr <- as.integer(input$fca_min_attributes)
    min_comments <- as.integer(input$fca_min_comments)
    ids <- which(colSums(intents > 0) >= min_attr & soportes_reales >= min_comments)
    sel_attrs <- input$fca_atributos
    if (!is.null(sel_attrs) && length(sel_attrs) > 0) {
      ids <- ids[sapply(ids, function(i) all(sel_attrs %in% nombres_atributos[intents[, i] > 0]))]
    }
    if (length(ids) == 0) return(NULL)
    ord <- ids[order(-soportes_reales[ids])]
    topn <- as.integer(input$fca_top_n)
    if (length(ord) > topn) ord <- ord[1:topn]
    
    # Crear nodos
    nodes <- data.frame(
      id = seq_along(ord),
      title = paste0("Concepto ", ord),
      label = paste0("C", ord, " (", soportes_reales[ord], ")"),
      value = pmax(1, soportes_reales[ord]),
      stringsAsFactors = FALSE
    )
    
    # Crear aristas según similitud Jaccard entre intensiones
    sub_intents <- intents[, ord, drop = FALSE]
    k <- ncol(sub_intents)
    edges_list <- list()
    if (k > 1) {
      for (i in 1:(k - 1)) {
        for (j in (i + 1):k) {
          a <- sub_intents[, i] > 0
          b <- sub_intents[, j] > 0
          inter <- sum(a & b)
          union <- sum(a | b)
          sim <- ifelse(union == 0, 0, inter / union)
          if (sim > 0.15) {
            edges_list[[length(edges_list) + 1]] <- data.frame(from = i, to = j, width = round(sim * 10, 2), title = paste0('sim=', round(sim, 2)))
          }
        }
      }
    }
    if (length(edges_list) > 0) {
      edges <- do.call(rbind, edges_list)
    } else {
      edges <- data.frame(from = integer(0), to = integer(0), width = numeric(0), title = character(0))
    }
    
    # Reuse saved positions if available
    saved_pos <- fca_net_positions()
    if (!is.null(saved_pos) && length(saved_pos) > 0) {
      xs <- sapply(nodes$id, function(i) {
        key <- as.character(i)
        if (!is.null(saved_pos[[key]])) saved_pos[[key]]$x else NA
      })
      ys <- sapply(nodes$id, function(i) {
        key <- as.character(i)
        if (!is.null(saved_pos[[key]])) saved_pos[[key]]$y else NA
      })
      if (!all(is.na(xs)) && !all(is.na(ys))) {
        nodes$x <- xs
        nodes$y <- ys
      }
    }
    
    # If positions not available, compute initial layout via igraph
    if (!"x" %in% names(nodes) || any(is.na(nodes$x))) {
      if (nrow(nodes) > 0 && nrow(edges) > 0) {
        ig <- igraph::graph_from_data_frame(d = edges[, c("from", "to")], vertices = nodes, directed = FALSE)
        if (igraph::vcount(ig) > 1) {
          lay <- igraph::layout_with_fr(ig)
          nodes$x <- lay[,1] * 100
          nodes$y <- lay[,2] * 100
        } else {
          nodes$x <- rep(0, nrow(nodes))
          nodes$y <- rep(0, nrow(nodes))
        }
      } else {
        nodes$x <- rep(0, nrow(nodes))
        nodes$y <- rep(0, nrow(nodes))
      }
    }
    
    visNetwork(nodes, edges) %>%
      # Añadimos visEdges con smooth = FALSE para forzar las líneas rectas
      visEdges(smooth = FALSE, color = list(highlight = "#FF7034")) %>% 
      visInteraction(dragNodes = TRUE, zoomView = TRUE) %>%
      visPhysics(enabled = FALSE) %>%
      visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
      visEvents(dragEnd = "function() { var pos = this.getPositions(); Shiny.setInputValue('fca_net_positions', pos, {priority:'event'}); }") %>%
      visLegend()
  })
  
  observeEvent(input$fca_net_positions, {
    # store the latest positions from client-side dragEnd
    fca_net_positions(input$fca_net_positions)
  }, ignoreNULL = TRUE)
  
  output$fca_attr_table <- DT::renderDataTable({
    input$fca_run
    res <- fca_resultados()
    validate(need(!is.null(res), "Pulsa 'Calcular FCA' para generar los resultados."))
    
    dat <- res$resumen_atributos[order(-res$resumen_atributos$Porcentaje), ]
    
    # Añadimos scrollX = TRUE para evitar cortes en pantallas pequeñas
    datatable(dat, options = list(pageLength = 5, autoWidth = TRUE, scrollX = TRUE), rownames = FALSE)
  })
  
  output$fca_attr_details <- renderUI({
    input$fca_run
    attr_sel <- input$fca_attr_single_sel
    res <- fca_resultados()
    if (is.null(res) || is.null(attr_sel) || attr_sel == "") return(helpText("Seleccione un atributo para ver detalles."))
    
    intents <- as.matrix(res$concepts_intents)
    nombres_atributos <- colnames(res$matriz)
    # hallar índices de conceptos que contienen el atributo
    attr_idx <- which(nombres_atributos == attr_sel)
    if (length(attr_idx) == 0) return(helpText("Atributo no encontrado."))
    
    conceptos_idx <- which(intents[attr_idx, ] > 0)
    if (length(conceptos_idx) == 0) return(helpText("Ningún concepto contiene este atributo."))
    
    n_comentarios <- nrow(res$matriz)
    soportes_reales <- round(res$soportes * n_comentarios)
    df <- data.frame(
      ID_Concepto = conceptos_idx,
      Num_Comentarios = soportes_reales[conceptos_idx],
      Atributos = apply(intents[, conceptos_idx, drop = FALSE], 2, function(col) paste(nombres_atributos[col > 0], collapse = ", ")),
      stringsAsFactors = FALSE
    )
    df <- df[order(-df$Num_Comentarios), , drop = FALSE]
    
    tagList(
      h4(paste0("Atributo: ", attr_sel)),
      p(paste0("Frecuencia: ", res$resumen_atributos$Frecuencia[res$resumen_atributos$Atributo == attr_sel],
               " (", res$resumen_atributos$Porcentaje[res$resumen_atributos$Atributo == attr_sel], " %)")),
      DT::dataTableOutput("fca_attr_concepts_dt"),
      br()
    )
  })
  
  output$fca_attr_concepts_dt <- DT::renderDataTable({
    input$fca_run
    input$fca_attr_single_sel
    res <- fca_resultados()
    validate(need(!is.null(res), "Pulsa 'Calcular FCA' para generar los resultados."))
    attr_sel <- input$fca_attr_single_sel
    if (is.null(attr_sel) || attr_sel == "") return(datatable(data.frame(Message = "Seleccione un atributo"), options = list(dom = 't')))
    
    intents <- as.matrix(res$concepts_intents)
    nombres_atributos <- colnames(res$matriz)
    attr_idx <- which(nombres_atributos == attr_sel)
    conceptos_idx <- which(intents[attr_idx, ] > 0)
    if (length(conceptos_idx) == 0) return(datatable(data.frame(Message = "Ningún concepto contiene este atributo"), options = list(dom = 't')))
    
    n_comentarios <- nrow(res$matriz)
    soportes_reales <- round(res$soportes * n_comentarios)
    df <- data.frame(
      ID_Concepto = conceptos_idx,
      Num_Comentarios = soportes_reales[conceptos_idx],
      Atributos = apply(intents[, conceptos_idx, drop = FALSE], 2, function(col) paste(nombres_atributos[col > 0], collapse = ", ")),
      stringsAsFactors = FALSE
    )
    df <- df[order(-df$Num_Comentarios), , drop = FALSE]
    datatable(df, options = list(pageLength = 10), rownames = FALSE)
  })
  
  output$fca_download <- downloadHandler(
    filename = function() {
      paste0("fca_concepts_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".md")
    },
    content = function(file) {
      res <- fca_resultados()
      validate(need(!is.null(res), "Pulsa 'Calcular FCA' para generar los resultados.'"))
      
      intents <- as.matrix(res$concepts_intents)
      nombres_atributos <- colnames(res$matriz)
      n_comentarios <- nrow(res$matriz)
      soportes_reales <- round(res$soportes * n_comentarios)
      
      min_attr <- as.integer(input$fca_min_attributes)
      min_comments <- as.integer(input$fca_min_comments)
      ids <- which(colSums(intents > 0) >= min_attr & soportes_reales >= min_comments)
      sel_attrs <- input$fca_atributos
      if (!is.null(sel_attrs) && length(sel_attrs) > 0) {
        ids <- ids[sapply(ids, function(i) all(sel_attrs %in% nombres_atributos[intents[, i] > 0]))]
      }
      if (length(ids) == 0) {
        writeLines("No hay conceptos con esos filtros", con = file)
        return()
      }
      ord <- ids[order(-soportes_reales[ids])]
      topn <- as.integer(input$fca_top_n)
      if (length(ord) > topn) ord <- ord[1:topn]
      
      lines <- sapply(ord, function(i) {
        atributos_activos <- nombres_atributos[intents[, i] > 0]
        paste0("- Concepto (", soportes_reales[i], " comentarios): ", paste(atributos_activos, collapse = ", "))
      })
      
      writeLines(c("# Exportación de conceptos FCA", "", lines), con = file)
    }
  )
}

# ===== HELPERS PARA INTERACTIVIDAD =====
  plotly_config <- function(widget) {
    widget |>
      config(
        modeBarButtonsToAdd = list("zoomIn2d", "zoomOut2d", "select2d", "lasso2d"),
        displaylogo = FALSE,
        toImageButtonOptions = list(format = "png", filename = "TFG_analisis", scale = 2)
      )
  }

  role_color <- function(role) {
    dplyr::case_when(
      role == "Broker (Conector)" ~ "#1f77b4",
      role == "Autoridad (Referencia)" ~ "#2ca02c",
      role == "Hub (Difusor activo)" ~ "#d62728",
      role == "Usuario Regular" ~ "#9467bd",
      TRUE ~ "#7f7f7f"
    )
  }

shinyApp(ui, server)