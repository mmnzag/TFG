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
      menuItem("Análisis de sentimiento", tabName = "sentimiento", icon = icon("smile-o")),
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
            uiOutput("sna_plot_ui")
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
            plotOutput("sent_plot", height = 650)
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
            plotOutput("topic_plot", height = 650)
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
  observeEvent(input$sent_metodo, {
    updateSelectInput(session, "sent_grafico", choices = sent_graficos[[input$sent_metodo]])
  }, ignoreInit = TRUE)
  
  observeEvent(input$topic_metodo, {
    updateSelectInput(session, "topic_grafico", choices = topic_graficos[[input$topic_metodo]])
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
  
  # --- SNA (sna.qmd) ---
  grafo_reddit <- reactiveVal(NULL)
  
  load_grafo_reddit <- function() {
    if (!is.null(grafo_reddit())) return(invisible(TRUE))
    
    data_sna <- readRDS("data/data_sna.rds")
    data_sna <- count(data_sna, from, to, name = "weight")
    
    g <- as_tbl_graph(data_sna, directed = TRUE) |>
      activate("nodes") |>
      mutate(
        degree = centrality_degree(mode = "all"),
        betweenness = centrality_betweenness(directed = TRUE),
        closeness = centrality_closeness(mode = "all"),
        pagerank = centrality_pagerank(),
        eigenvector = centrality_eigen()
      )
    
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
  
  output$sna_plot_ui <- renderUI({
    if (identical(input$sna_grafico, "interactive")) {
      plotOutput("sna_plot_interactive", height = 650)
    } else {
      plotOutput("sna_plot_static", height = 650)
    }
  })
  
  output$sna_plot_interactive <- renderPlot({
    input$sna_run
    isolate({
      tryCatch(
        {
          load_grafo_reddit()
          g0 <- grafo_reddit()
          validate(need(!is.null(g0), "No se pudo cargar `data/data_sna.rds`."))
          
          g1 <- generate_subgraph_advanced(
            umbral_nodos = input$sna_umbral_nodos,
            umbral_aristas = input$sna_umbral_aristas,
            g = g0
          )
          
          plot_graph(seed = input$sna_seed, grafo = g1)
        },
        error = function(e) {
          showNotification(conditionMessage(e), type = "error")
          plot.new()
          text(0.5, 0.5, conditionMessage(e), cex = 0.9)
        }
      )
    })
  })

  output$sna_plot_static <- renderPlot({
    plot_png(input$sna_grafico)
  })
  
  output$sent_plot <- renderPlot({
    plot_png(input$sent_grafico)
  })
  
  output$topic_plot <- renderPlot({
    plot_png(input$topic_grafico)
  })
  
  # --- FCA (fca.qmd) ---
  fca_resultados <- reactiveVal(NULL)
  fca_net_positions <- reactiveVal(NULL)
  
  build_matriz_fca <- function() {
    modelo_lda <- readRDS("data/modelo_lda.rds") 
    tabla_roles <- readRDS("data/tabla_roles.rds")
    data_texto <- readRDS("data/data_texto_procesado.rds")
    use_ia_data <- identical(input$fca_metodo, "ia")
    data_sentim <- if (use_ia_data) readRDS("data/data_sentim_ia.rds") else readRDS("data/data_sentim.rds")
    nombres_temas <- readRDS("data/nombres_temas.rds")
    df_temas <- as.data.frame(readRDS("data/df_temas_ia.rds"), stringsAsFactors = FALSE, check.names = FALSE)
    
    df_integrado <- data_sentim |>
      # Cruzamos con los temas (nos quedamos con los 700 que tienen tema)
      inner_join(df_temas, by = "comment_id") |> 
      # Cruzamos con las métricas de red del autor
      left_join(tabla_roles, by = c("author" = "name")) |>
      # Filtramos por si algún autor no estaba en la tabla de roles
      filter(!is.na(Rol))
    
    if ("name" %in% names(tabla_roles) && !"Usuario" %in% names(tabla_roles)) {
      tabla_roles$Usuario <- tabla_roles$name
    }
    if ("betweenness" %in% names(tabla_roles) && !"betweenness" %in% names(tabla_roles)) {
      tabla_roles$betweenness <- tabla_roles$betweenness
    }
    if ("pagerank" %in% names(tabla_roles) && !"pagerank" %in% names(tabla_roles)) {
      tabla_roles$pagerank <- tabla_roles$pagerank
    }
    if ("comunidad" %in% names(tabla_roles) && !"Comunidad" %in% names(tabla_roles)) {
      tabla_roles$Comunidad <- as.character(tabla_roles$comunidad)
    }
    
    # df_integrado <- data_sentim |>
    #   inner_join(df_temas, by = "comment_id") |>
    #   left_join(tabla_roles, by = c("author" = "Usuario")) |>
    #   filter(!is.na(.data$Rol))
    
    q1_influencia <- stats::quantile(df_integrado$pagerank, 0.25, na.rm = TRUE)
    q3_influencia <- stats::quantile(df_integrado$pagerank, 0.75, na.rm = TRUE)
    q1_intermediacion <- stats::quantile(df_integrado$betweenness, 0.25, na.rm = TRUE)
    q3_intermediacion <- stats::quantile(df_integrado$betweenness, 0.75, na.rm = TRUE)
    q1_engagement <- stats::quantile(df_integrado$score, 0.25, na.rm = TRUE)
    q3_engagement <- stats::quantile(df_integrado$score, 0.75, na.rm = TRUE)
    
    df_fca <- df_integrado |>
      mutate(
        Autor_Alta_Influencia = as.integer(.data$pagerank >= q3_influencia),
        Autor_Media_Influencia = as.integer(.data$pagerank > q1_influencia & .data$pagerank < q3_influencia),
        Autor_Baja_Influencia = as.integer(.data$pagerank <= q1_influencia),
        Autor_Alto_Puente = as.integer(.data$betweenness >= q3_intermediacion),
        Autor_Medio_Puente = as.integer(.data$betweenness > q1_intermediacion & .data$betweenness < q3_intermediacion),
        Autor_Bajo_Puente = as.integer(.data$betweenness <= q1_intermediacion),
        Autor_Comunidad_1 = as.integer(.data$comunidad == "1"),
        Autor_Comunidad_2 = as.integer(.data$comunidad == "2"),
        Autor_Comunidad_3 = as.integer(.data$comunidad == "3"),
        Autor_Comunidad_4 = as.integer(.data$comunidad == "4"),
        Autor_Comunidad_5 = as.integer(.data$comunidad == "5"),
        Autor_Comunidad_6 = as.integer(.data$comunidad == "6"),
        Autor_Comunidad_7 = as.integer(.data$comunidad == "7"),
        Autor_Comunidad_8 = as.integer(.data$comunidad == "8"),
        Autor_Rol_Regular = as.integer(.data$Rol == "Usuario Regular"),
        Autor_Rol_Broker = as.integer(.data$Rol == "Broker (Conector)"),
        Autor_Rol_Autoridad = as.integer(.data$Rol == "Autoridad (Referencia)"),
        Autor_Rol_Hub = as.integer(.data$Rol == "Hub (Difusor activo)"),
        Coment_Alto_Impacto = as.integer(.data$score >= q3_engagement),
        Coment_Medio_Impacto = as.integer(.data$score > q1_engagement & .data$score < q3_engagement),
        Coment_Bajo_Impacto = as.integer(.data$score <= q1_engagement),
        Sent_Muy_Positivo = as.integer((if (use_ia_data) .data$valencia_ia else .data$valencia) >= 3),
        Sent_Muy_Negativo = as.integer((if (use_ia_data) .data$valencia_ia else .data$valencia) <= -3),
        Sent_Positivo = as.integer((if (use_ia_data) .data$valencia_ia else .data$valencia) >= 1 & (if (use_ia_data) .data$valencia_ia else .data$valencia) < 3),
        Sent_Negativo = as.integer((if (use_ia_data) .data$valencia_ia else .data$valencia) <= -1 & (if (use_ia_data) .data$valencia_ia else .data$valencia) > -3),
        Sent_Neutro = as.integer((if (use_ia_data) .data$valencia_ia else .data$valencia) > -1 & (if (use_ia_data) .data$valencia_ia else .data$valencia) < 1),
        Emocion_Confianza = as.integer(if (use_ia_data) (!is.na(.data$emocion_ia) & .data$emocion_ia == "Trust") else (.data$trust > 0)),
        Emocion_Anticipacion = as.integer(if (use_ia_data) (!is.na(.data$emocion_ia) & .data$emocion_ia == "Anticipation") else (.data$anticipation > 0)),
        Emocion_Miedo = as.integer(if (use_ia_data) (!is.na(.data$emocion_ia) & .data$emocion_ia == "Fear") else (.data$fear > 0)),
        Emocion_Ira = as.integer(if (use_ia_data) (!is.na(.data$emocion_ia) & .data$emocion_ia == "Anger") else (.data$anger > 0)),
        Emocion_Alegria = as.integer(if (use_ia_data) (!is.na(.data$emocion_ia) & .data$emocion_ia == "Joy") else (.data$joy > 0)),
        Emocion_Tristeza = as.integer(if (use_ia_data) (!is.na(.data$emocion_ia) & .data$emocion_ia == "Sadness") else (.data$sadness > 0)),
        Emocion_Sorpresa = as.integer(if (use_ia_data) (!is.na(.data$emocion_ia) & .data$emocion_ia == "Surprise") else (.data$surprise > 0)),
        Emocion_Asco = as.integer(if (use_ia_data) (!is.na(.data$emocion_ia) & .data$emocion_ia == "Disgust") else (.data$disgust > 0)),
        Tema_CryptocurrencyCriticism = as.integer(.data$`Cryptocurrency Criticism` > 0.15),
        Tema_BlockchainTechnology = as.integer(.data$`Blockchain Technology` > 0.15),
        Tema_InvestmentAndScams = as.integer(.data$`Investment and Scams` > 0.15),
        Tema_PriceFluctuations = as.integer(.data$`Price Fluctuations` > 0.15),
        Tema_CryptoExchangeMistakes = as.integer(.data$`Crypto Exchange Mistakes` > 0.15),
        Tema_IllicitActivities = as.integer(.data$`Illicit Activities` > 0.15),
        Tema_MarketAnalysis = as.integer(.data$`Market Analysis` > 0.15),
        Tema_TechnologyIntegration = as.integer(.data$`Technology Integration` > 0.15)
      )
    
    matriz_fca_limpia <- df_fca |>
      select(
        Autor_Alta_Influencia, Autor_Media_Influencia, Autor_Baja_Influencia,
        Autor_Alto_Puente, Autor_Medio_Puente, Autor_Bajo_Puente,
        Autor_Comunidad_1, Autor_Comunidad_2, Autor_Comunidad_3,
        Autor_Comunidad_4, Autor_Comunidad_5, Autor_Comunidad_6,
        Autor_Rol_Regular, Autor_Rol_Broker, Autor_Rol_Autoridad, Autor_Rol_Hub,
        Coment_Alto_Impacto, Coment_Medio_Impacto, Coment_Bajo_Impacto,
        Sent_Muy_Positivo, Sent_Muy_Negativo, Sent_Positivo, Sent_Negativo, Sent_Neutro,
        Emocion_Confianza, Emocion_Anticipacion, Emocion_Miedo, Emocion_Ira,
        Emocion_Alegria, Emocion_Tristeza, Emocion_Sorpresa, Emocion_Asco,
        Tema_CryptocurrencyCriticism, Tema_BlockchainTechnology, Tema_InvestmentAndScams,
        Tema_PriceFluctuations, Tema_CryptoExchangeMistakes, Tema_IllicitActivities,
        Tema_MarketAnalysis, Tema_TechnologyIntegration
      )
    
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
    nombres_atributos <- colnames(matriz_fca_final)
    
    intensiones_texto <- apply(matriz_intensiones, 2, function(columna) {
      atributos_activos <- nombres_atributos[columna > 0]
      paste(atributos_activos, collapse = " + ")
    })
    
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
          column(7, plotOutput("fca_plot", height = 400)),
          column(5, DT::dataTableOutput("fca_attr_table"))
        ),
        br(),
        uiOutput("fca_attr_details")
      ),
      tabPanel(
        title = "Conceptos",
        value = "conceptos",
        fluidRow(
          column(7, DT::dataTableOutput("fca_concepts_table")),
          column(5, visNetworkOutput("fca_concepts_net", height = "500px"))
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

  output$fca_plot <- renderPlot({
    input$fca_run
    input$fca_tabs
    res <- fca_resultados()
    validate(need(!is.null(res), "Pulsa 'Calcular FCA' para generar los resultados."))
    # render siempre; se muestra en la pestaña Atributos
    
    top_attrs <- res$resumen_atributos[order(-res$resumen_atributos$Porcentaje), ]
    top_attrs <- utils::head(top_attrs, 15)
    
    ggplot(top_attrs, aes(x = stats::reorder(.data$Atributo, .data$Porcentaje), y = .data$Porcentaje)) +
      geom_col(fill = "steelblue") +
      coord_flip() +
      theme_minimal() +
      labs(subtitle = paste("Contexto FCA:", ifelse(input$fca_metodo == "ia", "IA", "Clásico"))) +
      labs(
        title = "Frecuencia de atributos en el contexto formal",
        x = NULL,
        y = "Porcentaje (%)"
      )
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

    datatable(df, options = list(pageLength = 10, autoWidth = TRUE), rownames = FALSE)
  })

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
    datatable(dat, options = list(pageLength = 15, autoWidth = TRUE), rownames = FALSE)
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

shinyApp(ui, server)