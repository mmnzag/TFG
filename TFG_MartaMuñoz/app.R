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
            selectInput(
              "fca_vista",
              "Vista",
              choices = c(
                "Frecuencia de atributos" = "atributos",
                "Top conceptos formales" = "conceptos"
              )
            ),
            actionButton("fca_run", "Calcular FCA", class = "btn-primary"),
            br(), br(),
            p("Análisis de Conceptos Formales integrando red, sentimiento y temas (como en fca.qmd).")
          ),
          box(
            width = 9,
            title = "Visualización",
            status = "primary",
            solidHeader = TRUE,
            uiOutput("fca_plot_ui"),
            tableOutput("fca_tabla")
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

  build_matriz_fca <- function() {
    modelo_lda <- readRDS("data/modelo_lda.rds")
    nombres_temas <- readRDS("data/nombres_temas.rds")
    tabla_roles <- readRDS("data/tabla_roles.rds")
    data_sentim <- readRDS("data/data_sentim.rds")

    gamma_matrix <- modelo_lda@gamma
    colnames(gamma_matrix) <- nombres_temas
    df_temas <- as.data.frame(gamma_matrix)
    df_temas$comment_id <- modelo_lda@documents

    if ("name" %in% names(tabla_roles) && !"Usuario" %in% names(tabla_roles)) {
      tabla_roles$Usuario <- tabla_roles$name
    }
    if ("betweenness" %in% names(tabla_roles) && !"Intermediacion" %in% names(tabla_roles)) {
      tabla_roles$Intermediacion <- tabla_roles$betweenness
    }
    if ("pagerank" %in% names(tabla_roles) && !"PageRank" %in% names(tabla_roles)) {
      tabla_roles$PageRank <- tabla_roles$pagerank
    }
    if ("comunidad" %in% names(tabla_roles) && !"Comunidad" %in% names(tabla_roles)) {
      tabla_roles$Comunidad <- as.character(tabla_roles$comunidad)
    }

    df_integrado <- data_sentim |>
      inner_join(df_temas, by = "comment_id") |>
      left_join(tabla_roles, by = c("author" = "Usuario")) |>
      filter(!is.na(.data$Rol))

    q1_influencia <- stats::quantile(df_integrado$PageRank, 0.25, na.rm = TRUE)
    q3_influencia <- stats::quantile(df_integrado$PageRank, 0.75, na.rm = TRUE)
    q1_intermediacion <- stats::quantile(df_integrado$Intermediacion, 0.25, na.rm = TRUE)
    q3_intermediacion <- stats::quantile(df_integrado$Intermediacion, 0.75, na.rm = TRUE)
    q1_engagement <- stats::quantile(df_integrado$score, 0.25, na.rm = TRUE)
    q3_engagement <- stats::quantile(df_integrado$score, 0.75, na.rm = TRUE)

    df_fca <- df_integrado |>
      mutate(
        Autor_Alta_Influencia = as.integer(.data$PageRank >= q3_influencia),
        Autor_Media_Influencia = as.integer(.data$PageRank > q1_influencia & .data$PageRank < q3_influencia),
        Autor_Baja_Influencia = as.integer(.data$PageRank <= q1_influencia),
        Autor_Alto_Puente = as.integer(.data$Intermediacion >= q3_intermediacion),
        Autor_Medio_Puente = as.integer(.data$Intermediacion > q1_intermediacion & .data$Intermediacion < q3_intermediacion),
        Autor_Bajo_Puente = as.integer(.data$Intermediacion <= q1_intermediacion),
        Autor_Comunidad_1 = as.integer(.data$Comunidad == "1"),
        Autor_Comunidad_2 = as.integer(.data$Comunidad == "2"),
        Autor_Comunidad_3 = as.integer(.data$Comunidad == "3"),
        Autor_Comunidad_4 = as.integer(.data$Comunidad == "4"),
        Autor_Comunidad_5 = as.integer(.data$Comunidad == "5"),
        Autor_Comunidad_6 = as.integer(.data$Comunidad == "6"),
        Autor_Comunidad_7 = as.integer(.data$Comunidad == "7"),
        Autor_Comunidad_8 = as.integer(.data$Comunidad == "8"),
        Autor_Rol_Regular = as.integer(.data$Rol == "Usuario Regular"),
        Autor_Rol_Broker = as.integer(.data$Rol == "Broker (Conector)"),
        Autor_Rol_Autoridad = as.integer(.data$Rol == "Autoridad (Referencia)"),
        Autor_Rol_Hub = as.integer(.data$Rol == "Hub (Difusor activo)"),
        Coment_Alto_Impacto = as.integer(.data$score >= q3_engagement),
        Coment_Medio_Impacto = as.integer(.data$score > q1_engagement & .data$score < q3_engagement),
        Coment_Bajo_Impacto = as.integer(.data$score <= q1_engagement),
        Sent_Muy_Positivo = as.integer(.data$valencia >= 3),
        Sent_Muy_Negativo = as.integer(.data$valencia <= -3),
        Sent_Positivo = as.integer(.data$valencia >= 1 & .data$valencia < 3),
        Sent_Negativo = as.integer(.data$valencia <= -1 & .data$valencia > -3),
        Sent_Neutro = as.integer(.data$valencia > -1 & .data$valencia < 1),
        Emocion_Confianza = as.integer(.data$trust > 0),
        Emocion_Anticipacion = as.integer(.data$anticipation > 0),
        Emocion_Miedo = as.integer(.data$fear > 0),
        Emocion_Ira = as.integer(.data$anger > 0),
        Emocion_Alegria = as.integer(.data$joy > 0),
        Emocion_Tristeza = as.integer(.data$sadness > 0),
        Emocion_Sorpresa = as.integer(.data$surprise > 0),
        Emocion_Asco = as.integer(.data$disgust > 0),
        Tema_MarketTrends = as.integer(.data$`Market Trends` > 0.15),
        Tema_DebtCrisis = as.integer(.data$`Debt Crisis` > 0.15),
        Tema_TrumpScam = as.integer(.data$`Trump Scam` > 0.15),
        Tema_GlobalCurrency = as.integer(.data$`Global Currency` > 0.15),
        Tema_BlockchainTech = as.integer(.data$`Blockchain Tech` > 0.15),
        Tema_MarketCrash = as.integer(.data$`Market Crash` > 0.15)
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
        Tema_MarketTrends, Tema_DebtCrisis, Tema_TrumpScam,
        Tema_GlobalCurrency, Tema_BlockchainTech, Tema_MarketCrash
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

    soportes <- fc$concepts$support()
    matriz_intensiones <- fc$concepts$intents()
    intensiones_texto <- apply(matriz_intensiones, 2, function(columna) {
      atributos_activos <- rownames(matriz_intensiones)[columna == 1]
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
      tabla_conceptos = tabla_conceptos
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
    if (identical(input$fca_vista, "atributos")) {
      plotOutput("fca_plot", height = 500)
    }
  })

  output$fca_plot <- renderPlot({
    input$fca_run
    input$fca_vista
    res <- fca_resultados()
    validate(need(!is.null(res), "Pulsa 'Calcular FCA' para generar los resultados."))
    validate(need(identical(input$fca_vista, "atributos"), invisible(NULL)))

    top_attrs <- res$resumen_atributos[order(-res$resumen_atributos$Porcentaje), ]
    top_attrs <- utils::head(top_attrs, 15)

    ggplot(top_attrs, aes(x = stats::reorder(.data$Atributo, .data$Porcentaje), y = .data$Porcentaje)) +
      geom_col(fill = "steelblue") +
      coord_flip() +
      theme_minimal() +
      labs(
        title = "Frecuencia de atributos en el contexto formal",
        x = NULL,
        y = "Porcentaje (%)"
      )
  })

  output$fca_tabla <- renderTable({
    input$fca_run
    input$fca_vista
    res <- fca_resultados()
    validate(need(!is.null(res), "Pulsa 'Calcular FCA' para generar los resultados."))
    validate(need(identical(input$fca_vista, "conceptos"), invisible(NULL)))

    utils::head(res$tabla_conceptos, 15)
  })
}

shinyApp(ui, server)
