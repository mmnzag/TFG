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
library(stringr)
library(syuzhet)
library(quanteda)
library(topicmodels)
library(igraph)
library(purrr)
library(tibble)
library(tidyr)

# Selector de fuente de datos ("reddit" o "kaggle")
fuente_activa <- "kaggle" 

# Construcción dinámica de la ruta
ruta_tabla_roles <- paste0("data/", fuente_activa, "/tabla_roles.rds")
ruta_data_sentim <- paste0("data/", fuente_activa, "/data_sentim_ia.rds")
ruta_df_temas <- paste0("data/", fuente_activa, "/df_temas_ia.rds")

# Carga de los datos (mantén el nombre de la variable que ya estuviera usando en el fichero original)
tabla_roles <- readRDS(ruta_tabla_roles)
data_sentim <- readRDS(ruta_data_sentim)
df_temas <- as.data.frame(readRDS(ruta_df_temas), stringsAsFactors = FALSE, check.names = FALSE)

# Dinámicamente añadir el método 'levels' a la clase ConceptLattice de fcaR para que fc$concepts$levels() funcione de forma nativa
fcaR::ConceptLattice$set("public", "levels", function() {
  self$.__enclos_env__$private$build_adjacency()
  self$.__enclos_env__$private$build_covering()
  cov <- self$.__enclos_env__$private$covering_matrix
  cover_edges_df <- fcaR:::sparse_matrix_to_edges(cov)
  
  # Calcular grados de todos los conceptos del retículo
  grados <- fcaR::calculate_grades(
    concept_ids = seq_len(self$size()),
    edge_from = cover_edges_df$from,
    edge_to = cover_edges_df$to
  )
  
  # Invertir para Hasse top-down (Top = 0, Bottom = max)
  max_grado <- max(grados)
  niveles_globales <- max_grado - grados
  return(as.integer(niveles_globales))
}, overwrite = TRUE)

plot_png <- function(filename) {
  path <- file.path("images", filename)
  validate(need(file.exists(path), paste("No se encontró el archivo:", path)))
  img <- readPNG(path)
  grid.newpage()
  grid.raster(img)
}

esperando_resultados_ui <- function(msg = "Esperando resultados...") {
  tags$div(
    class = "waiting-container",
    tags$div(
      class = "waiting-overlay",
      tags$img(src = "images/waiting_placeholder.png", class = "waiting-img"),
      tags$h3(msg, class = "waiting-title"),
      tags$p("Configure las opciones en el panel superior y haga clic en el botón de ejecución para generar los análisis.", class = "waiting-desc")
    )
  )
}

obtener_tema_plot <- function(light = FALSE) {
  if (light) {
    theme_minimal(base_family = "Inter") +
    theme(
      plot.background = element_rect(fill = "#ffffff", color = NA),
      panel.background = element_rect(fill = "#ffffff", color = NA),
      text = element_text(color = "#0f172a"),
      plot.title = element_text(face = "bold", color = "#0f172a", size = 14, hjust = 0.5),
      plot.subtitle = element_text(color = "#475569", size = 11, hjust = 0.5),
      axis.text = element_text(color = "#475569"),
      axis.title = element_text(color = "#475569"),
      panel.grid.major = element_line(color = "#cbd5e1", linewidth = 0.5),
      panel.grid.minor = element_line(color = "#f1f5f9", linewidth = 0.25),
      legend.text = element_text(color = "#0f172a"),
      legend.title = element_text(color = "#0f172a"),
      legend.position = "bottom"
    )
  } else {
    theme_minimal(base_family = "Inter") +
    theme(
      plot.background = element_rect(fill = "#131a26", color = NA),
      panel.background = element_rect(fill = "#131a26", color = NA),
      text = element_text(color = "#cbd5e1"),
      plot.title = element_text(face = "bold", color = "#f8fafc", size = 14, hjust = 0.5),
      plot.subtitle = element_text(color = "#94a3b8", size = 11, hjust = 0.5),
      axis.text = element_text(color = "#94a3b8"),
      axis.title = element_text(color = "#cbd5e1"),
      panel.grid.major = element_line(color = "#ffffff10", linewidth = 0.5),
      panel.grid.minor = element_line(color = "#ffffff05", linewidth = 0.25),
      legend.text = element_text(color = "#cbd5e1"),
      legend.title = element_text(color = "#cbd5e1"),
      legend.position = "bottom"
    )
  }
}

transitive_reduction <- function(g) {
  adj <- as.matrix(igraph::as_adjacency_matrix(g))
  n <- nrow(adj)
  redundant <- matrix(FALSE, nrow = n, ncol = n)
  
  for (u in seq_len(n)) {
    neighbors <- which(adj[u, ] > 0)
    if (length(neighbors) > 0) {
      reachable <- unique(unlist(lapply(neighbors, function(w) {
        setdiff(igraph::subcomponent(g, w, mode = "out"), w)
      })))
      redundant_neighbors <- intersect(neighbors, reachable)
      if (length(redundant_neighbors) > 0) {
        redundant[u, redundant_neighbors] <- TRUE
      }
    }
  }
  
  edges_df <- igraph::as_data_frame(g, what = "edges")
  v_names <- igraph::V(g)$name
  
  if (is.null(v_names)) {
    from_idx <- edges_df$from
    to_idx <- edges_df$to
  } else {
    from_idx <- match(edges_df$from, v_names)
    to_idx <- match(edges_df$to, v_names)
  }
  
  keep <- !sapply(seq_len(nrow(edges_df)), function(i) {
    redundant[from_idx[i], to_idx[i]]
  })
  
  reduced_edges <- edges_df[keep, , drop = FALSE]
  g_reduced <- igraph::graph_from_data_frame(d = reduced_edges, vertices = igraph::as_data_frame(g, what = "vertices"), directed = TRUE)
  return(g_reduced)
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

options(shiny.maxRequestSize = 200 * 1024^2)
shiny::addResourcePath("images", "images")

ui <- dashboardPage(
  title = "Reddit Analyser",
  skin = "blue",
  dashboardHeader(
    title = tagList(
      span(class = "logo-lg", "Reddit Analyser"),
      span(class = "logo-mini", "RA")
    ),
    tags$li(
      class = "dropdown download-rds-container",
      style = "padding: 12px 15px; display: flex; align-items: center;",
      uiOutput("download_fca_rds_ui")
    ),
    tags$li(
      class = "dropdown theme-switch-container",
      style = "padding: 12px 15px; display: flex; align-items: center; gap: 8px;",
      icon("sun"),
      prettySwitch(
        inputId = "theme_switch",
        label = NULL,
        value = FALSE,
        status = "info",
        slim = TRUE
      ),
      icon("moon")
    )
  ),
  dashboardSidebar(
    sidebarMenu(
      id = "menu",
      menuItem("Carga de datos", tabName = "carga", icon = icon("upload")),
      menuItem("Análisis de redes sociales", tabName = "sna", icon = icon("project-diagram")),
      menuItem("Análisis de sentimiento", tabName = "sentimiento", icon = icon("face-smile")),
      menuItem("Topic modeling", tabName = "topic", icon = icon("comments")),
      menuItem("FCA", tabName = "fca", icon = icon("sitemap"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600;700&family=Outfit:wght@300;400;500;600;700;800&display=swap"),
      tags$script(HTML("
        function fixSelects() {
          var isDark = $('body').hasClass('dark-theme');
          $('select[id^=\"nodeSelect\"], .visNetwork select, select.vis-network-select-node, select').each(function() {
            if (isDark) {
              $(this).css({
                'background': '#0d131f',
                'background-color': '#0d131f',
                'color': '#cbd5e1',
                'border-color': 'rgba(255, 255, 255, 0.08)'
              });
              $(this).find('option').css({
                'background-color': '#131a26',
                'color': '#f8fafc'
              });
            } else {
              $(this).css({
                'background': '#ffffff',
                'background-color': '#ffffff',
                'color': '#1e293b',
                'border-color': 'rgba(0, 0, 0, 0.12)'
              });
              $(this).find('option').css({
                'background-color': '#ffffff',
                'color': '#0f172a'
              });
            }
          });
          /* Fix vis-tooltip inline styles injected by vis.js */
          $('.vis-tooltip').each(function() {
            if (isDark) {
              this.style.setProperty('background', '#131a26', 'important');
              this.style.setProperty('color', '#f8fafc', 'important');
              this.style.setProperty('border', '1px solid rgba(255,255,255,0.08)', 'important');
              this.style.setProperty('box-shadow', '0 10px 30px rgba(0,0,0,0.4)', 'important');
              $(this).find('b, strong').each(function(){ this.style.setProperty('color', '#f8fafc', 'important'); });
            } else {
              this.style.setProperty('background', '#ffffff', 'important');
              this.style.setProperty('color', '#0f172a', 'important');
              this.style.setProperty('border', '1px solid rgba(0,0,0,0.08)', 'important');
              this.style.setProperty('box-shadow', '0 10px 30px rgba(0,0,0,0.05)', 'important');
              $(this).find('b, strong').each(function(){ this.style.setProperty('color', '#0f172a', 'important'); });
            }
          });
        }

        function styleTooltip(el) {
          var isDark = $('body').hasClass('dark-theme');
          if (isDark) {
            el.style.setProperty('background', '#131a26', 'important');
            el.style.setProperty('color', '#f8fafc', 'important');
            el.style.setProperty('border', '1px solid rgba(255,255,255,0.08)', 'important');
            el.style.setProperty('box-shadow', '0 10px 30px rgba(0,0,0,0.4)', 'important');
            el.style.setProperty('border-radius', '8px');
            el.style.setProperty('font-family', 'Inter, sans-serif');
            el.style.setProperty('font-size', '13px');
            el.style.setProperty('padding', '8px 12px');
            $(el).find('b, strong').each(function(){ this.style.setProperty('color', '#f8fafc', 'important'); });
          } else {
            el.style.setProperty('background', '#ffffff', 'important');
            el.style.setProperty('color', '#0f172a', 'important');
            el.style.setProperty('border', '1px solid rgba(0,0,0,0.08)', 'important');
            el.style.setProperty('box-shadow', '0 10px 30px rgba(0,0,0,0.05)', 'important');
            $(el).find('b, strong').each(function(){ this.style.setProperty('color', '#0f172a', 'important'); });
          }
        }

        $(document).ready(function() {
          $('body').addClass('sidebar-mini');
          setInterval(fixSelects, 300);

          /* MutationObserver: catch vis-tooltip the instant vis.js creates it */
          var observer = new MutationObserver(function(mutations) {
            mutations.forEach(function(m) {
              m.addedNodes.forEach(function(node) {
                if (node.nodeType === 1) {
                  if (node.classList && node.classList.contains('vis-tooltip')) {
                    styleTooltip(node);
                  }
                  /* Also check children in case tooltip is nested */
                  var inner = node.querySelectorAll ? node.querySelectorAll('.vis-tooltip') : [];
                  inner.forEach(function(el) { styleTooltip(el); });
                }
              });
              /* Also catch attribute mutations (vis.js may toggle visibility) */
              if (m.type === 'attributes' && m.target.classList && m.target.classList.contains('vis-tooltip')) {
                styleTooltip(m.target);
              }
            });
          });
          observer.observe(document.body, { childList: true, subtree: true, attributes: true, attributeFilter: ['style', 'class'] });
        });
        $(document).on('shiny:inputchanged', function(event) {
          if (event.name === 'theme_switch') {
            if (event.value) {
              $('body').addClass('dark-theme');
            } else {
              $('body').removeClass('dark-theme');
            }
            setTimeout(fixSelects, 50);
            /* Re-style any visible tooltips immediately on theme change */
            $('.vis-tooltip').each(function() { styleTooltip(this); });
          }
        });
      ")),
      tags$style(HTML("
        :root {
          /* Light Mode Styles (default) */
          --bg-global: #f1f5f9;
          --bg-card: #ffffff;
          --bg-sidebar: #ffffff;
          --border-color: rgba(0, 0, 0, 0.08);
          --border-color-focus: rgba(59, 130, 246, 0.5);
          --text-primary: #0f172a;
          --text-secondary: #475569;
          --text-muted: #94a3b8;
          --input-bg: #ffffff;
          --input-border: rgba(0, 0, 0, 0.12);
          --input-text: #1e293b;
          --btn-primary-bg: linear-gradient(135deg, #3b82f6, #1d4ed8);
          --btn-primary-hover: linear-gradient(135deg, #2563eb, #1e40af);
          --btn-success-bg: linear-gradient(135deg, #10b981, #059669);
          --btn-success-hover: linear-gradient(135deg, #059669, #047857);
          --table-header-bg: #f8fafc;
          --table-row-bg: #ffffff;
          --table-row-alt-bg: #f1f5f9;
          --box-shadow: 0 10px 30px rgba(0, 0, 0, 0.05);
          --box-shadow-hover: 0 15px 35px rgba(59, 130, 246, 0.1);
          --alert-warning-bg: rgba(245, 158, 11, 0.08);
          --alert-warning-text: #b45309;
          --alert-warning-border: rgba(245, 158, 11, 0.15);
          --alert-info-bg: rgba(59, 130, 246, 0.08);
          --alert-info-text: #1d4ed8;
          --alert-info-border: rgba(59, 130, 246, 0.15);
          --tab-active-bg: #ffffff;
          --tab-active-text: #3b82f6;
          --focus-ring: 0 0 0 3px rgba(59, 130, 246, 0.25);
        }

        .dark-theme {
          /* Dark Mode Styles */
          --bg-global: #080c14;
          --bg-card: #131a26;
          --bg-sidebar: #0b1329; /* Azul oscuro elegante */
          --border-color: rgba(255, 255, 255, 0.06);
          --border-color-focus: rgba(99, 102, 241, 0.5);
          --text-primary: #f8fafc;
          --text-secondary: #94a3b8;
          --text-muted: #64748b;
          --input-bg: #0d131f;
          --input-border: rgba(255, 255, 255, 0.08);
          --input-text: #cbd5e1;
          --btn-primary-bg: linear-gradient(135deg, #6366f1, #3b82f6);
          --btn-primary-hover: linear-gradient(135deg, #4f46e5, #2563eb);
          --btn-success-bg: linear-gradient(135deg, #10b981, #0d9488);
          --btn-success-hover: linear-gradient(135deg, #059669, #0f766e);
          --table-header-bg: #0b0f19;
          --table-row-bg: #131a26;
          --table-row-alt-bg: #182235;
          --box-shadow: 0 10px 30px rgba(0, 0, 0, 0.4);
          --box-shadow-hover: 0 15px 35px rgba(99, 102, 241, 0.15);
          --alert-warning-bg: rgba(245, 158, 11, 0.1);
          --alert-warning-text: #fbbf24;
          --alert-warning-border: rgba(245, 158, 11, 0.2);
          --alert-info-bg: rgba(6, 180, 212, 0.1);
          --alert-info-text: #22d3ee;
          --alert-info-border: rgba(6, 180, 212, 0.2);
          --tab-active-bg: #131a26;
          --tab-active-text: #6366f1;
          --focus-ring: 0 0 0 3px rgba(99, 102, 241, 0.25);
        }

        /* Custom Scrollbars */
        ::-webkit-scrollbar {
          width: 8px;
          height: 8px;
        }
        ::-webkit-scrollbar-track {
          background: var(--bg-global);
        }
        ::-webkit-scrollbar-thumb {
          background: var(--text-muted);
          border-radius: 4px;
        }
        ::-webkit-scrollbar-thumb:hover {
          background: var(--text-secondary);
        }

        /* Global styles */
        body, .content-wrapper, .right-side {
          background-color: var(--bg-global) !important;
          color: var(--text-primary) !important;
          font-family: 'Inter', sans-serif !important;
          transition: background-color 0.3s ease, color 0.3s ease;
          overflow: visible !important;
        }

        /* Typography elements */
        h1, h2, h3, h4, h5, h6, .box-title {
          font-family: 'Outfit', 'Inter', sans-serif !important;
          font-weight: 600 !important;
          color: var(--text-primary) !important;
        }

        /* Header styling */
        .main-header .logo {
          background-color: var(--bg-sidebar) !important;
          color: var(--text-primary) !important;
          font-family: 'Outfit', sans-serif !important;
          font-weight: 800 !important;
          border-bottom: 1px solid var(--border-color);
          transition: all 0.3s ease;
        }
        .main-header .navbar {
          background-color: var(--bg-card) !important;
          border-bottom: 1px solid var(--border-color);
          transition: all 0.3s ease;
        }
        .main-header .navbar .sidebar-toggle {
          color: var(--text-secondary) !important;
        }
        .main-header .navbar .sidebar-toggle:hover {
          background-color: rgba(255, 255, 255, 0.05) !important;
          color: var(--text-primary) !important;
        }
        .theme-switch-container span {
          transition: color 0.3s ease;
        }
        .main-header .navbar .dropdown.download-rds-container a {
          color: var(--text-secondary) !important;
          font-weight: 600;
          display: inline-flex;
          align-items: center;
          justify-content: center;
          font-size: 18px;
          text-decoration: none;
          transition: color 0.2s ease, transform 0.2s ease;
          padding: 0 !important;
          line-height: 1 !important;
        }
        .main-header .navbar .dropdown.download-rds-container a:hover:not(.disabled-download-link) {
          color: var(--tab-active-text) !important;
          transform: translateY(-1px);
        }

        /* Sidebar styling */
        .main-sidebar {
          background-color: var(--bg-sidebar) !important;
          border-right: 1px solid var(--border-color);
          transition: all 0.3s ease;
        }
        .sidebar-menu > li > a {
          color: var(--text-secondary) !important;
          border-left: 3px solid transparent !important;
          transition: all 0.2s ease;
          font-weight: 500;
        }
        .sidebar-menu > li.active > a {
          background-color: rgba(59, 130, 246, 0.06) !important;
          border-left-color: #3b82f6 !important;
          color: var(--text-primary) !important;
        }
        .dark-theme .sidebar-menu > li.active > a {
          background-color: rgba(99, 102, 241, 0.06) !important;
          border-left-color: #6366f1 !important;
        }
        body:not(.sidebar-collapse) .sidebar-menu > li:hover > a {
          background-color: rgba(59, 130, 246, 0.06) !important;
          border-left-color: #3b82f6 !important;
          color: var(--text-primary) !important;
        }
        body:not(.sidebar-collapse).dark-theme .sidebar-menu > li:hover > a {
          background-color: rgba(99, 102, 241, 0.06) !important;
          border-left-color: #6366f1 !important;
        }
        body.sidebar-collapse .sidebar-menu > li:hover > a {
          background-color: transparent !important;
        }
        .sidebar-menu > li > a > i {
          color: var(--text-muted) !important;
          transition: color 0.2s ease;
        }
        .sidebar-menu > li:hover > a > i, .sidebar-menu > li.active > a > i {
          color: #3b82f6 !important;
        }
        .dark-theme .sidebar-menu > li:hover > a > i, .dark-theme .sidebar-menu > li.active > a > i {
          color: #6366f1 !important;
        }

        /* Box & Cards styling */
        .box {
          background: var(--bg-card) !important;
          border: 1px solid var(--border-color) !important;
          border-top: 3px solid var(--border-color) !important;
          border-radius: 16px !important;
          box-shadow: var(--box-shadow) !important;
          margin-bottom: 24px !important;
          transition: box-shadow 0.3s ease, background-color 0.3s ease, border-color 0.3s ease;
          overflow: visible !important;
          position: relative !important;
        }
        .box:hover, .box:focus-within {
          box-shadow: var(--box-shadow-hover) !important;
          overflow: visible !important;
          z-index: 10005 !important;
        }
        .box-header {
          border-bottom: 1px solid var(--border-color) !important;
          padding: 18px 24px !important;
          position: relative;
          border-top-left-radius: 16px !important;
          border-top-right-radius: 16px !important;
        }
        .box-footer {
          border-bottom-left-radius: 16px !important;
          border-bottom-right-radius: 16px !important;
        }
        .box-body:last-child {
          border-bottom-left-radius: 16px !important;
          border-bottom-right-radius: 16px !important;
        }
        .box-body:first-child {
          border-top-left-radius: 16px !important;
          border-top-right-radius: 16px !important;
        }
        .box select {
          position: relative !important;
          z-index: 9999 !important;
          overflow: visible !important;
        }
        .dark-theme .box-header .box-title {
          border-left-color: #6366f1;
        }
        .box-body {
          padding: 24px !important;
          overflow: visible !important;
        }

        /* Alerts */
        .alert-warning {
          background-color: var(--alert-warning-bg) !important;
          color: var(--alert-warning-text) !important;
          border: 1px solid var(--alert-warning-border) !important;
          border-radius: 16px !important;
          padding: 15px !important;
          font-size: 14px;
        }
        .alert-info {
          background-color: var(--alert-info-bg) !important;
          color: var(--alert-info-text) !important;
          border: 1px solid var(--alert-info-border) !important;
          border-radius: 16px !important;
          padding: 15px !important;
          font-size: 14px;
        }

        /* Inputs and Forms */
        .form-control, .selectize-input, .selectize-control.single .selectize-input {
          background: var(--input-bg) !important;
          border: 1px solid var(--input-border) !important;
          color: var(--input-text) !important;
          border-radius: 10px !important;
          box-shadow: none !important;
          padding: 10px 15px !important;
          height: auto !important;
          transition: border-color 0.2s ease, box-shadow 0.2s ease, background-color 0.3s ease !important;
        }
        .form-control:focus, .selectize-input.focus {
          border-color: var(--border-color-focus) !important;
          box-shadow: var(--focus-ring) !important;
        }
        .selectize-control {
          overflow: visible !important;
        }
        .selectize-control .selectize-dropdown {
          position: absolute !important;
          top: 100% !important;
          left: 0 !important;
          margin-top: 0 !important;
          z-index: 3000 !important;
          border-radius: 10px !important;
        }
        .selectize-dropdown .selectize-dropdown-content {
          border-radius: 10px !important;
        }
        .selectize-dropdown .active {
          background-color: rgba(59, 130, 246, 0.15) !important;
          color: var(--text-primary) !important;
        }
        .dark-theme .selectize-dropdown .active {
          background-color: rgba(99, 102, 241, 0.15) !important;
        }
.box select {
  overflow: visible !important;
  z-index: 9999 !important;
  margin-bottom: 0 !important;
}
.box .selectize-control {
  position: relative !important;
  overflow: visible !important;
  z-index: 9999 !important;
}
.box .selectize-dropdown {
  position: absolute !important;
  overflow: visible !important;
  z-index: 9999 !important;
}

        /* visNetwork Select dropdown styling */
        select[id^='nodeSelect'],
        .visNetwork select,
        select.vis-network-select-node,
        .vis-network-container select,
        div[id^='htmlwidget-'] select,
        select {
          background: var(--input-bg) !important;
          background-color: var(--input-bg) !important;
          background-image: none !important;
          color: var(--input-text) !important;
          border: 1px solid var(--border-color) !important;
          border-radius: 8px !important;
          padding: 6px 12px !important;
          font-family: 'Inter', sans-serif !important;
          outline: none !important;
          font-size: 13px !important;
          height: auto !important;
          box-shadow: none !important;
          margin-bottom: 10px;
        }
        select[id^='nodeSelect'] option,
        .visNetwork select option,
        select.vis-network-select-node option,
        .vis-network-container select option,
        div[id^='htmlwidget-'] select option,
        select option {
          background: var(--bg-card) !important;
          background-color: var(--bg-card) !important;
          background-image: none !important;
          color: var(--text-primary) !important;
        }

        /* Buttons */
        .btn-primary, .btn-success, .btn-default, .btn-sm {
          border: none !important;
          border-radius: 10px !important;
          font-weight: 600 !important;
          padding: 10px 20px !important;
          transition: all 0.2s ease !important;
          cursor: pointer;
        }
        .btn-primary {
          background: var(--btn-primary-bg) !important;
          color: #ffffff !important;
        }
        .btn-primary:hover {
          background: var(--btn-primary-hover) !important;
          transform: translateY(-1px);
          box-shadow: 0 4px 15px rgba(99, 102, 241, 0.4) !important;
        }
        .btn-success {
          background: var(--btn-success-bg) !important;
          color: #ffffff !important;
        }
        .btn-success:hover {
          background: var(--btn-success-hover) !important;
          transform: translateY(-1px);
          box-shadow: 0 4px 15px rgba(16, 185, 129, 0.4) !important;
        }

        /* Sliders */
        .irs-bar {
          background: #3b82f6 !important;
          border-top-color: #3b82f6 !important;
          border-bottom-color: #3b82f6 !important;
        }
        .dark-theme .irs-bar {
          background: #6366f1 !important;
          border-top-color: #6366f1 !important;
          border-bottom-color: #6366f1 !important;
        }
        .irs-from, .irs-to, .irs-single {
          background: #3b82f6 !important;
        }
        .dark-theme .irs-from, .dark-theme .irs-to, .dark-theme .irs-single {
          background: #6366f1 !important;
        }
        .irs-grid-pol {
          background: var(--text-muted) !important;
        }
        .irs-grid-text {
          color: var(--text-secondary) !important;
        }

        /* Tables (DT) */
        .dataTables_wrapper {
          color: var(--text-secondary) !important;
          font-size: 14px;
        }
        .dataTables_wrapper table.dataTable {
          background-color: var(--bg-card) !important;
          border: 1px solid var(--border-color) !important;
          border-radius: 16px !important;
          overflow: hidden;
        }
        .dataTables_wrapper table.dataTable thead th {
          background-color: var(--table-header-bg) !important;
          color: var(--text-primary) !important;
          font-weight: 600 !important;
          border-bottom: 2px solid var(--border-color) !important;
          padding: 12px 15px !important;
        }
        .dataTables_wrapper table.dataTable tbody td {
          background-color: var(--table-row-bg) !important;
          color: var(--text-primary) !important;
          border-bottom: 1px solid var(--border-color) !important;
          padding: 12px 15px !important;
        }
        .dataTables_wrapper table.dataTable tbody tr:nth-child(even) td {
          background-color: var(--table-row-alt-bg) !important;
        }
        .dataTables_wrapper .dataTables_info, .dataTables_wrapper .dataTables_paginate {
          color: var(--text-secondary) !important;
          margin-top: 15px;
        }
        .dataTables_wrapper .dataTables_paginate .paginate_button {
          color: var(--text-secondary) !important;
          border-radius: 6px !important;
          border: 1px solid var(--border-color) !important;
          background: var(--bg-card) !important;
        }
        .dataTables_wrapper .dataTables_paginate .paginate_button.current, 
        .dataTables_wrapper .dataTables_paginate .paginate_button.current:hover {
          background: var(--btn-primary-bg) !important;
          color: #ffffff !important;
          border: none !important;
        }

        /* Tabs */
        .nav-tabs {
          border-bottom: 2px solid var(--border-color) !important;
          margin-bottom: 20px !important;
        }
        .nav-tabs > li > a {
          color: var(--text-secondary) !important;
          border-radius: 8px 8px 0 0 !important;
          border: none !important;
          padding: 12px 20px !important;
          font-weight: 500;
          transition: all 0.2s ease;
        }
        .nav-tabs > li > a:hover {
          background-color: rgba(0, 0, 0, 0.02) !important;
          color: var(--text-primary) !important;
        }
        .dark-theme .nav-tabs > li > a:hover {
          background-color: rgba(255, 255, 255, 0.02) !important;
        }
        .nav-tabs > li.active > a, .nav-tabs > li.active > a:focus, .nav-tabs > li.active > a:hover {
          background-color: var(--bg-card) !important;
          color: var(--tab-active-text) !important;
          border: 1px solid var(--border-color) !important;
          border-bottom-color: var(--bg-card) !important;
          font-weight: 600 !important;
        }

        /* visNetwork dark mode integration */
        .vis-network-container, .visNetwork {
          position: relative !important;
          background-color: var(--bg-card) !important;
          border: 1px solid var(--border-color) !important;
          border-radius: 16px !important;
        }
        .vis-network {
          position: relative !important;
          background-color: transparent !important;
          border: none !important;
          border-radius: 0 !important;
        }
        select.vis-network-select-node, select[id^='nodeSelect'] {
          position: absolute !important;
          top: 15px !important;
          left: 15px !important;
          z-index: 100 !important;
          margin-bottom: 0 !important;
        }

        /* visNetwork tooltips styling */
        div.vis-tooltip {
          background: transparent !important;
          background-color: transparent !important;
          border: none !important;
          border-radius: 8px !important;
          padding: 0 !important;
          box-shadow: none !important;
          overflow: visible !important;
        }

        /* Shiny floating notifications / progress */
        .shiny-notification {
          background-color: var(--bg-card) !important;
          border: 1px solid var(--border-color) !important;
          color: var(--text-primary) !important;
          border-radius: 16px !important;
          box-shadow: var(--box-shadow) !important;
          opacity: 0.95 !important;
          padding: 15px 20px !important;
          max-width: 450px !important;
        }
        .progress-bar {
          background: var(--btn-primary-bg) !important;
        }

        /* Waiting Placeholder Screen Styles */
        .waiting-container {
          display: flex;
          justify-content: center;
          align-items: center;
          min-height: 550px;
          background-color: var(--bg-card);
          border-radius: 16px;
          border: 1px solid var(--border-color);
          position: relative;
          overflow: hidden;
          padding: 40px;
        }
        .waiting-overlay {
          text-align: center;
          max-width: 450px;
          padding: 30px;
          background: rgba(0, 0, 0, 0.02);
          backdrop-filter: blur(8px);
          -webkit-backdrop-filter: blur(8px);
          border-radius: 20px;
          border: 1px solid rgba(0, 0, 0, 0.06);
          box-shadow: var(--box-shadow);
        }
        .dark-theme .waiting-overlay {
          background: rgba(255, 255, 255, 0.02);
          border: 1px solid rgba(255, 255, 255, 0.06);
        }
        .waiting-img {
          max-width: 250px;
          height: auto;
          opacity: 0.85;
          margin-bottom: 20px;
          filter: drop-shadow(0 10px 15px rgba(59, 130, 246, 0.15));
          border-radius: 12px;
        }
        .dark-theme .waiting-img {
          filter: drop-shadow(0 10px 15px rgba(99, 102, 241, 0.15));
        }
        .waiting-title {
          font-family: 'Outfit', sans-serif;
          font-weight: 700;
          color: var(--text-primary);
          margin-top: 15px;
          margin-bottom: 10px;
          font-size: 22px;
          letter-spacing: -0.5px;
        }
        .waiting-desc {
          color: var(--text-secondary);
          font-size: 14px;
          line-height: 1.5;
          margin-bottom: 0;
        }

        /* Custom styling for sun and moon icons based on active theme */
        .theme-switch-container .fa-sun {
          color: #f59e0b !important;
          transition: opacity 0.3s ease;
        }
        .theme-switch-container .fa-moon {
          color: #94a3b8 !important;
          transition: all 0.3s ease;
        }
        body.dark-theme .theme-switch-container .fa-moon {
          color: #a5b4fc !important;
        }
        body.dark-theme .theme-switch-container .fa-sun {
          color: #475569 !important;
        }
        .theme-switch-container {
          display: flex !important;
          align-items: center !important;
          gap: 8px !important;
          width: auto !important;
          float: right !important;
        }
        .theme-switch-container .shiny-input-container {
          width: auto !important;
          margin: 0 !important;
          padding: 0 !important;
          display: inline-block !important;
        }
        .theme-switch-container .pretty {
          margin-right: 0 !important;
          padding-top: 2px;
        }

        /* Fix alignment of inline radio buttons when they wrap */
        .shiny-options-group {
          display: flex;
          flex-wrap: wrap;
          gap: 15px;
        }
        .shiny-options-group .radio-inline, 
        .shiny-options-group .checkbox-inline {
          margin-left: 0 !important;
          margin-right: 10px;
          display: flex;
          align-items: center;
          gap: 4px;
        }
        .shiny-options-group .radio-inline + .radio-inline, 
        .shiny-options-group .checkbox-inline + .checkbox-inline {
          margin-left: 0 !important;
        }

        /* Responsive theme switch adjustment */
        @media (max-width: 767px) {
          .theme-switch-container {
            display: none !important;
          }
        }

        /* Scrollable dropdown height for FCA attributes selector (at most 6 items visible) */
        .fca-atributos-selectize .selectize-dropdown-content {
          max-height: 180px !important;
          overflow-y: auto !important;
        }
        
        /* Premium custom tooltip styling for visNetwork Hasse Diagram */
        div.vis-tooltip {
          position: absolute !important;
          background-color: var(--bg-card) !important;
          border: 1px solid var(--border-color) !important;
          border-radius: 16px !important;
          box-shadow: var(--box-shadow) !important;
          color: var(--text-primary) !important;
          font-family: 'Inter', sans-serif !important;
          padding: 12px 16px !important;
          font-size: 13px !important;
          line-height: 1.5 !important;
          max-width: 320px !important;
          word-wrap: break-word !important;
          z-index: 99999 !important;
          pointer-events: none !important;
        }
        .hasse-tooltip strong {
          color: var(--text-secondary) !important;
        }

        /* Estilos premium para la leyenda de la red interactiva (SNA) */
        .sna-legend-card {
          position: absolute;
          right: 20px;
          top: 20px;
          z-index: 999;
          background-color: var(--bg-card);
          border: 1px solid var(--border-color);
          border-radius: 12px;
          padding: 14px 18px;
          box-shadow: var(--box-shadow);
          pointer-events: none;
          transition: all 0.3s cubic-bezier(0.4, 0, 0.2, 1);
          min-width: 190px;
          backdrop-filter: blur(8px);
          -webkit-backdrop-filter: blur(8px);
        }
        .sna-legend-title {
          font-family: 'Outfit', sans-serif;
          font-weight: 700;
          font-size: 13px;
          color: var(--text-primary);
          margin-bottom: 10px;
          border-bottom: 1px solid var(--border-color);
          padding-bottom: 6px;
          letter-spacing: -0.2px;
        }
        .sna-legend-list {
          display: flex;
          flex-direction: column;
          gap: 8px;
        }
        .sna-legend-item {
          display: flex;
          align-items: center;
          gap: 10px;
        }
        .sna-legend-dot {
          width: 12px;
          height: 12px;
          border-radius: 50%;
          display: inline-block;
          flex-shrink: 0;
          box-shadow: 0 2px 4px rgba(0,0,0,0.12);
        }
        .sna-legend-label {
          font-family: 'Inter', sans-serif;
          font-size: 12px;
          font-weight: 500;
          color: var(--text-secondary);
          white-space: nowrap;
        }
      "))
    ),
    tabItems(
      tabItem(
        tabName = "carga",
        fluidRow(
          box(
            width = 12,
            title = "Cargar nuevo conjunto de datos",
            status = "primary",
            solidHeader = TRUE,
            
            # Aviso sobre la estructura requerida (alerta visual)
            tags$div(
              class = "alert alert-warning",
              icon("triangle-exclamation"), 
              tags$strong("Aviso de estructura: "),
              "El dataset debe contener las columnas author, date, url, timestamp, upvotes, downvotes y comment como mínimo.
              Si no se carga ningún archivo, se usarán los datos de prueba por defecto."
            ),
            
            # Nota aclaratoria sobre IA y algoritmos clásicos
            tags$div(
              class = "alert alert-info",
              icon("circle-info"),
              tags$strong("Nota de procesamiento: "),
              "El procesamiento en tiempo real utiliza algoritmos clásicos (SNA, NRC, LDA). El análisis masivo con IA requiere preprocesamiento en servidor y se usará en los datos de demostración."
            ),
            
            fileInput(
              "file_upload", 
              "Selecciona tu dataset (.rds o .csv)", 
              accept = c(".rds", ".csv"),
              buttonLabel = "Explorar...",
              placeholder = "Ningún archivo seleccionado"
            ),
            
            actionButton("btn_load_data", " Procesar y aplicar dataset", icon = icon("play"), class = "btn-success"),
            br(), br(),
            verbatimTextOutput("upload_status")
          )
        )
      ),
      tabItem(
        tabName = "sna",
        fluidRow(
          box(
            width = 12,
            title = "Configuración y Opciones de Red",
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = FALSE,
            fluidRow(
              column(4,
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
                )
              ),
              column(8,
                conditionalPanel(
                  condition = "input.sna_grafico == 'interactive'",
                  fluidRow(
                    column(4, sliderInput("sna_umbral_nodos", "Umbral nodos (grado)", min = 0, max = 100, value = 10, step = 1)),
                    column(4, sliderInput("sna_umbral_aristas", "Umbral aristas (weight)", min = 1, max = 50, value = 1, step = 1)),
                    column(4, 
                      checkboxInput("sna_highlight_roles", "Colorear por roles sociales", value = TRUE),
                      numericInput("sna_seed", "Seed", value = 123, min = 1, step = 1),
                      actionButton("sna_run", "Generar red", class = "btn-primary", style = "margin-top: 10px; width: 100%;")
                    )
                  )
                )
              )
            )
          ),
          box(
            width = 12,
            title = "Resultados del Análisis de Redes Sociales (SNA)",
            status = "primary",
            solidHeader = TRUE,
            uiOutput("sna_results_layout_ui")
          )
        )
      ),
      tabItem(
        tabName = "sentimiento",
        fluidRow(
          box(
            width = 12,
            title = "Opciones del Análisis de Sentimiento",
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = FALSE,
            fluidRow(
              column(8,
                radioButtons(
                  "sent_metodo",
                  "Metodología",
                  choices = c(
                    "Algoritmo clásico (NRC)" = "clasico",
                    "Inteligencia Artificial (LLM)" = "ia",
                    "Comparativa NRC vs IA" = "comparativa"
                  ),
                  selected = "clasico",
                  inline = TRUE
                )
              ),
              column(4,
                selectInput("sent_grafico", "Gráfico", choices = sent_graficos$clasico)
              )
            )
          ),
          box(
            width = 12,
            title = "Visualización de Sentimientos",
            status = "primary",
            solidHeader = TRUE,
            uiOutput("sent_titulo"),
            uiOutput("sent_plot_ui")
          )
        )
      ),
      tabItem(
        tabName = "topic",
        fluidRow(
          box(
            width = 12,
            title = "Opciones de Topic Modeling",
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = FALSE,
            fluidRow(
              column(4,
                radioButtons(
                  "topic_metodo",
                  "Metodología",
                  choices = c(
                    "Algoritmo clásico (LDA)" = "clasico",
                    "Inteligencia Artificial (LLM)" = "ia"
                  ),
                  selected = "clasico",
                  inline = TRUE
                )
              ),
              column(4,
                selectInput("topic_grafico", "Gráfico", choices = topic_graficos$clasico)
              ),
              column(4,
                conditionalPanel(
                  condition = "input.topic_metodo == 'clasico'",
                  uiOutput("topic_controls_ui")
                )
              )
            )
          ),
          uiOutput("topic_names_panel"),
          box(
            width = 12,
            title = "Visualización de Tópicos",
            status = "primary",
            solidHeader = TRUE,
            uiOutput("topic_titulo"),
            uiOutput("topic_plot_ui")
          )
        )
      ),
      tabItem(
        tabName = "fca",
        fluidRow(
          box(
            width = 12,
            title = "Opciones y Parámetros del FCA",
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = FALSE,
            fluidRow(
              column(3,
                radioButtons(
                  "fca_metodo",
                  "Contexto FCA",
                  choices = c(
                    "Algoritmo clásico (NRC / datos clásicos)" = "clasico",
                    "Inteligencia Artificial (IA / datos IA)" = "ia"
                  ),
                  selected = "ia"
                ),
                actionButton("fca_run", "Calcular FCA", class = "btn-primary", style = "width: 100%; margin-top: 10px;")
              ),
              column(9,
                conditionalPanel(
                  condition = "input.fca_tabs == 'conceptos'",
                  fluidRow(
                    column(4, numericInput("fca_min_attributes", "Min atributos por concepto", value = 1, min = 1, step = 1)),
                    column(4, numericInput("fca_min_comments", "Min comentarios (soporte)", value = 0, min = 0, step = 1)),
                    column(4, numericInput("fca_top_n", "Top N conceptos", value = 20, min = 1, step = 1))
                  ),
                  fluidRow(
                    column(8, uiOutput("fca_attr_selector")),
                    column(4, downloadButton("fca_download", "Exportar conceptos", style = "margin-top: 25px; width: 100%;"))
                  )
                ),
                conditionalPanel(
                  condition = "input.fca_tabs == 'atributos'",
                  uiOutput("fca_attr_single")
                ),
                conditionalPanel(
                  condition = "input.fca_tabs == 'ia'",
                  fluidRow(
                    column(4, radioButtons("fca_ia_type", "Interpretar como", choices = c("Concepto" = "concepto", "Implicación" = "implicacion"), selected = "concepto", inline = TRUE)),
                    column(4, uiOutput("fca_ia_item_selector")),
                    column(4, 
                      actionButton("fca_ia_interpret", "Interpretar con IA", class = "btn-success", style = "margin-top: 25px; width: 100%;"),
                      helpText("Se requiere un servidor Ollama local en http://localhost:11434.")
                    )
                  )
                )
              )
            )
          ),
          box(
            width = 12,
            title = "Visualización del Análisis de Conceptos Formales (FCA)",
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
          model = "qwen2.5:3b",
          messages = list(
            list(role = "system", content = "Eres un asistente experto en análisis de redes sociales. Debes responder siempre y obligatoriamente en español (España)."),
            list(role = "user", content = prompt)
          ),
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
  
  # === SECCIÓN 1: VARIABLES REACTIVAS (Almacén central) ===
  rv <- reactiveValues(
    dataset_original = NULL,
    dataset_procesado = NULL,
    grafo_completo = NULL,
    tabla_roles = NULL,
    matriz_sentimientos = NULL,
    modelo_lda = NULL,
    términos_tópicos = NULL,
    gamma_matriz = NULL,
    archivo_cargado = FALSE
  )
  
  # === SECCIÓN 2: FUNCIONES HELPER PARA PIPELINE ===
  limpiar_texto <- function(df, columna_texto = "comment") {
    if (!columna_texto %in% names(df)) {
      stop(paste("La columna", columna_texto, "no existe en el dataset"))
    }
    
    df <- df |>
      dplyr::mutate(
        # 1. Forzamos a que sea texto para evitar errores de stringr
        !!rlang::sym(columna_texto) := as.character(.data[[columna_texto]]),
        
        # 2. Aplicamos limpieza usando .data en lugar de .
        !!rlang::sym(columna_texto) := stringr::str_to_lower(.data[[columna_texto]]),
        !!rlang::sym(columna_texto) := stringr::str_remove_all(.data[[columna_texto]], "http[s]?://\\S+"),
        !!rlang::sym(columna_texto) := stringr::str_remove_all(.data[[columna_texto]], "[^a-záéíóúñ\\s]"),
        !!rlang::sym(columna_texto) := stringr::str_squish(.data[[columna_texto]])
      ) |>
      # Filtramos evitando los NAs
      dplyr::filter(!is.na(.data[[columna_texto]]) & .data[[columna_texto]] != "")
    
    return(df)
  }
  
  calcular_sna_metricas <- function(df, col_from = "from", col_to = "to") {
    df_edges <- df |>
      dplyr::count(!!rlang::sym(col_from), !!rlang::sym(col_to), name = "weight")
    g <- tidygraph::as_tbl_graph(df_edges, directed = TRUE) |>
      tidygraph::activate("nodes") |>
      dplyr::mutate(
        degree = tidygraph::centrality_degree(mode = "all"),
        betweenness = tidygraph::centrality_betweenness(directed = TRUE),
        closeness = tidygraph::centrality_closeness(mode = "all"),
        pagerank = tidygraph::centrality_pagerank(),
        eigenvector = tidygraph::centrality_eigen()
      )
    return(g)
  }
  
  analizar_sentimientos_nrc <- function(df, columna_texto = "comment") {
    emociones_df <- syuzhet::get_nrc_sentiment(as.character(df[[columna_texto]]))
    
    if (nrow(emociones_df) > 0) {
      emociones_df$valencia <- emociones_df$positive - emociones_df$negative
      # Obtener el índice de la emoción dominante (de las primeras 8 columnas) de forma segura y vectorizada
      indices_max <- max.col(emociones_df[, 1:8], ties.method = "first")
      emociones_df$emocion_dominante <- colnames(emociones_df[, 1:8])[indices_max]
    } else {
      emociones_df$valencia <- numeric(0)
      emociones_df$emocion_dominante <- character(0)
    }
    
    return(emociones_df)
  }
  
  entrenar_modelo_lda <- function(df, columna_texto = "comment", num_topics = 5) {
    corpus <- quanteda::corpus(df[[columna_texto]])
    quanteda::docnames(corpus) <- df$comment_id
    
    toks <- quanteda::tokens(corpus, remove_punct = TRUE, remove_symbols = TRUE, remove_numbers = TRUE, remove_url = TRUE) |>
      quanteda::tokens_remove(pattern = c(quanteda::stopwords("spanish"), quanteda::stopwords("english"))) |>
      quanteda::tokens_wordstem()
    
    dfm_obj <- quanteda::dfm(toks)
    
    # Aplicar dfm_trim de forma segura para evitar vaciar la matriz DFM (especialmente en archivos de prueba pequeños)
    if (quanteda::ndoc(dfm_obj) > 0 && quanteda::nfeat(dfm_obj) > 0) {
      dfm_trimmed <- tryCatch({
        quanteda::dfm_trim(dfm_obj, min_termfreq = 2, termfreq_type = "count", min_docfreq = 0.01, max_docfreq = 0.9, docfreq_type = "prop")
      }, error = function(e) {
        dfm_obj
      })
      
      # Si la matriz podada no quedó vacía, la adoptamos. Si no, conservamos la original
      if (quanteda::nfeat(dfm_trimmed) > 0 && quanteda::ndoc(dfm_trimmed) > 0) {
        dfm_obj <- dfm_trimmed
      }
    }
    
    # Eliminar documentos vacíos de dfm_obj para evitar que la dtm tenga filas vacías y prevenir el error 'x debe ser un array'
    row_sums <- rowSums(dfm_obj)
    if (any(row_sums == 0)) {
      dfm_obj <- dfm_obj[row_sums > 0, ]
    }
    
    # Validar dimensiones de la matriz antes de la conversión a topicmodels para evitar el error 'x debe ser un array'
    if (quanteda::ndoc(dfm_obj) < 2 || quanteda::nfeat(dfm_obj) < 2) {
      stop("No hay suficientes términos o comentarios con contenido válido para entrenar el modelo de tópicos (LDA). Se requieren al menos 2 comentarios y 2 términos distintos.")
    }
    
    dtm <- quanteda::convert(dfm_obj, to = "topicmodels")
    
    modelo_lda <- topicmodels::LDA(dtm, k = num_topics, method = "Gibbs",
                                   control = list(iter = 100, burnin = 50, thin = 10, seed = 123))
    terminos_por_tema <- topicmodels::terms(modelo_lda, k = 10)
    gamma_matriz <- topicmodels::posterior(modelo_lda)$topics
    return(list(
      modelo = modelo_lda,
      terminos = terminos_por_tema,
      gamma = gamma_matriz
    ))
  }
  
  plot_comunidades <- function(graph, comunidad_col, titulo, seed = 123, light = FALSE) {
    set.seed(seed)
    umbral_puente <- graph |> 
      tidygraph::activate("nodes") |> 
      dplyr::as_tibble() |> 
      dplyr::pull(betweenness) |> 
      quantile(0.90, na.rm = TRUE)
      
    graph <- graph |>
      tidygraph::activate("nodes") |>
      dplyr::mutate(etiqueta_broker = ifelse(betweenness >= umbral_puente, name, NA))
    
    q_val <- graph |> 
      tidygraph::convert(tidygraph::to_undirected) |> 
      tidygraph::with_graph(tidygraph::graph_modularity(group = as.factor(!!rlang::sym(comunidad_col))))
    
    node_text_color <- if (light) "#0f172a" else "#cbd5e1"
    bg_rect_color <- if (light) "#ffffff" else "#131a26"
    
    p <- ggraph(graph, layout = "graphopt") + 
      geom_edge_link(alpha = 0.08, color = "grey60") + 
      geom_node_point(aes(color = as.factor(.data[[comunidad_col]]), size = degree)) +
      geom_node_text(aes(label = etiqueta_broker), size = 3.5, na.rm = TRUE, vjust = -1, color = node_text_color, fontface = "bold", check_overlap = TRUE) +
      scale_color_viridis_d(option = "turbo") + 
      theme_void() +
      labs(title = titulo, subtitle = paste("Q:", round(q_val, 3))) +
      theme(
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold", color = node_text_color),
        plot.subtitle = element_text(hjust = 0.5, size = 12, color = (if(light) "#475569" else "#94a3b8")),
        plot.background = element_rect(fill = bg_rect_color, color = NA) 
      ) +
      guides(color = "none", size = guide_legend(title = "Grado"))
      
    return(p)
  }
  
  # === SECCIÓN 3: PIPELINE DE PROCESAMIENTO (EL MOTOR) ===
  observeEvent(input$btn_load_data, {
    req(input$file_upload)
    
    withProgress(message = "Procesando dataset...", value = 0, {
      tryCatch({
        # Paso 1: Carga del dataset
        incProgress(0.1, detail = "Cargando archivo...")
        ext <- tools::file_ext(input$file_upload$name)
        if (ext == "rds") {
          df <- readRDS(input$file_upload$datapath)
        } else if (ext == "csv") {
          # Detectar el delimitador (coma o punto y coma) leyendo la primera línea para evitar lecturas de una sola columna
          primera_linea <- readLines(input$file_upload$datapath, n = 1)
          num_comas <- stringr::str_count(primera_linea, ",")
          num_puntoycomas <- stringr::str_count(primera_linea, ";")
          sep_char <- if (num_puntoycomas > num_comas) ";" else ","
          
          df <- read.csv(input$file_upload$datapath, sep = sep_char, stringsAsFactors = FALSE)
        } else {
          stop("Formato no soportado. Por favor usa .rds o .csv")
        }
        
        # Verificar que es dataframe
        if (!is.data.frame(df) || nrow(df) == 0) {
          stop("El archivo no contiene datos válidos o está vacío.")
        }
        
        # Crear comment_id y score si no existen
        if (!"comment_id" %in% names(df)) {
          df$comment_id <- as.character(seq_len(nrow(df)))
        } else {
          df$comment_id <- as.character(df$comment_id)
        }
        if (!"score" %in% names(df)) {
          if (all(c("upvotes", "downvotes") %in% names(df))) {
            df$score <- df$upvotes - df$downvotes
          } else if ("upvotes" %in% names(df)) {
            df$score <- df$upvotes
          } else {
            df$score <- 1
          }
        }
        
        rv$dataset_original <- df
        
        # Paso 2: Limpieza del texto
        incProgress(0.2, detail = "Limpiando el texto...")
        col_texto <- intersect(names(df), c("comment", "comment_id", "body", "text", "comentario", "contenido"))
        col_texto <- intersect(col_texto, c("comment", "text", "comentario", "body"))[1]
        if (is.na(col_texto) || is.null(col_texto)) {
          char_cols <- names(df)[sapply(df, is.character)]
          if (length(char_cols) == 0) stop("No se encontró ninguna columna de texto en el dataset.")
          col_texto <- char_cols[1]
        }
        
        df_limpio <- limpiar_texto(df, columna_texto = col_texto)
        
        # Validar que al menos algún comentario contenga texto válido después de la limpieza
        if (nrow(df_limpio) == 0) {
          stop("El dataset no contiene ningún comentario con texto válido después de la limpieza. Verifique que la columna de comentarios esté bien identificada y no contenga solo NAs o caracteres vacíos.")
        }
        
        rv$dataset_procesado <- df_limpio
        
        # Paso 3: Cálculo de métricas de red (SNA)
        incProgress(0.4, detail = "Calculando métricas de red...")
        col_from <- intersect(names(df_limpio), c("from", "autor", "author", "user"))[1]
        col_to <- intersect(names(df_limpio), c("to", "destinatario", "menciona", "reply_to"))[1]
        col_parent <- intersect(names(df_limpio), c("parent_id", "reply_to_id", "parent"))[1]
        
        # Si no hay columna de destino directa (to) pero hay parent_id, reconstruimos la red conversacional
        if (is.na(col_to) && !is.na(col_parent) && !is.na(col_from)) {
          mapeo_padres <- df_limpio |>
            dplyr::select(comment_id, !!rlang::sym(col_from)) |>
            dplyr::rename(parent_author = !!rlang::sym(col_from))
          
          df_limpio_with_to <- df_limpio |>
            dplyr::left_join(mapeo_padres, by = c("parent_id" = "comment_id")) |>
            dplyr::rename(to = parent_author)
          
          if ("to" %in% names(df_limpio_with_to)) {
            col_to <- "to"
            df_limpio <- df_limpio_with_to
            rv$dataset_procesado <- df_limpio
          }
        }
        
        df_edges <- data.frame(from = character(0), to = character(0), stringsAsFactors = FALSE)
        
        # 1. Intentar red conversacional directa (reply)
        if (!is.na(col_from) && !is.na(col_to)) {
          df_edges_direct <- df_limpio |>
            dplyr::select(all_of(c(col_from, col_to))) |>
            stats::na.omit()
          if (nrow(df_edges_direct) > 0) {
            names(df_edges_direct) <- c("from", "to")
            df_edges <- df_edges_direct
          }
        }
        
        # 2. Si la red directa está vacía, intentar construir red de co-participación por post/hilo
        if (nrow(df_edges) == 0 && !is.na(col_from)) {
          col_post <- intersect(names(df_limpio), c("post_id", "url", "hilo_id", "thread_id"))[1]
          if (!is.na(col_post)) {
            participaciones <- df_limpio |>
              dplyr::select(!!rlang::sym(col_from), !!rlang::sym(col_post)) |>
              dplyr::filter(!is.na(.data[[col_from]]) & .data[[col_from]] != "" & .data[[col_from]] != "[deleted]") |>
              dplyr::distinct()
            
            if (nrow(participaciones) > 0) {
              edges_co <- participaciones |>
                dplyr::inner_join(participaciones, by = col_post, relationship = "many-to-many") |>
                dplyr::filter(.data[[paste0(col_from, ".x")]] < .data[[paste0(col_from, ".y")]]) |>
                dplyr::select(from = paste0(col_from, ".x"), to = paste0(col_from, ".y"))
              
              if (nrow(edges_co) > 0) {
                df_edges <- edges_co
              }
            }
          }
        }
        
        if (nrow(df_edges) > 0) {
          g <- calcular_sna_metricas(df_edges)
          
          # Clasificar roles dinámicamente y guardar en rv$tabla_roles
          tabla_roles_df <- g |>
            tidygraph::activate("nodes") |>
            dplyr::mutate(
              in_degree = tidygraph::centrality_degree(mode = "in"),
              out_degree = tidygraph::centrality_degree(mode = "out"),
              com_walktrap = tidygraph::group_walktrap()
            ) |>
            dplyr::as_tibble() |>
            dplyr::mutate(
              Rol = dplyr::case_when(
                betweenness > quantile(betweenness, 0.90, na.rm = TRUE) & in_degree < quantile(in_degree, 0.75, na.rm = TRUE) ~ "Broker (Conector)",
                pagerank > quantile(pagerank, 0.90, na.rm = TRUE) ~ "Autoridad (Referencia)",
                out_degree > quantile(out_degree, 0.90, na.rm = TRUE) ~ "Hub (Difusor activo)",
                TRUE ~ "Usuario Regular"
              ),
              comunidad = com_walktrap
            )
          
          rv$tabla_roles <- tabla_roles_df
          
          # Unir la columna de Rol de vuelta al grafo para que visNetwork y ggraph la puedan usar
          g_with_roles <- g |>
            tidygraph::activate("nodes") |>
            dplyr::left_join(dplyr::select(tabla_roles_df, name, Rol), by = "name")
          
          rv$grafo_completo <- g_with_roles
        } else {
          rv$grafo_completo <- NULL
          rv$tabla_roles <- NULL
        }
        
        # Generar roles por defecto si el dataset no tiene estructura de red de interacciones para no bloquear el FCA
        if (is.null(rv$tabla_roles) && !is.na(col_from)) {
          autores_unicos <- unique(df_limpio[[col_from]])
          rv$tabla_roles <- data.frame(
            name = autores_unicos,
            Rol = "Usuario Regular",
            comunidad = 1,
            pagerank = 0,
            betweenness = 0,
            stringsAsFactors = FALSE
          )
        }
        
        # Paso 4: Análisis de sentimiento (NRC)
        incProgress(0.6, detail = "Analizando sentimientos con NRC...")
        sentimientos <- analizar_sentimientos_nrc(df_limpio, columna_texto = col_texto)
        rv$matriz_sentimientos <- sentimientos
        
        # Paso 5: Generación de tópicos dinámicos (LDA)
        incProgress(0.8, detail = "Entrenando modelo de tópicos...")
        num_topics <- max(3, min(10, ceiling(nrow(df_limpio) / 100)))
        lda_results <- entrenar_modelo_lda(df_limpio, columna_texto = col_texto, num_topics = num_topics)
        rv$modelo_lda <- lda_results$modelo
        rv$términos_tópicos <- lda_results$terminos
        rv$gamma_matriz <- lda_results$gamma
        
        # Guardar estado de éxito y ocultar temporalmente la Metodología IA
        rv$archivo_cargado <- TRUE
        
        updateRadioButtons(session, "sent_metodo",
                           choices = c("Algoritmo clásico (NRC)" = "clasico"),
                           selected = "clasico")
        updateRadioButtons(session, "topic_metodo",
                           choices = c("Algoritmo clásico (LDA)" = "clasico"),
                           selected = "clasico")
        updateRadioButtons(session, "fca_metodo",
                           choices = c("Algoritmo clásico (NRC)" = "clasico"),
                           selected = "clasico")
        
        incProgress(1.0, detail = "¡Procesamiento finalizado!")
        output$upload_status <- renderText("¡Dataset procesado y cargado correctamente en tiempo real!")
        showNotification("✓ Procesamiento dinámico completado con éxito. Metodologías IA desactivadas temporalmente para este dataset.", type = "message")
        
      }, error = function(e) {
        rv$archivo_cargado <- FALSE
        output$upload_status <- renderText(paste("Error en el procesamiento del archivo:", conditionMessage(e)))
        showNotification(paste("Error en el procesamiento:", conditionMessage(e)), type = "error", duration = NULL)
        
        # Restaurar todas las opciones de metodología si el archivo falla o se restablece
        updateRadioButtons(session, "sent_metodo",
                           choices = c(
                             "Algoritmo clásico (NRC)" = "clasico",
                             "Inteligencia Artificial (LLM)" = "ia",
                             "Comparativa NRC vs IA" = "comparativa"
                           ),
                           selected = "clasico")
        updateRadioButtons(session, "topic_metodo",
                           choices = c(
                             "Algoritmo clásico (LDA)" = "clasico",
                             "Inteligencia Artificial (LLM)" = "ia"
                           ),
                           selected = "clasico")
        updateRadioButtons(session, "fca_metodo",
                           choices = c(
                             "Algoritmo clásico (NRC / datos clásicos)" = "clasico",
                             "Inteligencia Artificial (IA / datos IA)" = "ia"
                           ),
                           selected = "ia")
      })
    })
  })
  
  observeEvent(input$topic_recalcular, {
    # Si no se ha cargado un dataset dinámico, usaremos el dataset por defecto
    df_limpio <- rv$dataset_procesado
    
    if (is.null(df_limpio)) {
      # Cargar el dataset procesado por defecto según la fuente activa
      ruta_data_texto <- paste0("data/", fuente_activa, "/data_texto_procesado.rds")
      if (file.exists(ruta_data_texto)) {
        df_limpio <- readRDS(ruta_data_texto)
      } else {
        showNotification("No se encontró el archivo de texto procesado por defecto.", type = "error")
        return()
      }
    }
    
    # Identificar la columna de texto a limpiar/usar
    col_texto <- intersect(names(df_limpio), c("comment", "comment_id", "body", "text", "comentario", "contenido"))
    col_texto <- intersect(col_texto, c("comment", "text", "comentario", "body"))[1]
    if (is.na(col_texto) || is.null(col_texto)) {
      char_cols <- names(df_limpio)[sapply(df_limpio, is.character)]
      if (length(char_cols) == 0) {
        showNotification("No se encontró ninguna columna de texto en el dataset.", type = "error")
        return()
      }
      col_texto <- char_cols[1]
    }
    
    # Crear comment_id si no existe
    if (!"comment_id" %in% names(df_limpio)) {
      df_limpio$comment_id <- as.character(seq_len(nrow(df_limpio)))
    } else {
      df_limpio$comment_id <- as.character(df_limpio$comment_id)
    }
    
    # Entrenar LDA
    withProgress(message = "Recalculando tópicos...", value = 0.5, {
      tryCatch({
        num_topics <- if (!is.null(input$topic_k_input)) input$topic_k_input else 5
        lda_results <- entrenar_modelo_lda(df_limpio, columna_texto = col_texto, num_topics = num_topics)
        
        rv$dataset_procesado <- df_limpio
        rv$modelo_lda <- lda_results$modelo
        rv$términos_tópicos <- lda_results$terminos
        rv$gamma_matriz <- lda_results$gamma
        
        # Recalcular sentimientos si no existen para que FCA tenga datos consistentes alineados
        if (is.null(rv$matriz_sentimientos)) {
          sentimientos <- analizar_sentimientos_nrc(df_limpio, columna_texto = col_texto)
          rv$matriz_sentimientos <- sentimientos
        }
        
        # Generar roles por defecto si no existen
        if (is.null(rv$tabla_roles)) {
          col_from <- intersect(names(df_limpio), c("from", "autor", "author", "user"))[1]
          if (!is.na(col_from)) {
            autores_unicos <- unique(df_limpio[[col_from]])
            rv$tabla_roles <- data.frame(
              name = autores_unicos,
              Rol = "Usuario Regular",
              comunidad = 1,
              pagerank = 0,
              betweenness = 0,
              stringsAsFactors = FALSE
            )
          } else {
            autores_unicos <- unique(df_limpio$author)
            if (is.null(autores_unicos)) autores_unicos <- "Usuario"
            rv$tabla_roles <- data.frame(
              name = autores_unicos,
              Rol = "Usuario Regular",
              comunidad = 1,
              pagerank = 0,
              betweenness = 0,
              stringsAsFactors = FALSE
            )
          }
        }
        
        # Activar el flag para usar variables dinámicas
        rv$archivo_cargado <- TRUE
        
        # Desactivar temporalmente metodologías de IA
        updateRadioButtons(session, "sent_metodo",
                           choices = c("Algoritmo clásico (NRC)" = "clasico"),
                           selected = "clasico")
        updateRadioButtons(session, "topic_metodo",
                           choices = c("Algoritmo clásico (LDA)" = "clasico"),
                           selected = "clasico")
        updateRadioButtons(session, "fca_metodo",
                           choices = c("Algoritmo clásico (NRC)" = "clasico"),
                           selected = "clasico")
        
        showNotification(paste("✓ Tópicos recalculados con éxito (K =", num_topics, ")."), type = "message")
      }, error = function(e) {
        showNotification(paste("Error al recalcular tópicos:", conditionMessage(e)), type = "error", duration = NULL)
      })
    })
  })
  
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
      clasico = "Algoritmo clásico (LDA)",
      ia = "Inteligencia Artificial (LLM)",
      comparativa = "Comparativa entre metodologías"
    )
    tags$p(tags$strong(metodo), style = "color: #555; margin-bottom: 12px;")
  })
  output$topic_controls_ui <- renderUI({
    if (isTRUE(rv$archivo_cargado)) {
      tagList(
        sliderInput(
          "topic_k_input",
          "Número de tópicos (K):",
          min = 2,
          max = 15,
          value = if (!is.null(rv$modelo_lda)) rv$modelo_lda@k else 5,
          step = 1,
          width = "100%"
        ),
        actionButton(
          "topic_recalcular",
          " Recalcular tópicos",
          icon = icon("sync"),
          class = "btn-primary",
          style = "width: 100%; margin-top: -10px;"
        )
      )
    } else {
      NULL
    }
  })
  
  output$topic_names_panel <- renderUI({
    # Cargar nombres de temas clásicos
    if (rv$archivo_cargado) {
      req(rv$modelo_lda)
      clasico_temas <- paste0("Topic ", 1:rv$modelo_lda@k)
    } else {
      clasico_temas <- tryCatch(readRDS(paste0("data/", fuente_activa, "/nombres_temas.rds")), error = function(e) paste0("Topic ", 1:6))
    }
    
    # Cargar nombres de temas de IA
    ia_temas <- tryCatch(readRDS(paste0("data/", fuente_activa, "/temas_ia_puro.rds")), error = function(e) NULL)
    
    # Crear la lista de temas clásicos en HTML
    clasico_html <- tags$ul(
      style = "padding-left: 20px; margin: 0; line-height: 1.8; color: var(--text-primary);",
      lapply(seq_along(clasico_temas), function(i) {
        tags$li(
          clasico_temas[i]
        )
      })
    )
    
    # Crear la lista de temas de IA en HTML
    if (!is.null(ia_temas)) {
      ia_html <- tags$ul(
        style = "padding-left: 20px; margin: 0; line-height: 1.8; color: var(--text-primary);",
        lapply(seq_along(ia_temas), function(i) {
          tags$li(
            ia_temas[i]
          )
        })
      )
    } else {
      ia_html <- tags$p("No hay temas de IA precalculados disponibles.", style = "color: var(--text-secondary);")
    }
    
    # Si el usuario cargó su propio archivo, el modelo de IA no se aplica
    if (rv$archivo_cargado) {
      ia_section <- tagList(
        tags$h5(tags$strong("Temas IA (LLM / Ollama):"), style = "color: var(--text-secondary); margin-top: 0; font-weight: 600;"),
        tags$p("Los temas de IA no están disponibles para datasets cargados por el usuario (solo soportado en los datos de demostración).", style = "font-size: 13px; color: var(--text-muted);")
      )
    } else {
      ia_section <- tagList(
        tags$h5(tags$strong("Temas IA (LLM / Ollama):"), style = "color: var(--text-secondary); margin-top: 0; font-weight: 600;"),
        ia_html
      )
    }
    
    box(
      width = 12,
      title = "📋 Listado de Temas (Tópicos)",
      status = "info",
      solidHeader = TRUE,
      collapsible = TRUE,
      collapsed = TRUE,
      fluidRow(
        column(6,
          tags$h5(tags$strong("Temas Algoritmo Clásico (LDA):"), style = "color: var(--text-secondary); margin-top: 0; font-weight: 600;"),
          clasico_html
        ),
        column(6,
          ia_section
        )
      )
    )
  })

  output$topic_titulo <- renderUI({
    metodo <- switch(
      input$topic_metodo,
      clasico = "Algoritmo clásico (LDA / topicmodels)",
      ia = "Inteligencia Artificial (LLM / Ollama)"
    )
    tags$p(tags$strong(metodo), style = "color: #555; margin-bottom: 12px;")
  })
  
  # --- Renderizado dinámico o estático (Fallback) de gráficos ---
  output$sent_plot_ui <- renderUI({
    if (!is.null(input$file_upload) && !rv$archivo_cargado) {
      return(esperando_resultados_ui("Esperando a procesar el dataset cargado..."))
    }
    plotOutput("sent_plot", height = 650)
  })

  output$sent_plot <- renderPlot({
    req(input$sent_grafico)
    
    is_light <- !isTRUE(input$theme_switch)
    
    if (rv$archivo_cargado) {
      # Combinamos el dataset procesado con su matriz de sentimientos
      df_sent <- cbind(rv$dataset_procesado, rv$matriz_sentimientos)
      
      # Generar el gráfico dinámico seleccionado
      if (input$sent_grafico %in% c("sa_plot_relacion_sentimiento_popularidad.png", "sa_plot_relacion_sentimiento_popularidad_ia.png")) {
        # 1. Sentimiento vs popularidad
        ggplot(df_sent, aes(x = valencia, y = score)) +
          scale_y_log10(labels = scales::comma) +
          geom_jitter(alpha = 0.3, color = (if(is_light) "navy" else "#60a5fa"), width = 0.2) +
          geom_smooth(method = "gam", color = "darkorange", size = 1) +
          labs(
            title = "Relación entre Sentimiento Neto y Popularidad",
            subtitle = "Uso de escala logarítmica para el Score (Votos)",
            x = "Valencia (Negativo <---> Positivo)",
            y = "Score (Escala Log10)"
          ) +
          obtener_tema_plot(is_light)
      } else if (input$sent_grafico %in% c("sa_plot_evo_temp.png", "sa_grafico_temporal_ia.png")) {
        # 2. Evolución temporal con media móvil
        df_temp <- df_sent |>
          dplyr::mutate(fecha = as.Date(date)) |>
          dplyr::group_by(fecha) |>
          dplyr::summarise(valencia_diaria = mean(valencia, na.rm = TRUE))
        
        k_val <- min(7, nrow(df_temp))
        if (k_val > 1) {
          df_temp <- df_temp |>
            dplyr::mutate(media_movil = zoo::rollmean(valencia_diaria, k = k_val, fill = NA, align = "right"))
        } else {
          df_temp$media_movil <- df_temp$valencia_diaria
        }
        
        ggplot(df_temp, aes(x = fecha)) +
          geom_line(aes(y = valencia_diaria), alpha = 0.3, color = (if(is_light) "grey40" else "grey60")) +
          geom_line(aes(y = media_movil), color = "firebrick", size = 1) +
          geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5, color = (if(is_light) "black" else "white")) +
          labs(
            title = "Evolución Temporal del Sentimiento",
            subtitle = paste("La línea roja indica la tendencia suavizada (Media Móvil", k_val, "días)"),
            x = "Fecha",
            y = "Valencia Media"
          ) +
          obtener_tema_plot(is_light)
      } else if (input$sent_grafico %in% c("sa_plot_micro_contextos.png", "sa_plot_micro_contextos_ia.png")) {
        # 3. Micro-contextos por hilo
        top_urls <- df_sent |>
          dplyr::count(url, sort = TRUE) |>
          dplyr::slice_head(n = 12) |>
          dplyr::pull(url)
          
        comparativa_hilos <- df_sent |>
          dplyr::filter(url %in% top_urls) |> 
          dplyr::group_by(url) |>
          dplyr::summarise(across(any_of(c("trust", "fear", "joy", "sadness", "anger")), mean, na.rm = TRUE)) |>
          tidyr::pivot_longer(cols = -url, names_to = "emocion", values_to = "media") |>
          dplyr::filter(media != 0)
          
        ggplot(comparativa_hilos, aes(x = media, y = emocion, fill = emocion)) +
          geom_col() +
          facet_wrap(~url, scales = "free_y", labeller = label_wrap_gen(width = 40)) +
          labs(title = "Perfiles emocionales por hilo de discusión") +
          obtener_tema_plot(is_light) +
          theme(legend.position = "none", strip.text = element_text(size = 7, color = (if(is_light) "#0f172a" else "#cbd5e1")))
      } else if (input$sent_grafico %in% c("sa_plot_camara_eco.png", "sa_plot_camara_eco_ia.png")) {
        # 4. Cámaras de eco por comunidad
        roles_df <- if (!is.null(rv$tabla_roles)) rv$tabla_roles else tabla_roles
        data_echo <- roles_df |>
          dplyr::inner_join(df_sent, by = c("name" = "author")) |>
          dplyr::select(name, comunidad, valencia)
          
        ggplot(data_echo, aes(x = as.factor(comunidad), y = valencia, fill = as.factor(comunidad))) +
          geom_violin(alpha = 0.5, color = NA, trim = FALSE) +
          geom_jitter(width = 0.15, alpha = 0.4, size = 1, color = (if(is_light) "midnightblue" else "#60a5fa")) +
          stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "red") +
          labs(
            title = "Análisis de Polarización: Distribución Real del Sentimiento",
            subtitle = "Las concentraciones densas de puntos indican consenso (Cámaras de Eco)",
            x = "Comunidad",
            y = "Valencia (Negativo <---> Positivo)"
          ) +
          obtener_tema_plot(is_light) +
          theme(legend.position = "none")
      } else {
        # Perfil emocional global (para comparativa o correlación general)
        emociones_totales <- df_sent |>
          dplyr::summarise(across(any_of(c("anger", "anticipation", "disgust", "fear", "joy", "sadness", "surprise", "trust")), sum, na.rm = TRUE)) |>
          tidyr::pivot_longer(cols = everything(), names_to = "emocion", values_to = "valor")
        
        ggplot(emociones_totales, aes(x = reorder(emocion, valor), y = valor, fill = emocion)) +
          geom_col(alpha = 0.8, show.legend = FALSE) +
          coord_flip() +
          labs(title = "Perfil Emocional Global (NRC)", x = "Emoción", y = "Total") +
          obtener_tema_plot(is_light) +
          theme(legend.position = "none")
      }
    } else {
      # Fallback: Mostrar imagen estática
      plot_png(input$sent_grafico)
    }
  })
  
  output$topic_plot_ui <- renderUI({
    if (!is.null(input$file_upload) && !rv$archivo_cargado) {
      return(esperando_resultados_ui("Esperando a procesar el dataset cargado..."))
    }
    plotOutput("topic_plot", height = 650)
  })

  output$topic_plot <- renderPlot({
    req(input$topic_grafico)
    
    is_light <- !isTRUE(input$theme_switch)
    
    if (rv$archivo_cargado) {
      modelo_lda <- rv$modelo_lda
      nombres_temas <- paste0("Topic ", 1:modelo_lda@k)
      
      if (input$topic_grafico == "tm_plot_tuning.png") {
        # Optimización de k (tuning simulado rápido)
        tuning_mock <- data.frame(
          k = 2:10,
          metric = c(0.1, 0.2, 0.4, 0.6, 0.8, 0.85, 0.88, 0.89, 0.9)
        )
        ggplot(tuning_mock, aes(x = k, y = metric)) +
          geom_line(color = "#3b82f6", size = 1) + geom_point(size = 2.5, color = "#6366f1") +
          labs(title = "Optimización de K (Tuning) - Estimación Rápida", x = "Número de Tópicos (k)", y = "Métrica de Ajuste") +
          obtener_tema_plot(is_light)
      } else if (input$topic_grafico %in% c("tm_plot_prevalencia_impacto.png", "tm_plot_prevalencia_impacto_ia.png")) {
        # Prevalencia e impacto ponderado
        prevalencia <- tidytext::tidy(modelo_lda, matrix = "gamma") |>
          dplyr::group_by(topic) |>
          dplyr::summarise(promedio_gamma = mean(gamma)) |>
          dplyr::mutate(topic_name = nombres_temas[as.integer(topic)])
          
        impacto_real <- tidytext::tidy(modelo_lda, matrix = "gamma") |>
          dplyr::inner_join(rv$dataset_procesado, by = c("document" = "comment_id")) |>
          dplyr::mutate(score_ponderado = gamma * score,
                        topic_name = nombres_temas[as.integer(topic)]) |>
          dplyr::group_by(topic_name) |>
          dplyr::summarise(impacto_total = sum(score_ponderado, na.rm = TRUE))
          
        prev_imp <- prevalencia |>
          dplyr::inner_join(impacto_real, by = "topic_name") |>
          dplyr::rename(Prevalencia = promedio_gamma, Impacto = impacto_total) |>
          tidyr::pivot_longer(cols = c(Prevalencia, Impacto), names_to = "Metric", values_to = "Value")
          
        ggplot(prev_imp, aes(x = factor(topic_name), y = Value, fill = factor(topic_name))) +
          geom_col(show.legend = FALSE) +
          coord_flip() +
          facet_wrap(~Metric, scales = "free_x", ncol = 1) +
          labs(x = NULL, y = NULL, title = "Prevalencia e Impacto de Tópicos (LDA)") +
          obtener_tema_plot(is_light) +
          theme(strip.text = element_text(face = "bold", color = (if(is_light) "#0f172a" else "#cbd5e1")))
      } else if (input$topic_grafico %in% c("tm_plot_top_terms.png", "tm_plot_top_terms_ia.png")) {
        # Top términos
        top_terms <- tidytext::tidy(modelo_lda, matrix = "beta") |>
          dplyr::group_by(topic) |>
          dplyr::slice_max(beta, n = 10) |> 
          dplyr::ungroup() |>
          dplyr::mutate(
            topic_name = nombres_temas[as.integer(topic)],
            term = tidytext::reorder_within(term, beta, topic_name)
          )
          
        ggplot(top_terms, aes(x = beta, y = term, fill = factor(topic))) +
          geom_col(show.legend = FALSE, alpha = 0.7) +
          facet_wrap(~ topic_name, scales = "free_y", ncol = 2) +
          tidytext::scale_y_reordered() +
          scale_fill_viridis_d(option = "plasma") +
          labs(x = "Probabilidad Beta", y = NULL, title = "Términos más representativos por Tópico (Beta)") +
          obtener_tema_plot(is_light) +
          theme(
            axis.text.y = element_text(size = 7),
            strip.text = element_text(face = "bold", color = (if(is_light) "#0f172a" else "#cbd5e1"))
          )
      } else if (input$topic_grafico %in% c("tm_plot_red_coocurrencia.png", "tm_plot_red_coocurrencia_ia.png")) {
        # Red de coocurrencia
        cor_mat <- cor(rv$gamma_matriz)
        colnames(cor_mat) <- nombres_temas
        rownames(cor_mat) <- nombres_temas
        
        cor_df <- as.data.frame(as.table(cor_mat))
        colnames(cor_df) <- c("item1", "item2", "correlation")
        cor_df <- cor_df |>
          dplyr::filter(as.character(item1) < as.character(item2))
          
        grafo_topic <- igraph::graph_from_data_frame(cor_df, directed = FALSE)
        
        bg_color <- if (is_light) "#ffffff" else "#131a26"
        text_color <- if (is_light) "#0f172a" else "#cbd5e1"
        
        ggraph(grafo_topic, layout = "fr") +
          geom_edge_link(aes(edge_alpha = abs(correlation), edge_width = abs(correlation)), color = (if(is_light) "#3b82f6" else "#6366f1")) +
          geom_node_point(size = 8, color = (if(is_light) "#10b981" else "#00f5d4")) +
          geom_node_text(aes(label = name), repel = TRUE, size = 4.5, fontface = "bold", color = text_color) +
          theme_void() +
          labs(title = "Red de Co-ocurrencia de Tópicos (Correlación)") +
          theme(
            plot.background = element_rect(fill = bg_color, color = NA),
            plot.title = element_text(color = text_color, hjust = 0.5, face = "bold")
          )
      } else {
        plot_png(input$topic_grafico)
      }
    } else {
      # Fallback: Mostrar imagen estática
      plot_png(input$topic_grafico)
    }
  })
  
  # --- SNA (sna.qmd) ---
  grafo_reddit <- reactiveVal(NULL)
  
  load_grafo_reddit <- function() {
    if (!is.null(grafo_reddit())) return(invisible(TRUE))
    
    data_sna <- readRDS(paste0("data/", fuente_activa, "/data_sna.rds"))
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
    if (file.exists(ruta_tabla_roles)) {
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
  
  obtener_grafo_activo <- function() {
    if (rv$archivo_cargado) {
      return(rv$grafo_completo)
    }
    load_grafo_reddit()
    return(grafo_reddit())
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
    
    is_light <- !isTRUE(input$theme_switch)
    text_color <- if (is_light) "#0f172a" else "#cbd5e1"
    bg_color <- if (is_light) "#ffffff" else "#131a26"
    edge_color <- if (is_light) "grey75" else "grey35"
    
    ggraph(layout) +
      geom_edge_link(alpha = 0.12, color = edge_color) +
      geom_node_point(aes(size = .data$degree, color = .data$degree, alpha = 0.8), show.legend = TRUE) +
      geom_node_text(
        aes(label = ifelse(.data$degree > stats::quantile(.data$degree, 0.75), .data$name, NA)),
        repel = TRUE,
        size = 3.5,
        fontface = "bold",
        color = text_color,
        bg.color = (if (is_light) "white" else "black"),
        bg.r = 0.15
      ) +
      scale_color_viridis_c(option = "plasma") +
      scale_size_continuous(range = c(1, 8)) +
      theme_graph() +
      labs(
        title = "Red de interacciones en Reddit",
        subtitle = "Nodos filtrados"
      ) +
      theme(
        plot.background = element_rect(fill = bg_color, color = NA),
        plot.title = element_text(color = text_color, hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(color = (if (is_light) "#475569" else "#94a3b8"), hjust = 0.5),
        legend.text = element_text(color = text_color),
        legend.title = element_text(color = text_color)
      )
  }
  
  # --- Variables reactivas para el nodo de SNA ---
  selected_sna_node <- reactiveVal(NULL)
  selected_sna_node_explanation <- reactiveVal(NULL)
  
  output$sna_results_layout_ui <- renderUI({
    if (identical(input$sna_grafico, "interactive")) {
      fluidRow(
        column(8, uiOutput("sna_plot_ui")),
        column(4, uiOutput("sna_node_details"))
      )
    } else {
      fluidRow(
        column(12, uiOutput("sna_plot_ui"))
      )
    }
  })

  output$sna_plot_ui <- renderUI({
    if (identical(input$sna_grafico, "interactive") && (is.null(input$sna_run) || input$sna_run == 0)) {
      return(esperando_resultados_ui("Esperando a generar la Red Interactiva..."))
    }
    
    if (identical(input$sna_grafico, "interactive")) {
      tags$div(
        style = "position: relative; width: 100%; height: 650px;",
        visNetworkOutput("sna_network", height = 650),
        uiOutput("sna_network_legend_html")
      )
    } else {
      plotOutput("sna_plot_static", height = 650)
    }
  })
  
  output$sna_network_legend_html <- renderUI({
    req(input$sna_highlight_roles)
    
    tags$div(
      class = "sna-legend-card",
      tags$div(
        class = "sna-legend-title",
        "Roles Sociales"
      ),
      tags$div(
        class = "sna-legend-list",
        tags$div(
          class = "sna-legend-item",
          tags$span(class = "sna-legend-dot", style = "background-color: #1f77b4;"),
          tags$span(class = "sna-legend-label", "Broker (Conector)")
        ),
        tags$div(
          class = "sna-legend-item",
          tags$span(class = "sna-legend-dot", style = "background-color: #2ca02c;"),
          tags$span(class = "sna-legend-label", "Autoridad (Referencia)")
        ),
        tags$div(
          class = "sna-legend-item",
          tags$span(class = "sna-legend-dot", style = "background-color: #d62728;"),
          tags$span(class = "sna-legend-label", "Hub (Difusor activo)")
        ),
        tags$div(
          class = "sna-legend-item",
          tags$span(class = "sna-legend-dot", style = "background-color: #9467bd;"),
          tags$span(class = "sna-legend-label", "Usuario Regular")
        )
      )
    )
  })
  
  output$sna_network <- renderVisNetwork({
    req(input$sna_run)
    withProgress(message = "Calculando red...", {
      g0 <- obtener_grafo_activo()
      validate(need(!is.null(g0), "No hay datos de interacciones disponibles para este dataset"))
      
      g1 <- generate_subgraph_advanced(
        umbral_nodos = input$sna_umbral_nodos,
        umbral_aristas = input$sna_umbral_aristas,
        g = g0
      )
      
      nodes <- g1 |> activate("nodes") |> as_tibble()
      validate(need(nrow(nodes) > 0, "El umbral es muy alto. No hay nodos para mostrar con estos filtros."))
      
      if (!"Rol" %in% names(nodes)) nodes$Rol <- NA_character_
      
      color_nodos <- if (isTRUE(input$sna_highlight_roles)) {
        purrr::map_chr(nodes$Rol, role_color)
      } else {
        "#8fb9d4"
      }
      
      nodes <- nodes |>
        mutate(
          id = name,
          label = "",
          title = paste0(
            "<div style='background:var(--bg-card);color:var(--text-primary);padding:10px 14px;margin:-15px;border-radius:8px;font-family:Inter,sans-serif;font-size:13px;line-height:1.5;min-width:180px;'>",
            "<b style='color:var(--text-primary);'>", name, "</b><br>Grado: ", degree, "<br>Betweenness: ", round(betweenness, 3),
            "<br>Pagerank: ", round(pagerank, 4), "<br>Rol: ", ifelse(is.na(Rol), "N/A", as.character(Rol)),
            "</div>"
          ),
          value = pmax(degree, 1),
          color = color_nodos
        )
      
      edges <- g1 |>
        activate("edges") |>
        as_tibble() |>
        mutate(
          from = nodes$id[from],
          to = nodes$id[to],
          title = paste0(
            "<div style='background:var(--bg-card);color:var(--text-primary);padding:10px 14px;margin:-15px;border-radius:8px;font-family:Inter,sans-serif;font-size:13px;line-height:1.5;'>",
            "Peso: ", weight, "<br>De: ", from, "<br>Para: ", to,
            "</div>"
          ),
          width = pmax(weight / max(weight, 1) * 5, 1)
        )
      
      v_net <- visNetwork(nodes, edges, height = "650px") |>
        # 1. SOLUCIÓN AL "BAILE": Forzamos la semilla para que los nodos siempre salgan en el mismo sitio
        visIgraphLayout(layout = "layout_with_fr", randomSeed = input$sna_seed) |> 
        visNodes(shadow = list(enabled = TRUE, size = 20), scaling = list(min = 30, max = 80)) |>
        visEdges(arrows = "to", smooth = FALSE, color = list(highlight = "#FF7034")) |>
        visOptions(highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE), nodesIdSelection = list(enabled = TRUE, useLabels = TRUE)) |>
        visInteraction(navigationButtons = TRUE, zoomView = TRUE, dragNodes = TRUE) |>
        # 2. SOLUCIÓN A LA IA: htmlwidgets::JS() obliga al navegador a reconocer el evento del clic
        visEvents(selectNode = htmlwidgets::JS("function(event) { if (event.nodes && event.nodes.length) { Shiny.setInputValue('sna_node', event.nodes[0], {priority:'event'}); } }"))

      # Leyenda interna deshabilitada para usar una versión HTML limpia, alineada y responsive
      v_net
    })
  })
  
  output$sna_plot_static <- renderPlot({
    req(input$sna_grafico)
    
    is_light <- !isTRUE(input$theme_switch)
    bg_color <- if (is_light) "#ffffff" else "#131a26"
    text_color <- if (is_light) "#0f172a" else "#cbd5e1"
    
    if (rv$archivo_cargado) {
      g0 <- rv$grafo_completo
      validate(need(!is.null(g0), "No se han calculado métricas de red para este dataset."))
      
      # Generar el gráfico seleccionado
      if (input$sna_grafico == "sna_grafo_limpio.png") {
        # 1. Red de interacciones
        g1 <- generate_subgraph_advanced(
          umbral_nodos = input$sna_umbral_nodos,
          umbral_aristas = input$sna_umbral_aristas,
          g = g0
        )
        plot_graph(seed = input$sna_seed, grafo = g1)
        
      } else if (input$sna_grafico == "sna_distribucion_grado.png") {
        # 2. Distribución de grado
        df_grados <- g0 |>
          tidygraph::activate("nodes") |>
          dplyr::as_tibble() |>
          dplyr::select(grado = degree)
        mediana_grado <- median(df_grados$grado)
        media_grado   <- mean(df_grados$grado)
        
        ggplot(df_grados, aes(x = grado)) +
          geom_density(fill = "steelblue", alpha = 0.5, color = "darkblue", linewidth = 1) +
          geom_vline(aes(xintercept = mediana_grado, linetype = "Mediana"), color = "red", linewidth = 1) +
          geom_vline(aes(xintercept = media_grado, linetype = "Media"), color = "orange", linewidth = 1) +
          scale_linetype_manual(name = "Estadísticos", values = c("Mediana" = "dashed", "Media" = "dotted")) +
          scale_x_log10(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
          labs(
            title = "Distribución de Grado de la Red",
            subtitle = paste("Mediana =", mediana_grado, "| Media =", round(media_grado, 2), "| Max =", max(df_grados$grado)),
            x = "Grado (Escala Logarítmica)",
            y = "Densidad"
          ) +
          obtener_tema_plot(is_light)
          
      } else if (input$sna_grafico == "sna_distribucion_metrica.png") {
        # 3. Distribución de métricas
        df_metrics_long <- g0 |>
          tidygraph::activate("nodes") |>
          dplyr::as_tibble() |>
          dplyr::select(any_of(c("degree", "betweenness", "closeness", "pagerank"))) |>
          dplyr::rename(Grado = degree, Intermediacion = betweenness, Cercania = closeness, PageRank = pagerank) |>
          tidyr::pivot_longer(cols = everything(), names_to = "Metrica", values_to = "Valor")
          
        ggplot(df_metrics_long, aes(x = Valor, fill = Metrica)) +
          geom_histogram(bins = 30, show.legend = FALSE) +
          facet_wrap(~Metrica, scales = "free") +
          labs(title = "Distribución de Métricas de Centralidad",
               subtitle = "Análisis de la estructura de influencia en la red") +
          obtener_tema_plot(is_light)
               
      } else if (input$sna_grafico == "sna_louvain.png") {
        # 4. Comunidades (Louvain)
        g_undir <- g0 |>
          tidygraph::morph(tidygraph::to_undirected) |>
          dplyr::mutate(com_louvain = tidygraph::group_louvain(resolution = 2)) |>
          tidygraph::unmorph() |>
          tidygraph::activate("nodes") |>
          dplyr::mutate(community_louvain = forcats::fct_lump_n(as.factor(com_louvain), n = 4, other_level = "Periferia"))
          
        plot_comunidades(graph = g_undir, comunidad_col = "community_louvain", titulo = "Comunidades (Louvain)", light = is_light)
        
      } else if (input$sna_grafico == "sna_walktrap.png") {
        # 5. Comunidades (Walktrap)
        g_walk <- g0 |>
          tidygraph::activate("nodes") |>
          dplyr::mutate(com_walktrap = tidygraph::group_walktrap()) |>
          dplyr::mutate(community_walktrap = forcats::fct_lump_n(as.factor(com_walktrap), n = 4, other_level = "Periferia"))
          
        plot_comunidades(graph = g_walk, comunidad_col = "community_walktrap", titulo = "Comunidades (Walktrap)", light = is_light)
        
      } else if (input$sna_grafico == "sna_mapa_roles.png") {
        # 6. Mapa de roles
        roles_df <- rv$tabla_roles
        validate(need(!is.null(roles_df), "No se han detectado roles para este dataset."))
        
        ggplot(roles_df |> dplyr::filter(Rol != "Usuario Regular"), 
               aes(x = degree, y = betweenness, color = Rol)) +
          geom_point(aes(size = pagerank), alpha = 0.7) +
          labs(title = "Mapa de Roles Estratégicos en la Red",
               x = "Popularidad (Grado)", y = "Capacidad de Control (Intermediación)") +
          obtener_tema_plot(is_light)
                
      } else if (input$sna_grafico == "sna_ego_red.png") {
        # 7. Red egocéntrica
        roles_df <- rv$tabla_roles
        validate(need(!is.null(roles_df), "No se han detectado roles para este dataset."))
        
        top_broker <- roles_df |> 
          dplyr::filter(Rol == "Broker (Conector)") |> 
          dplyr::arrange(desc(betweenness)) |> 
          dplyr::slice(1) |> 
          dplyr::pull(name)
          
        validate(need(length(top_broker) > 0, "No se encontró ningún nodo Broker en este dataset para centrar la red egocéntrica."))
        
        ego_red <- g0 |>
          tidygraph::convert(tidygraph::to_local_neighborhood, 
                  node = which(tidygraph::.N()$name == top_broker), 
                  order = 1) |>
          tidygraph::activate("nodes") |>
          dplyr::mutate(es_ego = ifelse(name == top_broker, "EGO (Top Broker)", "Contacto (Alter)"))
          
        ggraph(ego_red, layout = "star", center = which(tidygraph::.N()$name == top_broker)) +
          geom_edge_link(alpha = 0.4, color = (if(is_light) "grey70" else "grey30")) +
          geom_node_point(aes(color = es_ego, size = es_ego)) +
          geom_node_text(aes(label = name), repel = TRUE, size = 3.5, color = text_color) +
          scale_size_manual(values = c("EGO (Top Broker)" = 8, "Contacto (Alter)" = 3)) +
          scale_color_manual(values = c("EGO (Top Broker)" = "red", "Contacto (Alter)" = "steelblue")) +
          theme_graph() +
          labs(title = paste("Red Egocéntrica del Top Broker:", top_broker),
               subtitle = "Análisis de la influencia directa en el flujo de información") +
          theme(
            plot.background = element_rect(fill = bg_color, color = NA),
            plot.title = element_text(color = text_color, hjust = 0.5, face = "bold"),
            plot.subtitle = element_text(color = (if (is_light) "#475569" else "#94a3b8"), hjust = 0.5),
            legend.position = "bottom",
            legend.text = element_text(color = text_color),
            legend.title = element_text(color = text_color)
          )
          
      } else {
        plot_png(input$sna_grafico)
      }
    } else {
      # Fallback: Mostrar imagen estática
      plot_png(input$sna_grafico)
    }
  })
  
  # Panel inteligente con detalles del nodo y explicación IA
  # 1. Al hacer clic en un nodo: Solo guardamos sus datos y limpiamos la IA anterior
  observeEvent(input$sna_node, {
    req(input$sna_node)
    g0 <- obtener_grafo_activo()
    node_name <- input$sna_node
    node_data <- g0 |>
      activate("nodes") |>
      as_tibble() |>
      filter(name == node_name) |>
      slice(1)
    
    validate(need(nrow(node_data) == 1, "Nodo no encontrado."))
    
    selected_sna_node(node_data)
    selected_sna_node_explanation(NULL) # Reseteamos la explicación para que salga el botón
  }, ignoreNULL = TRUE)
  
  # 2. Renderizado de la tarjeta: Muestra las métricas y el botón (o la respuesta si ya la hay)
  output$sna_node_details <- renderUI({
    node <- selected_sna_node()
    explanation <- selected_sna_node_explanation()
    
    if (is.null(node)) {
      return(helpText("Haz clic en un nodo para ver sus métricas detalladas."))
    }
    
    metricas_html <- paste0(
      "<b>Métricas de red:</b><br>",
      "• Grado (conexiones): ", node$degree, "<br>",
      "• Betweenness (intermediación): ", round(node$betweenness, 3), "<br>",
      "• Pagerank (influencia): ", round(node$pagerank, 4), "<br>",
      "• Rol en la red: ", ifelse(is.na(node$Rol), "N/A", node$Rol), "<br>"
    )
    
    # Si no hay explicación aún, mostramos el botón. Si la hay, mostramos el texto.
    if (is.null(explanation)) {
      seccion_ia <- tagList(
        br(),
        actionButton("sna_generate_ia_report", " Generar report IA", icon = icon("robot"), class = "btn-success btn-sm")
      )
    } else {
      seccion_ia <- tagList(
        tags$hr(),
        HTML(paste0("<b>Análisis inteligente:</b><br>", explanation))
      )
    }
    
    box(
      title = paste("📊 Nodo:", node$name),
      status = "info",
      solidHeader = TRUE,
      width = NULL,
      HTML(metricas_html),
      seccion_ia
    )
  })
  
  # 3. Al hacer clic en "Generar report IA": Disparamos a Ollama con un prompt estricto
  observeEvent(input$sna_generate_ia_report, {
    node_data <- selected_sna_node()
    req(node_data)
    
    # Mostramos una barra de progreso nativa de Shiny para que el usuario sepa que está cargando
    withProgress(message = "Consultando a Ollama...", value = 0.5, {
      # Prompt ajustado: Le pedimos máxima brevedad (1 o 2 frases) y respuesta obligatoria en español
      prompt <- glue::glue(
        "Eres un experto en análisis de redes sociales e interpretarás los datos obligatoriamente en español (España).\n",
        "Contexto: análisis SNA con métricas topológicas y roles de usuarios.\n",
        "Nodo seleccionado: {node_data$name}\n",
        "Métricas: Grado={node_data$degree}, Intermediación={round(node_data$betweenness,3)}, Pagerank={round(node_data$pagerank,4)}\n",
        "Rol: {ifelse(is.na(node_data$Rol), 'N/A', node_data$Rol)}\n",
        "Explica su importancia estratégica en la red. REGLA ESTRICTA: Tu respuesta debe estar redactada en español, tener como máximo 2 frases, ser extremadamente concisa y no contener palabras en portugués ni ningún otro idioma."
      )
      
      respuesta <- explain_with_ollama(prompt)
      selected_sna_node_explanation(respuesta)
    })
  })
  
  # --- FCA (fca.qmd) ---
  fca_resultados <- reactiveVal(NULL)
  fca_net_positions <- reactiveVal(NULL)
  
  build_matriz_fca <- function() {
    # 1. Cargar los datos y preparar cruces
    if (rv$archivo_cargado) {
      validate(
        need(!is.null(rv$modelo_lda), "No se ha entrenado el modelo LDA para este dataset."),
        need(!is.null(rv$tabla_roles), "No se han calculado los roles y comunidades de red para este dataset."),
        need(!is.null(rv$matriz_sentimientos), "No se han calculado los sentimientos para este dataset.")
      )
      
      modelo_lda <- rv$modelo_lda
      tabla_roles <- rv$tabla_roles
      data_texto <- rv$dataset_procesado
      use_ia_data <- FALSE
      
      data_sentim <- cbind(rv$dataset_procesado, rv$matriz_sentimientos)
      
      nombres_temas_automaticos <- paste0("Topic ", 1:modelo_lda@k)
      nombres_temas <- setNames(nombres_temas_automaticos, 1:modelo_lda@k)
      
      df_temas <- as.data.frame(rv$gamma_matriz)
      colnames(df_temas) <- nombres_temas_automaticos
      df_temas$comment_id <- rownames(rv$gamma_matriz)
      if (is.null(df_temas$comment_id) || all(df_temas$comment_id == "")) {
        df_temas$comment_id <- rv$dataset_procesado$comment_id
      }
    } else {
      modelo_lda <- readRDS(paste0("data/", fuente_activa, "/modelo_lda.rds")) 
      tabla_roles <- tabla_roles
      data_texto <- readRDS(paste0("data/", fuente_activa, "/data_texto_procesado.rds"))
      use_ia_data <- identical(input$fca_metodo, "ia")
      data_sentim <- if (use_ia_data) data_sentim else readRDS(paste0("data/", fuente_activa, "/data_sentim.rds"))
      nombres_temas <- readRDS(paste0("data/", fuente_activa, "/nombres_temas.rds"))
      df_temas <- df_temas
    }
    
    if ("name" %in% names(tabla_roles) && !"Usuario" %in% names(tabla_roles)) tabla_roles$Usuario <- tabla_roles$name
    if ("comunidad" %in% names(tabla_roles) && !"Comunidad" %in% names(tabla_roles)) tabla_roles$Comunidad <- as.character(tabla_roles$comunidad)
    
    df_integrado <- data_sentim |>
      inner_join(df_temas, by = "comment_id") |> 
      left_join(tabla_roles, by = c("author" = "name")) |>
      filter(!is.na(Rol))
    
    # Validar que el conjunto integrado no esté vacío para evitar errores o caídas en los cálculos posteriores
    if (is.null(df_integrado) || nrow(df_integrado) == 0) {
      stop("El conjunto de datos integrado para FCA está vacío. Por favor, asegúrese de que el dataset contiene los mismos identificadores de autor y comentario que los demás análisis.")
    }
    
    # Limitar el tamaño de la matriz FCA para evitar consumo excesivo de memoria y evitar caídas (crashes) de R con datasets grandes
    if (nrow(df_integrado) > 500) {
      df_integrado <- df_integrado |>
        dplyr::arrange(desc(score)) |>
        dplyr::slice_head(n = 500)
    }
    
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
    # fc$clarify() # Comentado para evitar desalineación entre columnas de la matriz y renglones de intensión
    fc$find_concepts()
    fc$find_implications()
    
    soportes <- fc$concepts$support()
    matriz_intensiones <- fc$concepts$intents()
    # Forzar ancho de consola muy grande para que cada regla sea una sola línea
    old_width <- getOption("width")
    options(width = 10000)
    implicaciones_raw <- capture.output(print(fc$implications))
    options(width = old_width)
    
    # Filtrar solo las líneas que empiezan con "Rule N:" y quitar el prefijo
    imp_rule_lines <- grep("^Rule \\d+:", implicaciones_raw, value = TRUE)
    implicaciones_parsed <- sub("^Rule \\d+:\\s*", "", imp_rule_lines)
    implicaciones_parsed <- trimws(implicaciones_parsed)
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
      implicaciones = implicaciones_parsed,
      soportes = soportes,
      matriz = matriz_fca_final,
      df_integrado = df_integrado
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
    if (is.null(fca_resultados())) {
      return(esperando_resultados_ui("Esperando a calcular el FCA..."))
    }
    
    # Tabbed view: Contexto Formal | Atributos | Conceptos | Interpretación IA
    tabsetPanel(
      id = "fca_tabs",
      tabPanel(
        title = "Contexto Formal",
        value = "contexto_formal",
        fluidRow(
          column(12, h4("Contexto Formal")),
          column(12, DT::dataTableOutput("fca_context_table"))
        )
      ),
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
        uiOutput("fca_attr_details"),
        DT::dataTableOutput("fca_attr_concepts_dt") # <-- ¡LO SACAMOS FUERA AQUÍ!
      ),
      tabPanel(
        title = "Conceptos",
        value = "conceptos",
        fluidRow(
          # Left column: Interactive network
          column(8, visNetworkOutput("fca_concepts_net", height = "600px")),
          
          # Right column: Node details panel
          column(4, uiOutput("fca_concept_details_panel"))
        ),
        fluidRow(
          column(12,
            box(
              title = "Conceptos Calculados",
              status = "primary",
              solidHeader = TRUE,
              collapsible = TRUE,
              collapsed = TRUE,
              width = NULL,
              DT::dataTableOutput("fca_concepts_table")
            )
          )
        )
      ),
      tabPanel(
        title = "Interpretación IA",
        value = "ia",
        fluidRow(
          column(12, uiOutput("fca_ia_selected_preview")),
          column(12, htmlOutput("fca_ia_result", container = tags$div))
        )
      )
    )
  })
  
  output$fca_attr_selector <- renderUI({
    res <- fca_resultados()
    if (is.null(res)) return(helpText("Pulsa 'Calcular FCA' para cargar atributos."))
    choices <- res$resumen_atributos$Atributo
    
    tags$div(
      class = "fca-atributos-selectize",
      selectizeInput(
        inputId = "fca_atributos",
        label = "Filtrar por atributos (opcional)",
        choices = choices,
        selected = input$fca_atributos,
        multiple = TRUE,
        options = list(
          placeholder = "Selecciona atributos..."
        )
      )
    )
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
      choices <- fca_displayed_ord()
      if (is.null(choices) || length(choices) == 0) {
        return(helpText("No hay conceptos disponibles con los filtros actuales."))
      }
      selectInput("fca_ia_item", "Seleccionar concepto", choices = setNames(choices, paste0("Concepto ", choices)), selected = choices[1])
    } else {
      imp <- res$implicaciones
      indices <- seq_along(imp)
      sel_attrs <- input$fca_atributos
      if (!is.null(sel_attrs) && length(sel_attrs) > 0) {
        keep <- sapply(imp, function(rule) {
          all(sapply(sel_attrs, function(attr) grepl(attr, rule, fixed = TRUE)))
        })
        imp <- imp[keep]
        indices <- indices[keep]
      }
      if (length(imp) == 0) {
        return(helpText("No se encontraron implicaciones con los atributos seleccionados."))
      }
      selectInput("fca_ia_item", "Seleccionar implicación", choices = setNames(indices, paste0("Implicación ", indices)), selected = indices[1])
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
        "Actúa como un sociólogo computacional y analista experto en dinámicas de redes sociales.",
        "Contexto: Se está analizando una comunidad digital anónima mediante Análisis Formal de Conceptos (FCA).",
        "Los atributos extraídos combinan la topología del usuario en la red (roles e influencia), sus emociones, el sentimiento de sus mensajes y los temas principales de los que habla.",
        "A continuación, tienes los atributos exactos que definen a un grupo específico de usuarios:",
        row$Atributos_Compartidos,
        "Tu tarea: Interpreta qué representa este perfil dentro del ecosistema de la comunidad. ¿Qué rol social o estratégico juegan? ¿Cómo procesan y difunden la información?",
        "REGLAS ESTRICTAS:",
        "1. No asumas un tema de debate específico a menos que lo deduzcas explícitamente de los atributos.",
        "2. Concéntrate en describir el comportamiento del usuario, el efecto de su posición en la red y su estado emocional colectivo.",
        "3. Utiliza terminología sociológica (ej. capital social, cámara de eco, polarización, sesgo).",
        "4. Sé directo, académico y muy conciso (máximo 2 párrafos).",
        "5. Escribe tu respuesta obligatoriamente en español (España).",
        sep = "\n"
      )
    } else {
      idx <- as.integer(item)
      imp <- res$implicaciones
      if (idx < 1 || idx > length(imp)) return("Implicación no encontrada.")
      prompt_text <- paste(
        "Actúa como un sociólogo computacional y analista experto en dinámicas de redes sociales.",
        "Contexto: Se está analizando una comunidad digital usando reglas lógicas extraídas mediante Análisis Formal de Conceptos (FCA).",
        "Interpreta la siguiente implicación lógica de comportamiento masivo (Si un grupo cumple A -> Entonces matemáticamente cumple B):",
        imp[idx],
        "Tu tarea: Explica el significado sociológico y estratégico de esta regla. ¿Por qué la presencia de esos antecedentes desencadena inevitablemente el consecuente en la comunidad?",
        "REGLAS ESTRICTAS:",
        "1. No asumas un tema de debate específico a menos que lo deduzcas explícitamente de los nombres de los atributos.",
        "2. Concéntrate en la relación de causa-efecto entre los roles de la red, las emociones y los temas.",
        "3. Utiliza terminología sociológica (ej. contagio emocional, influencia unidireccional, vulnerabilidad estructural).",
        "4. Sé directo, académico y muy conciso (máximo 2 párrafos).",
        "5. Escribe tu respuesta obligatoriamente en español (España).",
        sep = "\n"
      )
    }
    
    withProgress(message = "Consultando a Ollama...", value = 0.5, {
      tryCatch({
        resp <- request("http://localhost:11434/api/chat") |>
          req_body_json(list(
            model = "qwen2.5:3b",
            messages = list(
              list(role = "system", content = "Actúas como un sociólogo computacional y analista experto. Debes responder siempre y obligatoriamente en español (España)."),
              list(role = "user", content = prompt_text)
            ),
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
  })
  
  # Preview reactivo: muestra el concepto/implicación seleccionado al instante
  output$fca_ia_selected_preview <- renderUI({
    res <- fca_resultados()
    if (is.null(res)) return(helpText("Pulsa 'Calcular FCA' para cargar los datos."))
    
    type <- input$fca_ia_type
    item <- input$fca_ia_item
    if (is.null(item) || item == "") return(helpText("Selecciona un concepto o implicación."))
    
    if (type == "concepto") {
      idx <- as.integer(item)
      row <- res$tabla_conceptos[res$tabla_conceptos$ID_Concepto == idx, ]
      if (nrow(row) == 0) return(helpText("Concepto no encontrado."))
      attrs <- trimws(unlist(strsplit(row$Atributos_Compartidos, ",")))
      texto_formal <- paste0("{", paste(attrs, collapse = ", "), "}")
      tagList(
        h4(paste0("Concepto ", idx, " (", row$Num_Comentarios, " comentarios)")),
        tags$div(
          style = "background: rgba(59, 130, 246, 0.08); border: 1px solid rgba(59, 130, 246, 0.25); border-radius: 16px; padding: 14px 18px; font-family: 'Inter', monospace; font-size: 14px; color: var(--text-primary); margin-bottom: 16px;",
          tags$code(style = "font-size: 14px; word-break: break-word; color: #3b82f6;", texto_formal)
        )
      )
    } else {
      idx <- as.integer(item)
      imp <- res$implicaciones
      if (idx < 1 || idx > length(imp)) return(helpText("Implicación no encontrada."))
      texto_imp <- imp[idx]
      tagList(
        h4(paste0("Implicación ", idx)),
        tags$div(
          style = "background: rgba(59, 130, 246, 0.08); border: 1px solid rgba(59, 130, 246, 0.25); border-radius: 16px; padding: 14px 18px; font-family: 'Inter', monospace; font-size: 14px; color: var(--text-primary); margin-bottom: 16px;",
          tags$code(style = "font-size: 14px; word-break: break-word; color: #3b82f6;", texto_imp)
        )
      )
    }
  })
  
  output$fca_ia_result <- renderUI({
    req(input$fca_ia_interpret)
    result <- interpret_ia()
    if (is.null(result) || result == "") {
      helpText("Pulsa 'Interpretar con IA' para obtener una explicación.")
    } else {
      tagList(
        h4("Interpretación IA"),
        # MAGIA CSS: Obligamos al cajón gris a hacer saltos de línea
        tags$style(HTML("#fca_ia_text { white-space: pre-wrap; word-wrap: break-word; }")),
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
    
    is_light <- !isTRUE(input$theme_switch)
    
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
      geom_col(fill = (if(is_light) "#3b82f6" else "#6366f1"), alpha = 0.8) +
      coord_flip() +
      labs(
        title = "Frecuencia de atributos en el contexto FCA",
        subtitle = paste("Método:", ifelse(input$fca_metodo == "ia", "IA (Ollama)", "Clásico (NRC)")),
        x = NULL,
        y = "Porcentaje (%)"
      ) +
      obtener_tema_plot(is_light)
    
    plotly_config(ggplotly(p, tooltip = "text"))
  })
  output$fca_context_table <- DT::renderDataTable({
    input$fca_run
    res <- fca_resultados()
    validate(need(!is.null(res), "Pulsa 'Calcular FCA' para generar los resultados."))
    
    df_meta <- res$df_integrado
    text_col <- intersect(names(df_meta), c("comment", "body", "text"))[1]
    
    if (!is.na(text_col)) {
      df_meta_clean <- df_meta |>
        dplyr::select(comment_id, author, !!rlang::sym(text_col)) |>
        dplyr::mutate(
          Comentario = stringr::str_trunc(as.character(!!rlang::sym(text_col)), 60)
        ) |>
        dplyr::select(comment_id, author, Comentario)
    } else {
      df_meta_clean <- df_meta |>
        dplyr::select(comment_id, author)
    }
    
    df_meta_clean <- df_meta_clean |>
      dplyr::rename(ID = comment_id, Autor = author)
      
    df_matrix <- as.data.frame(res$matriz)
    df_matrix[] <- lapply(df_matrix, function(col) ifelse(col == 1, "X", ""))
    df_full <- cbind(df_meta_clean, df_matrix)
    
    # Identificar índices de las columnas de atributos (0-indexed en DataTables)
    attr_cols <- seq(from = ncol(df_meta_clean), to = ncol(df_full) - 1)
    
    DT::datatable(
      df_full, 
      options = list(
        pageLength = 5, 
        lengthMenu = list(c(5, 10, 25, 50, -1), c('5', '10', '25', '50', 'Todos')),
        autoWidth = TRUE, 
        scrollX = TRUE,
        columnDefs = list(
          list(className = 'dt-center', targets = attr_cols)
        )
      ), 
      rownames = FALSE
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
    if (length(ord) > topn) {
      bottom_id <- which.max(colSums(intents > 0))
      if (bottom_id %in% ids) {
        ord <- c(setdiff(ord, bottom_id)[1:(topn - 1)], bottom_id)
      } else {
        ord <- ord[1:topn]
      }
    }
    
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
    datatable(df, options = list(pageLength = 5, autoWidth = FALSE), rownames = FALSE)  })
  
  selected_fca_node <- reactiveVal(NULL)
  fca_displayed_ord <- reactiveVal(NULL)
  fca_displayed_edges <- reactiveVal(NULL)
  
  observeEvent(input$fca_node_selected, {
    selected_fca_node(input$fca_node_selected)
  }, ignoreNULL = FALSE)
  
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

if (length(ids) == 0) {

fca_displayed_ord(NULL)

fca_displayed_edges(NULL)

return(NULL)

}

ord <- ids[order(-soportes_reales[ids])]

topn <- as.integer(input$fca_top_n)

if (length(ord) > topn) {
  bottom_id <- which.max(colSums(intents > 0))
  if (bottom_id %in% ids) {
    ord <- c(setdiff(ord, bottom_id)[1:(topn - 1)], bottom_id)
  } else {
    ord <- ord[1:topn]
  }
}


# Nodos con estilo limpio

nodes <- data.frame(

id = seq_along(ord),

title = paste0("Concepto ", ord),

label = paste0("C", ord, " (", soportes_reales[ord], ")"),

value = pmax(1, soportes_reales[ord]),

stringsAsFactors = FALSE

)


# Aristas

sub_intents <- intents[, ord, drop = FALSE]

k <- ncol(sub_intents)

# Calcular el número de atributos (especificidad) para cada concepto
num_atributos <- sapply(seq_along(ord), function(idx) sum(sub_intents[, idx] > 0))
max_attr <- max(num_atributos, 1)

# Generar paleta de colores funcionales (cálido a frío: rojo -> naranja -> verde -> azul)
# A menor número de atributos (más general/alto), más cálido (rojo/naranja)
# A mayor número de atributos (más específico/bajo), más frío (verde/azul)
paleta_bg <- colorRampPalette(c("#ff6b6b", "#feca57", "#1dd1a1", "#54a0ff"))(max_attr + 1)
paleta_border <- colorRampPalette(c("#ee5253", "#ff9f43", "#10ac84", "#2e86de"))(max_attr + 1)

nodes$color.background <- paleta_bg[num_atributos + 1]
nodes$color.border <- paleta_border[num_atributos + 1]
nodes$title <- paste0(
  "<div style='background:var(--bg-card);color:var(--text-primary);padding:10px 14px;margin:-15px;border-radius:8px;font-family:Inter,sans-serif;font-size:13px;line-height:1.5;'>",
  "Concepto ", ord, " (", num_atributos, " atributos, ", soportes_reales[ord], " objetos)",
  "</div>"
)

edges_list <- list()

if (k > 1) {

for (i in 1:(k - 1)) {

for (j in (i + 1):k) {

a <- sub_intents[, i] > 0

b <- sub_intents[, j] > 0

inter <- sum(a & b)

union <- sum(a | b)

sim <- ifelse(union == 0, 0, inter / union)

if (sim > 0.20) {

# Direccionamos la arista del concepto más general al más específico

if (sum(a) < sum(b)) {

edges_list[[length(edges_list) + 1]] <- data.frame(from = i, to = j, width = round(sim * 10, 2))

} else if (sum(a) > sum(b)) {

edges_list[[length(edges_list) + 1]] <- data.frame(from = j, to = i, width = round(sim * 10, 2))

}

}

}

}

}

# Conectar nodos hojas al concepto vacío (bottom_id) si este está presente para evitar que quede aislado
bottom_id <- which.max(colSums(intents > 0))
bottom_idx <- which(ord == bottom_id)
if (length(bottom_idx) > 0 && k > 1) {
  from_nodes <- if (length(edges_list) > 0) sapply(edges_list, function(e) e$from) else integer(0)
  leaf_nodes <- setdiff(seq_len(k), c(from_nodes, bottom_idx))
  for (leaf in leaf_nodes) {
    a <- sub_intents[, leaf] > 0
    b <- sub_intents[, bottom_idx] > 0
    sim <- sum(a & b) / sum(a | b)
    width_val <- if (sim > 0) round(sim * 10, 2) else 1.0
    edges_list[[length(edges_list) + 1]] <- data.frame(from = leaf, to = bottom_idx, width = width_val)
  }
}

edges <- if (length(edges_list) > 0) do.call(rbind, edges_list) else data.frame(from = integer(0), to = integer(0), width = numeric(0))


# Reducción transitiva para limpiar aristas redundantes y mantener el esqueleto jerárquico limpio

if (nrow(edges) > 0 && k > 1) {

temp_vertices <- data.frame(name = as.character(seq_len(k)))

ig_temp <- igraph::graph_from_data_frame(d = edges, vertices = temp_vertices, directed = TRUE)

ig_reduced <- transitive_reduction(ig_temp)

edges <- igraph::as_data_frame(ig_reduced, what = "edges")

if (nrow(edges) > 0) {

edges$from <- as.integer(edges$from)

edges$to <- as.integer(edges$to)

} else {

edges <- data.frame(from = integer(0), to = integer(0), width = numeric(0), stringsAsFactors = FALSE)

}


# Calcular el layout de Sugiyama para posicionamiento inicial jerárquico y libre movimiento posterior

lay <- igraph::layout_with_sugiyama(ig_reduced)

nodes$x <- lay$layout[, 1] * 450 # Espaciar horizontalmente a lo ancho

nodes$y <- -lay$layout[, 2] * 260 # Ir de arriba a abajo e invertir

} else {

# Posicionamiento fallback si no hay aristas

if (k > 0) {

nodes$x <- cos(seq_len(k) * 2 * pi / k) * 200

nodes$y <- sin(seq_len(k) * 2 * pi / k) * 200

}

}


# Actualizar variables reactivas

fca_displayed_ord(ord)

fca_displayed_edges(edges)


# Renderizado final con diseño jerárquico nativo y libre arrastre de nodos

visNetwork(nodes, edges) |>
visNodes(
shape = "dot",
shadow = list(enabled = TRUE, size = 10),
scaling = list(min = 25, max = 65),
color = list(
highlight = list(background = "#FFB703", border = "#F48C06") # Naranja al seleccionar
)) |>
visEdges(
arrows = "to",
smooth = list(type = "cubicBezier", forceDirection = "vertical"), # Curvas suaves hacia abajo
color = list(color = "#C0C0C0", highlight = "#FF7034")
) |>
visInteraction(dragNodes = TRUE, zoomView = TRUE) |>
visPhysics(enabled = FALSE) |>
visOptions(highlightNearest = list(enabled = TRUE, degree = 1), nodesIdSelection = TRUE) |>
visEvents(
selectNode = htmlwidgets::JS("function(event) { if (event.nodes && event.nodes.length) { Shiny.setInputValue('fca_node_selected', event.nodes[0], {priority:'event'}); } }"),
deselectNode = htmlwidgets::JS("function(event) { Shiny.setInputValue('fca_node_selected', null); }")
)
}) 


  
  # Renderizado del panel de detalles del concepto FCA seleccionado
  output$fca_concept_details_panel <- renderUI({
    idx <- selected_fca_node()
    ord <- fca_displayed_ord()
    edges <- fca_displayed_edges()
    res <- fca_resultados()
    
    if (is.null(idx) || is.null(ord) || is.null(res)) {
      return(
        box(
          title = "📊 Detalles del Concepto",
          status = "info",
          solidHeader = TRUE,
          width = NULL,
          helpText("Haz clic en un concepto del retículo para ver sus métricas detalladas y sus relaciones.")
        )
      )
    }
    
    idx <- as.integer(idx)
    if (idx < 1 || idx > length(ord)) {
      return(NULL)
    }
    
    concept_id <- ord[idx]
    
    # 1. Obtener atributos definidores (Intensión)
    intents <- as.matrix(res$concepts_intents)
    nombres_atributos <- colnames(res$matriz)
    atributos_activos <- nombres_atributos[intents[, concept_id] > 0]
    
    # 2. Obtener objetos (comentarios en la Extensión)
    n_comentarios <- nrow(res$matriz)
    soportes_reales <- round(res$soportes * n_comentarios)
    soporte_concepto <- soportes_reales[concept_id]
    
    indices_comentarios <- as.vector(res$fc$concepts$extents()[, concept_id])
    df_meta <- res$df_integrado
    text_col <- intersect(names(df_meta), c("comment", "body", "text"))[1]
    comentarios_concepto <- df_meta[indices_comentarios > 0, ]
    
    # Limitar ejemplos de comentarios
    max_ejemplos <- 5
    ejemplos_html <- if (nrow(comentarios_concepto) > 0) {
      head_comments <- head(comentarios_concepto, max_ejemplos)
      list_items <- lapply(seq_len(nrow(head_comments)), function(r) {
        autor <- head_comments$author[r]
        texto <- head_comments[[text_col]][r]
        texto_corto <- stringr::str_trunc(as.character(texto), 120)
        tags$div(
          style = "padding: 8px 0; border-bottom: 1px solid var(--border-color); font-size: 13px;",
          tags$strong(paste0("u/", autor), style = "color: var(--text-secondary);"),
          tags$p(texto_corto, style = "margin: 4px 0 0 0; color: var(--text-primary);")
        )
      })
      tagList(
        tags$div(
          style = "max-height: 250px; overflow-y: auto; padding-right: 5px;",
          list_items
        ),
        if (nrow(comentarios_concepto) > max_ejemplos) {
          helpText(paste0("... y ", nrow(comentarios_concepto) - max_ejemplos, " comentarios más."))
        }
      )
    } else {
      helpText("No hay comentarios disponibles para este concepto.")
    }
    
    # 3. Vecinos superiores (Padres en el retículo actual)
    upper_ids <- c()
    if (!is.null(edges) && nrow(edges) > 0) {
      upper_ids <- edges$from[edges$to == idx]
    }
    
    upper_html <- if (length(upper_ids) > 0) {
      lapply(upper_ids, function(p_idx) {
        p_concept_id <- ord[p_idx]
        p_soporte <- soportes_reales[p_concept_id]
        p_attrs <- nombres_atributos[intents[, p_concept_id] > 0]
        tags$div(
          style = "padding: 8px; border: 1px solid var(--border-color); border-radius: 8px; margin-bottom: 8px; background: rgba(0, 0, 0, 0.02); font-size: 12px;",
          tags$strong(paste0("Concepto ", p_concept_id, " (C", p_idx, ") (", length(p_attrs), " atributos, ", p_soporte, " objetos)"), style = "color: #38A3A5;"),
          tags$p(style = "margin: 4px 0 0 0; font-family: monospace; color: var(--text-primary); font-size: 11px;", paste(p_attrs, collapse = " + "))
        )
      })
    } else {
      helpText("No tiene vecinos superiores en la red actual (es un concepto maximal/raíz).")
    }
    
    # 4. Vecinos inferiores (Hijos en el retículo actual)
    lower_ids <- c()
    if (!is.null(edges) && nrow(edges) > 0) {
      lower_ids <- edges$to[edges$from == idx]
    }
    
    lower_html <- if (length(lower_ids) > 0) {
      lapply(lower_ids, function(c_idx) {
        c_concept_id <- ord[c_idx]
        c_soporte <- soportes_reales[c_concept_id]
        c_attrs <- nombres_atributos[intents[, c_concept_id] > 0]
        tags$div(
          style = "padding: 8px; border: 1px solid var(--border-color); border-radius: 8px; margin-bottom: 8px; background: rgba(0, 0, 0, 0.02); font-size: 12px;",
          tags$strong(paste0("Concepto ", c_concept_id, " (C", c_idx, ") (", length(c_attrs), " atributos, ", c_soporte, " objetos)"), style = "color: #38A3A5;"),
          tags$p(style = "margin: 4px 0 0 0; font-family: monospace; color: var(--text-primary); font-size: 11px;", paste(c_attrs, collapse = " + "))
        )
      })
    } else {
      helpText("No tiene vecinos inferiores en la red actual (es un concepto minimal/hoja).")
    }
    
    # Renderizar el panel lateral con las 3 secciones desplegables
    tagList(
      # Sección 1: Objetos y Atributos (Desplegable)
      box(
        title = "🏷️ Objetos y Atributos",
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        collapsed = FALSE,
        width = NULL,
        tags$strong(paste0("Atributos (", length(atributos_activos), "):"), style = "font-size: 13px;"),
        tags$div(
          style = "margin: 8px 0 14px 0;",
          if (length(atributos_activos) > 0) {
            lapply(atributos_activos, function(a) {
              tags$span(
                style = "display: inline-block; background: rgba(59, 130, 246, 0.1); color: #3b82f6; border: 1px solid rgba(59, 130, 246, 0.2); border-radius: 8px; padding: 2px 6px; margin: 2px; font-size: 11px; font-weight: bold;",
                a
              )
            })
          } else {
            helpText("Concepto raíz vacío (sin atributos).")
          }
        ),
        tags$hr(style = "margin: 10px 0; border-top: 1px solid var(--border-color);"),
        tags$strong(paste0("Objetos (", soporte_concepto, "):"), style = "font-size: 13px;"),
        ejemplos_html
      ),
      
      # Sección 2: Vecinos Superiores (Padres) (Desplegable, colapsado por defecto)
      box(
        title = "⬆️ Vecinos Superiores (Más Generales)",
        status = "info",
        solidHeader = TRUE,
        collapsible = TRUE,
        collapsed = TRUE,
        width = NULL,
        upper_html
      ),
      
      # Sección 3: Vecinos Inferiores (Hijos) (Desplegable, colapsado por defecto)
      box(
        title = "⬇️ Vecinos Inferiores (Más Específicos)",
        status = "info",
        solidHeader = TRUE,
        collapsible = TRUE,
        collapsed = TRUE,
        width = NULL,
        lower_html
      )
    )
  })
  
  output$fca_attr_details <- renderUI({
    input$fca_run
    attr_sel <- input$fca_attr_single_sel
    res <- fca_resultados()
    
    if (is.null(res) || is.null(attr_sel) || attr_sel == "") {
      return(helpText("Selecciona un atributo en el menú para ver en qué conceptos aparece."))
    }
    
    fila_resumen <- res$resumen_atributos[res$resumen_atributos$Atributo == attr_sel, ]
    
    tagList(
      tags$hr(),
      h4(paste("Conceptos que contienen el atributo:", attr_sel)),
      p(paste0("Aparece en ", fila_resumen$Frecuencia, " comentarios (", fila_resumen$Porcentaje, "% del total).")),
      br()
    )
  })
  
  output$fca_attr_concepts_dt <- DT::renderDataTable({
    input$fca_run
    res <- fca_resultados()
    attr_sel <- input$fca_attr_single_sel
    
    validate(need(!is.null(res), ""))
    if (is.null(attr_sel) || attr_sel == "") return(datatable(data.frame(), options = list(dom = 't')))
    
    intents <- as.matrix(res$concepts_intents)
    nombres_atributos <- colnames(res$matriz)
    attr_idx <- which(nombres_atributos == attr_sel)
    
    if (length(attr_idx) == 0) return(datatable(data.frame(Mensaje = "Atributo no encontrado"), options = list(dom = 't')))
    
    conceptos_idx <- which(intents[attr_idx, ] > 0)
    if (length(conceptos_idx) == 0) return(datatable(data.frame(Mensaje = "Ningún concepto contiene este atributo"), options = list(dom = 't')))
    
    n_comentarios <- nrow(res$matriz)
    soportes_reales <- round(res$soportes * n_comentarios)
    
    # Aplicar los filtros que el usuario ha marcado en la interfaz
    min_attr <- as.integer(input$fca_min_attributes)
    min_comments <- as.integer(input$fca_min_comments)
    
    valid_idx <- conceptos_idx[
      colSums(intents[, conceptos_idx, drop = FALSE] > 0) >= min_attr &
        soportes_reales[conceptos_idx] >= min_comments
    ]
    
    if (length(valid_idx) == 0) {
      return(datatable(data.frame(Mensaje = "Los conceptos con este atributo son demasiado pequeños y no superan tus filtros de 'Min atributos' o 'Min comentarios'."), options = list(dom = 't')))
    }
    
    df <- data.frame(
      ID_Concepto = valid_idx,
      Num_Comentarios = soportes_reales[valid_idx],
      Atributos = apply(intents[, valid_idx, drop = FALSE], 2, function(col) paste(nombres_atributos[col > 0], collapse = ", ")),
      stringsAsFactors = FALSE
    )
    df <- df[order(-df$Num_Comentarios), , drop = FALSE]
    
    # Limitamos a 5 filas para que no rompa la pantalla y desactivamos autoWidth/scrollX para evitar descuadres
    datatable(df, options = list(pageLength = 5, autoWidth = FALSE), rownames = FALSE)
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
      if (length(ord) > topn) {
        bottom_id <- which.max(colSums(intents > 0))
        if (bottom_id %in% ids) {
          ord <- c(setdiff(ord, bottom_id)[1:(topn - 1)], bottom_id)
        } else {
          ord <- ord[1:topn]
        }
      }
      
      lines <- sapply(ord, function(i) {
        atributos_activos <- nombres_atributos[intents[, i] > 0]
        paste0("- Concepto (", soportes_reales[i], " comentarios): ", paste(atributos_activos, collapse = ", "))
      })
      
      writeLines(c("# Exportación de conceptos FCA", "", lines), con = file)
    }
  )
  
  output$download_fca_rds_ui <- renderUI({
    res <- fca_resultados()
    if (is.null(res)) {
      tags$a(
        href = "#",
        id = "download_fca_rds_disabled",
        class = "disabled-download-link",
        icon("save"),
        title = "Calcula el FCA para poder guardar el contexto",
        style = "cursor: not-allowed; opacity: 0.5; color: var(--text-secondary);",
        onclick = "Shiny.setInputValue('download_fca_rds_click_disabled', Math.random(), {priority: 'event'}); return false;"
      )
    } else {
      downloadLink(
        "download_fca_rds",
        label = icon("save"),
        title = "Guardar Contexto (.rds)"
      )
    }
  })
  
  observeEvent(input$download_fca_rds_click_disabled, {
    showNotification("El contexto formal no ha sido calculado. Pulsa 'Calcular FCA' primero.", type = "error")
  })
  
  output$download_fca_rds <- downloadHandler(
    filename = function() {
      paste0("contexto_formal_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds")
    },
    content = function(file) {
      res <- fca_resultados()
      saveRDS(res, file = file)
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