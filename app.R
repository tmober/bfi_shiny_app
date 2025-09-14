# app.R — Big Five Inventory (BFI) Dataset Exploration App
# -------------------------------------------------------------
# This single-file Shiny app uses the Big Five Inventory (BFI)
# data that ships with the `psych` package. It is designed for
# a psychometrics course: students can explore the data, compute
# reliability, inspect item statistics, and view item-level descriptives.
# -------------------------------------------------------------

# ---- Packages ----
# if (!requireNamespace("shiny", quietly = TRUE)) install.packages("shiny")
# if (!requireNamespace("shinythemes", quietly = TRUE)) install.packages("shinythemes")
# if (!requireNamespace("DT", quietly = TRUE)) install.packages("DT")
# if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
# if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
# if (!requireNamespace("tidyr", quietly = TRUE)) install.packages("tidyr")
# if (!requireNamespace("purrr", quietly = TRUE)) install.packages("purrr")
# if (!requireNamespace("psych", quietly = TRUE)) install.packages("psych")

library(shiny)
library(shinythemes)
library(DT)
library(ggplot2)
library(dplyr)
library(tidyr)
library(purrr)
library(psych)

# ---- Data helpers ----
load_bfi_data <- function() {
  df <- psych::bfi
  names(df) <- gsub("\\.", "_", make.names(names(df)))
  df
}

get_bfi_item_columns <- function(df) {
  grep("^(A|C|E|N|O)[1-9]$", names(df), value = TRUE)
}

get_bfi_scales <- function(df) {
  items <- get_bfi_item_columns(df)
  list(
    Agreeableness   = items[grepl("^A", items)],
    Conscientiousness = items[grepl("^C", items)],
    Extraversion    = items[grepl("^E", items)],
    Neuroticism     = items[grepl("^N", items)],
    Openness        = items[grepl("^O", items)]
  )
}

alpha_safe <- function(df_items) {
  out <- try(psych::alpha(df_items, check.keys = TRUE, warnings = FALSE), silent = TRUE)
  if (inherits(out, "try-error")) return(NULL) else return(out)
}

score_scale <- function(df_items) {
  a <- alpha_safe(df_items)
  if (is.null(a)) return(rep(NA_real_, nrow(df_items)))
  keys <- a$keys
  likert_min <- suppressWarnings(min(df_items, na.rm = TRUE))
  likert_max <- suppressWarnings(max(df_items, na.rm = TRUE))
  df_adj <- df_items
  if (!is.null(keys)) {
    for (nm in names(keys)) {
      if (!is.na(keys[[nm]]) && keys[[nm]] < 0) {
        df_adj[[nm]] <- (likert_max + likert_min) - df_adj[[nm]]
      }
    }
  }
  rowMeans(df_adj, na.rm = TRUE)
}

# ---- UI ----
ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("Big Five Inventory (BFI) Dataset Exploration App"),
  
  sidebarLayout(
    sidebarPanel(width = 3,
                 h4("Data"),
                 p("By default this app uses the BFI dataset included in the ", code("psych"), " package."),
                 fileInput("file", "Optionally upload a CSV to replace the data", accept = c(".csv")),
                 checkboxInput("header", "CSV has header", TRUE),
                 selectInput("sep", "CSV separator", c(Comma = ",", Semicolon = ";", Tab = "\t"), ","),
                 tags$hr(),
                 
                 h4("Filters"),
                 uiOutput("gender_ui"),
                 uiOutput("education_ui"),
                 sliderInput("age_range", "Age range", min = 0, max = 100, value = c(10, 90), step = 1),
                 checkboxInput("complete_cases", "Drop rows with any missing item responses", FALSE),
                 tags$hr(),
                 
                 h4("General"),
                 checkboxInput("zscore_items", "Z-score items before analyses (within current filter)", FALSE),
                 helpText("Tip: Use z-scoring to remove mean-level differences and focus on covariance structure."),
                 tags$hr(),
                 
                 
    ),
    
    mainPanel(width = 9,
              tabsetPanel(id = "tabs", type = "pills",
                          tabPanel("Documentation",
                                   br(),
                                   h3("About the BFI dataset"),
                                   p("The ", code("psych::bfi"), " dataset contains 25 self-report items from the International Personality Item Pool (IPIP), collected via the SAPA web-based project. It includes responses from about 2,800 participants and is commonly used to demonstrate scale construction, factor analysis, and IRT."),
                                   p("Along with the 25 items (A, C, E, N, O; five per trait), three demographics are available: ", strong("gender"), ", ", strong("education"), ", and ", strong("age"), "."),
                                   tags$ul(
                                     tags$li(HTML("<b>gender</b>: 1 = male, 2 = female")),
                                     tags$li(HTML("<b>education</b>: 1 = HS, 2 = finished HS, 3 = some college, 4 = college graduate, 5 = graduate degree")),
                                     tags$li(HTML("<b>age</b>: years"))
                                   ),
                                   br(),
                                   h4("Sources"),
                                   tags$ul(
                                     tags$li(tags$a(href = "https://www.rdocumentation.org/packages/psych/versions/2.5.6/topics/bfi", target = "_blank", "RDocumentation: psych::bfi")),
                                     tags$li(tags$a(href = "https://personality-project.org/r/html/bfi.html", target = "_blank", "Personality Project: bfi help page"))
                                   )
                          ),
                          
                          tabPanel("Item Descriptives",
                                   br(),
                                   DTOutput("item_desc"), br(), textOutput("item_desc_note")
                          ),
                          
                          tabPanel("Reliability",
                                   br(),
                                   fluidRow(
                                     column(5, selectInput("scale", "Select Big Five scale", choices = c("Agreeableness", "Conscientiousness", "Extraversion", "Neuroticism", "Openness")))
                                   ),
                                   verbatimTextOutput("alpha_text"),
                                   DTOutput("alpha_table"),
                                   br(),
                                   h4("All scales"),
                                   DTOutput("alpha_all_scales")
                          ),
                          
                          tabPanel("Correlations & Plots",
                                   br(),
                                   fluidRow(
                                     column(6, plotOutput("corr_heat", height = 400)),
                                     column(6, plotOutput("scale_hist", height = 400))
                                   ),
                                   br(),
                                   h4("Scatterplot matrix of Big Five scale scores"),
                                   p("Each point is a respondent. Upper triangle shows loess-smoothed trends; lower triangle shows Pearson r."),
                                   plotOutput("pairs_scales", height = 520)
                          )
              )
    )
  )
)

# ---- Server ----
server <- function(input, output, session) {
  default_df <- load_bfi_data()
  
  # Panels for pairs()
  panel.cor <- function(x, y, digits = 2, cex.cor = 1.3, ...) {
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r <- suppressWarnings(cor(x, y, use = "pairwise.complete.obs"))
    txt <- formatC(r, format = "f", digits = digits)
    text(0.5, 0.5, txt, cex = cex.cor)
  }
  panel.smooth.loess <- function(x, y, ...) {
    points(x, y, pch = 19, cex = 0.6, col = rgb(0,0,0,0.4))
    ok <- is.finite(x) & is.finite(y)
    if (sum(ok) > 5) {
      ord <- order(x[ok])
      xs <- x[ok][ord]; ys <- y[ok][ord]
      fit <- try(stats::loess(ys ~ xs), silent = TRUE)
      if (!inherits(fit, "try-error")) {
        xs2 <- seq(min(xs), max(xs), length.out = 200)
        ys2 <- predict(fit, newdata = data.frame(xs = xs2))
        lines(xs2, ys2, lwd = 2)
      }
    }
  }
  
  raw_data <- reactive({
    inFile <- input$file
    if (is.null(inFile)) return(default_df)
    read.csv(inFile$datapath, header = input$header, sep = input$sep, stringsAsFactors = FALSE)
  })
  
  output$gender_ui <- renderUI({
    df <- raw_data()
    if ("gender" %in% names(df)) {
      vals <- sort(unique(df$gender))
      selectInput("gender", "Gender", choices = c("All" = "all", vals), selected = "all")
    } else {
      helpText("No 'gender' column detected in data.")
    }
  })
  
  output$education_ui <- renderUI({
    df <- raw_data()
    if ("education" %in% names(df)) {
      vals <- sort(unique(df$education))
      selectInput("education", "Education", choices = c("All" = "all", vals), selected = "all")
    } else {
      helpText("No 'education' column detected in data.")
    }
  })
  
  filtered_data <- reactive({
    df <- raw_data()
    if ("age" %in% names(df)) df$age <- suppressWarnings(as.numeric(df$age))
    if ("gender" %in% names(df) && !is.null(input$gender) && input$gender != "all") {
      df <- df %>% filter(.data$gender == input$gender)
    }
    if ("education" %in% names(df) && !is.null(input$education) && input$education != "all") {
      df <- df %>% filter(.data$education == input$education)
    }
    if ("age" %in% names(df) && !any(is.na(input$age_range))) {
      df <- df %>% filter(is.na(.data$age) | (.data$age >= input$age_range[1] & .data$age <= input$age_range[2]))
    }
    items <- get_bfi_item_columns(df)
    keep <- unique(c(items, intersect(c("gender", "education", "age"), names(df))))
    df <- df[, keep, drop = FALSE]
    if (isTRUE(input$complete_cases) && length(items) > 0) {
      df <- df[stats::complete.cases(df[, items, drop = FALSE]), , drop = FALSE]
    }
    if (isTRUE(input$zscore_items) && length(items) > 0) {
      df[, items] <- scale(df[, items])
    }
    df
  })
  
  output$item_desc <- renderDT({
    df <- filtered_data()
    items <- get_bfi_item_columns(df)
    
    # Keep only BFI items; drop demographics
    df_items <- df[, items, drop = FALSE]
    df_items <- df_items[, setdiff(names(df_items), c("age","gender","education")), drop = FALSE]
    
    # Coerce to numeric and clean invalid values (only allow 1-6, others -> NA)
    for (nm in names(df_items)) {
      x <- df_items[[nm]]
      if (is.factor(x) || is.ordered(x)) x <- as.character(x)
      if (is.character(x)) x <- suppressWarnings(as.numeric(x))
      if (is.logical(x)) x <- as.numeric(x)
      x[!(x %in% 1:6)] <- NA
      df_items[[nm]] <- x
    }
    
    if (ncol(df_items) == 0) return(datatable(data.frame(Note = "No BFI item columns after cleaning.")))
    
    # Simple descriptives without using psych::describe to avoid type/class issues
    stats <- lapply(names(df_items), function(nm) {
      x <- df_items[[nm]]
      c(
        n = sum(!is.na(x)),
        missing = sum(is.na(x)),
        missing_pct = round(100 * mean(is.na(x)), 2),
        mean = round(mean(x, na.rm = TRUE), 3),
        sd = round(stats::sd(x, na.rm = TRUE), 3),
        median = round(stats::median(x, na.rm = TRUE), 3),
        min = suppressWarnings(min(x, na.rm = TRUE)),
        max = suppressWarnings(max(x, na.rm = TRUE))
      )
    })
    
    out <- do.call(rbind, stats)
    out <- data.frame(Item = names(df_items), out, row.names = NULL, check.names = FALSE)
    datatable(out, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  output$item_desc_note <- renderText({
    df <- filtered_data()
    items <- get_bfi_item_columns(df)
    if (length(items) == 0) {
      "No BFI item columns detected in the current dataset or after filtering."
    } else {
      ""
    }
  })
  
  output$alpha_text <- renderPrint({
    df <- filtered_data()
    scales <- get_bfi_scales(df)
    items <- scales[[input$scale]]
    req(length(items) > 0)
    a <- alpha_safe(df[, items])
    if (is.null(a)) return(cat("Alpha could not be computed (insufficient data)."))
    
    # McDonald's omega (total)
    omega_val <- try({
      suppressMessages(suppressWarnings(psych::omega(df[, items], plot = FALSE)$omega.tot))
    }, silent = TRUE)
    if (inherits(omega_val, "try-error") || is.null(omega_val)) omega_val <- NA_real_
    
    cat(sprintf("Cronbach's alpha (raw): %.2f\n", a$total$raw_alpha))
    cat(sprintf("Standardized alpha: %.2f\n", a$total$std.alpha))
    cat(sprintf("Average r (item intercorrelation): %.2f\n", a$total$average_r))
    cat(sprintf("G6(smc): %.2f\n", a$total$G6.smc))
    cat(sprintf("McDonald's omega (total): %.2f\n", omega_val))
  })
  
  
  output$alpha_table <- renderDT({
    df <- filtered_data()
    scales <- get_bfi_scales(df)
    items <- scales[[input$scale]]
    req(length(items) > 0)
    a <- alpha_safe(df[, items])
    if (is.null(a)) return(datatable(data.frame(Note = "Insufficient data for reliability")))
    tab <- data.frame(
      Item = rownames(a$item.stats),
      r_item_total = round(a$item.stats$r.drop, 2),
      alpha_if_deleted = round(a$alpha.drop$raw_alpha, 2),
      check.names = FALSE
    )
    datatable(tab, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  output$alpha_all_scales <- renderDT({
    df <- filtered_data()
    scales <- get_bfi_scales(df)
    tabs <- purrr::imap_dfr(scales, function(items, nm) {
      a <- alpha_safe(df[, items])
      omega_val <- try({
        suppressMessages(suppressWarnings(psych::omega(df[, items], plot = FALSE)$omega.tot))
      }, silent = TRUE)
      if (inherits(omega_val, "try-error") || is.null(omega_val)) omega_val <- NA_real_
      if (is.null(a)) {
        return(data.frame(
          Scale = nm, Items = length(items),
          Raw_Alpha = NA, Std_Alpha = NA, Avg_r = NA, Omega = round(omega_val, 2)
        ))
      }
      data.frame(
        Scale = nm,
        Items = length(items),
        Raw_Alpha = round(a$total$raw_alpha, 2),
        Std_Alpha = round(a$total$std.alpha, 2),
        Avg_r = round(a$total$average_r, 2),
        Omega = round(omega_val, 2)
      )
    })
    datatable(tabs, options = list(pageLength = 5, dom = 'tip'))
  })
  
  
  output$corr_heat <- renderPlot({
    df <- filtered_data()
    items <- get_bfi_item_columns(df)
    req(length(items) > 1)
    R <- suppressWarnings(cor(df[, items], use = "pairwise.complete.obs"))
    Rt <- as.data.frame(as.table(R))
    names(Rt) <- c("Item1", "Item2", "r")
    ggplot(Rt, aes(Item1, Item2, fill = r)) +
      geom_tile() +
      #geom_text(aes(label = sprintf("%.2f", r))) +
      scale_fill_gradient2(limits = c(-1, 1)) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(title = "Item Correlation Heatmap (r)", x = NULL, y = NULL)
  })
  
  output$scale_hist <- renderPlot({
    df <- filtered_data()
    scales <- get_bfi_scales(df)
    scored <- purrr::imap_dfc(scales, function(items, nm) {
      score_scale(df[, items])
    })
    names(scored) <- names(scales)
    long <- scored %>% mutate(row = row_number()) %>% pivot_longer(-row, names_to = "Scale", values_to = "Score")
    ggplot(na.omit(long), aes(x = Score)) +
      geom_histogram(bins = 20) +
      facet_wrap(~ Scale, scales = "free_y") +
      labs(title = "Scale Score Distributions", x = "Mean item score", y = "Count")
  })
  
  # Scatterplot matrix of scale scores (Agreeableness, Conscientiousness, Extraversion, Neuroticism, Openness)
  output$pairs_scales <- renderPlot({
    df <- filtered_data()
    scales <- get_bfi_scales(df)
    # Score each scale using current filters (auto reverse-key via alpha_safe keys)
    scored <- purrr::imap_dfc(scales, function(items, nm) score_scale(df[, items]))
    names(scored) <- names(scales)
    if (ncol(scored) < 2) return(NULL)
    pairs(scored,
          upper.panel = panel.smooth.loess,
          lower.panel = panel.cor,
          diag.panel = function(x) {
            par(new = TRUE)
            hist(x, main = "", xlab = "", ylab = "", col = "gray", border = NA)
          })
  })
  
  output$download_scores <- downloadHandler(
    filename = function() "bfi_scale_scores.csv",
    content = function(file) {
      df <- filtered_data()
      scales <- get_bfi_scales(df)
      scores <- purrr::imap_dfc(scales, function(items, nm) score_scale(df[, items]))
      names(scores) <- names(scales)
      out <- cbind(df[, intersect(c("gender", "education", "age"), names(df)), drop = FALSE], scores)
      write.csv(out, file, row.names = FALSE)
    }
  )
  
  output$download_reliability <- downloadHandler(
    filename = function() "bfi_reliability.csv",
    content = function(file) {
      df <- filtered_data()
      scales <- get_bfi_scales(df)
      tabs <- purrr::imap_dfr(scales, function(items, nm) {
        a <- alpha_safe(df[, items])
        if (is.null(a)) return(data.frame(Scale = nm, Items = length(items), Raw_Alpha = NA, Std_Alpha = NA, Avg_r = NA))
        data.frame(Scale = nm, Items = length(items), Raw_Alpha = a$total$raw_alpha,
                   Std_Alpha = a$total$std.alpha, Avg_r = a$total$average_r)
      })
      write.csv(tabs, file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)


# https://hbctraining.github.io/Training-modules/RShiny/lessons/shinylive.html

# https://tmober.github.io/bfi_shiny_app/
