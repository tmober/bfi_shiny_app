server <- function(input, output, session) {
  default_df <- load_bfi_data()
  
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
      df <- dplyr::filter(df, .data$gender == input$gender)
    }
    if ("education" %in% names(df) && !is.null(input$education) && input$education != "all") {
      df <- dplyr::filter(df, .data$education == input$education)
    }
    if ("age" %in% names(df) && !any(is.na(input$age_range))) {
      df <- dplyr::filter(df, is.na(.data$age) | (.data$age >= input$age_range[1] & .data$age <= input$age_range[2]))
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
  
  # ---------- Item Descriptives ----------
  output$item_desc <- renderDT({
    df <- filtered_data()
    items <- get_bfi_item_columns(df)
    df_items <- df[, items, drop = FALSE]
    df_items <- df_items[, setdiff(names(df_items), c("age","gender","education")), drop = FALSE]
    
    # Coerce to numeric; keep only 1..6, others -> NA
    for (nm in names(df_items)) {
      x <- df_items[[nm]]
      if (is.factor(x) || is.ordered(x)) x <- as.character(x)
      if (is.character(x)) x <- suppressWarnings(as.numeric(x))
      if (is.logical(x)) x <- as.numeric(x)
      x[!(x %in% 1:6)] <- NA
      df_items[[nm]] <- x
    }
    
    if (ncol(df_items) == 0) return(datatable(data.frame(Note = "No BFI item columns after cleaning.")))
    
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
    if (length(items) == 0) "No BFI item columns detected in the current dataset or after filtering." else ""
  })
  
  # ---------- Demographics tab ----------
  label_gender <- function(x) {
    if (is.numeric(x) && all(na.omit(unique(x)) %in% c(1,2))) {
      factor(x, levels = c(1,2), labels = c("Male","Female"))
    } else if (is.character(x) && all(na.omit(unique(x)) %in% c("1","2"))) {
      factor(as.integer(x), levels = c(1,2), labels = c("Male","Female"))
    } else {
      as.factor(x)
    }
  }
  label_education <- function(x) {
    labs <- c("HS","Finished HS","Some college","College graduate","Graduate degree")
    if (is.numeric(x) && all(na.omit(unique(x)) %in% 1:5)) {
      factor(x, levels = 1:5, labels = labs, ordered = TRUE)
    } else if (is.character(x) && all(na.omit(unique(x)) %in% as.character(1:5))) {
      factor(as.integer(x), levels = 1:5, labels = labs, ordered = TRUE)
    } else {
      as.factor(x)
    }
  }
  
  output$freq_gender <- renderDT({
    df <- filtered_data()
    if (!"gender" %in% names(df)) return(datatable(data.frame(Note = "No gender column")))
    g <- label_gender(df$gender)
    tab <- as.data.frame(sort(table(g), decreasing = TRUE))
    names(tab) <- c("Gender","Count")
    tab$Percent <- round(100 * tab$Count / sum(tab$Count), 2)
    datatable(tab, options = list(dom = 't', pageLength = 5))
  })
  
  output$freq_education <- renderDT({
    df <- filtered_data()
    if (!"education" %in% names(df)) return(datatable(data.frame(Note = "No education column")))
    e <- label_education(df$education)
    tab <- as.data.frame(sort(table(e), decreasing = FALSE))
    names(tab) <- c("Education","Count")
    tab$Percent <- round(100 * tab$Count / sum(tab$Count), 2)
    datatable(tab, options = list(dom = 't', pageLength = 5))
  })
  
  output$age_summary <- renderDT({
    df <- filtered_data()
    if (!"age" %in% names(df)) return(datatable(data.frame(Note = "No age column")))
    a <- suppressWarnings(as.numeric(df$age))
    out <- data.frame(
      N = sum(is.finite(a)),
      Missing = sum(!is.finite(a)),
      Mean = round(mean(a, na.rm = TRUE), 2),
      SD = round(stats::sd(a, na.rm = TRUE), 2),
      Median = round(stats::median(a, na.rm = TRUE), 2),
      Q1 = round(stats::quantile(a, 0.25, na.rm = TRUE), 2),
      Q3 = round(stats::quantile(a, 0.75, na.rm = TRUE), 2),
      Min = suppressWarnings(min(a, na.rm = TRUE)),
      Max = suppressWarnings(max(a, na.rm = TRUE))
    )
    datatable(out, options = list(dom = 't'))
  })
  
  output$age_hist <- renderPlot({
    df <- filtered_data()
    if (!"age" %in% names(df)) return(NULL)
    a <- suppressWarnings(as.numeric(df$age))
    ggplot(data.frame(age = a), aes(x = age)) +
      geom_histogram(bins = 30, na.rm = TRUE) +
      labs(x = "Age (years)", y = "Count", title = "Age Distribution")
  })
  
  # ---------- Reliability ----------
  output$alpha_text <- renderPrint({
    df <- filtered_data()
    scales <- get_bfi_scales(df)
    items <- scales[[input$scale]]
    req(length(items) > 0)
    a <- alpha_safe(df[, items])
    if (is.null(a)) return(cat("Alpha could not be computed (insufficient data)."))
    
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
  
  # ---------- Correlations & Plots ----------
  # Heatmap (no numeric overlays)
  output$corr_heat <- renderPlot({
    df <- filtered_data()
    items <- get_bfi_item_columns(df)
    req(length(items) > 1)
    R <- suppressWarnings(cor(df[, items], use = "pairwise.complete.obs"))
    Rt <- as.data.frame(as.table(R))
    names(Rt) <- c("Item1", "Item2", "r")
    ggplot(Rt, aes(Item1, Item2, fill = r)) +
      geom_tile() +
      scale_fill_gradient2(limits = c(-1, 1)) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(title = "Item Correlation Heatmap", x = NULL, y = NULL)
  })
  
  # Histograms of scored scales
  output$scale_hist <- renderPlot({
    df <- filtered_data()
    scales <- get_bfi_scales(df)
    scored <- purrr::imap_dfc(scales, function(items, nm) score_scale(df[, items]))
    names(scored) <- names(scales)
    long <- scored %>% mutate(row = dplyr::row_number()) %>%
      tidyr::pivot_longer(-row, names_to = "Scale", values_to = "Score")
    ggplot(na.omit(long), aes(x = Score)) +
      geom_histogram(bins = 20) +
      facet_wrap(~ Scale, scales = "free_y") +
      labs(title = "Scale Score Distributions", x = "Mean item score", y = "Count")
  })
  
  # Scatterplot matrix of Big Five scale scores (UI id: scatter_matrix)
  output$scatter_matrix <- renderPlot({
    df <- filtered_data()
    scales <- get_bfi_scales(df)
    scored <- purrr::imap_dfc(scales, function(items, nm) score_scale(df[, items]))
    names(scored) <- names(scales)
    if (ncol(scored) < 2) return(NULL)
    pairs(scored)
  })
  
  # Two-item correlation (dropdowns: corr_item_x / corr_item_y)
  observe({
    df <- filtered_data()
    items <- get_bfi_item_columns(df)
    if (length(items) >= 2) {
      updateSelectInput(session, "corr_item_x", choices = items, selected = items[1])
      updateSelectInput(session, "corr_item_y", choices = items, selected = items[2])
    } else if (length(items) == 1) {
      updateSelectInput(session, "corr_item_x", choices = items, selected = items[1])
      updateSelectInput(session, "corr_item_y", choices = items, selected = items[1])
    }
  })
  
  output$two_item_r <- renderPrint({
    req(input$corr_item_x, input$corr_item_y)
    df <- filtered_data()
    x <- df[[input$corr_item_x]]; y <- df[[input$corr_item_y]]
    to_num <- function(v){
      if (is.factor(v) || is.ordered(v)) v <- as.character(v)
      if (is.character(v)) v <- suppressWarnings(as.numeric(v))
      if (is.logical(v)) v <- as.numeric(v)
      v[!(v %in% 1:6)] <- NA
      v
    }
    x <- to_num(x); y <- to_num(y)
    n <- sum(is.finite(x) & is.finite(y))
    if (n < 3) { cat("Not enough paired data to compute a correlation."); return() }
    r <- suppressWarnings(cor(x, y, use = "pairwise.complete.obs"))
    cat(sprintf("Pearson r = %.2f (n = %d)", r, n))
  })
  
  output$two_item_scatter <- renderPlot({
    req(input$corr_item_x, input$corr_item_y)
    df <- filtered_data()
    x <- df[[input$corr_item_x]]; y <- df[[input$corr_item_y]]
    to_num <- function(v){
      if (is.factor(v) || is.ordered(v)) v <- as.character(v)
      if (is.character(v)) v <- suppressWarnings(as.numeric(v))
      if (is.logical(v)) v <- as.numeric(v)
      v[!(v %in% 1:6)] <- NA
      v
    }
    x <- to_num(x); y <- to_num(y)
    d <- data.frame(x = x, y = y)
    d <- d[is.finite(d$x) & is.finite(d$y), , drop = FALSE]
    ggplot(d, aes(x = x, y = y)) +
      geom_point(alpha = 0.5) +
      geom_smooth(method = "lm", se = FALSE) +
      scale_x_continuous(breaks = 1:6, limits = c(1,6)) +
      scale_y_continuous(breaks = 1:6, limits = c(1,6)) +
      labs(x = input$corr_item_x, y = input$corr_item_y, title = "Two-Item Scatterplot")
  })
  
  # Custom item correlation tool (selectors: item_x / item_y)
  observe({
    df <- filtered_data()
    items <- get_bfi_item_columns(df)
    updateSelectInput(session, "item_x", choices = items)
    updateSelectInput(session, "item_y", choices = items)
  })
  
  output$item_scatter <- renderPlot({
    df <- filtered_data()
    req(input$item_x, input$item_y)
    if (input$item_x == input$item_y) return(NULL)
    ggplot(df, aes_string(x = input$item_x, y = input$item_y)) +
      geom_point(alpha = 0.5) +
      geom_smooth(method = "lm", se = FALSE) +
      labs(title = paste("Scatterplot:", input$item_x, "vs", input$item_y))
  })
  
  output$item_corr <- renderPrint({
    df <- filtered_data()
    req(input$item_x, input$item_y)
    if (input$item_x == input$item_y) return("Select two different items.")
    r <- suppressWarnings(cor(df[[input$item_x]], df[[input$item_y]], use = "pairwise.complete.obs"))
    cat(sprintf("Correlation (Pearson r) between %s and %s: %.2f", input$item_x, input$item_y, r))
  })
}


shinyApp(ui, server)
