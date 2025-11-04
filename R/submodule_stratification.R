# ===============================================================
# 🧭 Stratification helpers (shared across analysis modules)
# ===============================================================

STRAT_CHOOSE_LABEL <- "Stratify by:"
STRAT_NONE_LABEL <- "None"
STRAT_ORDER_LABEL <- "Order of levels (first = reference):"
MAX_STRATIFICATION_LEVELS <- 10

stratification_ui <- function(id, ns = NULL) {
  ns_fn <- if (is.null(ns)) {
    NS(id)
  } else if (is.function(ns)) {
    function(x) ns(paste(id, x, sep = "-"))
  } else {
    NS(id)
  }
  
  tagList(
    uiOutput(ns_fn("stratify_var_ui")),
    uiOutput(ns_fn("strata_order_ui"))
  )
}

stratification_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # ---- Resolve data ----
    df <- reactive({
      d <- if (is.function(data)) data() else data
      req(is.data.frame(d))
      validate(need(nrow(d) > 0, "No data available for stratification."))
      d
    })
    
    # ---- UI: choose stratification variable ----
    output$stratify_var_ui <- renderUI({
      d <- req(df())
      cat_cols <- names(d)[vapply(d, function(x) is.factor(x) || is.character(x), logical(1))]
      choices <- c(STRAT_NONE_LABEL, cat_cols)
      
      current <- isolate(input$stratify_var)
      if (is.null(current) || !(current %in% choices)) {
        current <- STRAT_NONE_LABEL
      }
      
      selectInput(
        ns("stratify_var"),
        STRAT_CHOOSE_LABEL,
        choices = choices,
        selected = current
      )
    })
    
    # ---- Reactive: details about selected variable and levels ----
    strat_details <- reactive({
      d <- req(df())
      
      strat_var <- input$stratify_var
      if (is.null(strat_var) ||
          identical(strat_var, STRAT_NONE_LABEL) ||
          !nzchar(strat_var) ||
          !(strat_var %in% names(d))) {
        return(list(var = NULL, levels = NULL, available_levels = NULL))
      }
      
      values <- d[[strat_var]]
      values <- values[!is.na(values)]
      available_levels <- if (is.factor(values)) levels(values) else unique(as.character(values))
      available_levels <- available_levels[nzchar(available_levels)]
      
      n_levels <- length(available_levels)
      validate(need(n_levels <= MAX_STRATIFICATION_LEVELS,
                    sprintf("Stratification variable '%s' has too many levels (%d > %d).",
                            strat_var, n_levels, MAX_STRATIFICATION_LEVELS)
      ))
      
      selected_levels <- input$strata_order
      if (!is.null(selected_levels)) {
        selected_levels <- selected_levels[selected_levels %in% available_levels]
      }
      if (is.null(selected_levels) || length(selected_levels) == 0) {
        selected_levels <- available_levels
      }
      
      list(
        var = strat_var,
        levels = selected_levels,
        available_levels = available_levels
      )
    })
    
    # ---- UI: order of levels ----
    output$strata_order_ui <- renderUI({
      details <- strat_details()
      strat_var <- details$var
      # hide the widget silently if stratification is None
      if (is.null(strat_var)) return(NULL)
      
      available_levels <- details$available_levels
      if (is.null(available_levels) || length(available_levels) == 0) return(NULL)
      
      selectInput(
        ns("strata_order"),
        STRAT_ORDER_LABEL,
        choices = available_levels,
        selected = details$levels,
        multiple = TRUE
      )
    })
    
    # ---- Unified reactive output ----
    reactive({
      details <- strat_details()
      list(
        var = details$var,
        levels = details$levels
      )
    })
  })
}
