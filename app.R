library(shiny)
library(quantmod)
library(plotly)
library(tidyverse)


################################################################################
###  UI
################################################################################

ui <- fluidPage(
  headerPanel("Compare Forward Returns of Tickers from Yahoo Finance"),
  sidebarPanel(
    textInput('sym1', "Assets", value = "SPY"),
    textInput('sym2', NULL, value = "SPXL"),
    actionButton('update_assets', "Update Assets", icon = icon("money-bill")),
    helpText(
      "Any ticker symbol (eg. GOOG) recognized by",
      a("Yahoo Finance", href="https://finance.yahoo.com/lookup/"),
      "should work. If you get \"Error: subscript out of bounds\", one of",
      "your ticker symbols may be incorrect."
    ),
    hr(),
    numericInput('period', "Holding Period (Years)", 1,
                 min = 0.1, max = 15, step = 0.1),
    radioButtons('ret_type', "Return Metric (Y-Axis)",
      choices = c("Cumulative Log Returns", "Annualized Log Returns",
                  "Cumulative Percent Returns", "Annualized Percent Returns"),
      selected = "Annualized Log Returns"
    ),
    actionButton('update_plot', "Update Plot", icon = icon("chart-line")),
    hr(),
    helpText(
      "Want help, explanation or source code?",
      a("See git repo", href="https://github.com/jmsdao/shiny-forward-returns")
    ),
    width = 3
  ),
  mainPanel(plotlyOutput('plot', height = "600px"))
)

################################################################################
###  Server Functions
################################################################################

server <- function(input, output) {
  # Great explainer of isolate()
  # https://shiny.rstudio.com/articles/isolation.html
  
  
  get_data <- reactive({
    if (input$update_assets == 0) {
      load("test.RData")
    } else {
      sym1 <- toupper(isolate(input$sym1))
      sym2 <- toupper(isolate(input$sym2))
      data1 <- getSymbols(sym1, src = "yahoo", from = "1970-01-01", auto.assign = FALSE)
      data2 <- getSymbols(sym2, src = "yahoo", from = "1970-01-01", auto.assign = FALSE)
    }
    data <- list(data1, data2)
    data
  })

  
  output$plot <- renderPlotly({
    input$update_plot
    
    data <- get_data()
    sym1 <- toupper(isolate(input$sym1))
    sym2 <- toupper(isolate(input$sym2))
    
    trading_days <- min(nrow(data[[1]]), nrow(data[[2]]))
    max_period <- round(trading_days / 252 - 0.2, 1)
    holding_period <- isolate(input$period)
    holding_period <- max(holding_period, 0.1)
    holding_period <- min(holding_period, max_period)
    holding_period <- round(holding_period, 1)
    k.lag <- -as.integer(holding_period * 252)
    
    # Compute pct/log returns for asset 1
    c1 <- data[[1]][, paste0(sym1, ".Close")]
    cf1 <- lag.xts(c1, k.lag)
    
    a1 <- data[[1]][, paste0(sym1, ".Adjusted")]
    af1 <- lag.xts(a1, k.lag)
    
    lr1 <- log(af1 / a1)
    lr1.an <- lr1 / holding_period
    
    pr1 <- (af1 / a1 - 1) * 100
    pr1.an <- ((af1 / a1) ** (1 / holding_period) - 1) * 100
    
    names(c1) <- "Asset1.Close"
    names(cf1) <- "Asset1.Close.Fwd"
    names(a1) <- "Asset1.Adj"
    names(af1) <- "Asset1.Adj.Fwd"
    names(lr1) <- "Asset1.log_ret"
    names(lr1.an) <- "Asset1.log_ret_an"
    names(pr1) <- "Asset1.pct_ret"
    names(pr1.an) <- "Asset1.pct_ret_an"
    
    
    # Compute pct/log returns for asset 2
    c2 <- data[[2]][, paste0(sym2, ".Close")]
    cf2 <- lag.xts(c2, k.lag)
    
    a2 <- data[[2]][, paste0(sym2, ".Adjusted")]
    af2 <- lag.xts(a2, k.lag)
    
    lr2 <- log(af2 / a2)
    lr2.an <- lr2 / holding_period
    
    pr2 <- (af2 / a2 - 1) * 100
    pr2.an <- ((af2 / a2) ** (1 / holding_period) - 1) * 100
    
    names(c2) <- "Asset2.Close"
    names(cf2) <- "Asset2.Close.Fwd"
    names(a2) <- "Asset2.Adj"
    names(af2) <- "Asset2.Adj.Fwd"
    names(lr2) <- "Asset2.log_ret"
    names(lr2.an) <- "Asset2.log_ret_an"
    names(pr2) <- "Asset2.pct_ret"
    names(pr2.an) <- "Asset2.pct_ret_an"
    
    # Decide which metric to plot
    ret_type <- isolate(input$ret_type)
    fmt_yaxis_pct <- FALSE
    if (ret_type == "Cumulative Log Returns") {
      plot1 <- lr1
      plot2 <- lr2
    } else if (ret_type == "Annualized Log Returns") {
      plot1 <- lr1.an
      plot2 <- lr2.an
    } else if (ret_type == "Cumulative Percent Returns") {
      fmt_yaxis_pct <- TRUE
      plot1 <- pr1
      plot2 <- pr2
    } else if (ret_type == "Annualized Percent Returns") {
      fmt_yaxis_pct <- TRUE
      plot1 <- pr1.an
      plot2 <- pr2.an
    }
    names(plot1) <- "Asset1.plot"
    names(plot2) <- "Asset2.plot"
      
    
    # Merge all the timeseries
    ts <- merge.xts(
      c1, cf1, a1, af1, lr1, lr1.an, pr1, pr1.an,
      c2, cf2, a2, af2, lr2, lr2.an, pr2, pr2.an,
      plot1, plot2
    ) %>%
      as.data.frame() %>%
      rownames_to_column("date") %>%
      mutate(date_end = lag.xts(date, k = k.lag)) %>% 
      drop_na()

    # Create extra title info
    win_rate <- mean(ts$Asset1.log_ret_an > ts$Asset2.log_ret_an)
    
    if (win_rate > 0.5) {
      winner <-  sym1
      loser <- sym2
    } else {
      winner <-  sym2
      loser <- sym1
    }
    
    win_rate <- round(max(win_rate, 1 - win_rate) * 100, 1)
    
    title_text <- paste0(
      sprintf("Forward Returns (%s vs. %s)\n", sym1, sym2),
      sprintf("There are %d trading days where both assets could've been bought and held for a period of %.1f years\n", trading_days, holding_period),
      sprintf("%s outperforms %s %.1f%% of the time in the given dataset", winner, loser, win_rate)
    )
    
    # Make plot
    fig <- plot_ly(ts, type = 'scatter', mode = 'lines') %>%
      add_trace(
        x = ~date, y = ~Asset1.plot, name = sym1,
        text = ~paste0(
          "Ticker: ", sym1, "\n\n",
          "Period: ", date, " -> ", date_end, "\n",
          "Close Prices: $", sprintf(Asset1.Close, fmt = '%.2f'),
          " -> $", sprintf(Asset1.Close.Fwd, fmt = '%.2f'), "\n",
          "Adj. Close Prices: $", sprintf(Asset1.Adj, fmt = '%.2f'),
          " -> $", sprintf(Asset1.Adj.Fwd, fmt = '%.2f'), "\n\n",
          "Cumulative Log Return: ", round(Asset1.log_ret, 3), "\n",
          "Annualized Log Return: ", round(Asset1.log_ret_an, 3), "\n",
          "Cumulative Percent Return: ", round(Asset1.pct_ret, 1), "%\n",
          "Annualized Percent Return: ", round(Asset1.pct_ret_an, 1), "%\n"
        ),
        hoverinfo = 'text'
      ) %>%
      add_trace(
        x = ~date, y = ~Asset2.plot, name = sym2,
        text = ~paste0(
          "Ticker: ", sym2, "\n\n",
          "Period: ", date, " -> ", date_end, "\n",
          "Close Prices: $", sprintf(Asset2.Close, fmt = '%.2f'),
          " -> $", sprintf(Asset2.Close.Fwd, fmt = '%.2f'), "\n",
          "Adj. Close Prices: $", sprintf(Asset2.Adj, fmt = '%.2f'),
          " -> $", sprintf(Asset2.Adj.Fwd, fmt = '%.2f'), "\n\n",
          "Cumulative Log Return: ", round(Asset2.log_ret, 3), "\n",
          "Annualized Log Return: ", round(Asset2.log_ret_an, 3), "\n",
          "Cumulative Percent Return: ", round(Asset2.pct_ret, 2), "%\n",
          "Annualized Percent Return: ", round(Asset2.pct_ret_an, 2), "%\n"
        ),
        hoverinfo = 'text'
      ) %>% 
      layout(
        #title = title_text,
        title = list(text = title_text, font = list(size = 12)),
        xaxis = list(title = "Date"),
        yaxis = list(title = ret_type),
        hovermode = "compare"
      )
    
    if (fmt_yaxis_pct) {
      fig <- fig %>% layout(yaxis = list(ticksuffix = "%"))
    }
    
    fig
  })
}

################################################################################
###  Run App
################################################################################

shinyApp(ui = ui, server = server)
