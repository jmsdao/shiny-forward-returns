library(quantmod)
library(plotly)
library(tidyverse)
#library(ggfortify)

load("test.RData")

sym1 <- "SPY"
sym2 <- "SPXL"

holding_period <- 3
k.lag <- -as.integer(holding_period * 252)


# Compute pct/log returns for asset 1
c1 <- data1[, paste0(sym1, ".Close")]
cf1 <- lag.xts(c1, k.lag)

a1 <- data1[, paste0(sym1, ".Adjusted")]
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
c2 <- data2[, paste0(sym2, ".Close")]
cf2 <- lag.xts(c2, k.lag)

a2 <- data2[, paste0(sym2, ".Adjusted")]
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


# Merge all the timeseries
ts <- merge.xts(
  c1, cf1, a1, af1, lr1, lr1.an, pr1, pr1.an,
  c2, cf2, a2, af2, lr2, lr2.an, pr2, pr2.an
)

# ts2 <- ts %>% as.data.frame() %>% drop_na() %>% as.xts()
ts2 <- ts %>%
  as.data.frame() %>%
  rownames_to_column("date") %>%
  mutate(date_end = lag.xts(date, k = k.lag)) %>% 
  drop_na()
  

################################################################################
# PLOT
################################################################################


plot_ly(ts2, type = 'scatter', mode = 'lines') %>%
  add_trace(
    x = ~date, y = ~Asset1.log_ret_an,
    text = ~paste0(
      "Ticker: ", sym1, "\n\n",
      "Period: ", date, " -> ", date_end, "\n",
      "Close Prices: $", sprintf(Asset1.Close, fmt = '%.2f'),
      " -> $", sprintf(Asset1.Close.Fwd, fmt = '%.2f'), "\n",
      "Adj. Close Prices: $", sprintf(Asset1.Adj, fmt = '%.2f'),
      " -> $", sprintf(Asset1.Adj.Fwd, fmt = '%.2f'), "\n\n",
      "Absolute Log Return: ", round(Asset1.log_ret, 3), "\n",
      "Annualized Log Return: ", round(Asset1.log_ret_an, 3), "\n",
      "Absolute Percent Return: ", round(Asset1.pct_ret, 1), "%\n",
      "Annualized Percent Return: ", round(Asset1.pct_ret_an, 1), "%\n"
    ),
    hoverinfo = 'text'
  ) %>%
  add_trace(
    x = ~date, y = ~Asset2.log_ret_an,
    text = ~paste0(
      "Ticker: ", sym2, "\n\n",
      "Period: ", date, " -> ", date_end, "\n",
      "Close Prices: $", sprintf(Asset2.Close, fmt = '%.2f'),
      " -> $", sprintf(Asset2.Close.Fwd, fmt = '%.2f'), "\n",
      "Adj. Close Prices: $", sprintf(Asset2.Adj, fmt = '%.2f'),
      " -> $", sprintf(Asset2.Adj.Fwd, fmt = '%.2f'), "\n\n",
      "Absolute Log Return: ", round(Asset2.log_ret, 3), "\n",
      "Annualized Log Return: ", round(Asset2.log_ret_an, 3), "\n",
      "Absolute Percent Return: ", round(Asset2.pct_ret, 2), "%\n",
      "Annualized Percent Return: ", round(Asset2.pct_ret_an, 2), "%\n"
    ),
    hoverinfo = 'text'
  ) %>% 
  layout(
    title = paste0("Forward Returns (", sym1, " vs. ", sym2, ")"),
    xaxis = list(title = "Date"),
    yaxis = list(title = "Annualized Log Returns"),
    hovermode = "compare",
    showlegend = F
  )






