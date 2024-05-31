
---
  title: "Oregon Math Proficiency Rate by Ethic Group & Grade Level"
image: Math.jpg
---


library(tidyverse)
library(rvest)
library(jsonlite)
library(RPostgres)



# Include your code below! (And feel free to adjust the above if needed)
connection <- DBI::dbConnect(RPostgres::Postgres(),
                             dbname = Sys.getenv("PGDATABASE"),
                             host = "localhost",
                             port = "5432",
                             user = "postgres",
                             password = 'Skilla76!'
)

# Make our table if it doesn't already exist. I think dbWriteTable would do this automatically
# but this gives us more direct control over the exact column types
dbExecute(connection,
          "CREATE TABLE IF NOT EXISTS stocks (
            date DATE,
            open FLOAT,
            high FLOAT,
            low FLOAT,
            close FLOAT,
            volume FLOAT,
            symbol VARCHAR(5)
          )")

single_stock_info <- function(symbol) {
    # Function to grab a single stock from Alphavantage over last 100 days

    url <- URLencode(
            paste0("https://www.alphavantage.co/query?function=TIME_SERIES_DAILY&symbol=", 
                   symbol, 
                   "&apikey=MJ3E2VJLJHWCT7J7")
            )
    stock <- read_html(url) %>% html_text %>% fromJSON
    # The way the JSON was laid out, this required a transpose to get into a tidy format
    stock_df <- stock[[2]] %>% as_tibble %>% t %>% as_tibble(rownames='date')
    # In the process of making the transpose, we lost column names, so adding those back
    names(stock_df) <- c("date", "open", "high", "low", "close", "volume")
    # Need to get the column types correct again
    stock_df <- stock_df %>% mutate(across(c(open, high, low, close, volume), as.numeric))
    stock_df$symbol <- symbol # adding back symbol
    return(stock_df)
}

get_symbols <- function() {
    # Function to grab an easy listing of S%P 500 stock symbols

    url <- "https://www.slickcharts.com/sp500"
    table <- read_html(url) %>% html_table
    return(table[[1]]) # The second table is extra stuff we don't care about
}


# MAIN PROGRAM

# Getting the needed symbols table
symbol_table <- get_symbols()

# Looping over all the symbols
for (sym in symbol_table$Symbol) {
    # Giving some feedback about what is happening
    print(paste("Adding", sym, "......"))
    # Look up the given symbol from Alphavantage
    stock <- single_stock_info(sym)
    # Append contents to the earlier made table
    dbWriteTable(connection, "stocks", stock, row.names = FALSE, append = TRUE)
}

