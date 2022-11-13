library(jsonlite)
library(dplyr)
library(lubridate)
library(stringr)

# Get from configurations file
config <- 
    read.table("configurations.txt", sep="=", col.names=c("key", "value"))

apikey <- filter(config, key=="apikey")[1, 'value']

tickers <- filter(config, key=="tickers")[1, 'value']

# ,\\s* means "," or ", "
ticker_list <- as.character(str_split(tickers, ",\\s*", simplify=TRUE))

result_list <- vector(mode="list", length=length(ticker_list))

{
    for(i in 1:length(ticker_list)){
        symbol <- ticker_list[i]
        
        cat("API request for ", symbol, ".\n", sep="")
        
        api_url <- 
            paste0("https://www.alphavantage.co/query?function=TIME_SERIES_DAILY_ADJUSTED"
                   , "&symbol=", symbol
                   ,"&apikey=", apikey)
        
        api_data <- fromJSON(api_url)
        
        meta_data <- api_data$`Meta Data`
        time_series_daily <- api_data$`Time Series (Daily)`
        
        # Get closing price of latest date
        last_refreshed <- meta_data$`3. Last Refreshed`
        last_closing_price <- time_series_daily[[last_refreshed]]$`4. close`
        
        result_list[[i]] <- 
            data.frame(company_ticker=symbol
                       , price_per_share=last_closing_price
                       , price_as_of_date=last_refreshed)
        
        # Sleep for x seconds because free stock API service is 
        # up to 5 API requests per minute
        sleep_seconds <- 13
        
        if(i != length(ticker_list)){
            cat("Sleep for", sleep_seconds, "seconds.\n\n")
            Sys.sleep(sleep_seconds)
        }
    }
    
    # Union all results
    result_df <- 
        bind_rows(result_list) %>%
        mutate(price_per_share=as.numeric(price_per_share)
               , price_as_of_date=ymd(price_as_of_date))
    
    # Output results
    output_filename <- "latest_closing_prices.csv"
    write.csv(result_df, output_filename, row.names=FALSE)
    
    cat("\n")
    cat('Output to "', output_filename,'".\n', sep="")
}
