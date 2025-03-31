library(shiny)
library(shinydashboard)
library(tidyverse)
library(readr)
library(quantmod)
library(lubridate)
library(tidyquant)

max_date <- Sys.Date()
min_date <- Sys.Date() %m-% months(120)

# source: https://www.nasdaq.com/market-activity/stocks/screener?page=1&rows_per_page=25
ticker_df <- 
  read_csv("./symbol_list.csv") 

Symbols <- read_csv("./symbol_list.csv")$Symbol_Name

plot_theme <- 
  theme_bw() +
  theme(panel.background = element_rect(fill="white"),
        panel.grid.major.y = element_line(colour = "lightgrey", linetype="dashed"),
        panel.grid.major.x = element_line(colour = "lightgrey", linetype="dashed"),
        panel.grid.minor = element_line(colour = "white"),
        plot.title = element_text(size = 20, hjust=0.5, colour = "black", face = "bold"),
        axis.text.x = element_text(size = 20, hjust=1, colour = "black"),
        axis.text.y = element_text(size = 20, colour = "black"),
        axis.title.x = element_text(size = 20, colour = "black", margin = margin(t = 10, r = 0, b = 10, l = 0)), 
        axis.title.y = element_text(size = 20, colour = "black", margin = margin(t = 0, r = 10, b = 0, l = 0)),
        strip.text = element_text(size = 20, family = "serif")) +
  theme(plot.margin = unit(c(0.3,0.3,0.3,0.3), "cm"))

corr_theme <- 
  theme_bw() +
  theme(panel.background = element_rect(fill="white"),
        panel.grid.major.y = element_line(colour = "lightgrey", linetype="dashed"),
        panel.grid.major.x = element_line(colour = "lightgrey", linetype="dashed"),
        panel.grid.minor = element_line(colour = "white"),
        plot.title = element_text(size = 20, hjust=0.5, colour = "black", face = "bold"),
        axis.text.x = element_text(size = 20, hjust=1, colour = "black"),
        axis.text.y = element_text(size = 20, colour = "black"),
        axis.title.x = element_text(size = 20, colour = "black", margin = margin(t = 10, r = 0, b = 10, l = 0)), 
        axis.title.y = element_text(size = 20, colour = "black", margin = margin(t = 0, r = 10, b = 0, l = 0)),
        strip.text = element_text(size = 20),
        legend.position = "bottom",
        legend.text = element_text(size = 20, colour = "black"),
        legend.title = element_text(size = 20, colour = "black", face = "bold"),
        legend.key.width = unit(1, "cm"),
        legend.background = element_rect(fill = "white", colour = "black"),
        legend.margin = margin(r=5,l=5,t=5,b=5)) +
  theme(plot.margin = unit(c(0.3,0.3,0.3,0.3), "cm"))


# Define UI for application
ui <- fluidPage(
  tags$style(type='text/css', "table { font-size: 16px; }"),
  titlePanel("Monthly Stock Return Analyzer (Source: Yahoo finance)"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      selectInput("CompanyA", "Company A:",
                  choices = Symbols, 
                  selected = Symbols[grep("AAPL", Symbols)]
                  ),
      
      selectInput("CompanyB", "Company B:",
                  choices = Symbols, 
                  selected = Symbols[grep("AAPL", Symbols)]
                  ),
      
      sliderInput("date_range", "Select Date Range:",
                  min = min_date,
                  max = max_date,
                  value = c(min_date, max_date),
                  timeFormat = "%Y-%m",
                  dragRange = TRUE),
      
      hr(),
      h4(strong("Indicator Definition"), align = "Left"),
      h5("- N: Number of Months", style = "font-si10pt"),
      h5("- ExpReturn: Expected Return Rate (%)", style = "font-si10pt"),
      h5("- SD: Standard Deviation (%)", style = "font-si10pt"),
      h5("- CV: Coefficient of Variation", style = "font-si10pt"),
      h5("- Min: Minimum Return Rate (%)", style = "font-si10pt"),
      h5("- Max: Maximum Return Rate (%)", style = "font-si10pt"),
      h5("- Corr: Correlation Coefficient (-1 to 1)", style = "font-si10pt"),
      hr(),
      h5("Contact: dohyung.bang@gmail.com")
    ),
    
    
    
    # Show a plot of the generated distribution
    mainPanel(
      width = 9,
      fluidRow(column(6,
                      plotOutput("daily_price_plotA")),
               column(6,
                      plotOutput("daily_price_plotB"))
      ),
      hr(),
      fluidRow(column(6,
                      plotOutput("distribution_plotA")),
               column(6,
                      plotOutput("distribution_plotB"))
      ),
      hr(),
      fluidRow(column(8,
                      plotOutput("corr_plot")),
               column(4,
                      h3(strong("Summary Table")),
                      tableOutput("summary_table"))
      ),
      hr()

    )
  )
)

# Define server logic
server <- function(input, output) {
  
  returnA_df <- reactive({
    
    companyA_symbol <- strsplit(input$CompanyA, "[ | ]")[[1]][1]
    
    start_date <- input$date_range[1]
    end_date  <- input$date_range[2]
    
    getSymbols(companyA_symbol, 
               from =  ymd(start_date), 
               to = ymd(end_date), src = "yahoo")
    pricesA <- 
      Cl(get(companyA_symbol)) %>% as.data.frame()
    
    pricesA <- 
      pricesA %>% 
      mutate(date = row.names(pricesA) %>% ymd(),
             year = row.names(pricesA) %>% ymd() %>% year(),
             month = row.names(pricesA) %>% ymd() %>% month())
    
    names(pricesA) <- c("price", "date", "year", "month")
    
    dividendsA <- 
      getDividends(companyA_symbol, 
                   from =  ymd(start_date), 
                   to = ymd(end_date), src = "yahoo") %>% 
      as.data.frame()
    
    dividendsA <- 
      dividendsA %>% 
      mutate(year = row.names(dividendsA) %>% ymd() %>% year(),
             month = row.names(dividendsA) %>% ymd() %>% month())
    
    pr_dv_A <- 
      left_join(pricesA %>% select(price, year, month), dividendsA) 
    pr_dv_A[is.na(pr_dv_A)] <- 0
    names(pr_dv_A) <- c("price", "year", "month", "dividend")
    
    df_A <- 
      pr_dv_A %>% 
      group_by(year, month) %>% 
      summarise(monthly_return = round((last(dividend) + last(price) - first(price))/ first(price), digit = 3)*100) %>% 
      as.data.frame() %>% 
      mutate(date = ym(paste0(year, "-", month)))
    
    result_A <- list(df_A, pricesA)
    result_A
  })
  
  returnB_df <- reactive({
    
    companyB_symbol <- strsplit(input$CompanyB, "[ | ]")[[1]][1]
    
    start_date <- input$date_range[1]
    end_date  <- input$date_range[2]
    
    getSymbols(companyB_symbol, 
               from =  ymd(start_date), 
               to = ymd(end_date), src = "yahoo")
    pricesB <- 
      Cl(get(companyB_symbol)) %>% as.data.frame()
    
    pricesB <- 
      pricesB %>% 
      mutate(date = row.names(pricesB) %>% ymd(),
             year = row.names(pricesB) %>% ymd() %>% year(),
             month = row.names(pricesB) %>% ymd() %>% month())
    
    names(pricesB) <- c("price", "date", "year", "month")
    
    dividendsB <- 
      getDividends(companyB_symbol, 
                   from =  ymd(start_date), 
                   to = ymd(end_date), src = "yahoo") %>% 
      as.data.frame()
    
    dividendsB <- 
      dividendsB %>% 
      mutate(year = row.names(dividendsB) %>% ymd() %>% year(),
             month = row.names(dividendsB) %>% ymd() %>% month())
    
    pr_dv_B <- 
      left_join(pricesB %>% select(price, year, month), dividendsB) 
    pr_dv_B[is.na(pr_dv_B)] <- 0
    names(pr_dv_B) <- c("price", "year", "month", "dividend")
    
    df_B <- 
      pr_dv_B %>% 
      group_by(year, month) %>% 
      summarise(monthly_return = round((last(dividend) + last(price) - first(price))/ first(price), digit = 3)*100) %>% 
      as.data.frame() %>% 
      mutate(date = ym(paste0(year, "-", month)))
    
    # df_B
    result_B <- list(df_B, pricesB)
    result_B
  })
  
  output$distribution_plotA <- renderPlot({
    
    returnA <- returnA_df()[[1]]
    
    companyA_symbol <- strsplit(input$CompanyA, "[ | ]")[[1]][1]
    
    # Plot distribution
    ggplot(returnA, aes(x = monthly_return, y = ..density..)) + 
      geom_histogram(bins = 20, color = "black", fill = "white", binwidth=2) +
      geom_density(color = "red", linewidth = 1) + 
      ylab("Probability") + 
      xlab("Monthly Return Rate (%)") + 
      xlim(c(-40,40)) + 
      ggtitle(paste0("Distribution of Monthly Return Rate (%) for ", companyA_symbol)) + 
      plot_theme
  })
  
  output$distribution_plotB <- renderPlot({
    
    returnB <- returnB_df()[[1]]
    
    companyB_symbol <- strsplit(input$CompanyB, "[ | ]")[[1]][1]
    
    # Plot distribution
    ggplot(returnB, aes(x = monthly_return, y = ..density..)) + 
      geom_histogram(bins = 20, color = "black", fill = "white", binwidth=2) +
      geom_density(color = "red", linewidth = 1) + 
      ylab("Probability") + 
      xlab("Monthly Return Rate (%)") + 
      xlim(c(-40,40)) + 
      ggtitle(paste0("Distribution of Monthly Return Rate (%) for ", companyB_symbol)) + 
      plot_theme
  })
  
  output$daily_price_plotA <- renderPlot({
    
    priceA <- returnA_df()[[2]]
    
    companyA_symbol <- strsplit(input$CompanyA, "[ | ]")[[1]][1]
    
    # Plot distribution
    ggplot(priceA, aes(x = date, y = price)) + 
      geom_line(size = 1) +
      geom_point() +
      ylab("Daily Price - Closed ($)") + 
      xlab("Date") + 
      ggtitle(paste0("Daily Stock Price (%) for ", companyA_symbol)) + 
      corr_theme
  })
  x
  output$daily_price_plotB <- renderPlot({
    
    priceB <- returnB_df()[[2]]
    
    companyB_symbol <- strsplit(input$CompanyB, "[ | ]")[[1]][1]
    
    # Plot distribution
    ggplot(priceB, aes(x = date, y = price)) + 
      geom_line(size = 1) +
      geom_point() +
      ylab("Daily Price - Closed ($)") + 
      xlab("Date") + 
      ggtitle(paste0("Daily Stock Price (%) for ", companyB_symbol)) + 
      corr_theme
  })
  
  
  output$corr_plot <- renderPlot({
    
    returnA <- returnA_df()[[1]]
    returnB <- returnB_df()[[1]]
    
    companyA_symbol <- strsplit(input$CompanyA, "[ | ]")[[1]][1]
    companyB_symbol <- strsplit(input$CompanyB, "[ | ]")[[1]][1]
    
    if (nrow(returnA) == nrow(returnB)){
      merged_df <- 
        data.frame(date = ymd(returnA$date), 
                   returnA = returnA$monthly_return,
                   returnB = returnB$monthly_return) %>% 
        gather(returnA, returnB, key = "Company", value = "ReturnRate")
      
    } else if(nrow(returnA) > nrow(returnB)){
      
      merged_df <- 
        returnA %>% 
        rename(returnA = monthly_return) %>% 
        left_join(returnB %>% 
                    rename(returnB = monthly_return)) %>% 
        gather(returnA, returnB, key = "Company", value = "ReturnRate")
      
    } else if(nrow(returnA) < nrow(returnB)){
      
      merged_df <- 
        returnB %>% 
        rename(returnB = monthly_return) %>% 
        left_join(returnA %>% 
                    rename(returnA = monthly_return)) %>% 
        gather(returnA, returnB, key = "Company", value = "ReturnRate")
      
    }
    
    
    merged_df[merged_df$Company == "returnA", "Company"] <- companyA_symbol
    merged_df[merged_df$Company == "returnB", "Company"] <- companyB_symbol
    
    # Plot distribution
    ggplot(merged_df, aes(x = date, y = ReturnRate, color = Company)) + 
      geom_line(size = 1) +
      geom_point() + 
      ylab("Return Rate (%)") + 
      xlab("Date") + 
      ggtitle(paste0("Monthly Return Rate (%) Over Time: ", companyA_symbol, " and ", companyB_symbol)) + 
      corr_theme
  })
  
  output$summary_table <- renderTable({
    
    returnA <- returnA_df()[[1]]
    returnB <- returnB_df()[[1]]
    
    companyA_symbol <- strsplit(input$CompanyA, "[ | ]")[[1]][1]
    companyB_symbol <- strsplit(input$CompanyB, "[ | ]")[[1]][1]
    
    if (nrow(returnA) == nrow(returnB)){
      merged_df <- 
        data.frame(date = ymd(returnA$date), 
                   returnA = returnA$monthly_return,
                   returnB = returnB$monthly_return)
      
    } else if(nrow(returnA) > nrow(returnB)){
      
      merged_df <- 
        returnA %>% 
        rename(returnA = monthly_return) %>% 
        left_join(returnB %>% 
                    rename(returnB = monthly_return)) 
      
    } else if(nrow(returnA) < nrow(returnB)){
      
      merged_df <- 
        returnB %>% 
        rename(returnB = monthly_return) %>% 
        left_join(returnA %>% 
                    rename(returnA = monthly_return)) 
      
    }
    
    merged_subset <- merged_df %>% na.omit()
    corr <- cor(merged_subset$returnA, merged_subset$returnB)
    
    merged_df <- 
      merged_df %>% 
      gather(returnA, returnB, key = "Company", value = "ReturnRate") %>% 
      filter(!is.na(ReturnRate))
    
    merged_df[merged_df$Company == "returnA", "Company"] <- companyA_symbol
    merged_df[merged_df$Company == "returnB", "Company"] <- companyB_symbol
    
    merged_df <-
      merged_df %>%
      group_by(Company) %>%
      summarise(N = n(),
                `ExpReturn` = mean(ReturnRate),
                `SD` = sd(ReturnRate),
                `CV` = SD/ExpReturn,
                `Min` = min(ReturnRate),
                `Max` = max(ReturnRate)) %>%
      mutate(Corr = corr) %>% 
      gather(N:Corr, key = "Variable", value =  "Value") %>% 
      spread(key = Company, value = Value) %>% 
      mutate(Variable = factor(Variable, levels = c("N", "ExpReturn", "SD", "CV", "Min", "Max", "Corr"))) %>% 
      arrange(Variable)
    
    merged_df
    
  },
  include.css = TRUE,
  width = '100%')
  
  
}

# Run the application
shinyApp(ui = ui, server = server)

