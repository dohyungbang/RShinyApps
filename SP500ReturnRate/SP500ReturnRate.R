library(shiny)
library(shinydashboard)
library(tidyverse)
library(readr)
library(quantmod)
library(lubridate)

max_date <- Sys.Date()
min_date <- Sys.Date() %m-% months(120)

tickers <- 
  read_csv("./SP500symbols.csv")$Symbol

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
  tags$style(type='text/css', "table { font-size: 15px; }"),
  titlePanel("Monthly Return Rates of S&P500 Companies (Source: Yahoo finance)"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      selectInput("CompanyA", "Company A:",
                  choices = tickers, 
                  selected = "AAPL"),
      
      selectInput("CompanyB", "Company B:",
                  choices = tickers, 
                  selected = "AAPL"),
      
      # Input: Select a start year
      sliderInput("start_year", "Start Year:",
                  min = min_date,
                  max = max_date,
                  value = min_date,
                  timeFormat="%Y-%m"),
      
      # Input: Select an end year
      sliderInput("end_year", "End Year:",
                  min = min_date,
                  max = max_date,
                  value = max_date,
                  timeFormat="%Y-%m"),
      
      h4(strong("Indicator Definition"), align = "Left"),
      h5("- N: Number of Months", style = "font-si10pt"),
      h5("- ExpReturn: Expected Return Rate (%)", style = "font-si10pt"),
      h5("- SD: Standard Deviation (%)", style = "font-si10pt"),
      h5("- CV: Coefficient of Variation", style = "font-si10pt"),
      h5("- Min: Minimum Return Rate (%)", style = "font-si10pt"),
      h5("- Max: Maximum Return Rate (%)", style = "font-si10pt"),
      h5("- Corr: Correlation Coefficient (-1 to 1)", style = "font-si10pt")
    ),
    
    
    
    # Show a plot of the generated distribution
    mainPanel(
      width = 9,
      fluidRow(column(6,
                      plotOutput("distribution_plotA")),
               column(6,
                      plotOutput("distribution_plotB"))
      ),
      hr(),
      fluidRow(column(12,
                      plotOutput("corr_plot"))
      ),
      hr(),
      fluidRow(h3(align = "left", strong("Summary Table"))),
      fluidRow(
               column(12,
                      tableOutput("summary_table"))
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  returnA_df <- reactive({
    
    getSymbols(input$CompanyA, 
               from =  ymd(input$start_year), 
               to = ymd(input$end_year), src = "yahoo")
    pricesA <- 
      Cl(get(input$CompanyA)) %>% as.data.frame()
    
    pricesA <- 
      pricesA %>% 
      mutate(year = row.names(pricesA) %>% ymd() %>% year(),
             month = row.names(pricesA) %>% ymd() %>% month())
    
    dividendsA <- 
      getDividends(input$CompanyA, 
                   from =  ymd(input$start_year), 
                   to = ymd(input$end_year), src = "yahoo") %>% 
      as.data.frame()
    
    dividendsA <- 
      dividendsA %>% 
      mutate(year = row.names(dividendsA) %>% ymd() %>% year(),
             month = row.names(dividendsA) %>% ymd() %>% month())
    
    pr_dv_A <- 
      left_join(pricesA, dividendsA) 
    pr_dv_A[is.na(pr_dv_A)] <- 0
    names(pr_dv_A) <- c("price", "year", "month", "dividend")
    
    df_A <- 
      pr_dv_A %>% 
      group_by(year, month) %>% 
      summarise(monthly_return = round((last(dividend) + last(price) - first(price))/ first(price), digit = 3)*100) %>% 
      as.data.frame() %>% 
      mutate(date = ym(paste0(year, "-", month)))
    
    df_A
    
  })
  
  returnB_df <- reactive({
    
    getSymbols(input$CompanyB, 
               from =  ymd(input$start_year), 
               to = ymd(input$end_year), src = "yahoo")
    pricesB <- 
      Cl(get(input$CompanyB)) %>% as.data.frame()
    
    pricesB <- 
      pricesB %>% 
      mutate(year = row.names(pricesB) %>% ymd() %>% year(),
             month = row.names(pricesB) %>% ymd() %>% month())
    
    dividendsB <- 
      getDividends(input$CompanyB, 
                   from =  ymd(input$start_year), 
                   to = ymd(input$end_year), src = "yahoo") %>% 
      as.data.frame()
    
    dividendsB <- 
      dividendsB %>% 
      mutate(year = row.names(dividendsB) %>% ymd() %>% year(),
             month = row.names(dividendsB) %>% ymd() %>% month())
    
    pr_dv_B <- 
      left_join(pricesB, dividendsB) 
    pr_dv_B[is.na(pr_dv_B)] <- 0
    names(pr_dv_B) <- c("price", "year", "month", "dividend")
    
    df_B <- 
      pr_dv_B %>% 
      group_by(year, month) %>% 
      summarise(monthly_return = round((last(dividend) + last(price) - first(price))/ first(price), digit = 3)*100) %>% 
      as.data.frame() %>% 
      mutate(date = ym(paste0(year, "-", month)))
    
    df_B
    
  })
  
  output$distribution_plotA <- renderPlot({
    
    returnA <- returnA_df()
    
    # Plot distribution
    ggplot(returnA, aes(x = monthly_return, y = ..density..)) + 
      geom_histogram(bins = 20, color = "black", fill = "white", binwidth=2) +
      geom_density(color = "red", linewidth = 1) + 
      ylab("Probability") + 
      xlab("Monthly Return Rate (%)") + 
      xlim(c(-40,40)) + 
      ggtitle(paste0("Distribution of Monthly Return Rate (%) for ", input$CompanyA)) + 
      plot_theme
  })
  
  output$distribution_plotB <- renderPlot({
    
    returnB <- returnB_df()
    
    # Plot distribution
    ggplot(returnB, aes(x = monthly_return, y = ..density..)) + 
      geom_histogram(bins = 20, color = "black", fill = "white", binwidth=2) +
      geom_density(color = "red", linewidth = 1) + 
      ylab("Probability") + 
      xlab("Monthly Return Rate (%)") + 
      xlim(c(-40,40)) + 
      ggtitle(paste0("Distribution of Monthly Return Rate (%) for ", input$CompanyB)) + 
      plot_theme
  })
  
  
  output$corr_plot <- renderPlot({
    
    returnA <- returnA_df()
    returnB <- returnB_df()
    
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
    
    
    merged_df[merged_df$Company == "returnA", "Company"] <- input$CompanyA
    merged_df[merged_df$Company == "returnB", "Company"] <- input$CompanyB
    
    # Plot distribution
    ggplot(merged_df, aes(x = date, y = ReturnRate, color = Company)) + 
      geom_line(size = 1) +
      geom_point() + 
      ylab("Return Rate (%)") + 
      xlab("Date") + 
      ggtitle(paste0("Correlation of Monthly Return Rate")) + 
      corr_theme
  })
  
  output$summary_table <- renderTable({
    
    returnA <- returnA_df()
    returnB <- returnB_df()
    
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
    
    merged_df[merged_df$Company == "returnA", "Company"] <- input$CompanyA
    merged_df[merged_df$Company == "returnB", "Company"] <- input$CompanyB
    
    merged_df <-
      merged_df %>%
      group_by(Company) %>%
      summarise(N = n(),
                `ExpReturn` = mean(ReturnRate),
                `SD` = sd(ReturnRate),
                `CV` = SD/ExpReturn,
                `Min` = min(ReturnRate),
                `Max` = max(ReturnRate)) %>%
      mutate(Corr = corr)
    
    merged_df
    
  },
  include.css = TRUE,
  width = '100%')
  
  
}

# Run the application
shinyApp(ui = ui, server = server)
