library(shiny)
library(shinydashboard)
library(leaflet)
library(RColorBrewer)
library(tidyverse)
library(DT)
library(sqldf)
library(ggplot2) 
library(dplyr)
library(plotly)
library(datasets)
library(shinyWidgets)
library(shinythemes)
library(knitr)
library(kableExtra)
library(ggthemes)
library(gapminder)
library(scales)
library(tidycensus)
library(lubridate)
library(TTR)
library(tm)
library(wordcloud)
library(tidytext)
library(stringr)
library(tidyr)
library(textstem)
library(readr)

options(scipen=999)

# Dealing with data
# Q1_data
all <- readRDS("Hotel_Reviews_Location.rds")

# Q2_Data
#q2p1 <- read.csv("final_projectq2_1.csv")
#saveRDS(q2p1,"final_projectq2_1.rds")
#q2p3 <- read.csv("hotel_data_people.csv")
#saveRDS(q2p3,"hotel_data_people.rds")
q2p1 <- readRDS("final_projectq2_1.rds")
q2p3 <- readRDS("hotel_data_people.rds")
trip_type_data_overall <- q2p1 %>%
  group_by(trip_type) %>%
  summarise(n = n())

# Q3_data
  #hotel_q3 <- read.csv("Hotel_Reviews.csv")
  #saveRDS(hotel_q3,"Hotel_Reviews.rds")
  hotel <- readRDS("Hotel_Reviews.rds")
  a <- sqldf('SELECT Reviewer_Nationality, Reviewer_Score
      FROM hotel
      WHERE Reviewer_Nationality IN (SELECT Reviewer_Nationality FROM hotel GROUP BY Reviewer_Nationality ORDER BY COUNT(Reviewer_Nationality) DESC LIMIT 10) ')
  difference <- sqldf('SELECT Hotel_Name, Average_Score, Reviewer_Nationality, AVG(Reviewer_Score) AS avg_reviewer_score, ROUND(Average_Score-AVG(Reviewer_Score),2) AS Difference
      FROM hotel
      WHERE Hotel_Name IN ("Park Plaza Westminster Bridge London", "Hotel Da Vinci", "Best Western Premier Hotel Couture", "Park Grand Paddington Court", "Hilton London Metropole","Britannia International Hotel Canary Wharf", "The Student Hotel Amsterdam City","Strand Palace Hotel")
      GROUP BY Hotel_Name, Reviewer_Nationality')
  top <- sqldf('SELECT Reviewer_Nationality, Difference
      FROM difference
      GROUP BY Reviewer_Nationality
      ORDER BY Difference DESC
      LIMIT 10')
  top <- na.omit(top)
 
# Q4_data
  #hotel_q4 <- read.csv("Hotel_Cleaned_q4.csv")
  #saveRDS(hotel_q4,'Hotel_Cleaned_q4.rds')
  hotel_q4 <- readRDS('Hotel_Cleaned_q4.rds')
  name <- unique(hotel_q4$Name)
  country <- unique(hotel_q4$Country)
  name <- c("All",name)
  
  # Hilton
  hilton_by_country <- sqldf('SELECT Name, 
AVG(Reviewer_Score) AS avg_score, Country FROM hotel_q4
      WHERE Hotel_Name LIKE "%Hilton%"
      GROUP BY Country')
  
  # Marriott
  marriott_by_country <- sqldf('SELECT Name, 
AVG(Reviewer_Score) AS avg_score, Country FROM hotel_q4
      WHERE Hotel_Name LIKE "%Marriott%"
      GROUP BY Country')
  
  # Crowne Plaza
  crowne_plaza_by_country <- sqldf('SELECT Name, 
AVG(Reviewer_Score) AS avg_score, Country FROM hotel_q4
      WHERE Hotel_Name LIKE "%Crowne_Plaza%"
      GROUP BY Country')

  # Holiday Inn
  holiday_inn_by_country <- sqldf('SELECT Name, 
AVG(Reviewer_Score) AS avg_score, Country FROM hotel_q4
      WHERE Hotel_Name LIKE "%Holiday_Inn%"
      GROUP BY Country')

  # Ritz
  ritz_by_country <- sqldf('SELECT Name, 
AVG(Reviewer_Score) AS avg_score, Country FROM hotel_q4
      WHERE Hotel_Name LIKE "%Ritz%"
      GROUP BY Country')
  ritz_by_country
  
  # Waldorf
  waldorf_by_country <- sqldf('SELECT Name, 
AVG(Reviewer_Score) AS avg_score, Country FROM hotel_q4
      WHERE Hotel_Name LIKE "%Waldorf%"
      GROUP BY Country')
  waldorf_by_country
  
  hotel_all_ave <- rbind(hilton_by_country,
                         marriott_by_country,
                         crowne_plaza_by_country,
                         holiday_inn_by_country,
                         ritz_by_country,
                         waldorf_by_country)
  
# Q5
  hotel_review_sub <- readRDS('hotel_review_sub.rds')
  text_process <- function(country){
    if (country=="All"){
      review_narrative <- hotel_review_sub %>%
        select(Positive_Review,Negative_Review)%>%
        gather(Positive_Review,Negative_Review,key = 'review_type',value = 'narrative')%>%
        filter(narrative %in% c('No positive','No negative','no positive','no negative')==FALSE)%>%
        group_by(review_type)%>%
        summarise(narrative = paste0(narrative,collapse = " "))%>%
        ungroup()%>%
        unnest_tokens(word, narrative)}else{
          review_narrative <- hotel_review_sub %>%
            filter(Reviewer_Nationality %in% country)%>%
            select(Positive_Review,Negative_Review)%>%
            gather(Positive_Review,Negative_Review,key = 'review_type',value = 'narrative')%>%
            filter(narrative %in% c('No positive','No negative','no positive','no negative')==FALSE)%>%
            group_by(review_type)%>%
            summarise(narrative = paste0(narrative,collapse = " "))%>%
            ungroup()%>%
            unnest_tokens(word, narrative)       
        }
    review_narrative$word <- textstem::lemmatize_words(review_narrative$word)
    review_narrative$word <- gsub('[0-9]','' ,review_narrative$word)
    review_clean <- review_narrative %>%
      filter(word %in% stopwords("english") == FALSE)%>%
      mutate(word = tolower(word))%>%
      filter(str_detect(word,'[[:punct:] ]+|\\s+')==FALSE)%>%
      filter(word != '')%>%
      filter(word %in% c('aa','aaa','aaaa',letters)==FALSE)%>%
      count(review_type,word,sort = T)
    total_words <- review_clean %>% 
      group_by(review_type) %>%
      summarize(total = sum(n))
    review_clean<- left_join(review_clean, total_words)
    review_clean_tf_idf <- review_clean %>%
      bind_tf_idf(word,review_type, n)
    review_clean_tf_idf <-review_clean_tf_idf %>%
      select(-total) %>%
      arrange(desc(tf_idf))
    return(review_clean_tf_idf)
  }
  wordcloud_rep <- repeatable(wordcloud)
  word_cloud_pos <- function(max_word,freq,country){
    review_clean_tf_idf <- text_process(country)
    positive_tf_idf <- review_clean_tf_idf%>%
      filter(review_type == 'Positive_Review')
    print(wordcloud_rep(words = positive_tf_idf$word, freq = positive_tf_idf$tf_idf,
                        max.words=max_word, random.order=FALSE, rot.per=0.35, min.freq = freq,
                        colors=brewer.pal(8, "Dark2")))
  }
  word_cloud_neg <- function(max_word,freq,country){
    review_clean_tf_idf <- text_process(country)
    positive_tf_idf <- review_clean_tf_idf%>%
      filter(review_type == 'Negative_Review')
    print(wordcloud_rep(words = positive_tf_idf$word, freq = positive_tf_idf$tf_idf,
                        max.words=max_word, random.order=FALSE, rot.per=0.35, min.freq = freq,
                        colors=brewer.pal(8, "Dark2")))
  }
  m <- list(
    l = 200,
    r = 50,
    b = 50,
    t = 100,
    pad = 4
  )
  brks <- seq(-100, 120, 10)
  lbls = c(seq(100, 0, -10), seq(10, 120, 10))
  
  bar_plot_1 <- function(country){
    review_clean_tf_idf <- text_process(country)
    top_20_word <- review_clean_tf_idf%>%
      group_by(word)%>%
      filter(n()>1)%>%
      ungroup()%>%
      arrange(desc(n))%>%
      select(word)%>%
      distinct()%>%
      head(20)
    top_20 <- review_clean_tf_idf%>%
      filter(word %in% top_20_word$word)
    p1 <- top_20 %>%
      mutate(review_type=ifelse(review_type == 'Positive_Review','Positive Review','Negative Review'))%>%
      mutate(tf_idf = ifelse(review_type == 'Positive Review',-n,n))%>%
      ggplot(aes(x = word, y = tf_idf, fill = review_type,text = paste(
        "Word: ",word, "\n",
        "word frequency: ", n, "\n", 
        "Review type: ",review_type, "\n",
        sep = ""
      ))) +
      geom_bar(stat = "identity", width = .7) +
      labs(x= 'Word',
           y="Word Count",
           title="Top 20 words used both by positive reviews and negative reviews",
           fill = 'Review Type')+
      theme_minimal() +
      coord_flip()+
      theme(axis.ticks = element_blank())+
      scale_fill_hue(labels = c("Negative Reviews",'Positive Reviews'))+
      theme(legend.position = 'bottom')+
      theme(
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
    return(ggplotly(p1,tooltip = 'text')%>%layout(margin=m))
  }
  
  bar_plot_2 <- function(country){
    review_clean_tf_idf <- text_process(country)
    p_2 <- review_clean_tf_idf %>%
      mutate(review_type=ifelse(review_type == 'Positive_Review','Positive Review','Negative Review'))%>%
      arrange(desc(tf_idf)) %>%
      mutate(word = factor(word, levels = rev(unique(word)))) %>% 
      group_by(review_type) %>% 
      top_n(10) %>% 
      ungroup() %>%
      ggplot(aes(word, tf_idf, fill = review_type, text = paste(
        "Word: ",word, "\n",
        "Tf/idf score: ", tf_idf, "\n", 
        "Review type: ",review_type, "\n",
        sep = ""
      ))) +
      geom_col(show.legend = FALSE) +
      labs(x = NULL, y = "tf/idf") +
      labs(x= 'Word',
           y="Word tf/idf",
           title="Words Frequency between Positive and Negative Reviews",
           fill = 'Review Type')+
      theme_minimal() +
      coord_flip()+
      theme(axis.ticks = element_blank())+
      facet_wrap(~review_type, ncol = 2, scales = "free") +
      theme(
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
    return(ggplotly(p_2, tooltip = 'text')%>%layout(margin=m))
  }

# Q3
  country_names <- hotel_review_sub %>%
    select(Reviewer_Nationality)%>%
    distinct()

# Q1
pal <- colorRampPalette(c("blue", "red"))
var1 <- c("All", "Austria", "France", "Italy", "Netherlands", "Spain", "United Kingdom")


# Define UI for application
ui <- dashboardPage(dashboardHeader(title = "Hotel Explorer"),
                    dashboardSidebar(sidebarMenu(
                      menuItem("Dashboard", tabName = "Dashboard", icon = icon("dashboard")),
                      menuItem("Location", tabName = "Location",icon = icon("fas fa-map")), 
                      menuItem("Behavior", tabName ="Behavior",icon = icon("fas fa-users")),
                      menuItem("Nationality", tabName = "Nationality", icon = icon("fas fa-globe")),
                      menuItem("Hotels", tabName = "Hotels", icon = icon("fas fa-hotel")),
                      menuItem("Text", tabName = "Text", icon = icon("fas fa-comments"))
                    )
                    ),
                      dashboardBody(
                        tags$head(tags$style(HTML('
      .main-header .logo {
        font-family: "Georgia", Times, "Times New Roman", serif;
        font-weight: bold;
        font-size: 24px;
      }
    '))),
                        tabItems(
                          tabItem("Dashboard", 
                                  box(
                                    title = "About the team",
                                    status = "danger",
                                    width = 5,
                                    tags$h3(
                                      class = "text-center",
                                      tags$strong("Hi! We are team YYY.")
                                    ),
                                    tags$p(
                                      class = "text-center",
                                      tags$br("Members")
                                    ),
                                    tags$p(
                                      class = "text-center",
                                      tags$strong("Jiaqing Ge"),
                                      '\njg4185@columbia.edu'
                                    ),
                                    tags$p(
                                      class = "text-center",
                                      tags$strong("Ruowang Li"),
                                      '\nrl3096@columbia.edu'
                                    ),
                                    tags$p(
                                      class = "text-center",
                                      tags$strong("Yejie Yu"),
                                      '\nyy2835@columbia.edu'
                                    ),
                                    tags$p(
                                      class = "text-center",
                                      tags$strong("Shangqing Li"),
                                      '\nsl4633@columbia.edu'
                                    ),
                                    tags$p(
                                      class = "text-center",
                                      tags$strong("Zhoujun Zheng"),
                                      '\nzz2687@columbia.edu'
                                    )
                                  ),
                                  box(
                                    title = "About this Dashboard",
                                    status = "primary",
                                    width = 7,
                                    tags$p(
                                      class = "text-center",
                                      "Where to find top-rated hotels with lots of reviews?"
                                    ),
                                    tags$p(
                                      class = "text-center",
                                      "Whether the type of trip and/or with whom a guest travels will affect the score he/she gives to a hotel?"
                                    ),
                                    tags$p(
                                      class = "text-center",
                                      "People from which countries are more inclined to give a high score?"
                                    ),
                                    tags$p(
                                      class = "text-center",
                                      "Are hotels of the same brand receive similar scores in different countries?"
                                    ),
                                    tags$p(
                                      class = "text-center",
                                      "What are the positive reviews and negative reviews mainly about separately?"
                                    ),
                                    tags$p(
                                      class = "text-center",
                                      tags$strong(tags$em("Let's investigate all these questions with this dashboard!"))
                                    ),
                                    br(),
                                    tags$p(
                                      class = "text-center",
                                      tags$strong("About the dataset")
                                    ),
                                    tags$p(
                                      class = "text-center",
                                      "The dataset we use is named 515K Hotel Reviews Data in Europe. This dataset can be found at ",
                                      tags$a(href = "https://www.kaggle.com/jiashenliu/515k-hotel-reviews-data-in-europe", target = "_blank", "dataset link",'.')
                                    ),
                                    br(),
                                    tags$p(
                                      class = "text-center",
                                      tags$strong("About the environment")
                                    ),
                                    tags$p(
                                      class = "text-center",
                                      "This dashboard was built in",
                                      tags$a(href = "https://r-project.org", target = "_blank", "R"),
                                      "and", tags$a(href = "https://rstudio.com", target = "_blank", "RStudio"), "with",
                                      tags$strong("shiny,"),
                                      tags$strong("shinydashboard,"),
                                      tags$strong("plotly,"),
                                      tags$strong("ggplot,"),
                                      tags$strong("leaflet,"),
                                      tags$strong("tidyverse,"),
                                      "and many more packages."
                                    )
                                    )),
                          tabItem("Location", 
                                  fluidRow(
                                    column(width = 9,
                                           box(width = NULL, solidHeader = TRUE,
                                               leafletOutput("map", height = 500)),
                                           box(width = NULL, dataTableOutput("table"))),
                                    column(width = 3,
                                           box(width = NULL, status = "warning",
                                               selectInput("Country", "Country", var1),
                                               sliderInput("Average_Score", "Average Score",
                                                           min(all$Average_Score), max(all$Average_Score), value = range(all$Average_Score), step = 0.575, sep = ""),
                                               sliderInput("Total_Number_of_Reviews", "Total Number of Reviews",
                                                           min(all$Total_Number_of_Reviews), max(all$Total_Number_of_Reviews), value = range(all$Total_Number_of_Reviews), sep = "")),
                                           box(width = NULL, status = "warning",
                                               valueBoxOutput("vbox", width = NULL),
                                               plotOutput("histScore", height = 270),
                                               plotOutput("histReviews", height = 270))))
                                  ),
                          tabItem("Behavior",
                                  fluidRow(box(title = "Trip Type",status = "primary",
                                               plotlyOutput("trip_type", width = "100%", height = 485)),
                                           box(title = "Make selections",status = "warning",
                                               radioButtons(
                                                 "Trip_Type","Trip Type",
                                                 c("Business trip", "Leisure trip")),
                                               plotOutput("pie_chart", width = "100%"))),
                                  fluidRow(box(width = 12, title = "Boxplot", plotlyOutput("No_people",width=NULL,height=550))
                                           )
                                  ),
                          tabItem("Nationality",
                                  fluidRow(
                                    column(width = 3,
                                    valueBoxOutput('TN', width = NULL),
                                    valueBoxOutput('top', width = NULL),
                                    valueBoxOutput('last', width = NULL)
                                    ),
                                    column(width = 9,
                                      box(width = NULL, status = "warning", plotlyOutput('ggplot_a', height = 500)),
                                      br(),
                                      box(width = NULL, status = "primary", plotlyOutput('top10', height = 500))
                                    ),
                          )),
                          tabItem("Hotels", 
                                  fluidRow(column(width = 3, wellPanel(
                                    radioButtons("picture", "Chart Choice:", choices = list("Comparison among Countries" = 1, "Comparison among Hotels"= 2), 
                                                 selected = 1
                                  ))),
                                  column(width = 9,
                                         box(width = NULL, status = "warning",
                                             plotlyOutput("p1", height = 500, width = NULL),
                                             br(),
                                             textOutput("summary"))))),
                          tabItem("Text", 
                                  fluidPage(    
                            fluidRow(
                              column(width = 2,
                                wellPanel(
                                  pickerInput("level_select", "Level:",   
                                              choices = c("Overall","By country"), 
                                              selected = c("Overall"),
                                              multiple = FALSE),
                                  pickerInput("country_select", "Nationality of Customer(s):",   
                                              choices = c(as.character(country_names$Reviewer_Nationality),'All'), 
                                              options = list(`actions-box` = TRUE, `none-selected-text` = "Please make a selection!"),
                                              selected = c('All'),multiple = TRUE),
                                  sliderInput("max_word", step = 1,
                                              "Max number of words:",
                                              min = 1,
                                              max = 500,
                                              value = 100),
                                  sliderInput("freq",
                                              "Minimum Frequency:",
                                              min = 1,  max = 1000, value = 100))
                                
                              ),
                              column(width = 5,
                                box(width = NULL, plotOutput("word_cloud_pos",width = "100%"), 
                                    title = "Word Cloud for Positive Reviews", solidHeader = TRUE,status = "primary")
                                ),
                              column(width = 5,
                                box(width = NULL, plotOutput("word_cloud_neg",width = "100%"), 
                                    title = "Word Cloud for Negative Reviews", solidHeader = TRUE,status = "warning")
                                )
                            ),
                            fluidRow(
                              column(width = 12,
                                box(width = NULL,plotlyOutput("bar_plot_1_text"))
                              )
                            ),
                            fluidRow(
                              column(width = 12,
                                box(width = NULL,plotlyOutput("bar_plot_2_text"))
                              ))
                          )
                        )
                        )
                      )
)

# Define server logic
server <- function(input, output, session) {

# Q1
  data <- reactive({
    if(input$Country == "All"){all[all$Average_Score >= input$Average_Score[1] &
                                     all$Average_Score <= input$Average_Score[2] &
                                     all$Total_Number_of_Reviews >= input$Total_Number_of_Reviews[1] &
                                     all$Total_Number_of_Reviews <= input$Total_Number_of_Reviews[2] ,]}
    
    else{all[all$Country == input$Country &
               all$Average_Score >= input$Average_Score[1] &
               all$Average_Score <= input$Average_Score[2] &
               all$Total_Number_of_Reviews >= input$Total_Number_of_Reviews[1] &
               all$Total_Number_of_Reviews <= input$Total_Number_of_Reviews[2] ,]}
    })
  
  table_data <- reactive({
    req(input$table_rows_all)
    all[input$table_rows_all ,]
    })
  
  input_data <- reactive({
    if (nrow(table_data()) > 0 && nrow(table_data()) < nrow(data())){
      table_data()} else {data()}
    })
  
  output$map <- renderLeaflet({input_data() %>% leaflet() %>%
      addTiles(options = providerTileOptions(minZoom = 2)) %>%
      fitBounds(min(all$lng), min(all$lat), max(all$lng), max(all$lat))
    })
  
  output$table <- renderDataTable({
    datatable(data()[, c("Hotel_Name", "Hotel_Address", "Average_Score", "Total_Number_of_Reviews", "Country")],
              colnames = c("Hotel Name", "Address", "Average Score", "Total Reviews", "Country"),
              escape = FALSE, rownames = FALSE,
              options = list(pageLength = 5,
                             autoWidth = TRUE,
                             columnDefs = list(list(width = "320px", targets = c(0,1)),
                                               list(width = "210px", targets = 2),
                                               list(width = "220px", targets = c(3,4)),
                                               list(className = "dt-center", targets = "_all"),
                                               list(className = "dt-head-nowrap", targets = "_all"))
              ))})
  
  output$vbox <- renderValueBox({
    valueBox(h4("Total Number of Hotels"),
             nrow(input_data()))})
  
  output$histScore <- renderPlot({
    if (nrow(input_data()) == 0)
      return(NULL)
    
    hist(input_data()$Average_Score,
         breaks = c(5, 5.5, 6, 6.5, 7, 7.5, 8, 8.5, 9, 9.5, 10),
         main = "Average Score Distribution",
         xlab = "Average Score",
         xlim = c(5,10),
         col = "#ff9933",
         border = "white")})
  
  output$histReviews <- renderPlot({
    if (nrow(input_data()) == 0)
      return(NULL)
    
    hist(input_data()$Total_Number_of_Reviews,
         breaks = 20,
         main = "Reviews Amount Distribution",
         xlab = "Total Number of Reviews",
         xlim = c(0,20000),
         col = "#FA5858",
         border = "white")})
  
  observe({
    pal_country <- colorNumeric(pal(10), domain = all$Average_Score)
    color_score <- pal_country(input_data()$Average_Score)
    
    content_popup <- paste("<strong>","Hotel Name: ","</strong>", input_data()$Hotel_Name,"<br/>",
                           "<strong>","Average Score: ","</strong>", input_data()$Average_Score,"<br/>",
                           "<strong>","Total Number of Reviews: ","</strong>", input_data()$Total_Number_of_Reviews,"<br/>",
                           "<strong>","Address: ","</strong>", input_data()$Hotel_Address,"<br/>")
    
    leafletProxy("map", data = input_data()) %>% clearShapes() %>%
      fitBounds(min(input_data()$lng), min(input_data()$lat), max(input_data()$lng), max(input_data()$lat)) %>%
      
      addCircleMarkers(color = color_score,
                       stroke = FALSE,
                       radius = ifelse(input_data()$Total_Number_of_Reviews < 1000,
                                       2,
                                       input_data()$Total_Number_of_Reviews*0.003),
                       fillOpacity = 0.8,
                       popup = content_popup)
    })
  
  observe({
    proxy <- leafletProxy("map", data = input_data())
    proxy %>% clearControls()
    if (nrow(input_data()) > 1) {
      pal_country <- colorNumeric(pal(10), domain = all$Average_Score)
      proxy %>% addLegend(pal = pal_country, values = input_data()$Average_Score,
                          title = "Average Score", position = "bottomleft")}
    })
  
  observeEvent(input$table_rows_selected, {
    row_selected = data()[input$table_rows_selected ,]
    proxy <- leafletProxy("map")
    proxy %>% clearMarkers() %>% clearControls() %>%
      fitBounds(min(row_selected$lng), min(row_selected$lat), max(row_selected$lng), max(row_selected$lat))
    
    pal_country <- colorNumeric(pal(10), domain = all$Average_Score)
    color_score <- pal_country(row_selected$Average_Score)
    
    content_popup <- paste("<strong>","Hotel Name: ","</strong>", row_selected$Hotel_Name,"<br/>",
                           "<strong>","Average Score: ","</strong>", row_selected$Average_Score,"<br/>",
                           "<strong>","Total Number of Reviews: ","</strong>", row_selected$Total_Number_of_Reviews,"<br/>",
                           "<strong>","Address: ","</strong>", row_selected$Hotel_Address,"<br/>")
    
    proxy %>% addCircleMarkers(lng=row_selected$lng, 
                               lat=row_selected$lat,
                               color = color_score,
                               stroke = FALSE,
                               radius = ifelse(row_selected$Total_Number_of_Reviews < 1000,
                                               2,
                                               row_selected$Total_Number_of_Reviews*0.003),
                               fillOpacity = 0.8,
                               popup = content_popup)
    
    if (nrow(row_selected) > 1) {
      proxy %>% addLegend(pal = pal_country, values = row_selected$Average_Score,
                          title = "Average Score", position = "bottomleft")}
    })
  
  observeEvent(input$sidebarCollapsed, {
    output$table <- renderDataTable({
      datatable(data()[, c("Hotel_Name", "Hotel_Address", "Average_Score", "Total_Number_of_Reviews", "Country")],
                colnames = c("Hotel Name", "Address", "Average Score", "Total Reviews", "Country"),
                escape = FALSE, rownames = FALSE,
                options = list(pageLength = 5,
                               autoWidth = TRUE,
                               columnDefs = list(list(width = "320px", targets = c(0,1)),
                                                 list(width = "210px", targets = 2),
                                                 list(width = "220px", targets = c(3,4)),
                                                 list(className = "dt-center", targets = "_all"),
                                                 list(className = "dt-head-nowrap", targets = "_all"))
                ))})
  })
  

# Q2
  output$trip_type <- renderPlotly({
    ggplotly(
      ggplot(trip_type_data_overall) + 
        geom_col(aes(x = trip_type, y = n, fill = trip_type, text = paste("Trip Type: ", trip_type, "<br>Count: ", n))) + 
        labs(x = "Trip Type", y = "Count", title = "The Chart of Different Trip Type", fill = "Trip Type") + 
        theme_minimal() + theme(axis.ticks = element_blank()) + theme(legend.position = "right"), 
      tooltip = "text")
  })
  
  output$pie_chart <- renderPlot({
    if (input$Trip_Type == "Business trip") {
      data_p2 <- q2p1 %>%
        group_by(trip_type, score_level) %>%
        summarise(n = n())
      my_label <- filter(data_p2, trip_type == "Business trip")
      my_label_1 = as.vector(my_label$score_level)   
      my_label_2 = paste(my_label_1, " (", round(my_label$n / sum(my_label$n) * 100, 2), "%)", sep = "")  
      ggplot(my_label, aes(x = " ", y = n, fill = score_level)) + 
        geom_bar(stat = "identity", width = 1) + coord_polar(theta = "y") + 
        labs(x = "", y = "", title = "The Pie Chart of Score Level", fill = "Score Level") + 
        theme(axis.ticks = element_blank()) + theme(legend.position = "right") + 
        scale_fill_discrete(breaks = my_label$score_level, labels = my_label_2) + 
        theme_minimal() + theme(axis.text.x = element_blank())
    } else if(input$Trip_Type == "Leisure trip") {
      data_p2 <- q2p1 %>%
        group_by(trip_type, score_level) %>%
        summarise(n = n())
      my_label <- filter(data_p2, trip_type == "Leisure trip")
      my_label_1 = as.vector(my_label$score_level)   
      my_label_2 = paste(my_label_1, " (", round(my_label$n / sum(my_label$n) * 100, 2), "%)", sep = "")   
      ggplot(my_label, aes(x = " ", y = n, fill = score_level)) + 
        geom_bar(stat = "identity", width = 1) +
        coord_polar(theta = "y") + labs(x = "", y = "", title = "The Pie Chart of Score Level", fill = "Score Level") + 
        theme(axis.ticks = element_blank()) + theme(legend.position = "right") + 
        scale_fill_discrete(breaks = my_label$score_level, labels = my_label_2) + 
        theme_minimal() + theme(axis.text.x = element_blank())}
  })
 
  output$No_people<- renderPlotly({
      people_plot <- ggplot(q2p3) + 
        geom_boxplot(aes(x = No_people, y = Reviewer_Score, fill = No_people)) + 
        labs(x= "Number of People", y="Reviewer Score", 
             title="Reviewer Score of Different Number of People Traveled with", 
             fill = "Number of People") + theme_minimal() + 
        theme(legend.position = "right", axis.text.x = element_text(angle = 30))
  }) 
  
  
# Q3
  output$ggplot_a <- renderPlotly({
    layout(ggplotly(
      ggplot(data = a, aes(x = Reviewer_Nationality, y = Reviewer_Score, fill = Reviewer_Nationality))+
        geom_boxplot()+
        scale_fill_brewer(palette = "Set3")+
        theme(panel.background = element_blank(), axis.line = element_line(colour = "black"))+
        #guides(fill = guide_legend(title = "Reviewer Nationality"))+
        xlab("Reviewer Nationality")+
        ylab("Reviewer Score")+
        theme(legend.position = "none")), 
      margin = list(b = 160), xaxis = list(tickangle = 45))})
  
  output$top10 <- renderPlotly({
    ggplotly(
      ggplot(data = top)+
        geom_bar(aes(x = Reviewer_Nationality, y = Difference, fill = Reviewer_Nationality, 
                     text = paste("Reviewer Nationality: ", Reviewer_Nationality, "<br>Difference to Average Score: ", Difference)), stat = "identity", width = 0.9)+
        scale_fill_brewer(palette = "Set3")+
        theme(panel.background = element_blank(), axis.line = element_line(colour = "black"))+
        #guides(fill = guide_legend(title = "Reviewer Nationality"))+
        xlab("Reviewer Nationality")+
        ylab("Difference to Hotel Avg Score")+
        theme(axis.text.x = element_text(size=9), legend.position = "none"), 
      tooltip = "text")})
  
  output$TN <- renderValueBox({valueBox(227, "Total Nationlity", icon = icon("address-card"))})
  
  output$top <- renderValueBox({valueBox("7.4%", "Percentage of Reviewer Nationality higher than 9 points",
                                         icon = icon("chart-pie"),
                                         color = "yellow")})
  
  output$last <- renderValueBox({valueBox("1.3%", "Percentage of Reviewer Nationality lower than 6 points",
                                          icon = icon("chart-pie"), 
                                          color = "green")})

    
# Q5
  observeEvent(input$level_select, {
    if (input$level_select=="Overall") {
      updatePickerInput(session = session, inputId = "country_select", 
                        choices = "All", selected = "All")
    }
    
    if (input$level_select=="By country") {
      updatePickerInput(session = session, inputId = "country_select", 
                        choices = c(as.character(country_names$Reviewer_Nationality)), 
                        selected = c("United Kingdom"))
    }
  }, ignoreInit = TRUE)
  
  output$word_cloud_pos <- renderPlot({
    word_cloud_pos(input$max_word,input$freq ,input$country_select)
  })
  
  output$word_cloud_neg <- renderPlot({
    word_cloud_neg(input$max_word,input$freq ,input$country_select)
  })
  
  output$bar_plot_1_text <- renderPlotly({
    bar_plot_1(input$country_select)
  })
  
  output$bar_plot_2_text <- renderPlotly({
    bar_plot_2(input$country_select)
  })
  
  
# Q4
  output$p1 <- renderPlotly({
    
    if (input$picture == "1") {

        ggplotly(
          ggplot(hotel_all_ave, aes(x=factor(Country), y=avg_score, colour=Name, group=Name, 
                                    text=paste("Hotel Name: ", factor(Name), "<br>Average Score: ", avg_score, "<br>Country: ", Country)))+
            xlab("Country")+
            ylab("Average Score")+
            labs(colour="Hotel Name")+
            geom_line(size=0.42) +
            geom_point(size = 1.26)+
            theme_minimal(), 
          tooltip="text")
      
    } else if (input$picture == "2") {
      
      ggplotly(
        ggplot(hotel_all_ave, aes(x=factor(Name), y=avg_score, colour=Country, group=Country, 
                                  text=paste("Hotel Name: ", factor(Name), "<br>Average Score: ", avg_score, "<br>Country: ", Country)))+
          xlab("Hotel Name")+
          ylab("Average Score")+
          labs(colour="Hotel Name")+
          geom_line(size=0.42) +
          geom_point(size = 1.26)+
          theme_minimal(), 
        tooltip="text")}
    
  })
  
  output$summary <- renderText({
    
    if (input$picture == "1") {
      
      paste0("Average Score Comparison of Certain Brand Hotels among Different Countries")
      
    } else if (input$picture == "2") {
      
      paste0("Average Score Comparison among Certain Brand Hotels in the Same Country")
      
    }})
  
  
}


# Run the application
shinyApp(ui = ui, server = server)