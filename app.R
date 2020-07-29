# source('dataset.R', local = TRUE)
library(shiny)
library(shinydashboard)
library(DT)

sidebar <- dashboardSidebar(
  sidebarMenu(menuItem("Navigation", tabName="nav", icon = icon("dashboard")),
              menuItem("Map", icon = icon("map-signs"), tabName = "maps"),
              menuItem("Exploration", icon = icon("microscope"), tabName = "exploration",
                       menuSubItem("Data View", tabName = "data_view", icon = icon("table")),
                       menuSubItem("City-Wide Visualization", tabName = "city_viz", icon = icon("signal")),
                       menuSubItem("Heat Map", tabName = "city_heat", icon = icon("fire"))),
              menuItem("Restaurant Visualization", tabName = "rest_viz_start", icon = icon("utensils"),
                       menuSubItem("General Visualizations", tabName = "rest_viz", icon = icon("glasses")),
                       menuSubItem("Restaurant Heat Map", tabName = "rest_heat_map", icon = icon("fire"))),
              menuItem("Text Analysis", tabName = "text_analysis", icon = icon("text-width"))
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "nav",
            h1("Welcome to our Yelp app!"),
            h2("Completed by Xiang Gao, Siqi Hu, Alexander Ilyin, Ziyuan Yan")),
    tabItem(tabName = "maps", class = "active",
            fluidRow(
              box(selectInput("city_name", "City:",
                              choice = c(unique_cities))),
              box(selectInput("base_category", "Category of Business:",
                              choice = c(" ", unique_categories))),
              hr()),
            leafletOutput("yelp", width = 1000, height = 600)
    ),
    tabItem(tabName = "data_view",
            fluidRow(
              DT::dataTableOutput("table_display"))
    ),
    tabItem(tabName = "city_viz",
            fluidRow(
              box(selectInput("city_name_viz", "City:",
                              choice = c(" ", unique_cities))),
              hr(),
              plotOutput("yelp_cityviz_ratings"),
              br(),
              plotOutput("yelp_cityviz_reviews"))
    ),
    tabItem(tabName = "city_heat",
            fluidRow(
              box(selectInput("city_name3", "City:",
                              choice = c(unique_cities)),
                  plotOutput("heat_map_city", width = 1300), height = 500, width = 50)
            )
    ),
    tabItem(tabName = "rest_viz",
            fluidRow(
              box(plotOutput("rest_popularity_reviews"), solidHeader = T, collapsible = T),
              box(plotOutput("rest_popularity_pie"), solidHeader = T, collapsible = T)),
              br(),
              box(selectInput("city_name2", "City:",
                              choice = c(unique_cities)),
                  plotOutput("clustering"), collapsible = T, width = 100)
    ),
    tabItem(tabName = "rest_heat_map",
            fluidRow(
              box(selectInput("city_name5", "City:",choice = c(unique_cities)),
                  plotOutput("rest_heat", width = 1300), height = 500, width = 50))
    ),
    tabItem(tabName = "text_analysis",
            fluidRow(
              box(plotOutput("sentiment_by_cuisine"), solidHeader = T, collapsible = T, height = 600),
              box(selectInput("city_name4", "City:",
                              choice = c(unique_cities)),
                  plotOutput("bigram_wordcloud"), solidHeader = T, collapsible = T, height = 600)
            ))
  )
)

ui <- dashboardPage(
  dashboardHeader(title = "Yelp Data Visualizer"),
  sidebar,
  body,
  skin = 'red'
)


server <- function(input, output, session) {
  table_display <- reactive({
    table_display <- yelp.plot %>%
      select(-city) %>%
      rename(City = `toupper(city)`,
             Business_Name = name, 
             Address = address,
             State = state,
             Latitude = latitude,
             Longitude = longitude,
             Star_Ratings = stars,
             Review_Count = review_count,
             Open_Status = is_open,
             Sub_Categories = categories,
             Base_Category = base_category)
  })
  output$table_display <- DT::renderDataTable({
    datatable(table_display(), options = list(scrollX = T))
  })
  
  category_select <- reactive({
    category_select <- input$base_category
  })
  city_name <- reactive({
    city_name <- input$city_name
  })
  filtered_data <- reactive({
    filtered_data <- yelp %>%
      filter(`toupper(city)` == city_name() & base_category == category_select())
  })
  geoloc <- reactive({
    geoloc <- geocode(city_name())
  })
  output$yelp <- renderLeaflet({
    filtered_data <- filtered_data()
    base_category <- category_select()
    geoloc <- geoloc()
    leaflet(filtered_data) %>%
      setView(lng = geoloc$lon, lat = geoloc$lat, zoom=12) %>%
      addMarkers(lng = ~filtered_data$longitude, lat = ~filtered_data$latitude, 
                 popup = paste(filtered_data$name, "<br>",
                               filtered_data$address, "<br>",
                               filtered_data$stars, "stars", "<br>",
                               filtered_data$review_count, "reviews", "<br>",
                               base_category, "<br>")) %>%
      addTiles()
  })
  city_name_viz <- reactive({
    city_name_viz<- input$city_name_viz
  })
  filtered_data_viz <- reactive({
    filtered_data_viz <- yelp.plot %>%
      filter(`toupper(city)` == city_name_viz()) %>%
      group_by(base_category) %>%
      summarise(total = n(),
                avg_review_count = mean(review_count),
                avg_stars = mean(stars))
  })
  
  output$yelp_cityviz_ratings <- renderPlot({
    data <- filtered_data_viz()
    ggplot(data, aes(base_category, avg_stars, fill = base_category)) + geom_bar(stat = 'identity') +
      xlab("Category") + ylab("Average Rating") + theme(legend.title=element_blank(), axis.text.x = element_text(angle = 90, hjust = 1))
  })
  output$yelp_cityviz_reviews <- renderPlot({
    data <- filtered_data_viz()
    ggplot(data, aes(base_category, avg_review_count, fill = base_category)) + geom_bar(stat = 'identity') +
      xlab("Category") + ylab("Average Review Count") + theme(legend.position="none", axis.text.x = element_text(angle = 90, hjust = 1))
  })
  
  output$rest_popularity <- renderPlot({
    restaurants_full %>%
      filter(cuisine != 'Others') %>%
      group_by(cuisine) %>%
      summarise(n = n()) %>%
      ggplot(aes(x = fct_reorder(cuisine, n), y = n)) + geom_bar(stat = 'identity') + 
      coord_flip() + labs(x = 'Cuisine', y = 'Count', 
                          title = 'Popularity of Restaurants by Quantity')
  })
  
  output$rest_popularity_pie<- renderPlot({
    restaurants_full %>%
      filter(cuisine == 'American'| cuisine == 'Mexican'| cuisine == 'Chinese' | cuisine == 'Japanese' | cuisine == 'Italian') %>%
      group_by(cuisine) %>%
      summarise(n = n()) %>%
      ggplot(aes(x = "", y = n, fill = cuisine)) +
      geom_bar(width = 1, stat = "identity", color = "white") +
      coord_polar("y", start = 0)+
      geom_text(aes(y = n, label = paste0(round(n/sum(n)*100, 2), "%")), position = position_stack(vjust = 0.4), color = "white")+
      theme_void() +
      labs(title = 'Popularity of Restaurants by Quantity', 
           subtitle = "Restaurants Reviewed after 2015")
  })
  
  output$rest_popularity_reviews <- renderPlot({
    restaurants_full %>%
      filter(cuisine != 'Others') %>%
      group_by(cuisine) %>%
      summarise(n = n()) %>%
      ggplot(aes(x = fct_reorder(cuisine, n), y = n)) + geom_bar(stat = 'identity') + 
      coord_flip() + labs(x = 'Cuisine', y = 'Count', 
                          title = 'Popularity of Restaurants by Review Mentions', 
                          subtitle = "Restaurants after 2015")
  })
  
  output$sentiment_by_cuisine <- renderPlot({
    ggplot(topSentiments_review, aes(x = sentiment, y = proportion, fill = sentiment)) + 
       geom_bar(stat = 'identity') + facet_wrap(~cuisine) + 
       labs(title = "Sentiments in Top 5 Cuisine", 
            subtitle = "Restaurants Reviewed after 2015")
  })
  
  city_name2 <- reactive({
    city_name2 <- input$city_name2
  })
  clustering_df <- reactive({
    clustering_df <- restaurants_reviews_full %>%
      select(-c(review_id:text)) %>%
      filter(toupper(city) == city_name2())
  })
  clustered_df <- reactive({
    clustered_df <- restaurants_reviews_full %>%
      select(-c(review_id:text)) %>%
      filter(toupper(city) == city_name2()) %>%
      group_by(toupper(city), neighborhood) %>%
      summarize(total = n())
  })
  clusters <- reactive({
    clustered_df <- clustered_df()
    clustering_df <- clustering_df()
    set.seed(1234)
    clusters <- kmeans(clustering_df[,7:8], nrow(clustered_df)-1)
  })
  output$clustering <- renderPlot({
    if(is.null(input$city_name2)) {return('please wait...')}
    
    clustered_df <- clustered_df()
    clustering_df <- clustering_df()
    clusters <- clusters()
    
    clustering_df$clusters <- as.factor(clusters$cluster)
    
    map <- get_map(city_name2(), zoom = 11)
    ggmap(map) + geom_point(aes(x = clustering_df$longitude, y = clustering_df$latitude, 
                          color = as.factor(clusters)), data = clustering_df) + theme(legend.position = "none") +
                          labs(title = "K-Means Clustering by Neighborhood")

  })
  
  city_name3 <- reactive({
    city_name3 <- input$city_name3
  })
  city_heat_df <- reactive({
    city_heat_df <- yelp %>%
      filter(toupper(city) == city_name3())
  })
  output$heat_map_city <- renderPlot({
    city_heat_df <- city_heat_df()
    map <- get_map(city_name3(), zoom = 11)
    ggmap(map, extent = "device") + 
      geom_density2d(data = city_heat_df, aes(x = longitude, y = latitude), size = 0.3) + 
      stat_density2d(data = city_heat_df, aes(x = longitude, y = latitude, fill = ..level.., alpha = ..level..), 
                     size = 0.01, bins = 16, geom = "polygon") + 
      scale_fill_gradient(low = "green", high = "red") + 
      scale_alpha(range = c(0, 0.3), guide = FALSE) + 
      facet_wrap(~base_category, ncol=5, nrow=2) + 
      labs(x = "Longitude", y = "Latitude", title = "Heatmap of Business Distribution") +
      theme(legend.position = "none")
  })
  
  city_name5 <- reactive({
    city_name5 <- input$city_name5
  })
  rest_heat_df <- reactive({
    rest_heat_df <- restaurants_reviews_full %>%
      filter(toupper(city) == city_name5() & cuisine %in% c("American", "Mexican", "Italian", "Chinese", "Japanese"))
  })
  output$rest_heat<- renderPlot({
    rest_heat_df <- rest_heat_df()
    map <- get_map(city_name5(), zoom = 12)
    ggmap(map, extent = "device") + 
      geom_density2d(data = rest_heat_df, aes(x = longitude, y = latitude), size = 0.3) + 
      stat_density2d(data = rest_heat_df, aes(x = longitude, y = latitude, fill = ..level.., alpha = ..level..), 
                     size = 0.01, bins = 16, geom = "polygon") + 
      scale_fill_gradient(low = "green", high = "red") + 
      scale_alpha(range = c(0, 0.3), guide = FALSE) + 
      facet_wrap(~cuisine) + 
      labs(x = "Longitude", y = "Latitude", title = "Heatmap of Restaurant Distribution") +
      theme(legend.position = "none")
  })
  
  city_name4 <- reactive({
    city_name4 <- input$city_name4
  })
  reviews_by_city <- reactive({
    reviews_by_city <- restaurants_reviews_full %>%
      filter(toupper(city) == city_name4())
  })
  bigram_generate <- reactive({
    stop_words <- stop_words
    reviews_by_city <- reviews_by_city()
    bigrams <- reviews_by_city %>%
      unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%  
      separate(bigram, c("word1", "word2"), sep = " ") %>%               
      filter(
        !word1 %in% stop_words$word, 
        !word2 %in% stop_words$word,
        !str_detect(word2, pattern = "[[:digit:]]"),
        !str_detect(word1, pattern = "[[:punct:]]"), 
        !str_detect(word2, pattern = "[[:punct:]]"),
        !str_detect(word1, pattern = "\\b(.)\\b"),   
        !str_detect(word2, pattern = "\\b(.)\\b")
      ) %>%
      unite("bigram", c(word1, word2), sep = " ") %>%
      count(bigram) %>%
      top_n(30)
  })
  output$bigram_wordcloud <- renderPlot({
    wordcloud(bigram_generate()$bigram,
              bigram_generate()$n,
              scale = c(3,0.6),
              colors = brewer.pal(12, "Paired"))
  })
}

shinyApp(ui = ui, server = server)