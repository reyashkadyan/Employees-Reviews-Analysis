# Employees Reviews Analysis
# Visualisation tool
# By : Reyash Kadyan

# Libraries
library(shiny) 
library(ggplot2) 
library(leaflet) 
library(geojsonio)
library(rgdal)
library(dplyr) 
library(googleVis)
library(rlang)
library(tidyr)
library(tm)
library(plotly)
library(shinythemes)

# shiny - This library is used to build a viusally appealing and interactive shiny app.
# ggplot2 - This library is used to procure beautiful plots of our data, and better visualise our findings.
# leaflet - This library is used to obtain a map plot on our shiny app, to viusalise the location of sites in our data.
# dplyr - This library is used to wrangle our data; 'arrange' and 'mutate' functions are used.
# geojsonio - It is a library used to convert data from ‘GeoJSON’ to ‘TopoJSON’.
# rgdal - It is a 'Geospatial' Data Abstraction Library ('GDAL'). It is used to add polygons on the leaflet maps, according to the boundaries of the countries.
# googleVis - Library used to build interactive and beautiful visualisations, to better understand the data. I used it to build a ‘sankey’ diagram.
# rlang -Library used to work with R conditional panels. I used it to convert strings from UI in shiny app to be used as textual part of code in server.
# tidy - It is used write tidy code and cleaning data. I used ‘%>%’ pipe operator provided in the library.
# tidyr - It is used to write neat and understandable code, and also to wrangle data. 
# tm - Library used for text mining and processing.
# plotly - Library used to make interactive plots. It is used to make motion chart and donut chart.
# shinythemes - Library used to add themes to the application

# Reading reviews data
data <- read.csv('reviews_no_null.csv')
# Reading profits data
profit <- read.csv('Annual_profit.csv')
# Merging two datasets
data <- merge(data,profit, by = c('company','Year'))

# Download .shp file on the web (if not already downloaded in the current working directory)
#download.file("http://thematicmapping.org/downloads/TM_WORLD_BORDERS_SIMPL-0.3.zip" , destfile="world_shape_file.zip")
#system("unzip world_shape_file.zip")

# Read the file with the rgdal library in R
world_spdf <- readOGR( dsn= getwd() , layer="TM_WORLD_BORDERS_SIMPL-0.3")

# # UI OF SHINY APP
ui <- shinyUI( 
  # navbarPage is used to obtain multiple tabs on the app
  navbarPage("Employees reviews analysis",
             theme = shinytheme('sandstone'),
            tabsetPanel(
            # tab 1 - Introduction of the tool
            tabPanel("Introduction",
                     fluidRow(
                       column(7,h3("Overview"),
                              p("In today’s corporate world, every employee wants different things out of their professional career, be it opportunities of growth, cultural belonging, or financial benefits, and with multitude of companies in business, choosing the firms suiting one’s requirements poses a challenge to all. 
                                According to a survey, more than 50% employees in Australia are dissatisfied with their jobs."),
                              br(),
                              p("Major proportion of the freshly graduated students from all field struggles to find a company, best suiting their needs. This visualization will enable you to choose the best firm to suit your requirements, amongst the leader firms (in terms of stock prices) of the market, which are Apple, Amazon, Microsoft, Google, Netflix and Facebook."),
                              br(),
                              h4("Ratings"),
                              p("This tab consists of a choropleth map which will enable you to visualise average ratings for selected company for all country where the reviews came from. It will allow you to locate the best country to work in for any particular company."),
                              br(),
                              h4("Words"),
                              p("This tab consists of stacked bar chart will enable you to visualise most frequent words associated with any firm, both positive and negative, and will allow you to select a company which shares the same values as them."),
                              br(),
                              h4("Profits"),
                              p("This tab consists of motion chart, which will enable you to visualise the changes in ratings with annual profits, and to observe any correlation between firm’s profits and employee satisfaction.")
                              )
                     )),
            # tab 2 - Ratings plots
            # Choropleth map
            # Sankey diagram
            tabPanel("RATINGS",
                     fluidRow(
                       # headings
                       column(7,h3("Overall ratings choropleth map")),
                       column(5, h3("Ratings of companies over the years by Sankey diagram"))
                     ),
                     fluidRow(
                       # description
                       column(7,h5("Here we can see the changes in overall ratings for a company in each country over the years.
                             Click on any country to see its average ratings.")),
                       column(5,h5("Here we can see the changes in different ratings for a company over the years.
                             Hover over any word to see its frequency percentage."))
                     ),
                     fluidRow(
                       column(3,
                              # selecting year as a input from slider
                              sliderInput("map_year", "Select Year",
                                          min = 2008, max = 2018, value = 2008, animate = TRUE)),
                       column(2,
                              # selecting company from a dropdown
                              selectInput("Company_map", "Select Company", 
                                           choices = c("Google" = "google",
                                                       "Microsoft" = "microsoft",
                                                       "Netflix" ="netflix",
                                                       "Amazon" = "amazon",
                                                       "Facebook" = "facebook",
                                                       "Apple" = "apple"), selected = "apple")),
                       column(2, offset = 2,
                              # taking ratings type as input
                              selectInput("Ratings", "Select rating type", 
                                          choices = c("Work balance stars" = "work_balance_stars",
                                                      "Carrer opportunities stars" ="carrer_opportunities_stars",
                                                      "Senior mangemnet stars" = "senior_mangemnet_stars",
                                                      "Company benefit stars" = "comp_benefit_stars"), selected = "work_balance_stars"))
                     ),
                     fluidRow(
                       column(6,
                              # rendering choropleth map
                              h4(textOutput("map_caption")),
                              leafletOutput("mapplot")
                       ),
                       column(5,offset=1,
                              # rendering ratings map
                              h4(textOutput("ratings_caption")),
                              htmlOutput("ratings_plot"))
                     )
      ),
      # tab 3 - Common words plot
      # Donut chart - Top 10 words in a company
      # Radial bar chart - 5 frequent words for all companies
      tabPanel("WORDS", 
               fluidRow(
                 # heading
                 column(7,h3("Frequent words associated in reviews"))
               ),
               fluidRow(
                 # description
                 column(5,h5("Here we can see the top 10 words in a company's reviews.
                             Hover over any word to see its frequency percentage.")),
                 column(7,h5("Here we can see the top 5 words for all companies for a year.
                             Hover over any link to see average ratings for connecting company and year."))
               ),
               fluidRow(
                 # selecting company from a dropdown
                 column(3,
                        selectInput("Company_word", "Select Company", 
                                    choices = c("Google" = "google",
                                                "Microsoft" = "microsoft",
                                                "Netflix" ="netflix",
                                                "Amazon" = "amazon",
                                                "Facebook" = "facebook",
                                                "Apple" = "apple"), selected = "google")),
                 # selecting review type from a dropdown
                 column(3,offset=2,
                        selectInput("review", "Select review", 
                                    choices = c("Summary" = "summary",
                                                "Pros" = "pros",
                                                "Cons" ="cons"), selected = "pros")),
                 # selecting year as a input from slider
                 column(3,
                        sliderInput("word_year", "Select Year",
                                    min = 2008, max = 2018, value = 2008, animate = TRUE))
               ),
               fluidRow(
                 # rendering donut chart
                 column(5,
                        h4(textOutput("word_caption")),
                        plotlyOutput("wordplot")
                 ),
                 # rendering radial bar chart
                 column(7,
                        h4(textOutput("word_caption_2")),
                        plotOutput("wordplot_2"))
               )
      ),
      #tab4 - Profits
      # Motion bubble chart - Annual profits with Ratings
      # Line Chart - Annual profits with Years
      tabPanel("PROFITS", 
               fluidRow(
                 column(7,h3("Ratings of companies with Annual Profits"))
               ),
               fluidRow(
                 column(7,h5("Here we can see chnages in ratings with annual profits made by a firm, over the years.
                             Hover over any bubble to see the Annual profits and average ratings of that company, for the selected year.")),
                 column(5,h5("Here we can see the annual profits in 'Millions Dollars (US)' companies over the years.
                             Hover over any line to see the Annual profits of that company, for the selected year."))
               ),
               fluidRow(
                 # selecting company from a dropdown for Annual profits vs Ratings bubble chart
                 column(3,
                        selectInput("Company_profit_ratings", "Select Company", 
                                    choices = c("All" = "all",
                                                "Google" = "google",
                                                "Microsoft" = "microsoft",
                                                "Netflix" ="netflix",
                                                "Amazon" = "amazon",
                                                "Facebook" = "facebook",
                                                "Apple" = "apple"), selected = "all")),
                 # selecting company from a dropdown for Annual profits
                 column(3,offset=4,
                        selectInput("Company_profit", "Select Company", 
                                    choices = c("All" = "all",
                                                "Google" = "google",
                                                "Microsoft" = "microsoft",
                                                "Netflix" ="netflix",
                                                "Amazon" = "amazon",
                                                "Facebook" = "facebook",
                                                "Apple" = "apple"), selected = "all"))
               ),
               fluidRow(
                 # rendering bubble chart
                 column(7,
                        h4(textOutput("ratings_profits_caption")),
                        plotlyOutput("ratings_profits_plot")
                 ),
                 # rendering radial bar chart
                 column(5,
                        h4(textOutput("profits_caption")),
                        plotlyOutput("profits_plot"))
               )
      ),
      # tab-5 - Inferences 
      # results from my analysis
      tabPanel("Summary",
               fluidRow(
                 column(12,h3("Some inferences..."),
                        br(),
                        h4("Amazon"),
                        p("Amazon seems to be investing moderately towards providing more benefits and opportunities to its employees. 
                          Employees seem to be neutral towards senior management. 
                          It seems that people in Amazon are not contented with their jobs, as the word ‘bad’ pops up a lot. However, one seems to ‘learn’ a lot while working, and are paid good! "),
                        br(),
                        h4("Apple"),
                        p("Apple seems to follow aggressive approach when profit goes down. 
                          Bumps in ratings are in sync with profits, and increases and decreases proportionately with annual profits, which indicates strictness of management when profits go down! 
                          Apple ratings rise when the profits rise.
                          Employees seems to be happy about their ‘pay’. It seems that ‘family’ like ‘culture’ is prevalent."),
                        br(),
                        h4("Facebook"),
                        p("Facebook seem to invest highly towards providing more career opportunities and other benefits to its employees. 
                          Work- Balance satrs seems to be decresing for Facebook. They are losing track of their employees’ work load. 
                          Cultural values are on a setback, with its eyes on the price!"),
                        br(),
                        h4("Google"),
                        p("Google seem to invest highly towards providing more career opportunities and other benefits to its employees. 
                           Work- Balance seems to be constant.
                          Cultural values are on a setback, with its eyes on the price!
                          Google employees seems to be very contented with their jobs as words ‘amazing’ and ‘awesome’ pops up a lot in their reviews."),
                        br(),
                        h4("Microsoft"),
                        p("Microsoft’s has a fairly constant profits over year, and they don’t seem to increase company benefits and work load over employees. 
                          This steady flow seems to satisfy the employees, as the ratings increases with the passage of time.
                          Employee seems to be fed up of offfice 'policts' and 'bureaucracy' of 'management', consistently after year 2008."),
                        br(),
                        h4("Netflix"),
                        p("Netflix ratings seems to vary a lot, with dips in 2010, 2012, and 2015, in overall increasing graphs. 
                          This indicate to unstable working environment, where employees aren’t valued enough.
                          Employees seems to be working in the fear of being fired at any moment!"))
               ))
      
  
)))

# SERVER OF SHINY APP
server <- shinyServer(function(input,output){
  
    # making reactive title for choropleth map
    output$map_caption <- renderText({
      paste(" Ratings of ",input$Company_map,"~ Country, for year",input$map_year)
    })
  
    # making choropleth map
    output$mapplot <- renderLeaflet({
        
      # applying filter from ui to the data
        df <- data[(data$company == input$Company_map) & (data$Year == input$map_year),]
        world_spdf <- readOGR( dsn= getwd() , layer="TM_WORLD_BORDERS_SIMPL-0.3")
        
        # calculating average overall ratings for all countries
        means <- as.data.frame(df %>%
                                 group_by(country) %>%
                                 summarise_at(vars(overall_ratings), funs(mean(., na.rm=TRUE))))
        
        # making color palette
        palette <- colorBin("PuRd", c(0,5))
        
        # adding average overall ratings for all countries to the shape data
        for (c in means$country){
          world_spdf[world_spdf$NAME == c,'VALUE'] = means[means$country == c,'overall_ratings']
        }
        
        # replacing nulls with 0
        world_spdf[is.na(world_spdf$VALUE),'VALUE'] = 0
        
        # leaflet map - coloring countries based on average ratings computed above
        print(leaflet(world_spdf) %>% addTiles() %>% 
          setView( lat=10, lng=0 , zoom=2) %>%
          addPolygons(data = world_spdf, 
                      fillColor = ~colorBin("PuRd", c(0,5))(world_spdf$VALUE),fillOpacity = 0.6,         
                      color = "darkgrey",weight = 1.5,   
                      popup = paste0("<span style='color: salmon;'><strong>Country: </strong></span>",world_spdf$NAME, 
                                     "<br><span style='color: salmon;'><strong>Overall Ratings: </strong></span>",round(world_spdf$VALUE,2))) %>% 
          addLegend( pal=palette, values = world_spdf$VALUE,
                     opacity=0.9, title = "Ratings", position = "bottomleft")
        )
        
      })
    
    # making reactive title for Donut chart
    output$word_caption <- renderText({
      paste("Top 10 words in",input$Company_word, input$review," reviews, for year",input$word_year)
    })
    # making reactive title for Radial bar chart
    output$word_caption_2 <- renderText({
      paste("Top 5 words in ",input$review ,"reviews for year",input$word_year)
    })
    
    # making donut chart
    output$wordplot <- renderPlotly({
      companies <- c("google","amazon","facebook","netflix","apple","microsoft")
      
      # new dataframe to store grouped values
      summary_df <- data.frame(company=factor(),
                               Year=integer(),
                               Word=factor(),
                               Number=integer(),
                               stringsAsFactors=FALSE)
      
      # defining irrelevant words 
      words <- c("work","many","also","really","like","much","lots", "get", "can", "will","always", "still","good","company","amazon","com","place","google","amazon","facebook","netflix","apple","microsoft","great")
      
      # filtering data to the selected year from UI
      year.data <- data %>% filter(Year == input$word_year)
      
      # computing top 10 words for all companies
      for (c in companies){
        company.year.data <- year.data %>% filter(year.data$company == c)
        
        summary <- company.year.data[[input$review]] 
        summary <- Corpus(VectorSource(summary))
        summary <- tm_map(summary, content_transformer(tolower)) # converting to lower case
        summary <- tm_map(summary, removeNumbers) # removing numbers
        summary <- tm_map(summary, removeWords, stopwords("english")) # removing english stop words
        summary <- tm_map(summary, removeWords,words ) # removing irrelevant words
        summary <- tm_map(summary, removePunctuation) # removing punctuation
        summary <- tm_map(summary, stripWhitespace) # removing white spaces
        tdm1 <- TermDocumentMatrix(summary)
        summary <- as.matrix(tdm1)
        summary <- sort(rowSums(summary),decreasing=T) # sorting data frame based on the count of words
        summary_dataframe <- data.frame(Word = names(summary[1:10]),Number=summary[1:10])
        rownames(summary_dataframe) <- NULL
        
        summary_dataframe$Number <- round(summary_dataframe$Number/sum(summary_dataframe$Number)*100)
        company_years <- data.frame(Company = c(rep(c,10)), Year = c(rep(input$word_year,10)))
        # converting grouped data into dataframe
        summary_df <- rbind(summary_df,cbind(company_years,summary_dataframe))
        
      }
      
      d <- data.frame(
        labels = c(input$Company_word,as.character(head(summary_df[summary_df$Company == input$Company_word,'Word'],10))),
        parents = c('',rep(input$Company_word,10)),
        values = c('',head(summary_df[summary_df$Company == input$Company_word,'Number'],10))
      )
      # making donut chart using sunburst in plotly
      p <- plot_ly(d,labels=~labels,parents = ~parents,values = ~values, hoverinfo = ~paste0("Word: ",labels,"<br>Frequency: ",values," %"), 
                   type = 'sunburst')
      p
    })
    
    # making radial bar chart with ggplot 2
    output$wordplot_2 <- reactivePlot(function(){
      companies <- c("google","amazon","facebook","netflix","apple","microsoft")
      
      # new dataframe to store grouped values
      summary_df <- data.frame(company=factor(),
                               Year=integer(),
                               Word=factor(),
                               Number=integer(),
                               stringsAsFactors=FALSE)
      
      # defining irrelevant words 
      words <- c("work","lots","many","also","really","like","much", "get", "can", "will","always", "still","good","company","amazon","com","place","google","amazon","facebook","netflix","apple","microsoft","great")
      
      # filtering data to the selected year from UI
      year.data <- data %>% filter(Year == input$word_year)
      
      # computing top 10 words for all companies
      for (c in companies){
        company.year.data <- year.data %>% filter(year.data$company == c)
        
        summary <- company.year.data[[input$review]]
        summary <- Corpus(VectorSource(summary))
        summary <- tm_map(summary, content_transformer(tolower)) # converting to lower case
        summary <- tm_map(summary, removeNumbers) # removing numbers
        summary <- tm_map(summary, removeWords, stopwords("english")) # removing english stop words
        summary <- tm_map(summary, removeWords,words ) # removing irrelevant words
        summary <- tm_map(summary, removePunctuation) # removing punctuation
        summary <- tm_map(summary, stripWhitespace) # removing white spaces
        tdm1 <- TermDocumentMatrix(summary)
        summary <- as.matrix(tdm1)
        summary <- sort(rowSums(summary),decreasing=T) # sorting data frame based on the count of words
        summary_dataframe <- data.frame(Word = names(summary[1:5]),Number=summary[1:5])
        rownames(summary_dataframe) <- NULL
        
        summary_dataframe$Number <- round(summary_dataframe$Number/sum(summary_dataframe$Number)*100)+100
        
        company_years <- data.frame(Company = c(rep(c,5)), Year = c(rep(input$word_year,5)))
        summary_df <- rbind(summary_df,cbind(company_years,summary_dataframe))
        
      }
      # empty bars
      empty_bar=2
      to_add = data.frame( matrix(NA, empty_bar*length(unique(summary_df$Company)), ncol(summary_df)) )
      colnames(to_add) = colnames(summary_df)
      to_add$Company=rep(unique(summary_df$Company), each=empty_bar)
      summary_df=rbind(summary_df, to_add)
      summary_df=summary_df %>% arrange(Company)
      summary_df$id=seq(1, nrow(summary_df))
      
      # Get the name and the y position of each label
      label_data=summary_df
      number_of_bar=nrow(label_data)
      angle= 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
      label_data$hjust<-ifelse( angle < -90, 1, 0)
      label_data$angle<-ifelse(angle < -90, angle+180, angle)
      
      
      # prepare a data frame for base lines
      base_data=summary_df %>% 
        group_by(Company) %>% 
        summarize(start=min(id), end=max(id) - empty_bar) %>% 
        rowwise() %>% 
        mutate(title=mean(c(start, end)))
      
      # prepare a data frame for grid (scales)
      grid_data = base_data
      grid_data$end = grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
      grid_data$start = grid_data$start - 1
      grid_data=grid_data[-1,]
      
      max <- max(as.vector(summary_df$Number),na.rm = TRUE)
      
      # adjustments for labels
      hjust = c(1,1,1,0,0,0)
      
      # Make the plot
      q = ggplot(summary_df, aes(x=as.factor(id), y=Number, fill=Company)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
        
        geom_bar(aes(x=as.factor(id), y=Number, fill=Company), stat="identity", alpha = 0.8) +
        ylim(-100,max) +
        theme_minimal() +
        theme(
          legend.position = "none",
          axis.text = element_blank(),
          axis.title = element_blank(),
          panel.grid = element_blank(),
          plot.margin = unit(rep(-1,4), "cm") 
        ) +
        coord_polar() + 
        geom_text(data=label_data, aes(x=id, y=Number-100, label=paste(Word," (",Number-100,"%)"), hjust=hjust), color="black", fontface="bold",alpha=0.6, size=2.5, angle= label_data$angle, inherit.aes = FALSE ) +
        theme(
          legend.position = c(1,0.95),
          legend.justification = c("right", "top")
        )+
        geom_segment(data=base_data, aes(x = start, y = -5, xend = end, yend = -5), colour = "black", alpha=0.8, size=0.6 , inherit.aes = FALSE )  +
        geom_text(data=base_data, aes(x = title, y = -10, label=Company),hjust = hjust, colour = "black", alpha=0.8, size=3, fontface="bold", inherit.aes = FALSE)

      q
    })
    
    # reactive title for sankey diagram
    output$ratings_caption <- renderText({
      paste(" Company",input$Ratings,"~ Year")
    })
    
    #  Grouping data for sankey diagram and bubble chart
    DataSet <- reactive({
      dataset <- data %>% group_by(company,Year) %>% select(company,Year,(rlang::as_name(input$Ratings))) %>% summarize_at(.vars = input$Ratings, .funs = mean) %>% rename('Average'=input$Ratings) %>% subset(Average>0)
      dataset$Year = as.factor(dataset$Year)
      dataset
    })
    
    # making sankey diagram
    output$ratings_plot <- renderGvis(
      gvisSankey(DataSet(), from="company", to="Year", weight="Average",
                           options=list(
                             sankey="{link: { colorMode: 'source',colors: ['deeppink','grey','blue','green','orange','red'] },node: { colors: ['green','green','green','green','green'] }}"))
    )
    
    # reactive title for motion bubble chart
    output$ratings_profits_caption <- renderText({
      paste ("Ratings of", input$Company_profit_ratings,"~ Annual profits (USD 'Millions)")
    })
     
    # making motion bubble chart with plotly
    output$ratings_profits_plot <- renderPlotly({
      
      # grouping and filtering data
      profit_ratings <- data %>% group_by(company,Year,Millions_dollars_US) %>% 
        select(company,Year,Millions_dollars_US,overall_ratings) %>% 
        summarise(Average_ratings=mean(overall_ratings))
      
      if (input$Company_profit_ratings != "all"){
        profit_ratings <- profit_ratings[profit_ratings$company == input$Company_profit_ratings,]
      }
      # making motion chart
      p <- profit_ratings %>% ungroup() %>%
        plot_ly(
          x = ~Millions_dollars_US, 
          y = ~Average_ratings, 
          size = ~Millions_dollars_US, 
          color = ~company, 
          frame = ~Year,
          text = ~paste("Ratings: ", Average_ratings, "<br>Annual Profit: $", Millions_dollars_US," Millions"),
          hoverinfo = 'text',
          type = 'scatter',
          mode = 'markers'
        ) %>%
        layout(
          xaxis = list(
            type = "log"
          )
        ) %>% 
        animation_opts(
          1000,easing = "elastic", redraw = FALSE
        ) %>% 
        animation_slider(
          currentvalue = list(prefix = "YEAR ", font = list(color="red"))
        ) %>% layout(title = "Ratings of Companies with Annual Profit", 
                     yaxis = list(title = "Ratings"), 
                     xaxis = list(title = "Annual Profit (Millions)"),
                     plot_bgcolor='rgba(255,255,255,255)')
      p
    })
    
    # reactive title  for line chart of annual profits
    output$profits_caption <- renderText({
      paste("Profit of",input$Company_profit,"~Year") })
    
    # making line chart of annual profits
    output$profits_plot <- renderPlotly({
      # grouping and filtering data for bubble chart
      profit_ratings <- data %>% group_by(company,Year,Millions_dollars_US) %>% 
        select(company,Year,Millions_dollars_US,overall_ratings) %>% 
        summarise(Average_ratings=mean(overall_ratings)) %>% ungroup()
      
      if (input$Company_profit != "all"){
        profit_ratings <- profit_ratings[profit_ratings$company == input$Company_profit,]
      }
      
      # making line chart
      profit <- profit_ratings %>% plot_ly(y=~Millions_dollars_US,
                                            x=~Year,
                                            color = ~company,
                                            text = ~paste("Year: ", Year, "<br>Annual Profit: $", Millions_dollars_US," Millions"), 
                                            hoverinfo = 'text',
                                            type = 'scatter', mode='lines') %>% 
        layout(xaxis = list(title = "Year"), 
               yaxis = list(title = "Annual Profit (Millions)"))
      profit
      })
})
    
shinyApp(ui,server)
