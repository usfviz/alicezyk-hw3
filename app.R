library(shiny)
library(colorspace)
library(MASS)
library(ggplot2)
library(plotly)
library(pairsD3)
library(GGally)
library(directlabels)
library(ggthemes)

#check and install required packages
libs <- c('colorspace','plotly','pairsD3','GGally','directlabels','ggthemes')

for (lib in libs) {
  if (!require(lib,character.only = TRUE))
  {
    install.packages(lib,dep=TRUE)
  }
}

#setwd("/Users/Alice/workdata/data_viz_hw3")
data <- read.csv("dataset_Facebook.csv",sep = ";")

data$Post.Weekday <- as.factor(data$Post.Weekday)
levels(data$Post.Weekday) <- c('Mon','Tue','Wed','Thu','Fri','Sat','Sun')


ui <-fluidPage(
  
  # Application title
  titlePanel("Facebook Metrics for one brand in 2014"),
  
  # Sidebar with a slider input for month
  sidebarLayout(
    sidebarPanel(
      sliderInput("month",
                  "Month:",
                  min = 1,
                  max = 12,
                  value = 1,
                  animate = T)),
    mainPanel(
      tabsetPanel(
        tabPanel("bubble Plot", plotlyOutput("bubbleplot")),
        tabPanel("matrix Plot", pairsD3Output("matrixplot", width = '1000', height = '500')),
        tabPanel("parallel coordinates plot", plotOutput("parellelPlot"))
      )
    )
  )
)

server <- function(input, output) {
  
   #plot for the 1st tab
   output$bubbleplot <- renderPlotly({
     subset <- data[data$Post.Month == input$month,c("Lifetime.Post.Total.Impressions", "Total.Interactions","Post.Weekday")]
     grouped <- aggregate(subset, list(subset$Post.Weekday), mean)
     counted <- aggregate(subset, list(subset$Post.Weekday), length)
     colnames(grouped)[1] <- 'Post.Weekday'
     data <- cbind(grouped[,1:3], counted[,2])
     data$Post.Weekday <- as.factor(data$Post.Weekday)
     levels(data$Post.Weekday) <- c('Mon','Tue','Wed','Thu','Fri','Sat','Sun')
     colnames(data)[4] <- 'count'
     plot_ly(data, x = ~Lifetime.Post.Total.Impressions, y = ~Total.Interactions, 
             color = ~as.factor(Post.Weekday), size = ~count, colors = rev(rainbow_hcl(7)),
             type = 'scatter', mode = 'markers', sizes = c(10, 50),
             marker = list(symbol = 'circle', sizemode = 'diameter',opacity = 0.6,
                           line = list(width = 2, color = '#FFFFFF')), 
             hoverinfo = 'text',
             text = ~paste('Weekday:', Post.Weekday, '<br>Avg Interactions:', Total.Interactions, '<br>Avg Impression:', Lifetime.Post.Total.Impressions, '<br># of posts:', count)
     )%>%
       add_annotations(x = data$Lifetime.Post.Total.Impressions,
                       y = data$Total.Interactions,
                       text = data$Post.Weekday,
                       xref = "x",
                       yref = "y",
                       showarrow = FALSE) %>%
     layout(title = 'Total Interactions v.Total Impressions',
            xaxis = list(range = c(0,70000),title = 'Average Total Impressions'),
            yaxis = list(range = c(0,600),title = 'Average Total Interactions'),
            showlegend = FALSE)
  
    })
   
   #plot for the 2nd tab
   output$matrixplot <- renderPairsD3(
     pairsD3(data[data$Post.Month == input$month,c("like","comment","share","Post.Weekday")],
             group=data[data$Post.Month == input$month,'Post.Weekday']))

   #plot for the 3rd tab
   output$parellelPlot <- renderPlot({
     subset <- data[data$Post.Month == input$month,c("like","comment","share","Post.Weekday")]
     grouped <- aggregate(subset, list(subset$Post.Weekday), mean)
     colnames(grouped)[1] <- 'Weekday'
     ggparcoord(data = grouped, columns = 2:4, groupColumn = 1, scale = 'uniminmax') +
       theme_calc()+ scale_colour_calc(guide=FALSE) +  
       theme(axis.title = element_blank()) + 
       ggtitle("Average # of Like, Comment and Share by Weekday") + 
       geom_dl(aes(label = Weekday), method = list(dl.combine("first.points", "last.points"), cex = 0.8))
   })
   }

# Run the application 
shinyApp(ui = ui, server = server)
