## ui.R
require(rCharts)
#library(googleVis)
library(shinyIncubator)

# Define UI
shinyUI(fluidPage(
  
  titlePanel("2014 Yerel Seçim Sonuçları"),
  
  sidebarLayout(
    sidebarPanel(selectInput("il", label = h3("Şehir"), 
                             choices = as.list(levels(data$il))),
                 uiOutput('sidebar'),
                 conditionalPanel(condition='input.sandik == "TUMU"',
                                  checkboxInput("sizeByKatilim", label = "Katılıma göre boyutlandır", value = TRUE)
                                  )
                 ),
    mainPanel(showOutput('mainChart', 'nvd3'), value='Main'),
    position = "left", fluid=T)
  ))