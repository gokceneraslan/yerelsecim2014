## server.r
require(rCharts)
# library(googleVis)
library(plyr)
library(reshape2)

plot.sandik.bar <- function(input) {
  newdata <- subset(data, il==input$il & ilce==input$ilce & sandik==input$sandik)
  varnames <- names(newdata)
  
  #fix party names
  parties <- varnames[16:32]
  parties <- unlist(strsplit(parties, '_'))
  parties <- toupper(parties[parties != 'oy'])
  names(newdata)[16:32] <- parties
  zeros <- which(newdata[16:32] == 0)
  parties <- parties[-zeros]
  newdata[,-(zeros + 15)]
  
  newdata <- melt(newdata, measure.vars=parties, variable.name='Parti', value.name='Oy')
  
  pl <- rPlot(Oy~Parti,  color='Parti', data = newdata, type = 'bar', 
              tooltip = "#! function(e){return 'Oy: ' + e.Oy + '  Parti: ' + e.Parti + ' Alan: ' + e.alan} !#")
  pl$set(dom='mainChart')
  
  pl
}

plot.scatter <- function(input, all=F) {
  
  if (all)
    newdata <- subset(data, il==input$il)
  else 
    newdata <- subset(data, il==input$il & ilce==input$ilce)
  
  varnames <- names(newdata)

  #fix party names
  parties <- varnames[16:32]
  parties <- unlist(strsplit(parties, '_'))
  parties <- toupper(parties[parties != 'oy'])
  names(newdata)[16:32] <- parties
  newdata$KatılımYüzde <- round((newdata$oy_kullanan_kayitli_secmen/newdata$kayitli_secmen) * 100, 2)
  newdata$FazlaKatılım <- factor(ifelse(newdata$KatılımYüzde > 99, '> %99 Katılım', '< %99 Katılım'))
  
  #filter out some attributes
  newdata <- newdata[, c('sandik', 'alan', 'CHP', 'AKP', 'KatılımYüzde', 'FazlaKatılım')]
    
  if (input$sizeByKatilim) {
    size='KatılımYüzde'
  } else {
    size=list(const=3)
  }

  pl <- rPlot(CHP~AKP,  data = newdata, type = 'point', color='FazlaKatılım', size=size, tooltip = "#! function(e){return 'AKP: ' + e.AKP + '  CHP: ' + e.CHP + ' Alan:' + e.alan + ' Sandik:' + e.sandik + ' Katilim:%' + e.KatılımYüzde} !#")
  
  pl$set(dom='mainChart', height=500, width=800)
  pl
}


shinyServer(function(input, output, session) {

  output$mainChart <- renderChart({
    if(is.null(input$sandik)) { return(rCharts$new()) }
    
     if (input$ilce == 'TUMU') {
       plot.scatter(input, all=T)
     }
     else if (input$sandik != 'TUMU') {
      plot.sandik.bar(input)
    }
    else {
      plot.scatter(input) 
    }
  
})
  
  output$sidebar <- renderUI({
    sandiklar <- sort(subset(data, (il == input$il) & (ilce == input$ilce), sandik, drop=T))
    sandik.options <- c('--TÜMÜ--'='TUMU')
    
    if (is.null(input$ilce) || input$ilce != 'TUMU') {
      sandik.options <- c(sandik.options, sandiklar)
      sandik.input <- selectInput("sandik", label = h3("Sandık"), 
                                  choices = as.list(sandik.options))
    } else {
      sandik.input <- div()
    }
    
    div(selectInput("ilce", label = h3("İlçe"), 
                choices = as.list(c('--TÜMÜ--'='TUMU', levels(subset(data, il == input$il, ilce, drop=T)))), selected=input$ilce),    
        sandik.input
        )
  })

})
