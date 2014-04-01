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
  
  pl <- nPlot(Oy~Parti, color='Parti', data = newdata, type = 'discreteBarChart', id='mainChart')
  pl$set(dom='mainChart')
  pl$chart(tooltipContent = "#! function(key, x, y, e){ 
                                              return '<b>Parti:</b> ' + x + '<br/>  <b>Oy:</b> ' + y + '<br/> <b>Alan:</b>' + e.point.alan
                                             } !#")
  
  pl$yAxis(axisLabel = 'Oy sayisi')
  pl$xAxis(axisLabel = 'Partiler')
  pl$chart(margin = list(left = 100), showValues=T)
  pl
}

plot.scatter <- function(input) {
  newdata <- subset(data, il==input$il & ilce==input$ilce)
  varnames <- names(newdata)
  
  #fix party names
  parties <- varnames[16:32]
  parties <- unlist(strsplit(parties, '_'))
  parties <- toupper(parties[parties != 'oy'])
  names(newdata)[16:32] <- parties
  newdata$katilim <- round((newdata$oy_kullanan_kayitli_secmen/newdata$kayitli_secmen) * 100, 2)
  newdata$overKatilim <- ifelse(newdata$katilim > 100, '> %100', 'Katılım <= %100')
  
  #filter out some attributes
  newdata <- newdata[, c('sandik', 'alan', 'CHP', 'AKP', 'katilim', 'overKatilim')]
  
  pl <- nPlot(CHP~AKP, group='overKatilim', data = newdata, type = 'scatterChart', id='mainChart')
  pl$set(dom='mainChart')
  pl$chart(tooltipContent = "#! function(key, x, y, e){ 
                                               return '<b>AKP:</b> ' + x + '<br/>  <b>CHP:</b> ' + y + '<br/> <b>Alan:</b>' + e.point.alan + ' <br/><b>Sandik:</b>' + e.point.sandik + ' <br/><b>Katilim:</b>%' + e.point.katilim
                                              } !#")
  
  if (input$sizeByKatilim) {
    pl$chart(size = '#! function(d){return (d.katilim/100)} !#')
  }

  pl$yAxis(axisLabel = 'CHP')
  pl$xAxis(axisLabel = 'AKP')
  pl$chart(margin = list(left = 100), color=c('blue', 'green', 'yellow'))
  pl
}


shinyServer(function(input, output, session) {

  output$mainChart <- renderChart({
    if(is.null(input$sandik)) { return(rCharts$new()) }

    if (input$sandik != 'TUMU') {
      plot.sandik.bar(input)
    }
    else {
      plot.scatter(input) 
    }
  
})
  
  output$sidebar <- renderUI({
    div(selectInput("ilce", label = h3("İlçe"), 
                choices = as.list(levels(subset(data, il == input$il, ilce, drop=T))), selected=input$ilce),
    
        selectInput("sandik", label = h3("Sandık"), 
                choices = as.list(c('--TÜMÜ--'='TUMU', 
                                    sort(subset(data, (il == input$il) & (ilce == input$ilce), sandik, drop=T)))
                                  )))
  })

})
