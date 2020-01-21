# Discription
# j is list of tenures/columns selected by user (via dcc multi dropdown)
# n is tenures/columns selected by user (via dcc single dropdown)
# difference is number provided by user (via dcc input)

library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)
library(dashTable)
library(plotly)
library(ggplot2)# For plotting
library(ggthemes)
library(reshape2)
library(stats)
library(fitdistrplus)
library(ggfortify)
library(tseries)
library(rmutil) #for laplace
library(stats) #for norm,lnorm,t
library(zoo)
library(CDFt)
library(dplyr)
library(lawstat)
library(goft)
#devtools::install_github("jcizel/FredR")
#library("FredR")
#library(SMFI5) #for vasicek


filteri = function(Data)
  ### Basic Cleaning.
{
  Data= as.data.frame(Data)
  Data= subset(Data, Data[,2]!=".")
  Data$date = as.Date(paste(Data$date), format = "%Y-%m-%d")
  Data[,c(2:13)] = apply(Data[,c(2:13)], 2, function(x) as.numeric(as.character(x)))
  Data[,c(2:13)] = apply(Data[,c(2:13)],2,function(x) round(x, 2))
  
  cols <- c("Date","1_Mon", "2_Mon", "3_Mon", "4_Mon", "5_Mon", "6_Mon", "7_Mon", "8_Mon", "9_Mon", "10_Mon", "11_Mon","12_Mon")
  Data = setNames(Data, cols)
  return (Data)
}

diff = function(COPY,difference,j)
  ### Differencing the data by day.
{
  #difference gives us the lag we want in our data (via dcc input)
  if (difference == 0){
    COPY$Date = as.Date(paste(COPY$Date), format = "%Y-%m-%d")
    
    return (COPY)
  } else {
    
    Day <- data.frame("Date" = c(COPY$Date))
    Day$Date = as.Date(paste(Day$Date), format = "%Y-%m-%d")
    
    for (i in j) {
      Day[,i] = COPY[,i] - dplyr::lag(COPY[,i], n = as.integer(difference))
    }
    row.has.na = apply(Day, 1, function(x){any(is.na(x))})
    Day = Day[!row.has.na,]
    return (Day)
  }
}

gen_table <- function(cnames,rnames,datamatrx)
  ### function generates a plotly table from a supplied data matrix (datamatrix)
{
  p <- plot_ly(
    type = 'table',
    header = list(
      values = cnames,
      line = list(color = '#506784'),
      fill = list(color = "#2b608a"),
      align = c('left','center'),
      font = list(color = '#7fafdf', size = 12)
    ),
    cells = list(
      values = rbind(rnames, t(datamatrx)),
      line = list(color = '#506784'),
      fill = list(color = c("rgb(231, 245, 255)", 'white')),
      align = c('left', 'center'),
      font = list(color = c('#7fafdf'), size = 12)
    ))
  
  return(p)
}

generate_table <- function(data)
  ### function generates a plotly table of head&Tail from a supplied data frame
{
  l = as.integer(length(data[,1])/2)
  k = l+10
  middle = data[l:k,]
  df <- rbind(head(data),middle,tail(data))
  test=as.POSIXlt(df$Date, format = "%Y-%m-%d")
  df$Date = 1900+test$year+test$yday/366
  
  cnames = c(colnames(df))
  rnames = df$Date
  datamatrx = as.matrix(df[,-which(names(df) == "Date")])
  
  p = gen_table(cnames,rnames,datamatrx)
  return(p)
}

pie <- function(df)
  ### function generates a plotly pie of Columnar Values count, important for knowing NULL values
{
  na_count <-sapply(df, function(y) length(y)-sum(length(which(is.na(y)))))
  df = data.frame("na"=na_count)
  dfn = data.frame("Tenure"=rownames(df),"Count"=df[,"na"])
  my = names(COPY) %in% c("Date")
  #my = names(COPY) %in% c("1_Mon", "2_Mon", "3_Mon", "4_Mon", "5_Mon", "6_Mon", "7_Mon", "8_Mon", "9_Mon", "10_Mon", "11_Mon","1_Year")
  dfn <- dfn[!(dfn$Tenure %in% my),]
  
  colls = list("white","#003399","#5cf79c","#9A6324","#E8296A","#00ffff","#94FF11",
               "#D011FF","#FF11E7","#f58231","#fffac8","#DAE829","#469990")
  
  p <- plot_ly(dfn, labels = ~dfn[,"Tenure"], values = ~dfn[,"Count"], type = 'pie',hole = 0.4,marker = list(colors = colls,
                                                                                                             line = list(color = '#FFFFFF', width = 1))
  ) %>%
    layout(title = 'Count of #Data_points',height = "350",
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           annotations = list(x = 1.5, y = -0.1, text = "Missing values/discontinued tenures",
                              showarrow = F, xref='paper', yref='paper',
                              xanchor='right', yanchor='auto', xshift=0, yshift=0,
                              font=list(size=15, color="#2b608a"))
    )
  
  return(p)
}

#Not Used, but a simpler 3d
three_d1 = function(COPY){
  
  COPY$Date = as.Date(paste(COPY$Date), format = "%Y-%m-%d")
  Interest_rate = as.matrix(COPY[,-which(names(COPY) == "Date")])
  
  p<- plot_ly(z = ~Interest_rate, y = COPY$Date, x = colnames(COPY),colorscale = list(
    list(0,"rgb(231, 245, 255)"),
    list (1,"rgb(31, 119, 180)")),
    colors = gdocs_pal()(3)) %>%
    add_surface() %>%
    layout(title = "LIBOR <br />Source: St. Louis FRED",
           height = 600,
           scene = list(
             xaxis = list(title = "TENURE"),
             yaxis = list(title = "Date" ),
             zaxis = list(title = "Percent")))
  return(p)
}

## 3D plot
three_d = function(COPY)
  ### function generates a plotly 3d plot of Tenures.
  #Code adopted from plotly website itself.
{
  COPY$Date = as.Date(paste(COPY$Date), format = "%Y-%m-%d")
  Interest_rate = as.matrix(COPY[,-which(names(COPY) == "Date")])
  trace1 <- list(
    name = "Interest Rates", type = "surface",
    x = c(colnames(COPY[,-which(names(COPY) == "Date")])),
    y = COPY$Date,
    z = ~Interest_rate,
    inherit = FALSE,
    colorbar = list(title = "z"),
    lighting = list(ambient = 0.95, diffuse = 0.99, fresnel = 0.01, specular = 0.01, roughness = 0.01),
    showscale = FALSE,
    colorscale = list(c(0, "rgb(231, 245, 255)"),list(1, "rgb(31, 119, 180)"))
  )
  data <- list(trace1)
  layout <- list(
    scene = list(
      type = "surface",
      xaxis = list(type = "category", title = "TENURE", showgrid = TRUE, zeroline = FALSE),
      yaxis = list(type = "date", title = "Date", showgrid = TRUE, zeroline = FALSE),
      zaxis = list(title = "Libor_rate (%)", showgrid = TRUE, zeroline = FALSE, ticksuffix = "%"),
      camera = list(up = list(x = 0, y = 0, z = 1),
                    eye = list(x = -2.7, y = -0.03, z = 1.3),
                    center = list(x = 0, y = 0, z = 0)),
      aspectmode = "manual",
      aspectratio = list(x = 1, y = 4, z = 2)
    ),
    title = "LIBOR <br />Source: St. Louis FRED",
    height = 600,
    margin = list(b = 60, l = 20, r = 20, t = 60),
    autosize = FALSE,
    showlegend = FALSE)
  
  p <- plot_ly()
  p <- add_trace(p, name=trace1$name, type=trace1$type, x=trace1$x, y=trace1$y, zmax=trace1$zmax, zmin=trace1$zmin, z=trace1$z, inherit=trace1$inherit, colorbar=trace1$colorbar, lighting=trace1$lighting, showscale=trace1$showscale, colorscale=trace1$colorscale, colorscale.1=trace1$colorscale.1)
  p <- layout(p, scene=layout$scene, title=layout$title, height=layout$height, margin=layout$margin, autosize=layout$autosize, showlegend=layout$showlegend)
  
  return(p)
}

# 2D line plot
two_d = function(COPY)
  ### function generates a plotly 2d plot of Tenures using plotly alone
{
  colors = list("Date"= "white","1_Mon"= "#003399","2_Mon"= "#00ffff","3_Mon"= "#5cf79c","4_Mon"= "#D011FF","5_Mon"= "#FF11E7",
                "6_Mon"= "#9A6324","7_Mon" = "#f58231","8_Mon" = "#fffac8","9_Mon" = "#94FF11","10_Mon" = "#DAE829",
                "11_Mon" = "#469990","12_Mon" = "#E8296A")
  
  COPY$Date = as.Date(paste(COPY$Date), format = "%Y-%m-%d")
  mind = min(as.integer(format(COPY$Date,"%Y")))
  maxd = max(as.integer(format(COPY$Date,"%Y")))
  miny = min(COPY[,"1_Mon"])-1
  maxy = max(COPY[,"1_Mon"])+1
  rang = c(miny,maxy)
  rand = c(mind, maxd)
  test=as.POSIXlt(COPY$Date, format = "%Y-%m-%d")
  COPY[,"Num_Date"] = 1900+test$year+test$yday/366
  
  d2 = list(data = list(list(x = COPY$Num_Date,y = COPY[,"1_Mon"],line = list(color = colors[["1_Mon"]]), name = "1 Month", type = "scatter"),
                        list(x = COPY$Num_Date,y = COPY[,"2_Mon"],line = list(color = colors[["2_Mon"]]), name = "2 Month", yaxis = "y2"),
                        list(x = COPY$Num_Date,y = COPY[,"3_Mon"],line = list(color = colors[["3_Mon"]]), name = "3 Month", yaxis = "y3"),
                        list(x = COPY$Num_Date,y = COPY[,'4_Mon'],line = list(color = colors[["4_Mon"]]), name = "4 Month", yaxis = "y4"),
                        list(x = COPY$Num_Date,y = COPY[,'5_Mon'],line = list(color = colors[["5_Mon"]]), name = "5 Month", yaxis = "y5"),
                        list(x = COPY$Num_Date,y = COPY[,'6_Mon'],line = list(color = colors[["6_Mon"]]), name = "6 Month", yaxis = "y6"),
                        list(x = COPY$Num_Date,y = COPY[,'7_Mon'],line = list(color = colors[["7_Mon"]]), name = "7 Month", yaxis = "y7"),
                        list(x = COPY$Num_Date,y = COPY[,'8_Mon'],line = list(color = colors[["8_Mon"]]), name = "8 Month", yaxis = "y8"),
                        list(x = COPY$Num_Date,y = COPY[,'9_Mon'],line = list(color = colors[["9_Mon"]]), name = "9 Month", yaxis = "y9"),
                        list(x = COPY$Num_Date,y = COPY[,'10_Mon'],line = list(color = colors[["10_Mon"]]), name = "10 Month", yaxis = "y10"),
                        list(x = COPY$Num_Date,y = COPY[,'11_Mon'],line = list(color = colors[["11_Mon"]]), name = "11 Month", yaxis = "y11"),
                        list(x = COPY$Num_Date,y = COPY[,'12_Mon'],line = list(color = colors[["12_Mon"]]), name = "12 Month", yaxis = "y12")),
            layout = list(height = "350",autosize = TRUE,legend = list(x = 0.9, y = 0.30969810073003856,
                                                                       bgcolor = "rgba(255, 255, 255, 0)", font = list(size = 8)),
                          margin = list(r = 10, t = 10, b = 40, l = 30), showlegend = TRUE,
                          xaxis = list(autorange = FALSE, nticks = 24, range = rand, showgrid = FALSE, showline = TRUE, tickfont = list(size = 9), ticks = "outside",
                                       titlefont = list(color = "rgb(92, 53, 143)"), type = "linear", zeroline = FALSE),
                          
                          yaxis = list(range = rang, tickfont = list(size = 8)),
                          yaxis2 = list(anchor = "x",overlaying = "y", range = rang, side = "right", tickfont = list(size = 8), ticks = "outside", type = "linear", zeroline = FALSE),
                          yaxis3 = list(anchor = "x",overlaying = "y", range = rang, side = "right", tickfont = list(size = 8), ticks = "outside", type = "linear", zeroline = FALSE,
                                        autorange = FALSE, nticks = 12 , showgrid = FALSE, showline = TRUE),
                          yaxis4 = list(anchor = "x",overlaying = "y", range = rang, side = "right", tickfont = list(size = 8), ticks = "outside", type = "linear", zeroline = FALSE,
                                        autorange = FALSE, nticks = 12 , showgrid = FALSE, showline = TRUE),
                          yaxis5 = list(anchor = "x",overlaying = "y", range = rang, side = "right", tickfont = list(size = 8), ticks = "outside", type = "linear", zeroline = FALSE,
                                        autorange = FALSE, nticks = 12 , showgrid = FALSE, showline = TRUE),
                          yaxis6 = list(anchor = "x",overlaying = "y", range = rang, side = "right", tickfont = list(size = 8), ticks = "outside", type = "linear", zeroline = FALSE,
                                        autorange = FALSE, nticks = 12 , showgrid = FALSE, showline = TRUE),
                          yaxis7 = list(anchor = "x",overlaying = "y", range = rang, side = "right", tickfont = list(size = 8), ticks = "outside", type = "linear", zeroline = FALSE,
                                        autorange = FALSE, nticks = 12 , showgrid = FALSE, showline = TRUE),
                          yaxis8 = list(anchor = "x",overlaying = "y", range = rang, side = "right", tickfont = list(size = 8), ticks = "outside", type = "linear", zeroline = FALSE,
                                        autorange = FALSE, nticks = 12 , showgrid = FALSE, showline = TRUE),
                          yaxis9 = list(anchor = "x",overlaying = "y", range = rang, side = "right", tickfont = list(size = 8), ticks = "outside", type = "linear", zeroline = FALSE,
                                        autorange = FALSE, nticks = 12 , showgrid = FALSE, showline = TRUE),
                          yaxis10 = list(anchor = "x",overlaying = "y", range = rang, side = "right", tickfont = list(size = 8), ticks = "outside", type = "linear", zeroline = FALSE,
                                         autorange = FALSE, nticks = 12 , showgrid = FALSE, showline = TRUE),
                          yaxis11 = list(anchor = "x",overlaying = "y", range = rang, side = "right", tickfont = list(size = 8), ticks = "outside", type = "linear", zeroline = FALSE,
                                         autorange = FALSE, nticks = 12 , showgrid = FALSE, showline = TRUE),
                          yaxis12 = list(anchor = "x",overlaying = "y", range = rang, side = "right", tickfont = list(size = 8), ticks = "outside", type = "linear", zeroline = FALSE,
                                         autorange = FALSE, nticks = 12 , showgrid = FALSE, showline = TRUE)
            ))
  return (d2)
  
}

#Alternate fct for 2d plot using ggplot
# we use it for vasicek in the end
d2 = function(COPY)
  ### function generates a 2d plot using ggplot
{
  #COPY$Date = as.Date(paste(COPY$Date), format = "%Y-%m-%d")
  
  colls = list("Date"= "white","1_Mon"= "#003399","2_Mon"= "#00ffff","3_Mon"= "#5cf79c","4_Mon"= "#D011FF","5_Mon"= "#FF11E7",
               "6_Mon"= "#9A6324","7_Mon" = "#f58231","8_Mon"="#fffac8","9_Mon"="#94FF11","10_Mon" = "#DAE829",
               "11_Mon" = "#469990","12_Mon" = "#E8296A")
  
  ##Approach One: Hardcoding for each tenure
  g = ggplot(COPY, aes(x=COPY$Date)) +
    geom_line(aes(y = COPY[,"1_Mon"]), color = colls[["1_Mon"]])+
    geom_line(aes(y = COPY[,"2_Mon"]), color = colls[["2_Mon"]])+
    geom_line(aes(y = COPY[,"3_Mon"]), color = colls[["3_Mon"]])+
    geom_line(aes(y = COPY[,"4_Mon"]), color = colls[["4_Mon"]])+
    geom_line(aes(y = COPY[,"5_Mon"]), color = colls[["5_Mon"]])+
    geom_line(aes(y = COPY[,"6_Mon"]), color = colls[["6_Mon"]])+
    geom_line(aes(y = COPY[,"7_Mon"]), color = colls[["7_Mon"]])+
    geom_line(aes(y = COPY[,"8_Mon"]), color = colls[["8_Mon"]])+
    geom_line(aes(y = COPY[,"9_Mon"]), color = colls[["9_Mon"]])+
    geom_line(aes(y = COPY[,"10_Mon"]), color = colls[["10_Mon"]])+
    geom_line(aes(y = COPY[,"11_Mon"]), color = colls[["11_Mon"]])+
    geom_line(aes(y = COPY[,"12_Mon"]), color = colls[["12_Mon"]])+
    theme_light()+
    xlab("Date") +
    ylab("Libor_rate (%)") +
    ggtitle("LIBOR_rate:- 12 maturities")
  
  ##Approach TWo: More flexible, bcuz this allows data columns to be changed
  librates <- melt(COPY, id.vars = 'Date', variable.name = 'variable')
  k = ggplot(librates, aes(Date,value)) +
    geom_line(aes(colour = variable))+
    theme_light()+
    xlab("Date") +
    ylab("Libor_rate (%)") +
    ggtitle("LIBOR_rate:- 12 maturities")
  
  k = ggplotly(k)
  
  return(k)
}

diff_plot = function(COPY,j)
  ### function generates a plotly 2d plot, for day_differenced and columnar subsetted data...
  ### This is flexible in turns of columns supplies as this approach is hardcoded by each column
{
  # j is list of tenures/columns selected by user (via dcc multi dropdown)
  COPY$Date = as.Date(paste(COPY$Date), format = "%Y-%m-%d")
  mind = min(as.integer(format(COPY$Date,"%Y")))
  maxd = max(as.integer(format(COPY$Date,"%Y")))
  miny = min(COPY[,"1_Mon"])-1
  maxy = max(COPY[,"1_Mon"])+1
  rand = c(mind,maxd)
  ranr = c(miny,maxy)
  
  test=as.POSIXlt(COPY$Date, format = "%Y-%m-%d")
  COPY[,"Num_Date"] = 1900+test$year+test$yday/366
  
  colls = list("Date"= "white","1_Mon"= "#003399","2_Mon"= "#00ffff","3_Mon"= "#5cf79c","4_Mon"= "#D011FF","5_Mon"= "#FF11E7",
               "6_Mon"= "#9A6324","7_Mon" = "#f58231","8_Mon" = "#fffac8","9_Mon" = "#94FF11","10_Mon" = "#DAE829",
               "11_Mon" = "#469990","12_Mon" = "#E8296A")
  
  p<- plot_ly(COPY, x = COPY$Num_Date, y = COPY[,j[[1]]],color = colls[[j[[1]]]],type = 'scatter',name = j[1],mode='lines')%>%
    layout(height = "350",autosize = TRUE,legend = list(x = 0.9, y = 0.30969810073003856, bgcolor = "rgba(255, 255, 255, 0)", font = list(size = 8)),
           margin = list(r = 10, t = 10, b = 40, l = 30), showlegend = TRUE,
           xaxis = list(autorange = FALSE, nticks = 24, range = rand, showgrid = FALSE, showline = TRUE, tickfont = list(size = 9), ticks = "outside",
                        titlefont = list(color = "rgb(92, 53, 143)"), type = "linear", zeroline = FALSE),
           yaxis = list(anchor = "x",overlaying = "y", range = ranr, side = "right", tickfont = list(size = 8), ticks = "outside", type = "linear", zeroline = FALSE),
           annotations =
             list(x = 1, y = -0.1, text = "Differencing tenures by #.of.Days",
                  showarrow = F, xref='paper', yref='paper',
                  xanchor='right', yanchor='auto', xshift=0, yshift=0,
                  font=list(size=15, color="#2b608a"))
    )
  
  for(trace in c(j[2:length(j)])){
    p <- p %>% plotly::add_trace(y = COPY[,trace], color = colls[[trace]],name = trace,mode='lines')
  }
  return(p)
}

one_d = function(shift_Day,n)
  ### function generates a plotly 1d Histogram, for a tenure data...
{
  #Approach #1
  #d1 = list(data = list(list(x=COPY[,"1_Mon"],name= '1 Month',type= 'histogram',nbins=50)),
  #layout = list(height = "350",autosize = TRUE, showlegend = TRUE,orientation = 'h'))
  #return(d1)
  
  colls = list("Date"="white","1_Mon"="#003399","2_Mon"="#00ffff","3_Mon"="#5cf79c","4_Mon"="#D011FF","5_Mon"="#FF11E7",
               "6_Mon"="#9A6324","7_Mon" ="#f58231","8_Mon"="#fffac8","9_Mon"="#94FF11","10_Mon" ="#DAE829",
               "11_Mon"="#469990","12_Mon"="#E8296A")
  
  p <- ggplot(shift_Day, aes(shift_Day[,n])) +
    geom_histogram(aes(y = ..density..), fill = "white", color = "#7fafdf") +
    geom_density(fill = colls[[n]], alpha = 0.5) +
    geom_rug(aes(x = shift_Day[,n], y = 0), position = position_jitter(height = 0))+
    labs(x=n, y="Density", title="Histogram")+
    theme_light()+
    theme(plot.title = element_text(size = 12, face = "bold",hjust = 0.5))+
    geom_vline(aes(xintercept=mean(shift_Day[,n])),
               color="#2b608a", linetype="dashed", size=1)
  p <- ggplotly(p)
  return(p)
  
}

autoco = function(Day)
  ### function generates a autocorrelation plot, for a single tenure data...
{
  
  library("forecast")
  library("tseries")
  t = tseries::adf.test(Day)
  t = round(t$p.value,2)
  f = forecast::ndiffs(Day, test="adf")
  z = paste("Autocorrelation, ADF Test (p_val:alt. hyp being stationary): ",t, ", Lag Differencing needed: ", f)
  
  library(ggfortify)
  p1 <- autoplot(acf(Day, plot = FALSE), conf.int.fill = '#2b608a', conf.int.value = 0.8, conf.int.type = 'ma')+
    labs(x="Lag #", y="Correlation", title=z)+
    theme_light()+
    theme(plot.title = element_text(size = 12, face = "bold",hjust = 0.5))
  
  g = ggplotly(p1)
  return(g)
}

corrplot = function(COPY,j)
  ### function generates a correlation plot, for selected tenure data...
{
  # j is list of tenures/columns selected by user (via dcc multi dropdown)
  my = names(COPY) %in% j
  COPY4 = cor(COPY[,my], use = "complete.obs")
  Correlation = as.matrix(COPY4)
  histo <- plot_ly(z=~Correlation,x=j, y=j, text = j,colorscale = list(
    list(0,"rgb(231, 245, 255)"),
    list (1,"rgb(31, 119, 180)")),
    type = 'heatmap') %>%
    layout(title = list(text = "<b>Tenure Correlation</b>",y=0.999999999,x=0.25),
           #xaxis = list(tickcolor ='rgb(250, 250, 250)',title = 'Tenure', titlefont = list(color = 'rgb(0,0,0)'),zeroline=FALSE, showticklabels = TRUE, showline = FALSE, showgrid = FALSE ),
           #yaxis = list(tickcolor ='rgb(250, 250, 250)',title = 'Tenure', titlefont = list(color = 'rgb(0,0,0)'),zeroline=FALSE, showticklabels = TRUE, showline = FALSE, showgrid=FALSE),
           #plot_bgcolor = '#286494',plot_bgcolor = "#286494",autofill = TRUE,
           showlegend = TRUE
    )
  
  return(histo)
}

statis = function(shift_Day,j)
  ### function generates a plotly table of summary statistics, for selected tenures...
{
  # j is list of tenures/columns selected by user (via dcc multi dropdown)
  library("lawstat")
  library("goft")
  library("fitdistrplus")
  
  params <- data.frame("Statistics" = c('Mean','std'))
  for (p in j) {
    l = shift_Day[,p]
    k = na.omit(shift_Day[,p])
    #sum(length(which(is.na(l))))
    params[c(1:2),p]= c(mean(k),sd(k))
    
  }
  n = length(j)
  row.names(params)=params[,"Statistics"]
  params[,"Statistics"]= NULL
  params[,c(1:n)] = apply(params[,c(1:n)],2,function(x) round(x, 2))
  
  cnames = c('Tenure','Mean','std')
  rnames = colnames(params)
  datamatrx = t(as.matrix(params))
  
  p = gen_table(cnames,rnames,datamatrx)
  return(p)
}

pams = function(shift_Day,n)
  ### function generates a plotly table of dist_fitting parameters, for a selected tenure...
{
  library("fitdistrplus")
  nm <- fitdistrplus::fitdist(shift_Day[,n], "norm")
  lnm <- fitdistrplus::fitdist(shift_Day[,n], "lnorm")
  td <- fitdistrplus::fitdist(shift_Day[,n], "t", start=list(df=3))
  gm <- fitdistrplus::fitdist(shift_Day[,n], "gamma")
  
  params <- data.frame("Parameters" = c('mu','sd','aic','bic','loglik','KS_test'))
  params[c(1:5),'Norm'] = c(nm$estimate[1],nm$estimate[2],nm$aic,nm$bic,nm$loglik)
  params[c(1:5),'lnorm'] = c(lnm$estimate[1],lnm$estimate[2],lnm$aic,lnm$bic,lnm$loglik)
  params[c(1:5),'t'] = c(td$estimate[1],td$estimate[2],td$aic,td$bic,td$loglik)
  params[c(1:5),'gamma'] = c(gm$estimate[1],gm$estimate[2],gm$aic,gm$bic,gm$loglik)
  
  #laplace parameters
  m=median(shift_Day[,n])
  t = mean(abs(shift_Day[,n]-m))
  params[c(1:5),'Laplace']= c(m,t,0,0,0)
  
  library("rmutil") #for laplace
  library("stats") #for norm,lnorm,t
  library("lawstat")
  library("goft")
  
  num_of_samples = 10000
  p1 <- hist(shift_Day[,n],breaks=20)
  
  gm <- fitdistrplus::fitdist(shift_Day[,n], "gamma")
  y <- rgamma(num_of_samples, shape=gm$estimate[1], rate=gm$estimate[2])
  result = ks.test(shift_Day[,n], y)
  params[6,"gamma"] = result
  
  nm <- fitdistrplus::fitdist(shift_Day[,n], "norm")
  y1 <- rnorm(num_of_samples, mean=nm$estimate[1], sd=nm$estimate[2])
  result = ks.test(shift_Day[,n], y1)
  params[6,"Norm"] = result
  
  m=median(shift_Day[,n])
  t = mean(abs(shift_Day[,n]-m))
  y2 <- rlaplace(num_of_samples, m=m, s=t)
  result = ks.test(shift_Day[,n], y2)
  params[6,"Laplace"] = result
  
  lnm <- fitdistrplus::fitdist(shift_Day[,n], "norm")
  y3 <- rlnorm(num_of_samples, meanlog = lnm$estimate[1], sdlog = lnm$estimate[2])
  result = ks.test(shift_Day[,n], y3)
  params[6,"lnorm"] = result
  
  y4 <- rt(num_of_samples, df=length(shift_Day[,n])-1,ncp=10)
  result = ks.test(shift_Day[,n], y4)
  params[6,"t"] = result
  
  params[,c(2:6)] = apply(params[,c(2:6)],2,function(x) round(x, 2))
  
  cnames = c('<b>Parameters</b>', '<b>Norm</b>','<b>LgNorm</b>','<b>Gamma</b>','<b>Laplace</b>','<b>t</b>')
  rnames = c('<b>mu/df/shape</b>','<b>sd/rate</b>','<b>aic</b>', '<b>bic</b>', '<b>loglik</b>', '<b>Ks_test</b>')
  datamatrx = as.matrix(params[,-which(names(params) == "Parameters")])
  
  p = gen_table(cnames,rnames,datamatrx)
  
  return(p)
  
}

##This fcts is same as above, but was designed for dropdown menu of distributions
pamas = function(shift_Day,n,ds)
  ### function generates a plotly table of dist_fitting parameters, for a selected tenure...
{
  
  library("lawstat")
  library("goft")
  library("fitdistrplus")
  
  params <- data.frame("Parameters" = c('mu','sd','aic','bic','loglik','KS'))
  
  #ds = c("norm","lnorm","gamma","Laplace","t")
  for (p in ds) {
    if (p == 't'){
      dt <- fitdistrplus::fitdist(shift_Day[,n], p, start=list(df=100))
    }else if (p == 'Laplace') {
    }else{
      dt <- fitdistrplus::fitdist(shift_Day[,n], p)
    }
    
    if (p == 'Laplace'){
      m=median(shift_Day[,n])
      t = mean(abs(shift_Day[,n]-m))
      params[c(1:5),p] = c(m,t,0,0,0)
    }else {
      params[c(1:5),p] = c(dt$estimate[1],dt$estimate[2],dt$aic,dt$bic,dt$loglik)
    }
    
    library("rmutil") #for laplace
    library("stats") #for norm,lnorm,t
    library('zoo')
    library("CDFt")
    
    num_of_samples = 10000
    p1 <- hist(shift_Day[,n],breaks=20)
    
    if (p == 'gamma'){
      gm <- fitdistrplus::fitdist(shift_Day[,n], "gamma")
      y <- rgamma(num_of_samples, shape=gm$estimate[1], rate=gm$estimate[2])
      result = ks.test(shift_Day[,n], y)
      params[6,"gamma"] = result
      
    } else if (p == 'norm'){
      nm <- fitdistrplus::fitdist(shift_Day[,n], "norm")
      y1 <- rnorm(num_of_samples, mean=nm$estimate[1], sd=nm$estimate[2])
      result = ks.test(shift_Day[,n], y1)
      params[6,"norm"] = result
      
    } else if (p == 'Laplace'){
      m=median(shift_Day[,n])
      t = mean(abs(shift_Day[,n]-m))
      y2 <- rlaplace(num_of_samples, m=m, s=t)
      result = ks.test(shift_Day[,n], y2)
      params[6,"Laplace"] = result
      
    } else if (p == 'lnorm'){
      lnm <- fitdistrplus::fitdist(shift_Day[,n], "norm")
      y3 <- rlnorm(num_of_samples, meanlog = lnm$estimate[1], sdlog = lnm$estimate[2])
      result = ks.test(shift_Day[,n], y3)
      params[6,"lnorm"] = result
      
    } else if (p == 't') {
      y4 <- rt(num_of_samples, df=length(shift_Day[,n])-1,ncp=10)
      result = ks.test(shift_Day[,n], y4)
      params[6,"t"] = result
    }
  }
  
  params[,c(2:6)] = apply(params[,c(2:6)],2,function(x) round(x, 2))
  
  cnames = c('<b>Parameters</b>', '<b>Norm</b>','<b>LgNorm</b>','<b>Gamma</b>','<b>Laplace/b>','<b>t</b>')
  rnames = c('<b>mu/df/shape</b>','<b>sd/rate</b>','<b>aic</b>', '<b>bic</b>', '<b>loglik</b>', '<b>Ks_test</b>')
  datamatrx = as.matrix(params[,-which(names(params) == "Parameters")])
  
  p = gen_table(cnames,rnames,datamatrx)
  return(p)
}

#distribution Plot
dist = function(shift_Day, bins, n,difference)
  ### function generates a plot of distributions fitted, for a selected tenure...
{
  colls = list("Date"="white","1_Mon"="#003399","2_Mon"="#00ffff","3_Mon"="#5cf79c","4_Mon"="#D011FF","5_Mon"="#FF11E7",
               "6_Mon"="#9A6324","7_Mon" ="#f58231","8_Mon"="#fffac8","9_Mon"="#94FF11","10_Mon" ="#DAE829",
               "11_Mon"="#469990","12_Mon"="#E8296A")
  
  #if differenciing is zero we don't do fitting, reason is visible from histogram
  if (difference == 0){
    p <- ggplot(shift_Day, aes(shift_Day[,n])) +
      geom_histogram(aes(y = ..density..), fill = "white", color = "#7fafdf") +
      geom_density(fill = colls[[n]], alpha = 0.5) +
      ggtitle("Density with Histogram overlay")+
      geom_rug(aes(x = shift_Day[,n], y = 0), position = position_jitter(height = 0))+
      labs(x=n, y="Density", title="Distribution")+
      theme_light()+
      theme(plot.title = element_text(size = 12, face = "bold",hjust = 0.5))+
      geom_vline(aes(xintercept=mean(shift_Day[,n])),
                 color="#2b608a", linetype="dashed", size=1)
    p <- ggplotly(p)
    return(p)
    
  }else{
    
    library("fitdistrplus")
    nm <- fitdistrplus::fitdist(shift_Day[,n], "norm")
    lnm <- fitdistrplus::fitdist(shift_Day[,n], "lnorm")
    td <- fitdistrplus::fitdist(shift_Day[,n], "t", start=list(df=3))
    gm <- fitdistrplus::fitdist(shift_Day[,n], "gamma")
    
    #laplace parameters
    m=median(shift_Day[,n])
    t = mean(abs(shift_Day[,n]-m))
    library("rmutil") #for laplace
    library("stats") #for norm,lnorm,t
    
    #distribution curve data
    x <- seq(7,13, length.out=length(shift_Day[,n]))
    data=data.frame(x,stats::dlnorm(x,meanlog = lnm$estimate[1],sdlog=lnm$estimate[2]),
                    stats::dnorm(x,mean = nm$estimate[1],sd=nm$estimate[2]),
                    stats::dt(x, df=length(shift_Day[,n])-1,ncp=10),
                    stats::dgamma(x,shape = gm$estimate[1],rate = gm$estimate[2]),
                    rmutil::dlaplace(x, m = m, s = t))
    names(data)=c('x','lnorm','norm','t','gamma','laplace')
    
    #This way hover has correct names rather_than data$x
    Shifted_rate = data$x
    Norm = data$norm
    LogNorm = data$lnorm
    student_t = data$t
    Laplace = data$laplace
    Gamma = data$gamma
    
    g <- ggplot(shift_Day) +
      geom_histogram(aes(x = shift_Day[,n], y = ..density..),
                     bins=bins, fill = "white", color = "#7fafdf")+
      geom_line(data=data,aes(x=Shifted_rate,y=Norm,color='Normal')) +
      geom_line(data=data,aes(x=Shifted_rate,y=LogNorm,color='LogNorm')) +
      geom_line(data=data,aes(x=Shifted_rate,y=student_t,color='t')) +
      geom_line(data=data,aes(x=Shifted_rate,y=Laplace,color='Laplace')) +
      geom_line(data=data,aes(x=Shifted_rate,y=Gamma,color='Gamma')) +
      geom_rug(aes(x = shift_Day[,n], y = 0), position = position_jitter(height = 0))+
      labs(x=n, y="Density", title="Distribution")+
      theme_light()+
      theme(plot.title = element_text(size = 12, face = "bold",hjust = 0.5))+
      geom_vline(aes(xintercept=mean(shift_Day[,n])),
                 color="#2b608a", linetype="dashed", size=0.5)
    
    g <- ggplotly(g)
    return(g)
  }
}

qdist = function(shift_Day, bins, n)
  ### function generates a QQ plot...
{
  
  nm <- fitdistrplus::fitdist(shift_Day[,n], "norm")
  lnm <- fitdistrplus::fitdist(shift_Day[,n], "lnorm")
  td <- fitdistrplus::fitdist(shift_Day[,n], "t", start=list(df=3))
  gm <- fitdistrplus::fitdist(shift_Day[,n], "gamma")
  
  plot.legend <- c("norm", "lognormal", "t", "gamma")
  q = qqcomp(list(nm, lnm, td, gm), legendtext = plot.legend,plotstyle = "ggplot")+
    theme_light()+
    theme(plot.title = element_text(size = 12, face = "bold",hjust = 0.5))
  
  q <- ggplotly(q)
  return(q)
}
cdist = function(shift_Day, bins, n)
  ### function generates a cdf plot...
{
  
  nm <- fitdistrplus::fitdist(shift_Day[,n], "norm")
  lnm <- fitdistrplus::fitdist(shift_Day[,n], "lnorm")
  td <- fitdistrplus::fitdist(shift_Day[,n], "t", start=list(df=3))
  gm <- fitdistrplus::fitdist(shift_Day[,n], "gamma")
  
  plot.legend <- c("norm", "lognormal", "t", "gamma")
  c = cdfcomp(list(nm, lnm, td, gm), legendtext = plot.legend,plotstyle = "ggplot")+
    theme_light()+
    theme(plot.title = element_text(size = 12, face = "bold",hjust = 0.5))
  
  c <- ggplotly(c)
  return(c)
}
pdist = function(shift_Day, bins, n)
  ### function generates a PP plot...
{
  
  nm <- fitdistrplus::fitdist(shift_Day[,n], "norm")
  lnm <- fitdistrplus::fitdist(shift_Day[,n], "lnorm")
  td <- fitdistrplus::fitdist(shift_Day[,n], "t", start=list(df=3))
  gm <- fitdistrplus::fitdist(shift_Day[,n], "gamma")
  
  plot.legend <- c("norm", "lognormal", "t", "gamma")
  p = ppcomp(list(nm, lnm, td, gm), legendtext = plot.legend,plotstyle = "ggplot")+
    theme_light()+
    theme(plot.title = element_text(size = 12, face = "bold",hjust = 0.5))
  
  p <- ggplotly(p)
  return(p)
}

pca = function(Day,j)
  ### function generates a plot of Principle Components...
{
  # j is list of tenures/columns selected by user (via dcc multi dropdown)
  my = names(Day) %in% j
  n = length(j)
  #COPY4 = cor(COPY[,my], use = "complete.obs")
  
  #drop = names(Day) %in% c("Date","4_Mon","5_Mon","7_Mon","8_Mon","9_Mon","10_Mon","11_Mon")
  Day = Day[,my]
  row.has.na = apply(Day, 1, function(x){any(is.na(x))})
  Day= Day[!row.has.na,]
  
  Day.pca <- prcomp(Day, center = TRUE,scale. = TRUE)
  day_pca = as.data.frame(Day.pca$rotation)
  df = day_pca[,c(1:n)]
  df[,'Tenure'] = c(1:n)
  
  diff <- melt(df, id.vars = 'Tenure', variable.name = 'variable')
  conv <- ggplot(diff, aes(Tenure,value)) + geom_line(aes(colour = variable))+
    xlab("Tenure") +
    ylab("Weight") +
    theme_light()+
    theme(plot.title = element_text(size = 12, face = "bold",hjust = 0.5))+
    ggtitle("Principle Components")
  
  conv <- ggplotly(conv)
  return(conv)
}

pca1 = function(Day,j)
  ### function generates a pca plot...
{
  # j is list of tenures/columns selected by user (via dcc multi dropdown)
  my = names(Day) %in% j
  n = length(j)
  Day = Day[,my]
  n = length(j)
  
  row.has.na = apply(Day, 1, function(x){any(is.na(x))})
  Day= Day[!row.has.na,]
  
  Day.pca <- prcomp(Day, center = TRUE,scale. = TRUE)
  std_dev <- Day.pca$sdev
  pr_var <- std_dev^2
  prop_varex <- pr_var/sum(pr_var)
  mut <- data.frame("Components" = c(1:n),"Var" = c(prop_varex))
  Component = mut$Components
  Variance = mut[,"Var"]
  conv = ggplot(mut, aes(x=Components)) +
    geom_line(aes(y = Variance), color = "#2b608a")+
    theme_light()+
    theme(plot.title = element_text(size = 12, face = "bold",hjust = 0.5))+
    xlab("Principal Components") +
    ylab("Variance Explained") +
    ggtitle("P C A :  Variance_explained")
  
  conv <- ggplotly(conv)
  return(conv)
}

vas = function(Day,j)
  ### function generates a vasicek plot...
{
  # j is list of tenures/columns selected by user (via dcc multi dropdown)
  row.has.na = apply(Day, 1, function(x){any(is.na(x))})
  Day= Day[!row.has.na,]
  X = Day$Date
  mut <- data.frame("Date" = c(X))
  simR <- data.frame("Days" = c(1:260))
  params <- data.frame("params" = c('alpha','beta','sigma','q1','q2'))
  error <- data.frame("error" = c('alpha','beta','sigma','q1','q2'))
  row = nrow(simR)
  #3.84997199 0.20190743 0.02091909
  #vis = bond.vasicek(est2$param[1],est2$param[2],est2$param[3],est2$param[4],est2$param[5],tail(COPY[,i+1])[6],row-1,c(i/12),365)
  #simR[,l] = vis$r
  ANSWER = 1
  
  for (i in j) {
    p = as.integer(unlist(strsplit(i, '_'))[1])
    mut[,i] = rep(p/12,nrow(mut))
    
    k <- data.frame("Rate" = c(Day[,i]), "Mat" = c(mut[,i]))
    gb2 = apply(as.matrix(k),2,as.numeric)
    est2 = est.vasicek(gb2, method = "num")
    params[,i] = est2$param
    error[,i] = est2$error
    
    vis = bond.vasicek(est2$param[1],est2$param[2],est2$param[3],est2$param[4],est2$param[5],tail(COPY[,i])[6],row-1,c(p/12),255)
    simR[,i] = vis$r
  }
  
  return(list("params"=params,"error"=error,"simR"=simR))
}

aftcor = function(hist_data,simR,j)
  ### function generates a vasicek plot after including historical correlation...
{
  #simR = apply(simR, 2, function(x) as.numeric(as.character(x)))
  my = names(hist_data) %in% j
  A = cor(hist_data[,my], use = "complete.obs")
  
  L= t(chol(A))
  X = simR[,"Days"]
  U = simR[,-which(names(simR) == "Days")]
  U=t(U)
  cor(t(U),t(U))
  cor_version_ofU=L%*%U
  
  j=t(cor_version_ofU)
  
  j = as.data.frame(j)
  j[,'Days'] = X
  
  return(j)
}

pms = function(params)
  ### function generates a vasicek table parameters...
{
  row.names(params)=params[,"Parameters"]
  params[,"Parameters"]= NULL
  params = apply(params,2,function(x) round(x, 2))
  
  cnames = c('Tenure','alpha','beta','signma','q1','q2')
  rnames = colnames(params)
  datamatrx = t(as.matrix(params))
  
  p = gen_table(cnames,rnames,datamatrx)
  return(p)
}

two_d_vas = function(sim2,j,c)
  ### function generates a 2d plot using ggplot
{
  my = names(sim2) %in% c("Days",unlist(j))
  sim2 = sim2[,my]
  
  simp <- melt(sim2 , id.vars = 'Days', variable.name = 'variable')
  p1 <- ggplot(simp, aes(Days,value)) + geom_line(aes(colour = variable))+
    xlab("Days") +
    ylab("Libor_rate (%)") +
    theme_light()+
    theme(plot.title = element_text(size = 12, face = "bold",hjust = 0.5))+
    ggtitle(c)
  
  p1= ggplotly(p1)
  
  return(p1)
}
