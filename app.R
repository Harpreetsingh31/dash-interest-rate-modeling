rm(list = ls(all = TRUE))# Remove all previous objects
gc() # Garbage collection
assign("last.warning", NULL, envir = baseenv()) # Reset list of past warnings

source("assets/fcts.R")
A#devtools::install_github("jcizel/FredR")
#library("FredR")
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
library("CDFt")
library("dplyr")
library("lawstat")
library("goft")
#library("SMFI5") #for vasicek


# Getting the data from FredR, for US and CAD

#Activation key would needed
#fred <- FredR("dd#ed#f##ae#ca##b#bd##ab#c######")
#ids = c("1MTD156N","2MTD156N","3MTD156N","4MTD156N","5MTD156N","6MTD156N",
# "7MTD156N","8MTD156N","9MTD156N","10MD156N","11MD156N","12MD156N")
#us <- as.data.frame(fred$series.observations(series_id = paste("USD",ids[1],sep = "")))[,c("date","value")]
#cd <- as.data.frame(fred$series.observations(series_id = paste("CAD",ids[1],sep = "")))[,c("date","value")]

#for(i in c(1:11)){
#usd <- as.data.frame(fred$series.observations(series_id = paste("USD",ids[i+1],sep = "")))[,c("date","value")]
#cad <- as.data.frame(fred$series.observations(series_id = paste("CAD",ids[i+1],sep = "")))[,c("date","value")]
#us <- left_join(us, usd, by=c("date"))
#cd <- left_join(cd, cad, by=c("date"))
#}

#write.csv(us, file = "usd.csv",row.names=FALSE)
#write.csv(cd, file = "cad.csv",row.names=FALSE)

appName <- Sys.getenv("DASH_APP_NAME")

if (appName != ""){
  pathPrefix <- sprintf("/%s/", appName)
  
  Sys.setenv(DASH_ROUTES_PATHNAME_PREFIX = pathPrefix,
             DASH_REQUESTS_PATHNAME_PREFIX = pathPrefix)
}

#Loading/Cleaning/Formatting of the libor_data
COPY = filteri(read.csv('Data/usd.csv')) #usdrates
COPY2 = filteri(read.csv('Data/cad.csv')) #cadrates

#Dataframes below were generated using function "vas"
#simulation takes time, and might causes errors during run
#that is why it was pre_done.
sim1 = read.csv("Data/ussim.csv")
sim2 = read.csv("Data/cdsim.csv")
pms1 = read.csv("Data/uspms.csv")
pms2 = read.csv("Data/cdpms.csv")
#err1 = read.csv("Data/userr.csv")
#err2 = read.csv("Data/cderr.csv"

col1 <- c("Days","1_Mon", "2_Mon", "3_Mon", "4_Mon", "5_Mon", "6_Mon", "7_Mon", "8_Mon", "9_Mon", "10_Mon", "11_Mon","12_Mon")
sim1 = setNames(sim1, col1)
sim2 = setNames(sim2, col1)
col2 <- c("Parameters","1_Mon", "2_Mon", "3_Mon", "4_Mon", "5_Mon", "6_Mon", "7_Mon", "8_Mon", "9_Mon", "10_Mon", "11_Mon","12_Mon")
pms1 = setNames(pms1, col2)
pms2 = setNames(pms2, col2)
#col3 <- c("Error","1_Mon", "2_Mon", "3_Mon", "4_Mon", "5_Mon", "6_Mon", "7_Mon", "8_Mon", "9_Mon", "10_Mon", "11_Mon","12_Mon")
#err1 = setNames(err1, col3)
#err2 = setNames(err2, col3)

#https://dash.plot.ly/dash-daq/colorpicker

app <- Dash$new()


ft_colr = "#7fafdf"
bg_colr = "#286494"
bg_colr_dark = "#1a4e78"

space_line = htmlDiv(list(
  htmlHr(style = list(width = '100%','border-bottom'= '10px solid #286494'))
))

color_line = htmlDiv(list(
  htmlHr(style = list(width = '100%','border-bottom'= '20px solid white'))
))

tab_style = list('border'= '1px solid #7fafdf','fontWeight'= 'bold')
tab_selected_style = list('border'= '1px solid #7fafdf','backgroundColor'= '#2b608a','color'= '#7fafdf')
space = htmlH3("",style=list(margin='top'))

mks = list(
  "1985" = list("label" = "1985", "style" = list("color" = "#7fafdf")),
  "1990" = list("label" = "1990", "style" = list("color" = "#7fafdf")),
  "1995" = list("label" = "1995", "style" = list("color" = "#7fafdf")),
  "2000" = list("label" = "2000", "style" = list("color" = "#7fafdf")),
  "2005" = list("label" = "2005", "style" = list("color" = "#7fafdf")),
  "2010" = list("label" = "2010", "style" = list("color" = "#7fafdf")),
  "2015" = list("label" = "2015", "style" = list("color" = "#7fafdf")),
  "2020" = list("label" = "2020", "style" = list("color" = "#7fafdf"))
)

label_list = list(
  list("label" = "1 Month", "value" = "1_Mon"),list("label" = "2 Month", "value" = "2_Mon"),list("label" = "3 Month", "value" = "3_Mon"),
  list("label" = "4 Month", "value" = "4_Mon"),list("label" = "5 Month", "value" = "5_Mon"),list("label" = "6 Month", "value" = "6_Mon"),
  list("label" = "7 Month", "value" = "7_Mon"),list("label" = "8 Month", "value" = "8_Mon"),list("label" = "9 Month", "value" = "9_Mon"),
  list("label" = "10 Month", "value" = "10_Mon"),list("label" = "11 Month", "value" = "11_Mon"),list("label" = "12 Month", "value" = "12_Mon"))

app$layout(
  
  space_line,
  
  htmlDiv(list(htmlH1("Interest rate modelling", style=list('fontSize'= 75,color = 'white',display='inline-block','float' = "left")),
               htmlImg(src= "assets/dash-logo-new.png", style=list(display='inline-block','float' = "right"))
  ),style = list(width = "96%",margin="auto")),
  
  color_line,
  space_line,
  
  htmlDiv(list(
    htmlH1("Abstract: ", style=list(width= '100%','fontSize'= 40,'float' = "left",color = 'white',margin="auto")),
    htmlH6(" Interest rates affect almost all spheres of our lives, from 
           house mortgages, earnings on our savings and to our future pension plans. 
           It is because of this broad spectrum of impacts, the interest rates are 
           analogized to as Gravity is in the field of Physics. Therefore, stochastic 
           interest rate models for prediction play an important role in financial sector.
           In this app, we fetch historical libor rate data from 'fred.stlouisfed.org' for
           12 tenures in two currencies USD and CAD. On that dataset, we perform various 
           operations, like data stationing (using differencing), we look tenure correlations, 
           PCA Analysis, and finally Vasicek Model fitting."
           , style=list(width= '92%','float' = "right",margin="auto"))
    
  ), style=list(width = '92%',margin = "auto",'margin-top'='15px','backgroundColor'= bg_colr_dark)),
  
  space_line,
  
  htmlDiv(list(
    dccTabs(id="tab",value = "usd", children=list(
      dccTab(label='USD',value = "usd",style=tab_style, selected_style=tab_selected_style),
      dccTab(label='CAD',value = "cad",style=tab_style, selected_style=tab_selected_style)
    ),style=list( width = '50%',margin='auto'))
  ), style = list('margin-top' = "50px")),
  
  htmlDiv(list(
    htmlDiv(list(
      htmlDiv(list(htmlH6(" The Table here gives a snapshot with head, middle & tail of the dataset. Null values in tenures are either missing or the tenure is discontinued."))
              , style=list( width = '100%',margin = "auto",'margin-top'='15px','backgroundColor'= bg_colr_dark)),
      dccGraph(id='df')
    ),style = list(width= '100%',margin = "auto")),
    
    htmlDiv(list(dccGraph(id='3d')),style = list(width= '100%',margin = "auto")),
    htmlDiv(list(dccGraph(id='2d')),style = list(width= '75%',display= "inline-block",'margin-top'='10px','float' = "left")),
    htmlDiv(list(dccGraph(id='pi')),style = list(width= '25%',display= "inline-block",'margin-top'='10px','float' = "right"))
  ),style = list(margin="auto",width='90%')),
  
  space_line,
  
  htmlDiv(list(
    htmlH1("Analysis: ", style=list(width= '100%','fontSize'= 40,'float' = "left",color = 'white',margin="auto")),
    htmlDiv(list(htmlH6(" Sliders/dropdown selectors below will help us perform various data analytic operations. 
                        Date_slider and Multi-Tenure selector dropdown helps us subsetting our dataset based on selected dates and tenures.
                        Uni-Tenure selector dropdown is for univariate analysis like autocorrelation and distribution fittings.
                        For data stationarity and trend analysis, we difference our data based on #.of.days added into the input block. ")
    ), style=list(width= '92%','float' = "right",margin="auto",'backgroundColor'= bg_colr_dark,'margin-top'='15px')),
    
    htmlDiv(list(
      htmlDiv(list("Choose a Date Range...")),
      dccRangeSlider(id='Date-slider',min=1985,max=2020,step=1,value=list(1990, 2019),marks = mks)
    ),style = list( width= '80%',margin = "auto")),
    
    htmlDiv(list(
      htmlDiv(list("Multi-Tenure Analysis: Tenure Correlation, PCA, Vasicek Model...")),
      dccDropdown(
        id='select',options=label_list,value = c("1_Mon","2_Mon","3_Mon","6_Mon","12_Mon"),multi = TRUE, style = list('borderColor'= ft_colr))
    ),style = list(width= '60%',margin = "auto",'margin-top'='20px')),
    
    htmlDiv(list(
      htmlDiv(list("Uni-Tenure Analysis: Distribution fitting...")),
      dccDropdown(
        id='drop',options=label_list,value="1_Mon")
    ),style = list(width= '40%',margin = "auto",'margin-top'='15px')),
    
    htmlDiv(list(
      htmlDiv(list("Differencing: Trend/Autocorrelation...")),
      dccInput(
        id = "difs",placeholder = "Enter a value...",
        type = "number",debounce=TRUE,value = 25, style = list('color'= ft_colr,'backgroundColor'= bg_colr) ) 
    ),style = list(width= '25%',margin = "auto",'margin-top'='10px'))
    
    ),style = list(width='92%',margin="auto",'margin-top'='5px','backgroundColor'= bg_colr_dark)),
  
  space_line,
  
  htmlDiv(list(
    dccTabs(id="parts",value = "dt", children=list(
      
      ##Tab1:- Data cleaning.
      dccTab(label='Data Preprocessing',value = "dp",style=tab_style, selected_style=tab_selected_style,children = list(
        #Select a tenure for autocorrelation analysis, Univariate Analysis
        htmlDiv(list(
          #Difference plot
          #Slected Tenure correlation plot.
          htmlDiv(list(dccGraph(id='diff-corr')),style = list(width= '64%',display= "inline-block",'float' = "left",'margin-top'='10px')),
          htmlDiv(list(dccGraph(id='1d')),style = list(width= '35%',display= 'inline-block','float' = "right",'margin-top'='10px')),
          htmlDiv(list(dccGraph(id='diff')),style = list(width= '79%',display= "inline-block",'float' = "left",'margin-top'='10px')),
          htmlDiv(list(dccGraph(id='ps')),style = list(width= '20%',display= "inline-block",'float' = "right",'margin-top'='10px')),
          htmlDiv(list(dccGraph(id='autoco')),style = list(width= '79%',display= "inline-block",'float' = "left",'margin-top'='10px')),
          htmlDiv(list(htmlH6(" The Table gives summary statistics like mean, and std. If we change Day_lag from default (25) 
                              to lower # (try 0/1), we see std. changing significantly. Figure on the left gives 
                              autocorrelation and ADF test for trend in its title, p_val < 0.05, means the data is stationary.
                              ")),
                  style=list( width = '20%',display='inline-block','float'="right",'margin-top'='5px','backgroundColor'= bg_colr_dark)),
          htmlDiv(id='my-div')
          ),style = list(margin="auto",width='90%'))
          )), ##Tab1:- Excit
      
      ##Tab2:- Fitting Probabilistic distribution function
      dccTab(label='Distribution Fitting',value = "dt",style=tab_style, selected_style=tab_selected_style,children = list(
        
        htmlDiv(list(
          htmlDiv(list(dccGraph(id='diff-ht0')), style = list(width= '54%',display='inline-block','float' = "left",'margin-top'='10px')),
          htmlDiv(list(dccGraph(id='diff-ht1')), style = list(width= '45%',display='inline-block','float' = "right",'margin-top'='10px')),
          htmlDiv(list(dccGraph(id='diff-ht3')), style = list(width= '54%',display='inline-block','float' = "left",'margin-top'='10px')),
          htmlDiv(list(dccGraph(id='diff-ht2')), style = list(width= '45%',display='inline-block','float' = "right",'margin-top'='10px')),
          htmlDiv(list(htmlH6(" The Table here gives various distribution fitting parameters, like mean, shape, and standard deviation etc. 
                              The table also gives good of fitness statistics like aic, bic and Kolmogorov–Smirnov test (p_values). 
                              The best fit curve should've highest log_likelyhood, and aic/bic/ks_test(p_value) should be lowest. 
                              Regarding, QQ-PP-CDF plot, if the points are very much on the curve/fit lines then that curve is good fit. ")),
                  style=list( width = '45%',display='inline-block','float'="right",'margin-top'='10px','backgroundColor'= bg_colr_dark)),
          htmlDiv(list(dccGraph(id='diff-pam')), style = list(width= '54%',display='inline-block','float' = "left",'margin-top'='10px'))
          ),style = list(margin="auto",width='90%'))
          )), ##Tab2:-Exit.
      
      ##Tab3:- PCA Anaylsis.
      dccTab(label='P C A',value = "pc",style=tab_style, selected_style=tab_selected_style,children = list(
        
        htmlDiv(list(
          htmlDiv(list(htmlH6(" The Figure on the left displays principal components (level,slope,curvature) and, the
                              figure on the right gives info about the amount of variance explained.")),
                  style=list( width = '80%',margin="auto",'margin-top'='10px','backgroundColor'= bg_colr_dark)),
          htmlDiv(list(dccGraph(id='pca' )), style = list(width= '54%',display='inline-block','float' = "left",'margin-top'='10px')),
          htmlDiv(list(dccGraph(id='pca1')), style = list(width= '45%',display='inline-block','float' = "right",'margin-top'='10px'))
          ),style = list(margin="auto",width='90%'))
        
        )), ##Tab3:- PCA Exit.
      
      ##Tab4:- Vasicek Model.
      dccTab(label='Vasicek Model',value = "vsm",style=tab_style, selected_style=tab_selected_style,children = list(
        
        htmlDiv(list(
          #htmlDiv(list(htmlH1("Vasicek Model")), style=list( width = '100%', height = '20%')),
          htmlDiv(list(dccGraph(id='vas')), style = list(width= '54%',display='inline-block','float' = "left",'margin-top'='10px')),
          htmlDiv(list(dccGraph(id='vas1')), style = list(width= '45%',display='inline-block','float' = "right",'margin-top'='10px')),
          htmlDiv(list(dccGraph(id='vp')), style = list(width= '54%',display='inline-block','float' = "left",'margin-top'='10px')),
          htmlDiv(list(htmlH6(" The Table here gives parameters estimated using Package SMFI5, and are not guarranted to be accurate. 
                              Furthur, testing needs to be done.")),
                  style=list( width = '45%',display='inline-block','float'="right",'margin-top'='10px','backgroundColor'= bg_colr_dark))
          ),style = list(margin="auto",width='90%'))
        
        )) ##Tab4: Vas
      
      ), style=list( width = '50%',margin='auto'))
    
    ,space_line
      ), style = list('margin-top' = "50px")),
  
  space,
  htmlDiv(id='hidden-curr-div', style = list(display= 'none')),
  htmlDiv(id='hidden-diff',style=list(display = 'none')),
  htmlDiv(id='hidden-slider',style=list(display = 'none')),
  htmlDiv(id='hidden-shift_diff',style=list(display = 'none'))
      )

app$callback(
  output = list(id = 'hidden-curr-div', property = 'children'),
  params = list(input(id ='tab', property = 'value')),
  
  function(plot_type){
    if (plot_type == 'usd'){
      jsonlite::toJSON(COPY)
    }else{
      jsonlite::toJSON(COPY2)
    }
  }
)

app$callback(
  output = list(id = 'df', property = 'figure'),
  params = list(input(id='hidden-curr-div', property='children')),
  
  function(diff_df){
    COPY = jsonlite::fromJSON(diff_df)
    return(generate_table(COPY))
  }
)

#change graph due to radio button
app$callback(
  output = list(id = '3d', property = 'figure'),
  params = list(input(id='hidden-curr-div', property='children')),
  
  function(diff_df){
    COPY = jsonlite::fromJSON(diff_df)
    return(three_d(COPY))
  }
)

app$callback(
  output=list(id='hidden-slider', property='children'),
  params=list(input(id='hidden-curr-div', property='children'),
              input(id='Date-slider', property='value')),
  
  function(diff_df,value) {
    COPY = jsonlite::fromJSON(diff_df)
    i = as.integer(value[1])
    j = as.integer(value[2])
    COPY$Date = as.Date(paste(COPY$Date), format = "%Y-%m-%d")
    COPY[,"Year"] = as.integer(format(COPY$Date,"%Y"))
    COPY <- COPY[(COPY$Year >= i) & (COPY$Year <= j),]
    COPY[,"Year"] <- NULL
    jsonlite::toJSON(COPY)
  }
)

app$callback(
  output=list(id='2d', property='figure'),
  params=list(input(id='hidden-slider', property='children')),
  
  function(diff_df) {
    COPY = jsonlite::fromJSON(diff_df)
    return (two_d(COPY))
  }
)

app$callback(
  output=list(id='pi', property='figure'),
  params=list(input(id='hidden-slider', property='children')),
  
  function(diff_df) {
    COPY = jsonlite::fromJSON(diff_df)
    return (pie(COPY))
  }
)

app$callback(
  output=list(id='hidden-diff', property='children'),
  params=list(input(id='hidden-slider', property='children'),
              input(id='difs', property='value'),
              input(id='select', property = 'value')),
  
  function(diff_df,difference,j) {
    COPY = jsonlite::fromJSON(diff_df)
    diff_df = diff(COPY,difference,j)
    jsonlite::toJSON(diff_df)
  }
)

app$callback(
  output=list(id='diff', property='figure'),
  params=list(input(id='hidden-diff', property='children'),
              input(id='select', property = 'value')),
  
  function(diff_df,j) {
    df = jsonlite::fromJSON(diff_df)
    return (diff_plot(df,j))
  }
)

app$callback(
  output=list(id='ps', property='figure'),
  params=list(input(id='hidden-diff', property='children'),
              input(id='select', property = 'value')),
  
  function(diff_df,j) {
    df = jsonlite::fromJSON(diff_df)
    return (statis(df,j))
  }
)

app$callback(
  output=list(id='1d', property='figure'),
  params=list(input(id='hidden-slider', property='children'),
              input(id='drop', property = 'value')),
  
  function(diff_df,n) {
    COPY = jsonlite::fromJSON(diff_df)
    return (one_d(COPY,n))
  }
)

app$callback(
  output=list(id='autoco', property='figure'),
  params=list(input(id='hidden-diff', property='children'),
              input(id='drop', property = 'value')),
  
  function(diff_df,n) {
    df = jsonlite::fromJSON(diff_df)
    z = autoco(df[,n])
    return (z)
  }
)

app$callback(
  output=list(id='diff-corr', property='figure'),
  params=list(input(id='hidden-diff', property='children'),
              input(id='select', property = 'value')),
  
  function(diff_df,j) {
    df = jsonlite::fromJSON(diff_df)
    return (corrplot(df,j))
  }
)


app$callback(
  output=list(id='hidden-shift_diff', property='children'),
  params=list(input(id='hidden-diff', property='children')),
  
  function(df){
    df = jsonlite::fromJSON(df)
    df$Date = as.Date(paste(df$Date), format = "%Y-%m-%d")
    shift_df = df+10
    jsonlite::toJSON(shift_df)
  }
)

app$callback(
  output=list(id='diff-ht0', property='figure'),
  params=list(input(id='drop', property = 'value'),
              input(id='hidden-shift_diff', property='children'),
              input(id='difs', property='value')),
  
  function(n,shift_df,difference){
    df = jsonlite::fromJSON(shift_df)
    d = dist(df,bins=60,n,difference)
    return (d)
  }
)

app$callback(
  output=list(id='diff-ht1', property='figure'),
  params=list(input(id='drop', property = 'value'),
              input(id='hidden-shift_diff', property='children')),
  
  function(n,shift_df) {
    df = jsonlite::fromJSON(shift_df)
    p = pdist(df,bins=50,n)
    return (p)
  }
)

app$callback(
  output=list(id='diff-ht2', property='figure'),
  params=list(input(id='drop', property = 'value'),
              input(id='hidden-shift_diff', property='children')),
  
  function(n,shift_df) {
    df = jsonlite::fromJSON(shift_df)
    q = qdist(df,bins=50,n)
    return (q)
  }
)

app$callback(
  output=list(id='diff-ht3', property='figure'),
  params=list(input(id='drop', property = 'value'),
              input(id='hidden-shift_diff', property='children')),
  
  function(n,shift_df) {
    df = jsonlite::fromJSON(shift_df)
    c = cdist(df,bins=50,n)
    return (c)
  }
)

app$callback(
  output=list(id='diff-pam', property='figure'),
  params=list(input(id='drop', property = 'value'),
              input(id='hidden-shift_diff', property='children')),
  
  function(n,shift_df) {
    df = jsonlite::fromJSON(shift_df)
    return (pams(df,n))
  }
)

app$callback(
  output=list(id='pca', property='figure'),
  params=list(input(id='hidden-diff', property='children'),
              input(id='select', property = 'value')),
  
  function(diff_df,j) {
    df = jsonlite::fromJSON(diff_df)
    z = pca(df,j)
    return (z)
  }
)
app$callback(
  output=list(id='pca1', property='figure'),
  params=list(input(id='hidden-diff', property='children'),
              input(id='select', property = 'value')),
  
  function(diff_df,j) {
    df = jsonlite::fromJSON(diff_df)
    z = pca1(df,j)
    return (z)
  }
)

app$callback(
  output=list(id='vas', property='figure'),
  params=list(input(id ='tab', property = 'value'),
              input(id='select', property = 'value')),
  
  function(plot_type,j) {
    k = length(j)
    c = paste("LIBOR_rate:-", k," Maturities")
    
    if (plot_type == 'usd'){
      p1 =two_d_vas(sim1,j,c)
    }else{
      p1 =two_d_vas(sim2,j,c)
    }
    return (p1)
  }
)

app$callback(
  output=list(id='vas1', property='figure'),
  params=list(input(id ='tab', property = 'value'),
              input(id='select', property = 'value')),
  
  function(plot_type,j) {
    
    k = length(j)
    c = paste("LIBOR_rate (after historical_correlation):-", k," maturities")
    
    if (plot_type == 'usd'){
      my = names(sim1) %in% c("Days",unlist(j))
      sim1 = sim1[,my]
      df2 = aftcor(COPY,sim1,j)
      p1 =two_d_vas(df2,j,c)
      
    }else{
      my = names(sim2) %in% c("Days",unlist(j))
      sim2 = sim2[,my]
      df2 = aftcor(COPY2,sim2,j)
      p1 =two_d_vas(df2,j,c)
    }
    return (p1)
  }
)

app$callback(
  output=list(id='vp', property='figure'),
  params=list(input(id ='tab', property = 'value'),
              input(id='select', property = 'value')),
  
  function(plot_type,j) {
    if (plot_type == 'usd'){
      my = names(pms1) %in% c("Parameters",unlist(j))
      pms1 = pms1[,my]
      p1=pms(pms1)
    }else{
      my = names(pms2) %in% c("Parameters",unlist(j))
      pms2 = pms2[,my]
      p1=pms(pms2)
    }
    return (p1)
  }
)

app$run_server()
