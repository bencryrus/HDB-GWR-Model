# Library imports
library(sf)
library(httr)
library(jsonlite)
library(tidyverse)
library(zoo)
library(rgdal)
library(spdep)
library(tmap)
library(REAT)
library(SpatialAcc)
library(shiny)
library(leaflet)
library(ggpubr)
library(GWmodel)
library(shinydashboard)
library(shinycssloaders)
library(dashboardthemes)
library(plotly)
library(RColorBrewer)
library(caret)
library(xgboost)
library(gbm)

# Data Import
hawkers <- readOGR('data/spatial/attributes/hawkers.shp')
malls <- readOGR('data/spatial/attributes/malls.shp')
MLRT <- readOGR('data/spatial/attributes/mlrt.shp')
supermarkets <- readOGR('data/spatial/attributes/supermarkets.shp')
pschools <- readOGR('data/spatial/attributes/primary schools.shp')
ppschools <- readOGR('data/spatial/attributes/premium primary schools.shp')
ppreschools <- readOGR('data/spatial/attributes/premium preschools.shp')
eldercare <- readOGR('data/spatial/attributes/eldercare.shp')

colnames(ppreschools@data)[1] <- 'Name'
colnames(eldercare@data)[11] <- 'Name'
colnames(MLRT@data)[4] <- 'Name'

Flat_type <- c('1 ROOM', '2 ROOM', '3 ROOM', '4 ROOM', '5 ROOM',
               'EXECUTIVE', 'MULTI-GENERATION')
Flat_models <- c('Adjoined flat', 'Apartment', 'DBSS', 'Improved',
                 'Improved-Maisonette', 'Maisonette', 
                 'Model A', 'Model A-Maisonette',
                 'Model A2', 'Multi Generation', 'New Generation', 
                 'Premium Apartment',
                 'Simplified', 'Standard', 'Terrace', 
                 'Type S1', 'Type S2')

mpsz <- st_read(dsn = 'data/spatial', layer = 'MP14_SUBZONE_WEB_PL')
mpsz <- st_transform(mpsz, 3414)

hdb.data <- read_csv('data/aspatial/HDB Resale Prices with Location Data (2015-2018).csv')
hdb.data$X <- as.numeric(hdb.data$X)
hdb.data$Y <- as.numeric(hdb.data$Y)
hdb.data$date <- as.yearmon(hdb.data$month, format = '%Y-%m')
dates = sort(unique(hdb.data$date))

amenities.name = list('hawkers.mpsz', 'malls.mpsz', 'mlrt.mpsz', 'supermarkets.mpsz',
                      'pschools.mpsz', 'ppschools.mpsz',
                      'ppreschools.mpsz','eldercare.mpsz')

hdb.comp.sp <- readOGR('data/spatial/HDB2018/HDB Compiled GWR 2018.shp')
colnames(hdb.comp.sp@data) <- c('SN','Month','Town','Flat_type',
                                'Block','Street_name','Storey_range',
                                'Floor_area','Flat_model','Lease_commenced',
                                'Remaining_lease','Resale_price','Query','SEARCHVAL',
                                'BLK_NO','ROAD_NAME','BUILDING','ADDRESS','POSTAL',
                                'LATITUDE','LONGITUDE','LONGTITUDE','Query_sent',
                                'Hawker_Count', 'Mall_Count', 'MLRT_Count', 
                                'PriSchool_Count', 'PPriSchool_Count', 
                                'Supermarket_Count', 'PPreSchool_Count',
                                'Eldercare_Count', 'Storey',
                                '1 ROOM', '2 ROOM', '3 ROOM', '4 ROOM', '5 ROOM',
                                'EXECUTIVE', 'MULTI-GENERATION', 
                                'Adjoined flat', 'Apartment', 'DBSS', 'Improved',
                                'Improved-Maisonette', 'Maisonette', 
                                'Model A', 'Model A-Maisonette',
                                'Model A2', 'Multi Generation', 'New Generation', 
                                'Premium Apartment', 'Premium Apartment Loft', 
                                'Premium Apartment.','Premium Maisonette', 
                                'Simplified', 'Standard', 'Terrace', 
                                'Type S1', 'Type S2')

# OneMap query function
onemap.query <- function(data_name){
  # Setting parameters for query
  searchVal <- 'https://developers.onemap.sg/commonapi/search?searchVal='
  returnGeom <- '&returnGeom=Y'
  getAddrDetails<- '&getAddrDetails=Y&pageNum=1'
  query_address <- data_name
  query_result <- tibble()
  
  # Sending query to OneMap
  for(i in query_address){
    res <- GET(url = paste(searchVal, i, 
                           returnGeom, getAddrDetails, sep = '' ))
    data.parsed <- content(res, as = 'parsed')
    query.data <- data.parsed$results
    query.data <- query.data %>% bind_rows()
    query_result <- bind_rows(query_result, query.data[1,])
  }
  
  # Exporting results to csv
  query_result$query_sent <- query_address
  onemap.output <- cbind(data_name, query_result)
  return(onemap.output)
  
  # Checking for missing output
  missing.output <- onemap.output[is.na(onemap.output$SEARCHVAL), 'Name']
  print('Missing outputs: ')
  if(is_empty(missing.output)){
    print('None')
  }else{
    print(missing.output)
  }
}

# Attribute finder function
hdb.ext.factors <- function(flat_type, flat_model, floor, 
                            floor_area, remaining_lease, postal_code){
  # Setting up variables of interest
  var.data <- list(st_as_sf(hawkers), st_as_sf(malls), 
                   st_as_sf(MLRT), st_as_sf(supermarkets),
                   st_as_sf(pschools), st_as_sf(ppschools), 
                   st_as_sf(ppreschools), st_as_sf(eldercare))
  var.names <- list('Hawker_Count', 'Mall_Count', 'MLRT_Count', 
                    'PriSchool_Count', 'PPriSchool_Count', 
                    'Supermarket_Count', 'PPreSchool_Count',
                    'Eldercare_Count') 
  Flat_type <- c('1 ROOM', '2 ROOM', '3 ROOM', '4 ROOM', '5 ROOM',
                 'EXECUTIVE', 'MULTI-GENERATION')
  Flat_models <- c('Adjoined flat', 'Apartment', 'DBSS', 'Improved',
                   'Improved-Maisonette', 'Maisonette', 
                   'Model A', 'Model A-Maisonette',
                   'Model A2', 'Multi Generation', 'New Generation', 
                   'Premium Apartment', 'Premium Apartment Loft', 
                   'Premium Apartment.','Premium Maisonette', 
                   'Simplified', 'Standard', 'Terrace', 
                   'Type S1', 'Type S2')
  
  # Internal factors
  hdb.var = as.data.frame(matrix(0,1,length(var.data)))
  colnames(hdb.var) <- var.names
  hdb.var$Flat_type <- flat_type
  hdb.var$Flat_model <- flat_model
  hdb.var$Storey <- floor
  hdb.var$Floor_area <- floor_area
  hdb.var$Remaining_lease <- remaining_lease
  
  # External factors
  hdb <- onemap.query(postal_code)
  hdb$X <- as.numeric(hdb$X)
  hdb$Y <- as.numeric(hdb$Y)
  hdb.sf <- st_as_sf(hdb, coords = c('X','Y'), crs = 3414)
  hdb.sf.b <- st_buffer(hdb.sf, dist = 1000)
  
  i = 1
  while(i < length(var.data) + 1){
    hdb.var.temp <- st_join(hdb.sf.b, var.data[[i]]) 
    hdb.var.temp <- hdb.var.temp%>% n_distinct(hdb.var.temp$Name, na.rm = TRUE)
    hdb.var[1,var.names[[i]]] <- hdb.var.temp
    hdb.sf[1,var.names[[i]]] <- hdb.var.temp
    i <- i + 1
  }
  
  # Bin flat type
  for(i in Flat_type){
    hdb.var[,as.character(i)] <- ifelse(hdb.var$Flat_type == i,1,0)
  }
  
  # Bin flat model
  for(i in Flat_models){
    hdb.var[,as.character(i)] <- ifelse(hdb.var$Flat_model == i,1,0)
  }
  
  hdb.sf$Flat_type <- flat_type
  hdb.sf$Flat_model <- flat_model
  hdb.sf$Storey <- floor
  hdb.sf$Floor_area <- floor_area
  hdb.sf$Remaining_lease <- remaining_lease
  
  ft_encoding <- function(data){
    data$Flat_type <- as.character(data$Flat_type)
    data[data$Flat_type == '1 ROOM', 'Flat_type'] <- 1
    data[data$Flat_type == '2 ROOM', 'Flat_type'] <- 2
    data[data$Flat_type == '3 ROOM', 'Flat_type'] <- 3
    data[data$Flat_type == '4 ROOM', 'Flat_type'] <- 4
    data[data$Flat_type == '5 ROOM', 'Flat_type'] <- 5
    data[data$Flat_type == 'EXECUTIVE', 'Flat_type'] <- 6
    data[data$Flat_type == 'MULTI-GENERATION', 'Flat_type'] <- 7
    data$Flat_type <- as.numeric(data$Flat_type)
  }
  hdb.sf$Flat_type <- ft_encoding(hdb.sf)
  
  return(list(hdb.var, hdb.sf))
}

# Attribute finder plot function
hdb.ext.factors.loc <- function(postal_code){
  # Setting up variables of interest
  var.data <- list(st_as_sf(hawkers), st_as_sf(malls),
                   st_as_sf(MLRT), st_as_sf(supermarkets),
                   st_as_sf(pschools), st_as_sf(ppschools),
                   st_as_sf(ppreschools), st_as_sf(eldercare))
  var.names <- list('Hawkers', 'Malls', 'MRT/LRT',
                    'Supermarkets', 'Primary Schools', 'Premium Primary Schools',
                    'PreSchools', 'Eldercare')
  for(i in seq(1,length(var.data))){
    var.data[[i]]['Category'] <- var.names[i]
  }
  
  # External factors
  hdb <- onemap.query(postal_code)
  hdb$X <- as.numeric(hdb$X)
  hdb$Y <- as.numeric(hdb$Y)
  hdb.sf <- st_as_sf(hdb, coords = c('X','Y'), crs = 3414)
  hdb.sf.b <- st_buffer(hdb.sf, dist = 1000)
  
  hdb.var.loc <- list()
  # Finding attribute locations
  i = 1
  while(i <= length(var.data)){
    hdb.var.temp <- st_join(var.data[[i]], hdb.sf.b)
    hdb.var.temp <- hdb.var.temp[!is.na(hdb.var.temp$data_name), ] %>%
      select(Name, Category)
    hdb.var.loc <- append(hdb.var.loc, list(hdb.var.temp))
    i <- i + 1
  }
  
  hdb.var.df <- do.call('rbind', list(hdb.var.loc[[1]], hdb.var.loc[[2]],
                                      hdb.var.loc[[3]], hdb.var.loc[[4]],
                                      hdb.var.loc[[5]], hdb.var.loc[[6]],
                                      hdb.var.loc[[7]], hdb.var.loc[[8]]))
  return(list(hdb.var.df, hdb.sf.b, hdb.sf))
}


theme_blue_gradient <- shinyDashboardThemeDIY(
  
  ### general
  appFontFamily = "Tw Cen MT"
  ,appFontColor = "rgb(0,0,0)"
  ,primaryFontColor = "rgb(0,0,0)"
  ,infoFontColor = "rgb(0,0,0)"
  ,successFontColor = "rgb(0,0,0)"
  ,warningFontColor = "rgb(0,0,0)"
  ,dangerFontColor = "rgb(0,0,0)"
  ,bodyBackColor = "rgb(248,248,248)"
  
  ### header
  ,logoBackColor = "rgb(23,103,124)"
  
  ,headerButtonBackColor = "rgb(238,238,238)"
  ,headerButtonIconColor = "rgb(75,75,75)"
  ,headerButtonBackColorHover = "rgb(210,210,210)"
  ,headerButtonIconColorHover = "rgb(0,0,0)"
  
  ,headerBackColor = "rgb(238,238,238)"
  ,headerBoxShadowColor = "#aaaaaa"
  ,headerBoxShadowSize = "2px 2px 2px"
  
  ### sidebar
  ,sidebarBackColor = cssGradientThreeColors(
    direction = "down"
    ,colorStart = "rgb(20,97,117)"
    ,colorMiddle = "rgb(56,161,187)"
    ,colorEnd = "rgb(3,22,56)"
    ,colorStartPos = 0
    ,colorMiddlePos = 50
    ,colorEndPos = 100
  )
  ,sidebarPadding = 0
  
  ,sidebarMenuBackColor = "transparent"
  ,sidebarMenuPadding = 0
  ,sidebarMenuBorderRadius = 0
  
  ,sidebarShadowRadius = "3px 5px 5px"
  ,sidebarShadowColor = "#aaaaaa"
  
  ,sidebarUserTextColor = "rgb(255,255,255)"
  
  ,sidebarSearchBackColor = "rgb(55,72,80)"
  ,sidebarSearchIconColor = "rgb(153,153,153)"
  ,sidebarSearchBorderColor = "rgb(55,72,80)"
  
  ,sidebarTabTextColor = "rgb(255,255,255)"
  ,sidebarTabTextSize = 16
  ,sidebarTabBorderStyle = "none none solid none"
  ,sidebarTabBorderColor = "rgb(35,106,135)"
  ,sidebarTabBorderWidth = 3
  
  ,sidebarTabBackColorSelected = cssGradientThreeColors(
    direction = "right"
    ,colorStart = "rgba(44,222,235,1)"
    ,colorMiddle = "rgba(44,222,235,1)"
    ,colorEnd = "rgba(0,255,213,1)"
    ,colorStartPos = 0
    ,colorMiddlePos = 30
    ,colorEndPos = 100
  )
  ,sidebarTabTextColorSelected = "rgb(0,0,0)"
  ,sidebarTabRadiusSelected = "0px 20px 20px 0px"
  
  ,sidebarTabBackColorHover = cssGradientThreeColors(
    direction = "right"
    ,colorStart = "rgba(44,222,235,1)"
    ,colorMiddle = "rgba(44,222,235,1)"
    ,colorEnd = "rgba(0,255,213,1)"
    ,colorStartPos = 0
    ,colorMiddlePos = 30
    ,colorEndPos = 100
  )
  ,sidebarTabTextColorHover = "rgb(50,50,50)"
  ,sidebarTabBorderStyleHover = "none none solid none"
  ,sidebarTabBorderColorHover = "rgb(75,126,151)"
  ,sidebarTabBorderWidthHover = 1
  ,sidebarTabRadiusHover = "0px 20px 20px 0px"
  
  ### boxes
  ,boxBackColor = "rgb(255,255,255)"
  ,boxBorderRadius = 5
  ,boxShadowSize = "0px 1px 1px"
  ,boxShadowColor = "rgba(0,0,0,.1)"
  ,boxTitleSize = 20
  ,boxDefaultColor = "rgb(210,214,220)"
  ,boxPrimaryColor = "rgba(44,222,235,1)"
  ,boxInfoColor = "rgb(210,214,220)"
  ,boxSuccessColor = "rgba(0,255,213,1)"
  ,boxWarningColor = "rgb(244,156,104)"
  ,boxDangerColor = "rgb(255,88,55)"
  
  ,tabBoxTabColor = "rgb(255,255,255)"
  ,tabBoxTabTextSize = 20
  ,tabBoxTabTextColor = "rgb(0,0,0)"
  ,tabBoxTabTextColorSelected = "rgb(0,0,0)"
  ,tabBoxBackColor = "rgb(255,255,255)"
  ,tabBoxHighlightColor = "rgba(44,222,235,1)"
  ,tabBoxBorderRadius = 5
  
  ### inputs
  ,buttonBackColor = "rgb(245,245,245)"
  ,buttonTextColor = "rgb(0,0,0)"
  ,buttonBorderColor = "rgb(200,200,200)"
  ,buttonBorderRadius = 5
  
  ,buttonBackColorHover = "rgb(235,235,235)"
  ,buttonTextColorHover = "rgb(100,100,100)"
  ,buttonBorderColorHover = "rgb(200,200,200)"
  
  ,textboxBackColor = "rgb(255,255,255)"
  ,textboxBorderColor = "rgb(200,200,200)"
  ,textboxBorderRadius = 5
  ,textboxBackColorSelect = "rgb(245,245,245)"
  ,textboxBorderColorSelect = "rgb(200,200,200)"
  
  ### tables
  ,tableBackColor = "rgb(255,255,255)"
  ,tableBorderColor = "rgb(240,240,240)"
  ,tableBorderTopSize = 1
  ,tableBorderRowSize = 1
  
)

ui <- dashboardPage(title = 'Flat Earthers',
  dashboardHeader(title = strong('Flat Earthers')),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "Overview", icon = icon("home")),
      menuItem("Hedonic Pricing Model", tabName = "HDB_Search", icon = icon("donate")),
      menuItem("GWR Model", tabName = "GWR_Model", icon = icon("map-marked-alt")),
      menuItem("Demand Analysis", tabName = "Demand_Analysis", icon = icon("connectdevelop")),
      menuItem("Subzone Analysis", tabName = "Subzone_Analysis", icon = icon("first-order")),
      menuItem('Help', tabName = 'Help', icon = icon('hire-a-helper'))
    )
  ),
  dashboardBody(
    theme_blue_gradient, tags$head(tags$style(HTML(".main-sidebar { font-size: 20px; }"))),
    tabItems(
    # First tab: Overview
    tabItem(tabName = 'Overview',
      h2(strong('PROJECT OVERVIEW')),
      p('Using geospatial analytics to understand HDB resale prices'),
      fluidRow(box(width = 4, title = strong('PROJECT TOPIC'), status = 'primary', solidHeader = TRUE, height = 200,
                   textOutput(outputId = 'ProjectTopic')),
               box(width = 4, title = strong('MOTIVATION'), status = 'primary', solidHeader = TRUE, height = 200,
                   textOutput(outputId = 'Motivation')),
               box(width = 4, title = strong('DATA USED'), status = 'primary', solidHeader = TRUE, height = 200,
                   textOutput(outputId = 'DataUsed'))),
      h2(strong('APPLICATIONS')),
      p('Geospatial tools to understand HDB resale prices'),
      fluidRow(box(width = 4, title = strong('HEDONIC PRICING MODEL'), height = 200, status = 'success', solidHeader = TRUE,
                   textOutput(outputId = 'HedonicPricingModel')), 
               box(width = 4, title = strong('DEMAND ANALYSIS'), height = 200, status = 'success', solidHeader = TRUE,
                   textOutput(outputId = 'DemandAnalysis')), 
               box(width = 4, title = strong('SUBZONE ANALYSIS'), height = 200, status = 'success', solidHeader = TRUE,
                   textOutput(outputId = 'SubzoneAnalysis')))
    ),
    # Second tab: HDB Search
    tabItem(tabName = "HDB_Search",
            h2(strong('Hedonic Pricing Model')),
            p('Choose from 1 of 3 models to value a HDB flat'),
            fluidRow(
              column(width = 3, box(status = 'primary', width = NULL,
                numericInput(inputId = 'POSTAL', label = 'Postal Code: ',
                             value = 562308),
                numericInput(inputId = 'FLOOR_LEVEL', label = 'Floor Level: ',
                             value = 5),
                numericInput(inputId = 'AREA', label = 'Area(in sqm): ',
                             value = 70),
                numericInput(inputId = 'REMAINING_LEASE', label = 'Remaining Lease: ',
                             value = 92),
                selectInput(inputId = 'FLAT_MODEL', label = 'Flat Model: ',
                            choices = Flat_models,
                            selected = 'DBSS'),
                selectInput(inputId = 'FLAT_TYPE', label = 'Flat Type: ',
                            choices = Flat_type,
                            selected = '3 ROOM'),
                # selectInput(inputId = 'DIST_MEASURE', label = 'Distance Measure: ',
                #             choices = c('Euclidean Distance' = 'euclidean',
                #                         'Network Distance' = 'network')),
                radioButtons(inputId = 'MODEL', label = 'Model: ',
                            choices = c('Linear Regression' = 'model.lm', 
                                        'xgBoost' = 'model.xgboost', 
                                        'Geographical Weighted Regression' = 'model.gwr'),
                            selected = 'model.lm'),
                submitButton(text = 'Search'))
              ),
              
              column(width = 9,
                valueBoxOutput(outputId = 'HPM.Value', width = 4),
                column(width = 12, tableOutput(outputId = 'table.df')),
                leafletOutput(outputId = 'leafletmap', height = 400) %>% withSpinner()))),
    
    # Third tab: GWR Model
    tabItem(tabName = "GWR_Model",
            h2(strong('Geographical Weighted Regression Model')),
            p('A local form of linear regression used to model spatially varying relationships'),
            fluidRow(
              column(width = 3, box(status = 'primary', width = NULL,
                selectInput(inputId = 'KERNEL', label = 'Kernel: ',
                            choices = c('Gaussian' = 'gaussian', 
                                        'Exponential' = 'exponential',
                                        'Bi-Square' = 'bisquare',
                                        'Tri-Cube' = 'tricube',
                                        'BoxCar' = 'boxcar'),
                            selected = 'Gaussian'),
                selectInput(inputId = 'BWDISTANCE', 
                            label = 'Bandwidth Distance Type:',
                            choices = c('Adaptive' = 'adaptive',
                                        'Fixed' = 'fixed'), 
                            selected = 'Adaptive'),
                radioButtons(inputId = 'BWSELECTION', label = 'Bandwidth Selection: ',
                            choices = c('Auto' = 'auto',
                                         'Manual' = 'manual'),
                            selected = 'auto', inline = TRUE),
                numericInput(inputId = 'BANDWIDTH', label = 'Bandwidth: ',
                            value = 25),
                numericInput(inputId = 'GWR_POSTAL', label = 'Postal Code: ',
                             value = 562308),
                numericInput(inputId = 'GWR_FLOOR_LEVEL', label = 'Floor Level: ',
                             value = 5),
                numericInput(inputId = 'GWR_AREA', label = 'Area(in sqm): ',
                             value = 70),
                numericInput(inputId = 'GWR_REMAINING_LEASE', label = 'Remaining Lease: ',
                             value = 92),
                selectInput(inputId = 'GWR_FLAT_MODEL', label = 'Flat Model: ',
                            choices = Flat_models,
                            selected = 'DBSS'),
                selectInput(inputId = 'GWR_FLAT_TYPE', label = 'Flat Type: ',
                            choices = Flat_type,
                            selected = '3 ROOM'),
                submitButton(text = 'Search')
                )),
                
                # GWR Tabs
                column(width = 9,box(width = NULL,
                  tabBox(width = NULL,id = "GWR_Output",
                    tabPanel(title = "Model Predictions",
                             valueBoxOutput(outputId = 'LM.pred', width = 6),
                             valueBoxOutput(outputId = 'GWR.pred', width = 6),
                             tableOutput(outputId = 'gwr.table.df'),
                             leafletOutput(outputId = 'gwrleafletmap', height = 450) %>% withSpinner()),
                    tabPanel(title = "MLR Model",
                             strong(p('Diagnostic Measures')),
                             valueBoxOutput(outputId = 'LM_R2', width = 4),
                             valueBoxOutput(outputId = 'LM_AIC', width = 4),
                             valueBoxOutput(outputId = 'LM_DF', width = 4),
                             valueBoxOutput(outputId = 'LM_aR2', width = 4),
                             valueBoxOutput(outputId = 'LM_AICc', width = 4),
                             valueBoxOutput(outputId = 'LM_Sigma', width = 4),
                             strong(p('Parameter Coefficients')),
                             column(width = 12,
                             dataTableOutput(outputId = 'LM_Table') %>% withSpinner())),
                    tabPanel(title = "GWR Model",
                             strong(p('Diagnostic Measures')),
                             valueBoxOutput(outputId = 'GWR_R2', width = 4),
                             valueBoxOutput(outputId = 'GWR_AIC', width = 4),
                             valueBoxOutput(outputId = 'GWR_EDF', width = 4),
                             valueBoxOutput(outputId = 'GWR_aR2', width = 4),
                             valueBoxOutput(outputId = 'GWR_AICc', width = 4),
                             valueBoxOutput(outputId = 'GWR_ENP', width = 4),
                             strong(p('Parameter Coefficients')),
                             column(width = 4,
                                    radioButtons(inputId = 'RESULT_VAR', label = 'Result Variable: ',
                                          choices = c('Estimate' = 'estimate',
                                                      'Standard Error' = 'std_error',
                                                      't-value' = 'tvalue'),
                                          selected = 'estimate', inline = TRUE)),
                             column(width = 3,submitButton(text = 'Search')),
                             column(width = 12,
                             dataTableOutput(outputId = 'GWR_Table') %>% withSpinner()))))))),
    
    # Fourth tab: Demand Analysis
    tabItem(tabName = "Demand_Analysis",
            h2(strong('Demand Analysis')),
            p('Analyse the volume and price of resale flats'),
            fluidRow(column(width = 3,
                box(status = "primary", width = NULL,
                selectInput(inputId = 'START_DATE', label = 'Start Date',
                            choices = dates),
                selectInput(inputId = 'END_DATE', label = 'End Date',
                            choices = dates),
                radioButtons(inputId = 'DEMAND_ANALYSIS', label = 'Distribution Plot',
                            choices = c('Number of resold flats' = 'dist_flats',
                                        'Resale prices' = 'dist_prices',
                                        'Resale prices (per sqm)' = 'dist_prices_ps',
                                        'Hawker Centres' = 'hawkers.mpsz',
                                        'MRT & LRT' = 'mlrt.mpsz',
                                        'Shopping Malls' = 'malls.mpsz',
                                        'Supermarkets' = 'supermarkets.mpsz',
                                        'Primary Schools' = 'pschools.mpsz',
                                        'Premium Primary Schools' = 'ppschools.mpsz',
                                        'Premium Pre-Schools' = 'ppreschools.mpsz',
                                        'Eldercare' = 'eldercare.mpsz'),
                            selected = 'dist_flats'),
                submitButton(text = 'Search')
              )),
              column(width = 9,
                leafletOutput(outputId = 'DA_Plot', height = 480) %>% withSpinner())),
            fluidRow(column(width = 12),
                div(dataTableOutput(outputId = 'DA_Table'), 
                    style = 'text-align:center;'))
            ),
    
    # Fifth tab: Subzone Analysis
    tabItem(tabName = "Subzone_Analysis",
            h2(strong('Subzone Analysis')),
            p('Analyse the distribution of resale flats by subzone'),
            fluidRow(column(width = 3,
                box(status = "primary", width = NULL,
                selectInput(inputId = 'SA_START_DATE', label = 'Start Date',
                            choices = dates),
                selectInput(inputId = 'SA_END_DATE', label = 'End Date',
                            choices = dates),
                selectInput(inputId = 'SUBZONE', label = 'Subzone',
                            choices = sort(unique(mpsz$SUBZONE_N)),
                            selected = 'TAMPINES EAST'),
                radioButtons(inputId = 'SA_PLOT', label = 'Variable to plot: ',
                             choices = c('Resale Price' = 'Resale Price',
                                         'Resale Price(per sqm)' = 'Resale Price(per sqm)'),
                             selected = 'Resale Price'),
                submitButton(text = 'Search'))
              ),
              column(width = 9,
                tableOutput(outputId = 'sa.amenities.table'),
                leafletOutput(outputId = 'SA_TMPlot', height = 400) %>% withSpinner())),
            fluidRow(box(width = 12, height = 240,
              column(width = 4, plotlyOutput(outputId = 'SA_Plotly_rp')),
                     column(width = 4, plotlyOutput(outputId = 'SA_Plotly_fm')),
                     column(width = 4, plotlyOutput(outputId = 'SA_Plotly_ft'))))),
    
    # Sixth tab: Help and Information
    tabItem(tabName = 'Help',
            h1(strong('Help and Information')),
            tabBox(width = 12, 
                   tabPanel(title = 'Hedonic Pricing Model',
                            h3('1. What is Hedonic Pricing'),
                            h4('Hedonic pricing is a model, which identifies price factors, according to the premise that price is determined both by internal characteristics of the good being sold and external factors affecting it. A hedonic pricing model is often used to estimate quantitative values for environmental or ecosystem services that directly impact market prices for homes. '),
                            h3('2. How Hedonic Pricing Works'),
                            h4('The most common example of the hedonic pricing method is in the housing market, wherein the price of a building or piece of land is determined by the characteristics of the property itself, as well as characteristics of its surrounding environment.'),
                            h3('3. Issues of Hedonic Pricing'),
                            h4('Hedonic pricing also has significant drawbacks, including its ability to only capture consumers\' willingness to pay for what they perceive are environmental differences and their resulting consequences. For example, if potential buyers are not aware of a contaminated water supply or impending early morning construction next door, the price of the property in question will not change accordingly. Hedonic pricing also does not always incorporate external factors or regulations, such as taxes and interest rates, which could also have a significant impact on prices.')
                            ),
                   tabPanel(title = 'Geographically Weighted Regression (GWR)',
                            h3('1. What is GWR'),
                            h4('Geographically Weighted Regression (GWR) is a spatial regression technique that provides a local model of the variable or process you are trying to understand/predict by fitting a regression equation to every feature in the dataset. GWR constructs these separate equations by incorporating the dependent and explanatory variables of features falling within the bandwidth of each target feature. The shape and size of the bandwidth is dependent on user input for the Kernel type, Bandwidth method'),
                            h3('2. Kernel'),
                            tableOutput(outputId = 'kernel.table'),
                            h3('3. Bandwidth type'),
                            tableOutput(outputId = 'bandwidth_type.table'),
                            h3('4. Bandwidth'),
                            h4('Value to indicate the size of the bandwidth used in the weighting function, possibly calculated by using a fixed or adaptive distance')
                            ), 
                   tabPanel(title = 'Multi Linear Regression (MLR)',
                            h3('1. What is Multi Linear Regression (MLR)'),
                            h4('In statistics, linear regression is a linear approach to modelling the relationship between a scalar response (or dependent variable) and one or more explanatory variables (or independent variables). The case of one explanatory variable is called simple linear regression. For more than one explanatory variable, the process is called multiple linear regression. This term is distinct from multivariate linear regression, where multiple correlated dependent variables are predicted, rather than a single scalar variable')
                            ),
                   tabPanel(title = 'Diagnostic Measures',
                            tableOutput(outputId = 'diagnostic.table'))
                   ))
    )
  )
)

server <- function(input, output) {
##################################################################################  
##############################  PROJECT OVERVIEW  ###############################
##################################################################################
output$ProjectTopic <- renderText({
  'In Singapore, the large majority of the population live in HDB flats. Given the scarcity of land, housing prices tend to hold a large price tag as with HDB flats. HDB prices could be affected due to various internal and external factors. While there may be several factors that are glaringly apparent, it may not be clear as to which factors have a higher weightage in affecting the prices. Thus, we aim to use geospatial analytics to create a hedonic pricing model to value HDB flats.'
})  
output$Motivation <- renderText({
  'As Singapore government ramps up efforts to collect and share data sets to the public, various kinds of information are readily available. There are however, no readily available tools that could allow analysts who are well versed in real estate, but do not have the coding know-how to perform analysis. Thus, our group hopes to bridge the gap and provide these analysts with a friendly user interface and experience.'
})  
output$DataUsed <- renderText({
  'We used the publicly available data from Data.gov.sg to obtain the resale prices of HDB flats, locational data of hawker centres, shopping malls, supermarkets, MRTs & LRTs, Primary Schools, Pre-Schools and Eldercare'
})  
output$HedonicPricingModel <- renderText({
  'Using Multiple Linear Regression (MLR) and Geographically Weighted Regression (GWR), we are able to determine the value of a HDB flat based on its internal and external factors.'
})  
output$DemandAnalysis <- renderText({
  'To supplement the hedonic pricing model, we created a choropleth map to visualize the demand for resale flats and the distribution of amenities by the different subzones in Singapore which can be filtered by time.'
})  
output$SubzoneAnalysis <- renderText({
  'After looking at the overall demand of resale flats, we then want to find out the reason for the mean resale price in each subzone. Thus, we created a dashboard to look at the distribution of the resale prices, flat type and flat models for each subzone as well as an accompanying bubble map to visualize the resale prices of each flat and the amenities in the subzone.'
})  

##################################################################################  
#############################  HEDONIC PRICE MODEL  ##############################
################################################################################## 
  # Running regression models
  hdb.data <- reactive({
    req(input$MODEL)               
    predictors <- c('Floor_area', 'Remaining_lease', 'Storey',
                    '1 ROOM', '2 ROOM', '3 ROOM', '4 ROOM', '5 ROOM',
                    'EXECUTIVE', 'MULTI-GENERATION', 
                    'Adjoined flat', 'Apartment', 'DBSS', 'Improved',
                    'Improved-Maisonette', 'Maisonette', 'Model A', 'Model A-Maisonette',
                    'Model A2', 'Multi Generation', 'New Generation', 
                    'Premium Apartment', 'Premium Apartment Loft', 'Premium Apartment.',
                    'Premium Maisonette', 'Simplified', 'Standard', 'Terrace', 
                    'Type S1', 'Type S2',
                    'Hawker_Count', 'Mall_Count', 'MLRT_Count', 
                    'PriSchool_Count', 'PPriSchool_Count', 
                    'Supermarket_Count', 'PPreSchool_Count',
                    'Eldercare_Count')
    
    hdb.searched <- hdb.ext.factors(flat_type = input$FLAT_TYPE, 
                                    flat_model = input$FLAT_MODEL, 
                                    floor = input$FLOOR_LEVEL, 
                                    floor_area = input$AREA, 
                                    remaining_lease = input$REMAINING_LEASE, 
                                    postal_code = input$POSTAL)
    
    if(input$MODEL == 'model.xgboost'){
      model.xgboost <- readRDS('data/models/xgboost.rds')
      hdb.price <- predict.train(object = model.xgboost, 
                                 hdb.searched[[1]][,predictors], type="raw")
    }
    else if(input$MODEL == 'model.lm'){
      model.lm <- readRDS('data/models/linear regression.rds')
      hdb.price <- predict.train(object = model.lm, 
                                 hdb.searched[[1]][,predictors], type="raw")
    }
    else if(input$MODEL == 'model.gwr'){
      if(input$FLAT_MODEL %in% c('Adjoined flat','Apartment',
                                 'Multi Generation', 'Terrace', 'Type S1', 'Type S2')){
        input_FLAT_MODEL <- 'DBSS'
      }
      else if(input$FLAT_MODEL %in% c('Improved-Maisonette','Model A-Maisonette')){
        input_FLAT_MODEL <- 'Maisonette'
      }
      else if(input$FLAT_MODEL %in% c('Model A2')){
        input_FLAT_MODEL <- 'Model A'
      }
      else if(input$FLAT_MODEL %in% c('New Generation','Standard')){
        input_FLAT_MODEL <- 'Improved'
      }
      else{
        input_FLAT_MODEL <- input$FLAT_MODEL
      }
      preds <- gwr.predict(formula = Resale_price ~ Floor_area + 
                             Remaining_lease + Storey + Flat_type +
                             Hawker_Count + Mall_Count + MLRT_Count + 
                             PPriSchool_Count + PriSchool_Count + Supermarket_Count +
                             PPreSchool_Count + Eldercare_Count,
                           data = hdb.comp.sp[hdb.comp.sp$Flat_model == input_FLAT_MODEL,],
                           predictdata = as_Spatial(hdb.searched[[2]]) ,
                           bw = 25,
                           kernel = 'gaussian',
                           adaptive = TRUE,
                           longlat = FALSE)
      
      hdb.price <- preds$SDF$prediction
    }
    hdb.price
  })

  # Table
  output$table.df <- renderTable({
    hdb.searched <- hdb.ext.factors(flat_type = input$FLAT_TYPE,
                                    flat_model = input$FLAT_MODEL,
                                    floor = input$FLOOR_LEVEL,
                                    floor_area = input$AREA,
                                    remaining_lease = input$REMAINING_LEASE,
                                    postal_code = input$POSTAL)
    hdb.searched <- hdb.searched[[1]][1:8]
    colnames(hdb.searched) <- c('Hawker Count', 'Mall Count', 'MRT/LRT Count',
                                'Primary School Count', 'Premium Primary School Count',
                                'Supermarket Count', 'Preschool Count',
                                'Eldercare Count')
    hdb.searched
    },
    bordered = TRUE, hover = TRUE, striped = TRUE, digits = 0, align = 'c')

  # Prediction
  output$HPM.Value <- renderValueBox({
    pred <- hdb.data()
    valueBox(value = paste('$',round(pred,0)), subtitle = 'HDB Valuation',
             width = 6, color = 'teal', icon = icon('building'))
  })

  
  # Plot searched HDB flat and attributes
  output$leafletmap <- renderLeaflet({
    hdb <- hdb.ext.factors.loc(input$POSTAL)
    `1km Radius` <- hdb[[2]]
    Amenities <- hdb[[1]]
    `HDB Flat`<- hdb[[3]] %>% select(ADDRESS, POSTAL, ROAD_NAME, BUILDING)
    
    tm <- tm_shape(`1km Radius`) +
        tm_polygons(col = 'blue', alpha = 0.05, popup.vars = FALSE) +
      tm_shape(Amenities) +
        tm_dots('Category', size = 0.3, border.alpha = 0.3) +
      tm_shape(`HDB Flat`) +
        tm_markers(size = 1, id = 'ADDRESS')
    
    tmap_leaflet(tm)
  })

##################################################################################  
###################################  GWR MODEL   #################################
##################################################################################   
    
  # HDB Data for GWR Tab
  gwr.hdb.data <- reactive({
    req(input$GWR_FLAT_MODEL)
    hdb.searched <- hdb.ext.factors(flat_type = input$GWR_FLAT_TYPE, 
                                    flat_model = input$GWR_FLAT_MODEL, 
                                    floor = input$GWR_FLOOR_LEVEL, 
                                    floor_area = input$GWR_AREA, 
                                    remaining_lease = input$GWR_REMAINING_LEASE, 
                                    postal_code = input$GWR_POSTAL)
    hdb.searched <- hdb.searched[[1]][1:13]
    colnames(hdb.searched) <- c('Hawker Count', 'Mall Count', 'MRT/LRT Count',
                                'Primary School Count', 'Premium Primary School Count',
                                'Supermarket Count', 'Preschool Count',
                                'Eldercare Count', 
                                'Flat Type', 'Flat Model', 'Floor',
                                'Floor area', 'Remaining lease')
    hdb.searched <- hdb.searched[,1:8]
    hdb.searched
    
  })
  
  # HDB Data of GWR tab
  output$gwr.table.df <- renderTable({
    gwr.table <- gwr.hdb.data()
    gwr.table}, 
    bordered = TRUE, hover = TRUE, striped = TRUE, digits = 0, align = 'c')
  
  # Leaflet for GWR Tab
  output$gwrleafletmap <- renderLeaflet({
    hdb <- hdb.ext.factors.loc(input$GWR_POSTAL)
    `1km Radius` <- hdb[[2]]
    Amenities <- hdb[[1]]
    `HDB Flat`<- hdb[[3]] %>% select(ADDRESS, POSTAL, ROAD_NAME, BUILDING)
    tm <- tm_shape(`1km Radius`) +
      tm_polygons(col = 'blue', alpha = 0.05, popup.vars = FALSE) +
      tm_shape(Amenities) +
      tm_dots('Category', size = 0.3, border.alpha = 0.3) +
      tm_shape(`HDB Flat`) +
      tm_markers(size = 1, id = 'ADDRESS')
    
    tmap_leaflet(tm)
  })
  
  # Bandwidth selection
  GWR_BW <- reactive({
    if(input$GWR_FLAT_MODEL %in% c('Adjoined flat','Apartment',
                                   'Multi Generation', 'Terrace', 'Type S1', 'Type S2')){
      input_FLAT_MODEL <- 'DBSS'
    }
    else if(input$GWR_FLAT_MODEL %in% c('Improved-Maisonette','Model A-Maisonette')){
      input_FLAT_MODEL <- 'Maisonette'
    }
    else if(input$GWR_FLAT_MODEL %in% c('Model A2')){
      input_FLAT_MODEL <- 'Model A'
    }
    else if(input$GWR_FLAT_MODEL %in% c('New Generation','Standard')){
      input_FLAT_MODEL <- 'Improved'
    }
    else{
      input_FLAT_MODEL <- input$GWR_FLAT_MODEL
    }
    bw.adaptive <- bw.gwr(formula = Resale_price ~ Floor_area + 
                            Remaining_lease + Storey + Flat_type + Hawker_Count +
                            Mall_Count + MLRT_Count + PPriSchool_Count +
                            PriSchool_Count + Supermarket_Count + 
                            PPreSchool_Count + Eldercare_Count,
                          data=hdb.comp.sp[hdb.comp.sp$Flat_model == input_FLAT_MODEL,], 
                          approach="CV",
                          kernel=input$KERNEL,
                          adaptive=ifelse(input$BWDISTANCE == 'adaptive',TRUE, FALSE),
                          longlat = FALSE)
    bw.adaptive
  })
  
  # Basic GWR Model
  GWR_Model <- reactive({
    req(input$GWR_FLAT_MODEL, input$BWSELECTION)
    if(input$GWR_FLAT_MODEL %in% c('Adjoined flat','Apartment',
                                   'Multi Generation', 'Terrace', 'Type S1', 'Type S2')){
      input_FLAT_MODEL <- 'DBSS'
    }
    else if(input$GWR_FLAT_MODEL %in% c('Improved-Maisonette','Model A-Maisonette')){
      input_FLAT_MODEL <- 'Maisonette'
    }
    else if(input$GWR_FLAT_MODEL %in% c('Model A2')){
      input_FLAT_MODEL <- 'Model A'
    }
    else if(input$GWR_FLAT_MODEL %in% c('New Generation','Standard')){
      input_FLAT_MODEL <- 'Improved'
    }
    else{
      input_FLAT_MODEL <- input$GWR_FLAT_MODEL
    }
    
    gwr <- gwr.basic(formula = Resale_price ~ Floor_area + 
                       Remaining_lease + Storey + Flat_type +
                       Hawker_Count + Mall_Count + MLRT_Count + 
                       PPriSchool_Count + PriSchool_Count + Supermarket_Count +
                       PPreSchool_Count + Eldercare_Count,
                     data = hdb.comp.sp[hdb.comp.sp$Flat_model == input_FLAT_MODEL,],
                     bw = ifelse(input$BWSELECTION == 'auto',GWR_BW() ,
                                        input$BANDWIDTH),
                     kernel = input$KERNEL,
                     adaptive = ifelse(input$BWDISTANCE == 'adaptive',TRUE, FALSE),
                     longlat = FALSE)
  })
  
  # Output for MLR Model
  output$LM_Table <- renderDataTable({
    gwr <- GWR_Model()
    gwr.lm <- as.data.frame(summary(gwr$lm)[4]$`coefficients`,
                            row.names = c('Intercept', 'Floor Area', 'Remaining Lease',
                                          'Storey', 'Flat Type', 'Hawker Count',
                                          'Mall Count', 'MRT/LRT Count',
                                          'Premium Primary School Count',
                                          'Primary School Count',
                                          'Supermarket Count',
                                          'PreSchool Count', 'Eldercare Count'))
    gwr.lm <- round(gwr.lm, 3)
    gwr.lm$Parameter <- row.names(gwr.lm)
    gwr.lm <- gwr.lm %>% select(Parameter, Estimate, `Std. Error`, `t value`,
                                `Pr(>|t|)`)
    gwr.lm},
    options = list(pageLength = 8, searching = FALSE, lengthChange = FALSE,
                   autoWidth = TRUE))
  
  output$LM_R2 <- renderValueBox({
    gwr <- GWR_Model()
    lm.r2 <- summary(gwr$lm)$`r.squared`
    lm.r2
    valueBox(subtitle = 'R-Squared', round(lm.r2,3), icon = icon('registered'), color = 'aqua')
  })
  
  output$LM_AIC <- renderValueBox({
    gwr <- GWR_Model()
    MLR.AIC <- AIC(gwr$lm, k=0)
    MLR.AIC
    valueBox(subtitle = 'AIC', round(MLR.AIC,0), icon = icon('amilia'), color = 'purple')
  })  
  
  output$LM_DF <- renderValueBox({
    gwr <- GWR_Model()
    lm.df <- unlist(summary(gwr$lm)$df)[2]
    lm.df
    valueBox(subtitle = 'Degrees of Freedom', lm.df, icon = icon('equals'), color = 'red')
  })
  
  output$LM_aR2 <- renderValueBox({
    gwr <- GWR_Model()
    lm.ar2 <- summary(gwr$lm)$`adj.r.squared`
    lm.ar2
    valueBox(subtitle = 'Adjusted R-Squared', round(lm.ar2,3), icon = icon('registered'), color = 'teal')
  })
  
  output$LM_AICc <- renderValueBox({
    gwr <- GWR_Model()
    MLR.AICc <- AIC(gwr$lm, k=2)
    MLR.AICc
    valueBox(subtitle = 'AICc', round(MLR.AICc,0), icon = icon('amilia'), color = 'maroon')
  })
  
  output$LM_Sigma <- renderValueBox({
    gwr <- GWR_Model()
    lm.sigma <- summary(gwr$lm)$`sigma`
    lm.sigma
    valueBox(subtitle = 'Sigma', round(lm.sigma,0), icon = icon('stripe-s'), color = 'yellow')
  })
  
  # Output for GWR Model
  output$GWR_Table <- renderDataTable({
    gwr <- GWR_Model()
    gwr.table <- as.data.frame(gwr$SDF)
    if(input$RESULT_VAR == 'estimate'){
      gwr.table <- gwr.table[,1:13]
      colnames(gwr.table) <- c('Intercept', 'Floor Area', 'Remaining Lease',
                               'Storey', 'Flat Type', 'Hawker Count',
                               'Mall Count', 'MRT/LRT Count',
                               'Premium Primary School Count',
                               'Primary School Count',
                               'Supermarket Count',
                               'PreSchool Count', 'Eldercare Count')
      gwr.table <- round(gwr.table[,],0)
    }
    else if(input$RESULT_VAR == 'std_error'){
      gwr.table <- gwr.table[,19:31]
      colnames(gwr.table) <- c('Intercept', 'Floor Area', 'Remaining Lease',
                               'Storey', 'Flat Type', 'Hawker Count',
                               'Mall Count', 'MRT/LRT Count',
                               'Premium Primary School Count',
                               'Primary School Count',
                               'Supermarket Count',
                               'PreSchool Count', 'Eldercare Count')
      gwr.table <- round(gwr.table[,],0)
    }
    else if(input$RESULT_VAR == 'tvalue'){
      gwr.table <- gwr.table[,32:44]
      colnames(gwr.table) <- c('Intercept', 'Floor Area', 'Remaining Lease',
                               'Storey', 'Flat Type', 'Hawker Count',
                               'Mall Count', 'MRT/LRT Count',
                               'Premium Primary School Count',
                               'Primary School Count',
                               'Supermarket Count',
                               'PreSchool Count', 'Eldercare Count')
      gwr.table <- round(gwr.table[,],2)
    }
    gwr.table},
    options = list(pageLength = 5, searching = FALSE, lengthChange = FALSE,
                   autoWidth = TRUE, scrollX = TRUE))
  
  output$GWR_R2 <- renderValueBox({
    gwr <- GWR_Model()
    gwr.r2 <- gwr$GW.diagnostic$gw.R2
    valueBox(subtitle = 'R-Squared', round(gwr.r2,3), icon = icon('registered'), color = 'aqua')
  })

  output$GWR_AIC <- renderValueBox({
    gwr <- GWR_Model()
    GWR.AIC <- gwr$GW.diagnostic$AIC
    valueBox(subtitle = 'AIC', round(GWR.AIC,0), icon = icon('amilia'), color = 'purple')
  })
  
  output$GWR_ENP <- renderValueBox({
    gwr <- GWR_Model()
    GWR.ENP <- gwr$GW.diagnostic$enp
    valueBox(subtitle = 'Effective Number of Parameters', round(GWR.ENP,0), icon = icon('etsy'), color = 'yellow')
  })
  
  output$GWR_aR2 <- renderValueBox({
    gwr <- GWR_Model()
    gwr.ar2 <- gwr$GW.diagnostic$gwR2.adj
    valueBox(subtitle = 'Adjusted R-Squared', round(gwr.ar2,3), icon = icon('registered'), color = 'teal')
  })
  
  output$GWR_AICc <- renderValueBox({
    gwr <- GWR_Model()
    GWR.AICc <- gwr$GW.diagnostic$AICc
    valueBox(subtitle = 'AICc', round(GWR.AICc,0), icon = icon('amilia'), color = 'maroon')
  })
  
  output$GWR_EDF <- renderValueBox({
    gwr <- GWR_Model()
    GWR.EDF <- gwr$GW.diagnostic$edf
    valueBox(subtitle = 'Effective Degrees of Freedom', round(GWR.EDF,0), icon = icon('etsy'), color = 'red')
  })
  
  # MLR Predction
  mlr.pred <- reactive({
    req(input$GWR_FLAT_MODEL)
    gwr <- GWR_Model()
    hdb.searched <- hdb.ext.factors(flat_type = input$GWR_FLAT_TYPE, 
                                    flat_model = input$GWR_FLAT_MODEL, 
                                    floor = input$GWR_FLOOR_LEVEL, 
                                    floor_area = input$GWR_AREA, 
                                    remaining_lease = input$GWR_REMAINING_LEASE, 
                                    postal_code = input$GWR_POSTAL)
    preds <- predict(gwr$lm, hdb.searched[[2]])[[1]]
    preds
  })
  
  # GWR Prediction
  gwr.pred <- reactive({
    req(input$GWR_FLAT_MODEL)
    hdb.searched <- hdb.ext.factors(flat_type = input$GWR_FLAT_TYPE, 
                                    flat_model = input$GWR_FLAT_MODEL, 
                                    floor = input$GWR_FLOOR_LEVEL, 
                                    floor_area = input$GWR_AREA, 
                                    remaining_lease = input$GWR_REMAINING_LEASE, 
                                    postal_code = input$GWR_POSTAL)
    
    if(input$GWR_FLAT_MODEL %in% c('Adjoined flat','Apartment',
                               'Multi Generation', 'Terrace', 'Type S1', 'Type S2')){
      input_FLAT_MODEL <- 'DBSS'
    }
    else if(input$GWR_FLAT_MODEL %in% c('Improved-Maisonette','Model A-Maisonette')){
      input_FLAT_MODEL <- 'Maisonette'
    }
    else if(input$GWR_FLAT_MODEL %in% c('Model A2')){
      input_FLAT_MODEL <- 'Model A'
    }
    else if(input$GWR_FLAT_MODEL %in% c('New Generation','Standard')){
      input_FLAT_MODEL <- 'Improved'
    }
    else{
      input_FLAT_MODEL <- input$GWR_FLAT_MODEL
    }
    preds <- gwr.predict(formula = Resale_price ~ Floor_area + 
                           Remaining_lease + Storey + Flat_type +
                           Hawker_Count + Mall_Count + MLRT_Count + 
                           PPriSchool_Count + PriSchool_Count + Supermarket_Count +
                           PPreSchool_Count + Eldercare_Count,
                         data = hdb.comp.sp[hdb.comp.sp$Flat_model == input_FLAT_MODEL,],
                         predictdata = as_Spatial(hdb.searched[[2]]) ,
                         bw = ifelse(input$BWSELECTION == 'auto',GWR_BW() ,
                                     input$BANDWIDTH),
                         kernel = input$KERNEL,
                         adaptive = ifelse(input$BWDISTANCE == 'adaptive',TRUE, FALSE),
                         longlat = FALSE)
    
    hdb.price <- preds$SDF$prediction
  })
  
  output$GWR.pred <- renderValueBox({
    gwr.pred <- gwr.pred()
      valueBox(subtitle = 'GWR Valuation', paste('$',round(gwr.pred,0)),
               width = 6, color = 'teal', icon = icon('building'))
    })
  
  output$LM.pred <- renderValueBox({
    lm.pred <- mlr.pred()
    valueBox(subtitle = 'MLR Valuation', paste('$',round(lm.pred,0)),
             width = 6, icon = icon('building'))
  })  
  
  
  
##################################################################################  
################################  DEMAND ANALYSIS   ##############################
##################################################################################  
  # Filter HDB data
  hdb.date <- reactive({
    req(input$END_DATE, input$START_DATE)
    
    hdb.data <- read_csv('data/aspatial/HDB Resale Prices with Location Data (2015-2018).csv')
    hdb.data$X <- as.numeric(hdb.data$X)
    hdb.data$Y <- as.numeric(hdb.data$Y)
    hdb.data$date <- as.yearmon(hdb.data$month, format = '%Y-%m')
    hdb.data <- filter(hdb.data, date >= input$START_DATE, date <= input$END_DATE)
    hdb.data.sf <- st_as_sf(hdb.data, coords = c('X','Y'), crs = 3414)
    
    plot.data <- as.data.frame(st_join(st_as_sf(hdb.data.sf), mpsz))
    plot.data <- plot.data %>% group_by(SUBZONE_N) %>%
      mutate(resale_mean = mean(resale_price)) %>%
      mutate(sz_count = n()) %>%
      mutate(resale_psm_mean = mean(sum(resale_price)/ sum(floor_area_sqm))) %>%
      select(SUBZONE_N, resale_mean, sz_count, resale_psm_mean) %>%
      ungroup() %>%
      distinct()
    plot.data <- left_join(mpsz,plot.data, by = c('SUBZONE_N' = 'SUBZONE_N')) %>%
      rename('Number of resold flats' = 'sz_count', 'Mean resale price' = 'resale_mean', 'Mean resale price(per sqm)'= 'resale_psm_mean')
    plot.data
  })
  
  # Amenities by subzone
  mpsz.amenities <- reactive({
    var.data = list(st_as_sf(hawkers),st_as_sf(malls),st_as_sf(MLRT),
                    st_as_sf(supermarkets),st_as_sf(pschools),st_as_sf(ppschools),
                    st_as_sf(ppreschools),st_as_sf(eldercare))
    var.names = list('hawkers.mpsz', 'malls.mpsz', 'mlrt.mpsz', 'supermarkets.mpsz',
                     'pschools.mpsz', 'ppschools.mpsz',
                     'ppreschools.mpsz','eldercare.mpsz')
    mpsz.data = list()
    for(i in seq(1,length(var.data))){
      k <- st_join(mpsz, var.data[[i]])
      k <- k %>% group_by(SUBZONE_N) %>%
        mutate(Count = n_distinct(Name, na.rm = TRUE)) %>%
        distinct(SUBZONE_N, .keep_all= TRUE) %>%
        select(SUBZONE_N, Count)
      k[k$Count == 0, 'Count'] <- NA
      mpsz.data <- append(mpsz.data, list(k))
    }
    
    mpsz.data.df <- as.data.frame(matrix(0,8,1), row.names = var.names)
    colnames(mpsz.data.df) <- 'var'
    mpsz.data.df$var <- mpsz.data
    mpsz.data.df
  })
    
  # Plot distribution for Demand Analysis
  output$DA_Plot <- renderLeaflet({
    req(input$DEMAND_ANALYSIS)
    mpsz.data.df <- mpsz.amenities()
    if(input$DEMAND_ANALYSIS == 'dist_flats'){
      `Number of Resold Flats` <- hdb.date()
      tm <- tm_shape(`Number of Resold Flats`) +
        tm_fill('Number of resold flats', id = 'SUBZONE_N',
                textNA = 'None', contrast = c(0,1), colorNA = NULL,
                palette = 'YlGn') +
        tm_borders(col = 'black', alpha = 0.1)
      tmap_leaflet(tm)
    }
    else if(input$DEMAND_ANALYSIS == 'dist_prices'){
      `Mean Resale Price` <- hdb.date()
      tm <- tm_shape(`Mean Resale Price`) +
        tm_fill('Mean resale price', id = 'SUBZONE_N',
                textNA = 'None', contrast = c(0,1), colorNA = NULL,
                palette = 'YlGn') +
        tm_borders(col = 'black', alpha = 0.1)
      tmap_leaflet(tm)       
    }
    else if(input$DEMAND_ANALYSIS == 'dist_prices_ps'){
      `Mean Resale Price(per sqm)` <- hdb.date()
      tm <- tm_shape(`Mean Resale Price(per sqm)`) +
        tm_fill('Mean resale price(per sqm)', id = 'SUBZONE_N',
                textNA = 'None', contrast = c(0,1), colorNA = NULL,
                palette = 'YlGn') +
        tm_borders(col = 'black', alpha = 0.1)
      tmap_leaflet(tm)
    }
    else if(input$DEMAND_ANALYSIS %in% amenities.name){
      Amenities <- mpsz.data.df[input$DEMAND_ANALYSIS,][[1]]
      tm <- tm_shape(Amenities) +
        tm_fill('Count', style = 'pretty', id = 'SUBZONE_N',
                textNA = 'None', contrast = c(0,1), colorNA = NULL,
                palette = 'YlGn') +
        tm_borders(col = 'black', alpha = 0.1)
      tmap_leaflet(tm)
    }
  })
  
  # Data table for Demand Analysis
  output$DA_Table <- renderDataTable({
    mpsz.data.df <- mpsz.amenities()
    hdb.df <- as.data.frame(hdb.date()) %>% 
      select('SUBZONE_N','Mean resale price','Mean resale price(per sqm)','Number of resold flats')
    for(i in seq(1,length(amenities.name))){
      y.df <- as.data.frame(mpsz.data.df[amenities.name[[i]],][[1]]['Count']) %>% 
        select('Count')
      hdb.df <- cbind(hdb.df, y.df)
    }
    colnames(hdb.df)[1:12] <- c('Subzone', 'Mean Resale Price', 'Mean Resale Price(per sqm)',
                                'Number of Resold Flats', 'Hawker Count', 
                                'Mall Count', 'MRT/LRT Count', 
                                'Supermarket Count', 'Primary School Count', 
                                'Premium Primary School Count', 'PreSchool Count',
                                'Eldercare Count')
    hdb.df$`Mean Resale Price` <- ifelse(hdb.df$`Mean Resale Price` == '',
                                         paste(''),
                                         paste('$', round(hdb.df$`Mean Resale Price`)))
    hdb.df$`Mean Resale Price(per sqm)` <- ifelse(hdb.df$`Mean Resale Price(per sqm)` == '',
                                         paste(''),
                                         paste('$', round(hdb.df$`Mean Resale Price(per sqm)`)))
    hdb.df},
    options = list(pageLength = 3, searching = FALSE, lengthChange = FALSE))

##################################################################################  
################################  SUBZONE ANALYSIS   #############################
##################################################################################    
  
  # Data for Subzone Analysis
  sa.hdb <- reactive({
    req(input$SA_END_DATE, input$SA_START_DATE)
    
    data <- read_csv('data/aspatial/HDB Resale Prices with Location Data (2015-2018).csv')
    data$X <- as.numeric(data$X)
    data$Y <- as.numeric(data$Y)
    data$date <- as.yearmon(data$month, format = '%Y-%m')
    data <- filter(data, date >= input$SA_START_DATE, date <= input$SA_END_DATE)
    data.sf <- st_as_sf(data, coords = c('X','Y'), crs = 3414)
    
    hdb.mpsz <- st_join(data.sf, mpsz)
    hdb.mpsz.f <- hdb.mpsz %>% filter(SUBZONE_N == input$SUBZONE) %>%
      select(flat_type, flat_model, floor_area_sqm, resale_price, POSTAL, ADDRESS, BUILDING, ROAD_NAME, geometry)
    hdb.mpsz.f
  })
  
  sa.mpsz <- reactive({
    req(input$SUBZONE)
    mpsz.f <- mpsz %>% filter(SUBZONE_N == input$SUBZONE)
  })
  
  # Table for number of amenities
  sa.amenities <- reactive({
    req(input$SUBZONE)
    mpsz.data.df <- mpsz.amenities()
    hdb.df <- as.data.frame(hdb.date()) %>% 
      select('SUBZONE_N','Mean resale price','Number of resold flats')
    for(i in seq(1,length(amenities.name))){
      y.df <- as.data.frame(mpsz.data.df[amenities.name[[i]],][[1]]['Count']) %>% 
        select('Count')
      hdb.df <- cbind(hdb.df, y.df)
    }
    colnames(hdb.df)[1:11] <- c('Subzone', 'Mean Resale Price', 
                                'Number of Resold Flats', 'Hawker Count', 
                                'Mall Count', 'MRT/LRT Count', 
                                'Supermarket Count', 'Primary School Count', 
                                'Premium Primary School Count', 'PreSchool Count',
                                'Eldercare Count')
    hdb.df$`Mean Resale Price` <- ifelse(hdb.df$`Mean Resale Price` == '',
                                         paste(''),
                                         paste('$', round(hdb.df$`Mean Resale Price`)))
    hdb.df <- filter(hdb.df, Subzone == input$SUBZONE) %>% select(-Subzone)
    hdb.df})
  
  output$sa.amenities.table <- renderTable({
    sa.amenities()},
    digits = 0, colnames = TRUE, bordered = TRUE, striped = TRUE)
  
  # Plot for amenities
  sa.amenities.plot <- reactive({
    req(input$SUBZONE)
    var.data = list(st_as_sf(hawkers),st_as_sf(malls),st_as_sf(MLRT),
                    st_as_sf(supermarkets),st_as_sf(pschools),st_as_sf(ppschools),
                    st_as_sf(ppreschools),st_as_sf(eldercare))
    var.names = list('Hawkers', 'Malls', 'MRT/LRT',
                     'Supermarkets', 'Primary Schools', 'Premium Primary Schools',
                     'PreSchools', 'Eldercare')
    for(i in seq(1,length(var.data))){
      var.data[[i]] <- select(var.data[[i]], Name)  
      var.data[[i]]['Category'] <- var.names[i] 
    }
    
    mpsz.data = list()
    for(i in seq(1,length(var.data))){
      k <- st_join(var.data[[i]],mpsz)
      k <- k %>% filter(SUBZONE_N == input$SUBZONE) %>% select(Name, Category)
      mpsz.data <- append(mpsz.data, list(k))
    }
    amenities.data <- do.call('rbind', mpsz.data)
    amenities.data
  })
  
  # tmap plot for Subzone Analysis
  output$SA_TMPlot <- renderLeaflet({
    if(nrow(sa.hdb()) == 0){
      tm <- tm_shape(mpsz) +
        tm_polygons(col = 'grey', alpha = 0.05, popup.vars = FALSE)
      tmap_leaflet(tm)}
    else{
      Flats <- sa.hdb()
      colnames(Flats) <- c('Flat Type','Flat Model', 'Floor Area(in sqm)', 'Resale Price', 'Postal Code', 'Address', 'Building', 'Road Name', 'geometry')
      Flats$`Resale Price` <- round(Flats$`Resale Price`,0)
      Flats$`Postal Code` <- as.character(Flats$`Postal Code`)
      Flats$`Resale Price(per sqm)` <- round(Flats$`Resale Price` / Flats$`Floor Area(in sqm)`,0)
      Flats$rp <- paste('$', round(Flats$`Resale Price`,0))
      Flats$rpps <- paste('$', round(Flats$`Resale Price(per sqm)`,0))
      
      Subzone <- sa.mpsz()
      Amenities <- sa.amenities.plot()
      tm <- tm_shape(Flats) +
          tm_bubbles(col = input$SA_PLOT, size = input$SA_PLOT, style = 'pretty',
                   id = ifelse(input$SA_PLOT == 'Resale Price', 'rp', 'rpps'),
                   border.alpha = 0, perceptual = TRUE, 
                   scale = 1.2, palette = 'YlGn', alpha = 1, 
                   popup.vars = c('Flat Type','Flat Model', 'Floor Area(in sqm)','Resale Price','Resale Price(per sqm)', 'Postal Code')
                   ) +
        tm_shape(Subzone) +
          tm_polygons(col = 'blue', alpha = 0.01, popup.vars = FALSE) +
        tm_shape(Amenities) +
          tm_markers(size = 0.2, clustering = FALSE, perceptual = FALSE)
      tmap_leaflet(tm)}
  })
  
  # Plotly for Subzone Analysis
  output$SA_Plotly_rp <- renderPlotly({
    hdb.mpsz.f <- sa.hdb()
    rp <- plot_ly(
      x = hdb.mpsz.f$resale_price,
      type = 'histogram',
      marker = list(line = list(color = 'rgb(0,0,0)', width = 1))) %>% 
      layout(title = 'Distribution of Resale Prices',yaxis = list(title = 'Count'), xaxis = list(title = 'Resale Price'), autosize = F, height = 240, width = 400)
    rp
  })
  
  output$SA_Plotly_ft <- renderPlotly({
    hdb.mpsz.f <- sa.hdb()
    ft <- plot_ly(
      x = hdb.mpsz.f$flat_type,
      type = 'histogram',
      marker = list(color = brewer.pal(6, 'Blues'), line = list(color = 'rgb(0,0,0)', width = 1))) %>% 
      layout(title = 'Distribution of Flat Types',yaxis = list(title = 'Count'),autosize = F, height = 240, width = 400)
  })
  
  output$SA_Plotly_fm <- renderPlotly({
    hdb.mpsz.f <- sa.hdb()
    fm <- plot_ly(
      x = hdb.mpsz.f$flat_model,
      type = 'histogram',
      marker = list(color = brewer.pal(9, 'Blues'), line = list(color = 'rgb(0,0,0)', width = 1))) %>% 
      layout(title = 'Distribution of Flat Models',yaxis = list(title = 'Count'),autosize = F, height = 240, width = 400)
    fm
  })
  
##################################################################################  
#######################################  HELP   ##################################
##################################################################################  
  output$kernel.table <- renderTable({
    kernel.data <- read_csv('data/text/kernel.csv')
    kernel.df <- as.data.frame(kernel.data)
  })
  
  output$bandwidth_type.table <- renderTable({
    bw.data <- read_csv('data/text/bandwidth type.csv')
    bw.df <- as.data.frame(bw.data)
  })
  
  output$diagnostic.table <- renderTable({
    diag.data <- read_csv('data/text/diagnostic stats.csv')
    diag.df <- as.data.frame(diag.data)
  })
  
}

shinyApp(ui, server)
