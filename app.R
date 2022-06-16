library(leaflet)
library(rgdal)
library(tidyverse)
library(shiny)
library(maps)
library(shinyFeedback)
library(echarts4r)
library(shinycssloaders)
library(shinyjs)
library(readxl)

counties_kenya <-read_excel("~/Programming/R/DATA/counties_kenya.xlsx")
county_shp <-readOGR(dsn="~/Programming/R/PROJECTS/elections/Elections-2022-Visual-Data-Analysis/www/shp/county.shp",layer ="county", verbose = FALSE, stringsAsFactors = FALSE)
colnames(county_shp@data)[colnames(county_shp@data) == "ADM1_EN"] <- "name"
county_shp$zoom <- c(8.2, 9.3, 9, 9, 9, 9.2, 8, 9,
                     8, 7.5, 9, 9.3, 8.5, 7.6, 9.5, 10.4,
                     9, 8, 8.5, 8.6, 8.6, 9, 8.5, 7.7,
                     7, 9.2, 7.2, 11, 9, 10, 9, 8,
                     10.2, 9.2, 9.2, 8.2, 9, 7.6, 7.6, 9,
                     9.3, 7, 9.2, 10, 7, 8.5, 9)


#percent_per county
rao_p_per <- runif(47,1,99)
rao_p_per <- rao_p_per%>%round(1)
undecided_p_per <- c(0.009*rao_p_per)
undecided_p_per <-undecided_p_per %>%round(1)
ruto_p_per <- c(100-(rao_p_per+undecided_p_per))
ruto_p_per<-ruto_p_per%>%round(1)

#overall_percentages
total_cast_votes_p <- rao_p_per+ruto_p_per
total_cast_mean_p <- mean(total_cast_votes_p)

#total_numeric_votes
total_reg_votes <- sum(county_shp$Popultn)-26000000
total_cast_votes <-(total_cast_mean_p/100) *total_reg_votes

#average_votes_garnered
n<-47

rao_win_p<-(sum(rao_p_per)/n)%>%round(1)
ruto_win_p <-(sum(ruto_p_per)/n)%>%round(1)
undecided_win_p<- (sum(undecided_p_per)/n)%>%round(1)

#pass_votes
pass_mark <- ((50/100)*total_cast_votes)+1
pass_mark <- round(pass_mark,0)
pass_mark_p <- (pass_mark/total_cast_votes)*100
#total_real_votes
rao_real_votes<- (rao_win_p/100)*total_cast_votes
rao_real_votes <- round(rao_real_votes,0)
ruto_real_votes <- (ruto_win_p/100)*total_cast_votes
ruto_real_votes <- round(ruto_real_votes,0)

county_shp@data <- county_shp@data %>%
  mutate(RAILA =rao_p_per)
county_shp@data <- county_shp@data %>%
  mutate(RUTO =ruto_p_per)
county_shp@data <- county_shp@data %>%
  mutate(UNDECIDED =undecided_p_per)
county_shp@data <- county_shp@data %>%
  mutate (col= case_when(RAILA > RUTO ~'blue',TRUE~'yellow'))
rao_led <-  as.list(which(county_shp$col%in%'blue'))%>%length()
ruto_led <-  as.list(which(county_shp$col%in%'yellow'))%>%length()
my_colors <- c('blue','yellow','brown')
data1 <- data.frame(COUNTRY= c('% VOTES','% VOTES','% VOTES'),
                    CANDIDATE= c('RAILA','RUTO','UNDECIDED'),
                    PERCENTAGE= c(rao_win_p,ruto_win_p,undecided_win_p)
                    )
today<- format(Sys.Date(),format="%B %d %Y")%>%toupper()

ui <- fluidPage(
  fluidRow(
    tags$head(tags$link(rel='stylesheet',type='text/css',
                               href='styles.css')),
    tags$style(type = "text/css", "#livemap {height: calc(100vh - 80px) !important;}"),
    tags$head(
      tags$style(HTML(".leaflet-container { background:#787b7d; }"))
    ), tags$head(tags$style(
      type="text/css",
      "#button {
            width: 40%;
            display: block;
            margin-left: auto;
            margin-right: auto;
        }",
      "#vote_tables {
      display: block;
        }",
      "#graph {
            width: 60%;
            display: block;
            margin-left: auto;
            margin-right: auto;
        }",
      "#graph1 {
            width: 60%;
            display: block;
            margin-left: auto;
            margin-right: auto;
        }",
      "#raila_table {
            width: 60%;
            display: block;
            margin-left: auto;
            margin-right: auto;
        }",
      "#ruto_table {
            width: 60%;
            display: block;
            margin-left: auto;
            margin-right: auto;
        }",
      "#county_vote_tables {
            width: 60%;
            display: block;
            margin-left: auto;
            margin-right: auto;
        }",
      "#overall {
            width: 60%;
            display: block;
            margin-left: auto;
            margin-right: auto;
        }",
      "#overallA {
            width: 60%;
            display: block;
            margin-left: auto;
            margin-right: auto;
        }",
      "#rao_counties1 {
            display: block;
            margin-left: auto;
            margin-right: auto;
            text-align: left;

        }",
      "#ruto_counties1 {
            display: block;
            margin-left: auto;
            margin-right: auto;
            text-align: left;
        }",
      "#rao_counties2 {
            display: block;
            margin-left: auto;
            margin-right: auto;
            text-align: left;
        }",
      "#ruto_counties2 {
            display: block;
            margin-left: auto;
            margin-right: auto;
            text-align: left;
        }",
      "#controls {
          border-style: ridge;
          border-color: red;
      }",
      "#counties1 {
          border-style: ridge;
          border-color: red;
      }",
      "#counties2 {
          border-style: ridge;
          border-color: red;
      }"
    )),
    leafletOutput("livemap"),
    fixedPanel(id='timer',class = "panel panel-default", top =10 , left =100, right ="auto",bottom ='auto',tags$style(".panel {background-color: rgb(220,225,225);}"),
               width = 205, height = 80,
              tags$div(class="p-2 kd-on bg-white",
                       style = "font-size: 17px; font-weight: bold; border-style: ridge;padding-bottom: 6px; padding-left: 2px;",              img(src ="kenya.png",
                    'KENYA DECIDES 2022'),
                flipdownr::flipdown(downto = "2022-08-09 00:00:00 EAT", id = "flipdown", theme = "youkous")
                )),
    
    fixedPanel(id='socials',class = "panel panel-default", top =530, left =10, right ="auto",bottom ="auto",tags$style(".panel {background-color: rgb(220,225,225);}"),
               width = 100, height = 80,
               tags$div(class="p2",
                        style = "font-size: 17px; font-weight: bold; border-style: ridge;padding-bottom: 0px; padding-left: 2px;", img(src ="author.png",'FIND ME'),
                        tags$div(style=" display: flex; border: ridge;background-color: #ccffff;",
                                 tags$li(
                                   actionLink("LinkedIn",                                              label = "",                                                   icon = icon("linkedin"),                                      onclick = "window.open('https://www.linkedin.com/in/jefferson-ndeke-027062202/')")),
                                 tags$li(
                                   actionLink("GitHub",
                                              label = "", 
                                              icon = icon("github"),
                                              onclick = "window.open('https://github.com/ndeke254')")),
                                 tags$li(
                                   actionLink("Twitter", 
                                              label = "", 
                                              icon = icon("twitter"),
                                              onclick = "window.open('https://twitter.com/jefferson_ndeke')")))
               )),
    absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                  draggable = TRUE, top = 84, left = "auto", right = 20,
                  bottom ='auto',tags$style(".panel{background-color: rgb(220,225,225);text-align: center;}"),
                  width = 500, height = 550,
                  tags$div(class="p-2 kd-on bg-white",
                           style = "border-style: ridge; background-color: #ccffff;",
tags$h2(paste('ELECTIONS AS PREDICTED TODAY', today)),
div(style="display:inline-block;",
    id='button',
    tags$h2(actionButton('back','View Details',
                         icon = icon("bar-chart-o")))),
div(style="display:inline-block",
   tags$h2(uiOutput('county')))),
conditionalPanel(
  condition="input.livemap_shape_click",
 tabsetPanel(id='county_vote_tables',
              tabPanel(title =tags$h2('VOTE COUNT'),value='my_taba',
                       column(12,tableOutput('overallA'))
                       ),
              tabPanel(title =tags$h2('GRAPH'),value='my_tabb',
                       column(12, withSpinner( echarts4rOutput('graph1'),
                                               type=1,color="#b33e48",
                                               hide.ui=FALSE))
                       )
              )
  ),
conditionalPanel(
  condition = "input.back",
tabsetPanel(id='vote_tables',
            tabPanel(title =tags$h2('OVERALL'),
                     value='my_tab1',
                     column(12,tableOutput('overall')),
                     column(12, br(),img(src ="state.jpg", width = "30%", 
                                         style = "display: block; margin-left: auto; margin-right: auto;"))),
            tabPanel(title =tags$h2('RAILA'),value='my_tab2',
                     column(12,tableOutput('raila_table')),
                     column(12, br(),img(src ="azimio.jpg",
                                         width = "30%", 
                                         style = "display: block; margin-left: auto; margin-right: auto;"))),
            tabPanel(title =tags$h2('RUTO'),value='my_tab3',
                     column(12,tableOutput('ruto_table')),
                     column(12, br(),img(src ="uda.jpg", 
                                         width = "50%", 
                                         style = "display: block; margin-left: auto; margin-right: auto;"))),
            tabPanel(title = tags$h2('GRAPH'),value='my_tab4',
                     column(12, withSpinner( echarts4rOutput('graph'), type=1
                                             ,color="#b33e48",hide.ui=FALSE))
                     ),
            
            tabPanel(title =tags$h2('STATUS'),value='my_tab5',
           column(12,tags$h2(textOutput('statement'))),
           column(12, br(),img(src ="ballot.png", width = "50%", 
                               style = "display: block; margin-left: auto; margin-right: auto;")))
           ) 
),
conditionalPanel(
  condition="input.vote_tables=='my_tab2'",
  absolutePanel(id = "counties1", class = "panel panel-default", fixed = TRUE,top = 100, left = 20, right = "auto",
                bottom ='auto',tags$style(".panel{background-color: rgb(220,225,225);}"),
                width = 250, height ="auto",
                fluidRow(
                  tags$div(class="p-2 kd-on bg-white",
                           style = "border-style: ridge; background-color: #ccffff;width: 245px; margin-left: auto; margin-right: auto;",
                           tags$h2("RAILA LEAD COUNTIES")),
                column(6,
               tableOutput('rao_counties1')),
               column(6,
                      tableOutput('rao_counties2'))
                )
) 
),
conditionalPanel(
  condition="input.vote_tables=='my_tab3'",
  absolutePanel(id = "counties2", class = "panel panel-default", fixed = TRUE,top = 100, left = 20, right = "auto",
                bottom ='auto',tags$style(".panel{background-color: rgb(220,225,225);}"),
                width = 250, height ="auto",
                fluidRow(
                  tags$div(class="p-2 kd-on bg-white",
                           style = "border-style: ridge; background-color: #ccffff; width: 245px; margin-left: auto; margin-right: auto;",
                           tags$h2("RUTO LEAD COUNTIES")),
                  column(6,
                         tableOutput('ruto_counties1')),
                  column(6,
                         tableOutput('ruto_counties2'))
                )
                )
  ))
)
)
server <- function(input, output,session) {
  output$statement <- renderText({
    if(rao_win_p>pass_mark_p){
      statement <- 'AZIMIO-OKA WINS IN ROUND 1'
    }else if(ruto_win_p>pass_mark_p){
      statement <- 'KENYA KWANZA WINS IN ROUND 1'
    } else if(rao_win_p>ruto_win_p){
      statement <- 'AZIMIO-OKA WINS WITH A RUN-OFF'
    } else {
      statement <- 'KENYA KWANZA WINS WITH A RUN-OFF'
    }
  })
  tag.map.title <- tags$style(HTML("
  .leaflet-control.map-title { 
    transform: translate(-50%,20%);
    position: fixed !important;
    left: 50%;
    text-align: center;
    padding-left: 10px; 
    padding-right: 10px; 
    background: rgb(220,225,225);
    font-weight: bold;
    font-size: 20px;
    font-family:Candara;
    border-style: ridge;
  }
"))
  
  title <- tags$div(
    tag.map.title, HTML("PRESIDENTIAL CANDIDATES 2022 POPULARITY PER COUNTY")
  )  
  output$overall <- renderTable(
    data.table::data.table(Parameter= c('Total cast Votes','Pass Mark Votes','Undecided %'), Value=c(as.character(round(total_cast_votes,0)),as.character(pass_mark),as.character(paste(undecided_win_p,'%')))
    )
  )
  output$raila_table <- renderTable(
    data.table::data.table(Parameter= c('Garnered Votes','% Votes','Lead Counties'), Value=c(as.character(rao_real_votes),as.character(paste(rao_win_p,'%')),as.character(rao_led))
    )
  )
  output$ruto_table <-renderTable(
    data.table::data.table(Parameter= c('Garnered Votes','% Votes','Lead Counties'), Value=c(as.character(ruto_real_votes),as.character(paste(ruto_win_p,'%')),as.character(ruto_led))
    )
  )
  output$graph <- renderEcharts4r({
  data1 |> 
      group_by(CANDIDATE) |> 
      e_chart(COUNTRY) |>
      e_bar(PERCENTAGE) |>
      e_animation(duration = 4000)|>
      e_axis_labels(x='',y = '% VOTES GARNERED')|> 
      e_tooltip(trigger='item')|>
      e_toolbox_feature(feature = "saveAsImage")|>
      e_color(my_colors)
    })
  output$livemap <- renderLeaflet({
    leaflet(county_shp) %>%
      addPolygons(color = "brown",
                  layerId = county_shp$name,
                  weight = 1,
                  smoothFactor = 0.5,
                  opacity = 3,
                  fillOpacity = 2,
                  fillColor = county_shp$col,
                  highlightOptions = highlightOptions(color = "black",
                                                      weight = 2,
                                                      bringToFront = TRUE),
                  label = paste(
                    "<strong>County:</strong>",county_shp$name
                  ) %>%
                    lapply(htmltools::HTML),
                  labelOptions = labelOptions( style = list("font-weight" = "normal", 
                                                            padding = "3px 8px"), 
                                               textsize = "13px", direction = "auto")
      )%>%addLegend(
        position = "topright",
        colors=c('blue','yellow'),
        labels = c('AZIMIO-OKA','KENYA KWANZA'),
        opacity = 3,
        title ='POLITICAL PARTY',
        className = "info legend"
      )%>% addControl(title, position = "topleft", className="map-title")
  })
  observe({
    click <- input$livemap_shape_click
    if(is.null(click))
      return()
    idx <- which(county_shp$name == click$id)
    name1 <-county_shp$name[[idx]]
   cnt <- county_shp@data%>%filter(name%in%name1)
   county_shp@data <- cnt
   county_shp@polygons <-list(county_shp@polygons[[idx]])
   mapInd <-maps::map(county_shp,fill = TRUE, plot = FALSE)
   cnt1<- counties_kenya%>%filter(name%in%name1)
  leafletProxy("livemap")%>% 
     clearShapes() %>% 
    addPolygons(data = county_shp,
       color = "brown",
       layerId= county_shp$name,
                 weight = 1,
                 smoothFactor = 0.5,
                 opacity = 3,
                 fillOpacity = 2,
                 fillColor = county_shp$col,
                 highlightOptions = highlightOptions(color = "black",
                                                     weight = 2,
                                                     bringToFront = TRUE),
       
       label = paste(
         "<strong>S/no:</strong>",cnt1$SNo,
         "<br>",
         "<strong>County:</strong>",county_shp$name,
         "<br>",
         "<strong>Region:</strong>",cnt1$Region,
         "<br>",
         "<strong>Total Population:</strong>",county_shp$Popultn,
         "<br>",
         "<strong>Capital Town:</strong>",cnt1$Capital,
         "<br>",
         "<strong>Area(km2):</strong>",cnt1$`Area (km2)`,
         "<br>",
         "<strong>Population Density:</strong>",county_shp$Ppltn_D,
         "<br>",
         "<strong>Governor:</strong>",cnt1$governor
       ) %>%
         lapply(htmltools::HTML),
       labelOptions = labelOptions( style = list("font-weight" = "normal", 
                                                 padding = "3px 8px"), 
                                    textsize = "13px", direction = "auto")
       )%>%
       setView(lng = ((mapInd$range[[1]] + mapInd$range[[2]])/2),
               lat = ((mapInd$range[[3]] + mapInd$range[[4]])/2),
               zoom = county_shp$zoom)
  output$county <- renderUI( 
    paste(toupper(name1),'COUNTY',sep =' ')
  )
  shiny::showTab('county_vote_tables',target = 'my_taba',session=session)
  shiny::showTab('county_vote_tables',target = 'my_tabb',session=session)
  
  shiny::hideTab('vote_tables',target = 'my_tab1',session=session)
  shiny::hideTab('vote_tables',target = 'my_tab2',session=session)
  shiny::hideTab('vote_tables',target = 'my_tab3',session=session)
  shiny::hideTab('vote_tables',target = 'my_tab4',session=session)
  shiny::hideTab('vote_tables',target = 'my_tab5',session=session)
  per <- 0.4533715
  vote <- per*county_shp$Popultn
  vote <- round(vote,0)
  
  rao_county_vote <- (county_shp$RAILA*vote)/100
  rao_county_vote <- round(rao_county_vote,0)
  
  ruto_county_vote <-  (county_shp$RUTO*vote)/100
  ruto_county_vote <- round(ruto_county_vote,0)
  
  undecided_county_vote <-  (county_shp$UNDECIDED*vote)/100
  undecided_county_vote <- round(undecided_county_vote,0)
  
  output$overallA <- renderTable(
    data.table::data.table(
      Parameter= c('Total cast Votes','Raila Vote','Ruto Vote','Undecided Vote'),
      Value=c(as.character(vote),as.character(rao_county_vote),as.character(ruto_county_vote),as.character(undecided_county_vote)),
      Percentage =c('',paste(county_shp$RAILA,'%'),paste(county_shp$RUTO,'%'),paste(county_shp$UNDECIDED,'%'))
      )
  )
  data2 <- data.frame(COUNTY=c("% VOTES","% VOTES","% VOTES"),
                      CANDIDATE= c('RAILA','RUTO','UNDECIDED'),
                      PERCENTAGE= c(county_shp$RAILA,county_shp$RUTO
                                    ,county_shp$UNDECIDED)
                      )
  output$graph1 <- renderEcharts4r({
    data2 |> 
      group_by(CANDIDATE) |> 
      e_chart(COUNTY) |>
      e_bar(PERCENTAGE) |>
      e_animation(duration = 4000)|>
      e_axis_labels(x='',y = '% VOTES GARNERED')|> 
      e_tooltip(trigger='item')|>
      e_toolbox_feature(feature = "saveAsImage")|>
      e_color(my_colors)
  })
  updateActionButton(session, "back",label = "Back",icon=icon("backward"))
  
  })
  
  observeEvent(input$back,{
    leafletProxy("livemap") %>%
      clearShapes() %>% 
      setView(lng=37.9083,lat=0.1769,zoom = 6) %>%
      addPolygons(data =county_shp,
                  color = "brown",
                  layerId = county_shp$name,
                  weight = 1,
                  smoothFactor = 0.5,
                  opacity = 3,
                  fillOpacity = 2,
                  fillColor = county_shp$col,
                  highlightOptions = highlightOptions(color = "black",
                                                      weight = 2,
                                                      bringToFront = TRUE),
                  
                  label = paste(
                    "<strong>County:</strong>",county_shp$name
                  ) %>%
                    lapply(htmltools::HTML),
                  labelOptions = labelOptions( style = list("font-weight" = "normal", 
                                                            padding = "3px 8px"), 
                                               textsize = "13px", direction = "auto")
      )
    updateActionButton(session, "back",label = "Refresh",icon=icon("sync"))
    output$county <- renderUI( 
      paste('STATE-HOUSE RACE 2022')
    )
      
    shiny::showTab('vote_tables',target = 'my_tab1',session=session)
    shiny::showTab('vote_tables',target = 'my_tab2',session=session)
    shiny::showTab('vote_tables',target = 'my_tab3',session=session)
    shiny::showTab('vote_tables',target = 'my_tab4',session=session)
    shiny::showTab('vote_tables',target = 'my_tab5',session=session)
    
    shiny::hideTab('county_vote_tables',target = 'my_taba',session=session)
    shiny::hideTab('county_vote_tables',target = 'my_tabb',session=session)
    
    resetLoadingButton('back')
  })
    names_1<- county_shp@data%>%filter(col%in%'blue')%>%select(name)
    colnames(names_1)[colnames(names_1) == "name"] <- "RAILA LEAD COUNTIES"
    n<-0.5*nrow(names_1)
    names1_1<- head(names_1,n)
    names2_1<- tail(names_1,n)
    output$rao_counties1 <- renderTable(
      names1_1,colnames = FALSE,spacing = "xs",width = 'auto'
      )
    output$rao_counties2 <- renderTable(
      names2_1,colnames = FALSE,spacing = "xs",width = 'auto'
    )
     names<- county_shp@data%>%filter(col%in%'yellow')%>%select(name)
     colnames(names)[colnames(names) == "name"] <- "RUTO LEAD COUNTIES"
     n<-0.5*nrow(names)
     names1<- head(names,n)
     names2<- tail(names,n)
     output$ruto_counties1 <- renderTable(
       names1,colnames = FALSE,spacing = "xs",width = 'auto'
      )
     output$ruto_counties2 <- renderTable(
       names2,colnames = FALSE,spacing = "xs",width = "auto"
     )
  }


shinyApp(ui, server)
  
  