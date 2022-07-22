library(leaflet)
library(rgdal)
library(tidyverse)
library(shiny)
library(maps)
library(shinyFeedback)
library(echarts4r)
library(shinycssloaders)
library(shinyBS)
library(shinyjs)
library(readxl)
library(shinyjqui)

#import data
counties_kenya <-read_excel("counties_kenya.xlsx")
county_totals <-read_excel("county_totals.xlsx")
county_shp <-readOGR(dsn="county.shp",layer ="county", verbose = FALSE, stringsAsFactors = FALSE)
colnames(county_shp@data)[colnames(county_shp@data) == "ADM1_EN"] <- "name"
#transfer data to the shape file
#2022
county_shp$reg_voters_2022<- counties_kenya$reg_voters_2022
county_shp$projected_p_cast_2022 <- counties_kenya$projected_p_cast_2022
county_shp$projected_cast_2022<- counties_kenya$projected_cast_2022
#2017
county_shp$reg_voters_2017<- counties_kenya$reg_voters_2022
county_shp$cast_votes_2017<- counties_kenya$cast_votes_2017
county_shp$uhuru_votes_2017<- counties_kenya$uhuru_votes_2017
county_shp$raila_votes_2017<- counties_kenya$raila_votes_2017
county_shp$uhuru_p_2017<- counties_kenya$uhuru_p_2017
county_shp$raila_p_2017<- counties_kenya$raila_p_2017
#2013
county_shp$reg_voters_2013<- counties_kenya$reg_voters_2013
county_shp$cast_votes_2013<- counties_kenya$cast_votes_2013
county_shp$uhuru_votes_2013<- counties_kenya$uhuru_votes_2013
county_shp$raila_votes_2013<- counties_kenya$raila_votes_2013
county_shp$uhuru_p_2013<- counties_kenya$uhuru_p_2013
county_shp$raila_p_2013<- counties_kenya$raila_p_2013
#set zoom level for each county onclick
county_shp@data$zoom <- c(8.2, 8.7, 9.0, 9.0, 9.0, 9.2, 7.0, 9.0,
                     7.5, 7.5, 9.0, 9.3, 8.5, 7.6, 9.5, 10.4,
                     8.5, 7.5, 8.5, 8.0, 8.6, 9, 8.2, 7.7,
                     7.0, 9.2, 8.5, 11.0, 9.0, 10.0, 9.0, 8.0,
                     10.2, 9.2, 9.2, 8.2, 9, 7.6, 7.0, 9.0,
                     9.3, 7.0, 9.2, 10.0, 7.0, 8.0, 8.5)
#2013 general elections
voter_turn_out2013<- 12330028
voter_p_turn_out2013<- 86
rejected_votes2013 <- 108975
#2017 general elections
voter_turn_out2017 <- 15114622
voter_p_turn_out2017 <- 78
rejected_votes2017 <- 8168 
#2022 general elections projected
voter_turn_out2022<- county_totals$projected_cast_2022[2]%>%round(0)
voter_p_turnout2022 <- county_totals$projected_p_cast_2022[2]%>%round(2)
reg_voters_2022<- county_totals$reg_voters_2022[2]
rejection_estimate<- ((8168+108975)/2)%>%round(0)
#percent_per county
rao_p_per <- runif(47,1,99)%>%round(2)
undecided_p_per <- c(0.009*rao_p_per)%>%round(2)
ruto_p_per <- c(100-(rao_p_per+undecided_p_per))%>%round(2)
#average_votes_garnered
n<-47
rao_win_p<-(sum(rao_p_per)/n)%>%round(2)
ruto_win_p <-(sum(ruto_p_per)/n)%>%round(2)
undecided_win_p<- (sum(undecided_p_per)/n)%>%round(2)

#pass_votes
pass_mark <- ((0.5*voter_turn_out2022)+1)%>%round(0)
pass_mark_p <- (pass_mark/voter_turn_out2022)*100
#total_real_votes
rao_real_votes<- ((rao_win_p/100)*voter_turn_out2022)%>%round(0)
ruto_real_votes <-((ruto_win_p/100)*voter_turn_out2022)%>%round(0)
undecided_real_votes <-  ((undecided_win_p/100)*voter_turn_out2022)%>%round(0)
county_shp$rao_p_per <-c(rao_p_per)
county_shp$rao_county_votes <- (county_shp$rao_p_per)/100*(county_shp$projected_cast_2022)%>%round(0)
county_shp$ruto_p_per <-c(ruto_p_per)
county_shp$ruto_county_votes <- (county_shp$ruto_p_per)/100*(county_shp$projected_cast_2022)%>%round(0)
county_shp$undecided_p_per <-c(undecided_p_per)
county_shp$undecided_county_votes <- (county_shp$undecided_p_per)/100*(county_shp$projected_cast_2022)%>%round(0)
#map colors
county_shp@data <- county_shp@data %>%
  mutate (col2022= case_when(rao_p_per > ruto_p_per ~'blue', TRUE~'yellow'))
county_shp@data <- county_shp@data %>%
  mutate (col2017= case_when(uhuru_p_2017 > raila_p_2017 ~'red', TRUE~'darkblue'))
county_shp@data <- county_shp@data %>%
  mutate (col2013= case_when(uhuru_p_2013 > raila_p_2013 ~'red', TRUE~'skyblue'))
#2017
raila_votes_2017<- county_totals$raila_votes_2017[2]%>%round(0)
uhuru_votes_2017 <- county_totals$uhuru_votes_2017[2]%>%round(0)
raila_p_2017 <- county_totals$raila_p_2017[2]%>%round(2)
uhuru_p_2017 <- county_totals$uhuru_p_2017[2]%>%round(2)
#2013
raila_votes_2013<- county_totals$raila_votes_2013[2]%>%round(0)
uhuru_votes_2013 <- county_totals$uhuru_votes_2013[2]%>%round(0)
raila_p_2013 <- county_totals$raila_p_2013[2]%>%round(2)
uhuru_p_2013 <- county_totals$uhuru_p_2013[2]%>%round(2)

#others for 2013/2017
county_shp@data <- county_shp@data %>%
  mutate(others2017=c(100-(uhuru_p_2017+raila_p_2017)))
county_shp@data <- county_shp@data %>%
  mutate(others2017=c(100-(uhuru_p_2013+raila_p_2013)))
others_overall_2017 <- (100-(county_totals$uhuru_p_2017[2]+county_totals$raila_p_2017[2]))%>%round(2)
others_overall_2013 <- (100-(county_totals$uhuru_p_2013[2]+county_totals$raila_p_2013[2]))%>%round(2)
#counties leaders
rao_led2022 <-  as.list(which(county_shp$col2022%in%'blue'))%>%length()
ruto_led2022 <-  as.list(which(county_shp$col2022%in%'yellow'))%>%length()
uhuru_led2017<-as.list(which(county_shp$col2017%in%'red'))%>%length()
rao_led2017<-as.list(which(county_shp$col2017%in%'darkblue'))%>%length()
uhuru_led2013<-as.list(which(county_shp$col2013%in%'red'))%>%length()
rao_led2013<-as.list(which(county_shp$col2013%in%'skyblue'))%>%length()
#graph colors
my_colors2022 <- c('blue','yellow','brown')
my_colors2017 <- c('brown','darkblue','red')
my_colors2013 <-c('brown','skyblue','red')
#graph data
data2022 <- data.frame(COUNTRY= c('% VOTES','% VOTES','% VOTES'),
                    CANDIDATE= c('RAILA','RUTO','UNDECIDED/OTHERS'),
                    PERCENTAGE= c(rao_win_p,ruto_win_p,undecided_win_p)   )
data2017 <-  data.frame(COUNTRY= c('% VOTES','% VOTES','% VOTES'),
                     CANDIDATE= c('UHURU','RAILA','OTHERS'),
                     PERCENTAGE= c(uhuru_p_2017,raila_p_2017,others_overall_2017)    )
data2013 <-  data.frame(COUNTRY= c('% VOTES','% VOTES','% VOTES'),
                     CANDIDATE= c('UHURU','RAILA','OTHERS'),
                     PERCENTAGE= c(uhuru_p_2013,raila_p_2013, others_overall_2013)      )
#>25% quorum
rao_quorum_2022<- county_shp@data%>%filter(rao_p_per>25)%>%nrow()
ruto_quorum_2022<-county_shp@data%>%filter(ruto_p_per>25)%>%nrow()
rao_quorum_2017<- county_shp@data%>%filter(raila_p_2017>25)%>%nrow()
uhuru_quorum_2017<- county_shp@data%>%filter(uhuru_p_2017>25)%>%nrow()
rao_quorum_2013<- county_shp@data%>%filter(raila_p_2013>25)%>%nrow()
uhuru_quorum_2013<- county_shp@data%>%filter(uhuru_p_2013>25)%>%nrow()
#the county names
names_rao_quorum22 <-county_shp@data%>%filter(rao_p_per>25)%>%select(name)
names_ruto_quorum22 <-county_shp@data%>%filter(ruto_p_per>25)%>%select(name)
names_rao_quorum17 <-county_shp@data%>%filter(raila_p_2017>25)%>%select(name)
names_uhuru_quorum17 <-county_shp@data%>%filter(uhuru_p_2017>25)%>%select(name)
names_rao_quorum13 <-county_shp@data%>%filter(raila_p_2013>25)%>%select(name)
names_uhuru_quorum13 <-county_shp@data%>%filter(uhuru_p_2017>25)%>%select(name)
#date of the day
today<- format(Sys.Date(),format="%B %d %Y")%>%toupper()
#edit the sno to codes with prefix
serial <-counties_kenya$Sno%>%as.character()
counties_kenya<-counties_kenya%>%mutate(county_code=case_when(nchar(serial)==2 ~paste("0",serial,sep = ""),TRUE~paste("00",serial,sep = "")))
ui <- fluidPage(
  fluidRow(
    useShinyjs(),
    tags$head(tags$link(rel='stylesheet',type='text/css',
                               href='styles.css')),
    tags$style(type = "text/css", "#livemap,#livemap1 {height: calc(100vh - 40px) !important;}"),
    tags$head(
      tags$style(HTML(".leaflet-container {
                      background: #787b7d;
                      }"))
    ),
    bsTooltip("button_home","Open","right","hover"),
    bsTooltip("button_home2",'Close',"right","hover"),
    bsTooltip("twitter","Twitter","right","hover"),
    bsTooltip("linkedin","LinkedIn","right","hover"),
    bsTooltip("github","Github","right","hover"),
    bsTooltip("view","Clickable Counties","right","hover"),
    bsTooltip("check","Extract county","right","hover"),
    
    tags$head(tags$style(
      type="text/css",
      "#years_1,#years_2,#years_3  {
      padding-top: 4px;
      }",
      "#logo1,#logo2,#check_it {
      background-color: #787b7d;
      }",
      "#controls {
      border-style: ridge;
      border-radius: 5px;
      }",
      "#county {
      padding-left: 10px;
      }",
      "#county_vote_tables{
      padding-left: 70px;
      border-top: ridge 2px;

      }",
      "#vote_tables,#vote_tabes1{
        border-top: ridge 2px;
      }"
    )),
    conditionalPanel(
      condition="input.view",
        withSpinner(leafletOutput("livemap"), 
                    type=1,color="#b33e48",hide.ui=FALSE)
      ),
      withSpinner(leafletOutput("livemap1"),
                  type=1,color="#b33e48",hide.ui=FALSE),
    absolutePanel(id = "logo1",
                  class = "panel panel-default",
                  top = 270,
                  left = 320,
                  right = "auto",
                  bottom ='auto',
                  width = 100,
                  height ="auto",
                  tags$div(
                    class = 'p1',
                    img(src ="azimio.png",
                        width = "100%"
                        )
                    )
                  ),
    absolutePanel(id = "logo2",
                  class = "panel panel-default", 
                  top = 270, 
                  left = "auto", 
                  right = 340,
                  bottom ='auto',
                  width = 100,
                  height ="auto",
                  tags$div(
                    class = 'p1',
                    img(src ="uda.png", 
                        width = "100%" 
                        )
                    )
                  ),
    absolutePanel(id='timer',
                  class = "panel panel-default",
                  top =10 , 
                  left =67, 
                  right ="auto",
                  bottom ='auto',
                  width = 'auto', 
                  height = 'auto',
                  tags$div(class="p2",
                       tags$div(
                         class ='p3',
                         img(src ="kenya.png", ' KENYA DECIDES 2022')
                         ),
               tags$div(
                 class='p6',
                 flipdownr::flipdown(
                   downto = "2022-08-09 00:00:00 EAT",
                   id = "flipdown", 
                   theme = "youkous"
                   )
                 )
                )
               ),
    conditionalPanel(
      condition="input.button_home2",
        absolutePanel(id='timer_center',
                   class = "panel panel-default", 
                   top =260 ,
                   left ='45%',
                   right ="auto",
                   width = 'auto',
                   height = 'auto',
                   tags$div(class="p2",
                            tags$div(
                              class = 'p3',
                            img(src ="kenya.png", ' KENYA DECIDES 2022')
                            ),
                            tags$div(
                              class = 'p6',
                              flipdownr::flipdown(
                              downto = "2022-08-09 00:00:00 EAT",
                              id = "flipdown2", 
                              theme = "youkous")
                                )
                            )
                   )
      ),
    absolutePanel(
      id='socials',
      class = "panel panel-default",
      top =545,
      left =10,
      right ="auto",
      bottom ="auto",
      width = 'auto',
      height = 'auto',
      tags$div(class="p2",
               tags$div(
                 class ='p3',
                 tags$img(src ="author.png",' FIND ME')
                          ),
               tags$div(
                 class ='p7',
                 tags$li(
                   tags$div(
                     class = 'p8',
                   actionLink("linkedin", 
                              label ="",
                              icon = icon("linkedin"),
                              onclick = "window.open('https://www.linkedin.com/in/jefferson-ndeke-027062202/')")
                   )
                   ),
                 tags$li(
                   tags$div(
                     class = 'p8',
                   actionLink("github",
                              label = "",
                              icon = icon("github"),
                              onclick = "window.open('https://github.com/ndeke254')"))
                   ),
                 tags$li(
                   tags$div(
                     class = 'p8',
                   actionLink("twitter",
                              label = "",
                              icon = icon("twitter"),
                              onclick = "window.open('https://twitter.com/jefferson_ndeke')"))
                 )
                 ),
               tags$a(href = "https://www.knbs.or.ke/", "Data: KNBS|",
                      target = "_blank"),
               tags$a(href = "https://www.iebc.or.ke/resources/", "IEBC",
                      target = "_blank")
               )
      ),
    absolutePanel(id = "home_button", 
                  class = "panel panel-default", 
                  top = 100, 
                  left = 20,
                  right = "auto",
                  bottom ='auto',
                  width = NULL, 
                  height =NULL,
                  tags$div(id='button_1',
                           actionButton('button_home','',
                                        icon = icon("folder-open")
                                        )
                           )
                  ),
    conditionalPanel(
      condition="input.livemap_shape_click",
      absolutePanel(id = "check_it", 
                    class = "panel",
                    top = 100,
                    left = '50%',
                    right = "auto",
                    bottom ='auto',
                    width =0,
                    height =0,
                    tags$div(
                      shiny::checkboxInput(
                        'check',NULL)
                    )
      )
    ),
    conditionalPanel(
      condition="input.button_home2",
      absolutePanel(id='socials_1',
                    class = "panel panel-default", 
                    top =350, 
                    left ='50%',
                    right ="auto",
                    bottom ="auto", 
                    width = 'auto',
                    height = 'auto',
                    tags$div(class="p2",
                             tags$div(
                               class='p3',
                               tags$img(src ="author.png",' FIND ME')
                               ),
                             tags$div(
                               class ='p7',
                               tags$li(
                                 tags$div(
                                   class = 'p8',
                                 actionLink("linkedin",
                                            label = "",
                                            icon = icon("linkedin"), 
                                            onclick = "window.open('https://www.linkedin.com/in/jefferson-ndeke-027062202/')")
                                 )
                                 ),
                               tags$li(
                                 tags$div(
                                 class = 'p8',
                                 actionLink("github",
                                            label = "", 
                                            icon = icon("github"),
                                            onclick = "window.open('https://github.com/ndeke254')")
                                 )
                                 ),
                               tags$li(
                                 tags$div(
                                   class = 'p8',
                                 actionLink("twitter",
                                            label = "", 
                                            icon = icon("twitter"),
                                            onclick = "window.open('https://twitter.com/jefferson_ndeke')")))
                               ),
                             tags$a(href = "https://www.knbs.or.ke/", "Data: KNBS|",
                                    target = "_blank"),
                             tags$a(href = "https://www.iebc.or.ke/resources/", "IEBC",
                                    target = "_blank")
                             )
                    )
    ),
    conditionalPanel(
      condition="input.button_home",
    absolutePanel(id = "home_button2", 
                  class = "panel panel-default", 
                  top = 450,
                  left = 20,
                  right = "auto",
                  bottom ='auto',
                  width ='auto',
                  height ='auto',
                  tags$div(id='button_2',
                           actionButton('button_home2','',
                                        icon = icon("folder-open"))
                  )
                  )
    ),
    conditionalPanel(
      condition="input.button_home",
    absolutePanel(id = "election_years",
                  class = "panel panel-default", 
                  top = 110, 
                  left = 20, 
                  right = "auto",
                  bottom ='auto',
                  width ='auto',
                  height ="auto",
                  tags$div(class="p5",
                           id="years1",
                           tags$div(
                             class = 'p9',
                           tags$h2("ELECTION YEARS")
                           ),
                           tabsetPanel(id='years_1',
                                       tabPanel(title='2022'),
                                       tabPanel(title='2017'),
                                       tabPanel(title='2013')
                           ),
                           tags$div(id='button_3',
                                    class='p4',
                                    actionButton('view','View Details',
                                            icon = icon("chart-bar"))
                                    )
                           ),
                  conditionalPanel(
                    condition="input.view",
                    tags$div(class="p5",
                             id="years2",
                             tags$div(
                               class = 'p9',
                             tags$h2("ELECTION YEARS")
                             ),
                             tabsetPanel(id='years_2',
                                         tabPanel(title='2022'),
                                         tabPanel(title='2017'),
                                         tabPanel(title='2013')
                                         ),
                             tags$div(id='button_4',
                                        class='p4',
                                      actionButton('back',
                                                   'Back',
                                                   icon = icon("backward")
                                                   )
                                      )
                             )
                    ),
                  conditionalPanel(
                    condition="input.livemap_shape_click",
                    tags$div(class="p5",
                             id="years3",
                             tags$div(
                               class = 'p9',
                             tags$h2("ELECTION YEARS")
                             ),
                             tabsetPanel(id='years_3',
                                         tabPanel(title='2022'),
                                         tabPanel(title='2017'),
                                         tabPanel(title='2013')
                                         ),
                             tags$div(id='button_5',
                                      class='p4',
                                      actionButton('back2',
                                                   'Back',
                                                   icon = icon("backward"))
                                      )
                             )
                    )
                  )
    ),
    conditionalPanel(
      condition="input.button_home",
      absolutePanel(id = "controls", 
                    class = "panel panel-default",
                    top = 95,
                    right = 10,
                    left = "auto",
                    bottom ='auto',
                    width = 'auto', 
                    height = 'auto',
                    tags$div(class="p2",
                          tags$h2(textOutput("race_title")),
                          tags$h1(textOutput("race_date")),
                    conditionalPanel(
                      condition="input.years_1",
                      tags$div(id="home",
                               tags$div(
                                 class = 'p10',
                                 tags$img(src ="state1.png",'',
                                          style="padding-right: 7px;"),
                                 textOutput('race')
                               ),
                               tags$div(
                                 class ='p11',
                                 tableOutput('race_info')
                                 ),
                                 tags$div(
                                   class ='p11',
                                 tableOutput('others')
                                 ),
                                 tags$div(
                                   class ='p11',
                                 tableOutput("race_facts")
                                 )
                               )
                      ),
                    conditionalPanel(
                      condition="input.view",
                      tags$div(id="vote_tables1",
                               tags$div(
                                 class= 'p13',
                                 tags$img(src ="ballot.png",
                                          width='10%',
                                          textOutput('county')
                               )
                               ),
                               tags$div(
                                 class="p12",
                               tabsetPanel(id="vote_tables",
                                           tabPanel(title =tags$h1('OVERALL'),
                                                    value='my_tab1',
                                                    tags$div(
                                                      class ='p11',
                                                      tableOutput('overall')
                                                      ),
                                                    tags$div(
                                                      class ='p11',
                                                      img(src ="state.png",
                                                          width = "20%")
                                                      )
                                                    ),
                           tabPanel(title =tags$h1('RAILA'),
                                    value='my_tab2',
                                    tags$div(
                                      class ='p11',
                                      tableOutput('raila_table')
                                      ),
                                    conditionalPanel(
                                    condition="input.years_2=='2022'",
                                    tags$div(
                                      class ='p11',
                                      img(src ="azimio.jpg",
                                          width = "20%")
                                      )
                                    ),
                                    conditionalPanel(
                                      condition="input.years_2=='2017'",
                                      tags$div(
                                        class ='p11',
                                        img(src ="nasa.png",
                                            width = "20%")
                                        )
                                      ),
                                    conditionalPanel(
                                      condition="input.years_2=='2013'",
                                      tags$div(
                                        class ='p11', 
                                        img(src ="cord.png",                                                          width = "20%")
                                        )
                                      )
                                    ),
                           tabPanel(title =tags$h1('RUTO'),
                                    value='my_tab3',
                                    tags$div(
                                      class ='p11',
                                      tableOutput('ruto_table')
                                      ),
                                      tags$div(
                                        class ='p11',
                                        img(src ="uda.png", 
                                            width = "20%" )
                                        )
                                      ),
                                    tabPanel(title =tags$h1('UHURU'),
                                             value='my_tab10',
                                             tags$div(
                                               class ='p11',
                                               tableOutput('uhuru_table')
                                             ),
                                               tags$div(
                                                 class ='p11',
                                                 img(src ="jubilee.png",
                                                     width = "20%")
                                                 )
                                             ),
                           tabPanel(title = tags$h1('GRAPH'),
                                    value='my_tab4',
                                    tags$div(
                                      class='p12',
                                      withSpinner( 
                                        echarts4rOutput('graph'),
                                        type=8,
                                        color="#b33e48",
                                        hide.ui=FALSE,
                                        size=0.5)
                                      )
                                    ),
                           tabPanel(title =tags$h1('STATUS'),
                                    value='my_tab5',
                                    tags$div(
                                      class ='p11',
                                           "A round 1 win requires:",br(),
                                           "At least 50% plus one of the total votes cast in the elections.",br(),
                                           "At least 25% of votes cast in each of more than half of the Counties."),
                                      tags$div(
                                        class ='p12',
                                           tags$h1(textOutput('statement'))
                                      ),
                                    tags$div(
                                      class ='p11',
                                      img(src ="ballot.png",
                                          width = "20%"
                                          )
                                      )
                                    )
                           )
                           )
                           )
                      ),
    conditionalPanel(
      condition="input.livemap_shape_click",
      tags$div(id="single1",
               tags$div(
                 class= 'p13',
                 tags$img(src ="shape.png",
                          width='10%',
                          textOutput('county1')
                 )
               ),
               tabsetPanel(id ="county_vote_tables",
                           tabPanel(title =tags$h1('Vote Count'),
                                    value='my_taba',
                                    tags$div(
                                      class ='p11',
                                      tableOutput('overallA')
                                      )
                                    ),
                           tabPanel(title =tags$h1('Graph'),
                                    value='my_tabb',
                                    tags$div(
                                      class ='p12',
                                           withSpinner( 
                                             echarts4rOutput('graph1'),
                                             type=8,
                                             color="#b33e48",
                                             hide.ui=FALSE,
                                             size=0.5)
                                      )
                                    )
                           )
               )
      )
    )
    ),
    conditionalPanel(
      condition="input.vote_tables=='my_tab2'",
      absolutePanel(id = "counties1",
                    class = "panel panel-default",
                    top = 100, 
                    left = 280, 
                    right = "auto",
                    bottom ='auto',
                    width = 'auto',
                    height ="auto",
                    tags$div(
                      class='p9',
                      tags$h1("RAILA LED COUNTIES")
                      ),
                    column(6,
                           tableOutput('rao_counties1')),
                    column(6,
                           tableOutput('rao_counties2'))
                    ) 
          ),
    conditionalPanel(
      condition="input.vote_tables=='my_tab10'",
      absolutePanel(id = "counties3", 
                    class = "panel panel-default", 
                    top = 100, 
                    left =280,
                    right = "auto",
                    bottom ='auto',
                    width = 'auto',
                    height ="auto",
                      tags$div(class="p9",
                               tags$h1("UHURU LED COUNTIES")
                               ),
                      column(6,
                             tableOutput('uhuru_counties1')),
                      column(6,
                             tableOutput('uhuru_counties2'))
                    )
      ),
    conditionalPanel(
      condition="input.vote_tables=='my_tab3'",
      absolutePanel(id = "counties2",
                    class = "panel panel-default", 
                    top = 100, 
                    left = 280, 
                    right = "auto",
                    bottom ='auto',
                    width = 'auto', 
                    height ="auto",
                      tags$div(
                        class="p9",
                        tags$h1("RUTO LED COUNTIES")
                        ),
                    column(6,
                           tableOutput('ruto_counties1')),
                    column(6,
                           tableOutput('ruto_counties2'))
                    )
      )
    )
  )
)

server <- function(input, output,session) {
  #first map on load
  output$livemap1 <- renderLeaflet({
    leaflet(county_shp)%>%
      setView(lng=37.9083,lat=0.1769,zoom = 6)%>%
      addPolygons(color ="brown",
                  layerId = county_shp$name,
                  weight = 1,
                  smoothFactor = 0.5,
                  opacity = 3,
                  fillOpacity = 2,
                  fillColor = county_shp$col2022,
                  highlightOptions = highlightOptions(
                    color ="black", weight = 2, bringToFront = TRUE),
                  label = paste(
                    "<strong>ONE KENYA:</strong>","ONE PEOPLE"
                  ) %>%
                    lapply(htmltools::HTML),
                  labelOptions = labelOptions( style = list("font-weight" = "normal","font-size" = "15px",padding = "3px 8px"),textsize = "15px", direction = "auto"))%>% 
      addControl(title1, position = "topleft", className="map-title")
    
  }) 
  
  #win statement
  observeEvent(input$years_2,{
    if(input$years_2%in%"2022"){
      shiny::hideTab("vote_tables",target='my_tab10',session = session)
      shiny::showTab("vote_tables",target='my_tab3',session = session)
      output$county <- renderText(
       paste("FRONT-RUNNERS 2022")
      )
      output$race_title <- renderText(
        ' GENERAL ELECTIONS 2022'
      )
      output$race_date <- renderText(
        paste('DATE:',' 09-08-2022')
      )
      output$rao_counties1 <- renderTable(
        names1_1,colnames = FALSE,spacing = "xs",width = 'auto'
      )
      output$rao_counties2 <- renderTable(
        names2_1,colnames = FALSE,spacing = "xs",width = 'auto'
      )
      output$ruto_counties1 <- renderTable(
        names1,colnames = FALSE,spacing = "xs",width = 'auto'
      )
      output$ruto_counties2 <- renderTable(
        names2,colnames = FALSE,spacing = "xs",width = "auto"
      )
    
  output$statement <- renderText({
    if(rao_win_p>ruto_win_p){
      if(rao_win_p >pass_mark_p){
        if(rao_quorum_2022>23){
          statement <- 'AZIMIO-OKA WINS IN ROUND 1'
        }else{
          statement <-"AZIMIO-OKA WINS WITH A RUN-OFF"
        }
      }else{
        statement <-"AZIMIO-OKA WINS WITH A RUN-OFF"
      }
    }else{
      if(ruto_win_p >pass_mark_p){
        if(ruto_quorum_2022>23){
          statement<-"KENYA KWANZA WINS ROUND 1"
        }else {
          statement <- "KENYA KWANZA WINS WITH A RUN OFF"
        }
      }else {
        statement <-"KENYA KWANZA WINS WITH A RUN OFF"
      }
    }
  })
  
  #others years statement
    } else if(input$years_2%in%"2017"){
      shiny::hideTab("vote_tables",target='my_tab3',session = session)
      shiny::showTab("vote_tables",target='my_tab10',session = session)
      output$county <- renderText(
        "FRONT-RUNNERS 2017"
      )
      output$race_title <- renderText(
        ' GENERAL ELECTIONS 2017'
      )
      output$race_date <- renderText(
        paste('DATE:',' 08-08-2017')
      )
      output$rao_counties1 <- renderTable(
        names1_2,colnames = FALSE,spacing = "xs",width = 'auto'
      )
      output$rao_counties2 <- renderTable(
        names2_2,colnames = FALSE,spacing = "xs",width = 'auto'
      )
      output$uhuru_counties1 <- renderTable(
        names5_1,colnames = FALSE,spacing = "xs",width = 'auto'
      )
      output$uhuru_counties2 <- renderTable(
        names6_1,colnames = FALSE,spacing = "xs",width = 'auto'
      )
      output$statement<- renderText(
        paste("Jubilee won in round one. ",
        "Nullified by the Supreme Court. ",
        "Won again in a disputed repeat on Oct 26, 2017")
      )
     
    } else if(input$years_2%in%"2013"){
      shiny::hideTab("vote_tables",target='my_tab3',session = session)
      shiny::showTab("vote_tables",target='my_tab10',session = session)
      output$county <- renderText(
        "FRONT-RUNNERS 2013"
      )
      output$race_title <- renderText(
        ' GENERAL ELECTIONS 2013'
      )
      output$race_date <- renderText(
        paste('DATE:',' 04-03-2013')
      )
      output$statement<- renderText(
      paste("Jubilee won in round one. ",
      "Their win upheld by the Supreme Court.")
      )
      output$rao_counties1 <- renderTable(
        names1_3,colnames = FALSE,spacing = "xs",width = 'auto'
      )
      output$rao_counties2 <- renderTable(
        names2_3,colnames = FALSE,spacing = "xs",width = 'auto'
      )
      output$uhuru_counties1 <- renderTable(
        names5_2,colnames = FALSE,spacing = "xs",width = 'auto'
      )
      output$uhuru_counties2 <- renderTable(
        names6_2,colnames = FALSE,spacing = "xs",width = 'auto'
      )
    }else{return()}
  })
  #leaflet title style
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
    font-size: 18px;
    font-family:Candara;
    border-style: ridge;  
    border-radius:5px;
  }
"))
  #title of the leaflet
  title <- tags$div(
    tag.map.title, HTML("PRESIDENTIAL CANDIDATES 2022 POPULARITY PER COUNTY")
  )  
  title1 <-tags$div(
    tag.map.title, HTML("STATE HOUSE RACE 2022")
  )
  #use years as first controls(tables)
  observe({
    if(input$years_1%in%"2022"){
      output$race <- renderText(
        "STATE HOUSE RACE 2022"
      )
     output$race_title <- renderText(
       ' GENERAL ELECTIONS 2022'
     )
     output$race_date <- renderText(
      paste('DATE:',' 09-08-2022')
     )
     output$race_info<- renderTable(
       data.table::data.table(Candidates=c("David Mwaure", "George Wajackoyah","Raila Odinga","William Ruto"),
                             `Running Mates`=c("Ruth Mutua","Justina Wamae", "Martha Karua","Rigathi Gachagua"),
                             Party=c("Agano Party","Roots Party","Azimio","UDA"
                             ))
     )
     output$others <- renderTable(
       return()
     )
     output$race_facts<- renderTable(
       data.table::data.table(Parameter=c('Registered Voters',"Diaspora","Candidates"),
                              Value=c("22120458","5803","4")
       )
     )

    } else if(input$years_1%in%'2017'){
      output$race <- renderText(
        " STATE HOUSE RACE 2017"
      )
      output$race_title <- renderText(
        ' GENERAL ELECTIONS 2017'
      )
      output$race_date <- renderText(
        paste('DATE:',' 08-08-2017')
      )
      output$race_info<- renderTable(
        data.table::data.table(Candidates=c("Raila Odinga","Uhuru Kenyatta"),
                               `Running Mates`=c("Kalonzo Musyoka","William Ruto"),
                               Party=c("NASA","Jubilee")
        )
      )
      output$others <- renderTable(
        data.table::data.table(
         ` `=c("Shakhalagakhwa Jirongo","Japheth Kaluyu"),
                 Others=c("Ekuru Aukot","Abduba Dida"),
                 ` `=c("Michael Mwaura","William Nyagah"))
      )
      output$race_facts<- renderTable(
        data.table::data.table(Parameter=c('Registered Voters',"Diaspora","Candidates"),
                               Value=c("19611423","4393","8")
        )
      )
    } else if(input$years_1%in%"2013") {
      output$race <- renderText(
        " STATE HOUSE RACE 2013"
      )
      output$race_title <- renderText(
        ' GENERAL ELECTIONS 2013'
      )
      output$race_date <- renderText(
        paste('DATE:',' 04-03-2013')
      )
      output$race_info <-renderTable(
      data.table::data.table(Candidates=c("Uhuru Kenyatta","Raila Odinga"),
                             `Running Mates`=c("William Ruto","Kalonzo Musyoka"),
                             Party=c("Jubilee","CORD")
      ))
      output$others <- renderTable(
        data.table::data.table(
          ` `=c("Abduba Dida","Martha Karua"),
          Others=c("Peter Kenneth","James Kiyiapi"),
          ` `=c("Musalia Mudavadi","Paul Muite"))
      )
  output$race_facts<- renderTable(
    data.table::data.table(Parameter=c('Registered Voters',"Diaspora","Candidates"),
                           Value=c("14388781","2637","8")
    )
  )
    }else {
      return()
    }
  })
  
  #A click of the year Tabpanels(map)
  observeEvent(input$years_1, {
    if(input$years_1%in%"2022"){
   leafletProxy("livemap1") %>%
      clearShapes() %>% 
        setView(lng=37.9083,lat=0.1769,zoom = 6)%>%
      addPolygons(data =county_shp,
                  color = "brown",
                  layerId =county_shp$name,
                  weight = 1,
                  smoothFactor = 0.5,
                  opacity = 3,
                  fillOpacity = 2,
                  fillColor = county_shp$col2022,
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
        layerId="key",
        position = "topright",
        colors=c('blue','yellow'),
        labels = c('AZIMIO-OKA','KENYA KWANZA'),
        opacity = 3,
        title ='POLITICAL PARTY',
        className = "info legend")
            } else if(input$years_1%in%"2017"){
          leafletProxy("livemap1") %>%
            clearShapes() %>% 
                setView(lng=37.9083,lat=0.1769,zoom = 6)%>%
            addPolygons(data =county_shp,
                        color = "yellow",
                        layerId =county_shp$name,
                        weight = 1,
                        smoothFactor = 0.5,
                        opacity = 3,
                        fillOpacity = 2,
                        fillColor = county_shp$col2017,
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
              layerId="key",
              position = "topright",
              colors=c('red','darkblue'),
              labels = c('JUBILEE','NASA'),
              opacity = 3,
              title ='POLITICAL PARTY',
              className = "info legend")
      
    } else if(input$years_1%in%"2013"){
      leafletProxy("livemap1") %>%
        setView(lng=37.9083,lat=0.1769,zoom = 6)%>%
        clearShapes() %>% 
        addPolygons(data =county_shp,
                    color = "grey",
                    layerId =county_shp$name,
                    weight = 1,
                    smoothFactor = 0.5,
                    opacity = 3,
                    fillOpacity = 2,
                    fillColor = county_shp$col2013,
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
          layerId="key",
          position = "topright",
          colors=c('red','SKYblue'),
          labels = c('JUBILEE','CORD'),
          opacity = 3,
          title ='POLITICAL PARTY',
          className = "info legend")
      
    }else{
      return()
    }
  })
  
  observeEvent(input$view,{
    jqui_hide('#home', effect = 'fade')
    jqui_hide('#years1', effect = 'fade')
    jqui_show('#vote_tables1', effect = 'fade')
    jqui_show('#years2', effect = 'fade')
    jqui_show('#counties1', effect = 'fade')
    jqui_show('#counties2', effect = 'fade')
    jqui_show('#counties3', effect = 'fade')
    jqui_hide('#livemap1', effect = 'fade')
    jqui_show('#livemap', effect = 'fade')
    
    if(input$years_2%in%"2022"){
      output$race_title <- renderText(
        ' GENERAL ELECTIONS 2022'
      )
      output$race_date <- renderText(
        paste('DATE:',' 09-08-2022')
      )
      output$livemap <- renderLeaflet({
        leaflet(county_shp) %>%
          setView(lng=37.9083,lat=0.1769,zoom = 6)%>%
        addPolygons(data =county_shp,
                    color = "brown",
                    layerId = county_shp$name,
                    weight = 1,
                    smoothFactor = 0.5,
                    opacity = 3,
                    fillOpacity = 2,
                    fillColor = county_shp$col2022,
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
          layerId="key",
          position = "topright",
          colors=c('blue','yellow'),
          labels = c('AZIMIO-OKA','KENYA KWANZA'),
          opacity = 3,
          title ='POLITICAL PARTY',
          className = "info legend")%>% 
          addControl(title, position = "topleft", className="map-title")
        })
      
    }else if(input$years_2%in%"2017"){
      output$race_title <- renderText(
        ' GENERAL ELECTIONS 2017'
      )
      output$race_date <- renderText(
        paste('DATE:',' 09-08-2017')
      )
      output$livemap <- renderLeaflet({
        leaflet(county_shp) %>%
          setView(lng=37.9083,lat=0.1769,zoom = 6)%>%
        addPolygons(data =county_shp,
                    color = "yellow",
                    layerId = county_shp$name,
                    weight = 1,
                    smoothFactor = 0.5,
                    opacity = 3,
                    fillOpacity = 2,
                    fillColor = county_shp$col2017,
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
          layerId="key",
          position = "topright",
          colors=c('red','darkblue'),
          labels = c('JUBILEE','NASA'),
          opacity = 3,
          title ='POLITICAL PARTY',
          className = "info legend")%>% 
          addControl(title, position = "topleft", className="map-title")
      })
      
    }else if(input$years_2%in%"2013"){
      output$race_title <- renderText(
        ' GENERAL ELECTIONS 2013'
      )
      output$race_date <- renderText(
        paste('DATE:',' 09-08-2013')
      )
      output$livemap <- renderLeaflet({
        leaflet(county_shp) %>%
          setView(lng=37.9083,lat=0.1769,zoom = 6)%>%
        addPolygons(data =county_shp,
                    color = "grey",
                    layerId = county_shp$name,
                    weight = 1,
                    smoothFactor = 0.5,
                    opacity = 3,
                    fillOpacity = 2,
                    fillColor = county_shp$col2013,
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
          layerId="key",
          position = "topright",
          colors=c('red','SKYblue'),
          labels = c('JUBILEE','CORD'),
          opacity = 3,
          title ='POLITICAL PARTY',
          className = "info legend")%>% 
          addControl(title, position = "topleft", className="map-title")
      })
      
    }else {
      return()
    }
  })
  observeEvent(input$livemap_shape_click,{
    jqui_hide('#home', effect = 'fade')
    jqui_hide('#years1', effect = 'fade')
    jqui_hide('#vote_tables1', effect = 'fade')
    jqui_hide('#years2', effect = 'fade')
    jqui_show('#years3', effect="fade")
    jqui_show('#single1', effect="fade")
  })
  #control by new tabsetPanel
  observeEvent(input$years_2, {
    if(input$years_2%in%"2022"){
      leafletProxy("livemap") %>%
        clearShapes() %>% 
        setView(lng=37.9083,lat=0.1769,zoom = 6)%>%
        addPolygons(data =county_shp,
                    color = "brown",
                    layerId = county_shp$name,
                    weight = 1,
                    smoothFactor = 0.5,
                    opacity = 3,
                    fillOpacity = 2,
                    fillColor = county_shp$col2022,
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
          layerId="key",
          position = "topright",
          colors=c('blue','yellow'),
          labels = c('AZIMIO-OKA','KENYA KWANZA'),
          opacity = 3,
          title ='POLITICAL PARTY',
          className = "info legend")
      
    } else if(input$years_2%in%"2017"){
      leafletProxy("livemap") %>%
        clearShapes() %>%
        setView(lng=37.9083,lat=0.1769,zoom = 6)%>%
        addPolygons(data =county_shp,
                    color = "yellow",
                    layerId = county_shp$name,
                    weight = 1,
                    smoothFactor = 0.5,
                    opacity = 3,
                    fillOpacity = 2,
                    fillColor = county_shp$col2017,
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
          layerId="key",
          position = "topright",
          colors=c('red','darkblue'),
          labels = c('JUBILEE','NASA'),
          opacity = 3,
          title ='POLITICAL PARTY',
          className = "info legend")
      
    } else if(input$years_2%in%"2013"){
      leafletProxy("livemap") %>%
        clearShapes() %>% 
        setView(lng=37.9083,lat=0.1769,zoom = 6)%>%
        addPolygons(data =county_shp,
                    color = "grey",
                    layerId = county_shp$name,
                    weight = 1,
                    smoothFactor = 0.5,
                    opacity = 3,
                    fillOpacity = 2,
                    fillColor = county_shp$col2013,
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
          layerId="key",
          position = "topright",
          colors=c('red','SKYblue'),
          labels = c('JUBILEE','CORD'),
          opacity = 3,
          title ='POLITICAL PARTY',
          className = "info legend")
      
    }else{
      return()
    }
  })
  #overall tables
  observeEvent(input$years_2,{
    #tables for 2022
    if(input$years_2%in%"2022"){
  output$overall <- renderTable(
    data.table::data.table(Estimate= c('Total cast','Pass Mark','Undecided',"Rejected"), 
                           Votes=c(as.character(voter_turn_out2022),as.character(pass_mark),as.character(undecided_real_votes),as.character(rejection_estimate)),
                           Percent= c(paste(voter_p_turnout2022,"%"),"50% + 1",paste(undecided_win_p,'%'),"")
    )
  )
  output$raila_table <- renderTable(
    data.table::data.table(Estimate= c('Garnered','Lead',"> 25%"),
                           Votes=c(as.character(rao_real_votes),paste(rao_led2022," counties"),paste(rao_quorum_2022,"counties")),
                           Percent=c(as.character(paste(rao_win_p,'%')),"","")
    )
  )
  output$ruto_table <-renderTable(
    data.table::data.table(Estimate= c('Garnered','Lead',"> 25%"),
                           Votes=c(as.character(ruto_real_votes),paste(ruto_led2022," counties"),paste(ruto_quorum_2022,"counties")),
                           Percent=c(as.character(paste(ruto_win_p,'%')),"","")
    )
  )
  #overall graph
  output$graph <- renderEcharts4r({
    data2022 |> 
      group_by(CANDIDATE) |> 
      e_chart(COUNTRY) |>
      e_bar(PERCENTAGE) |>
      e_animation(duration = 4000)|>
      e_axis_labels(x='',y = '% VOTES GARNERED')|> 
      e_tooltip(trigger='item')|>
      e_toolbox_feature(feature = "saveAsImage")|>
      e_color(my_colors2022)
  })
  #tables for 2017
    }else if(input$years_2%in%"2017"){
      pass_mark1 <- ((0.5*voter_turn_out2017)+1)%>%round(0)
      output$overall <- renderTable(
        data.table::data.table(Estimate= c('Total cast','Pass Mark','Rejected',"Others"), 
                               Votes=c(as.character(voter_turn_out2017),as.character(pass_mark1),as.character(rejected_votes2017),""),
                               Percent= c(paste(voter_p_turn_out2017,"%"),"50% + 1","",paste(others_overall_2017,'%'))
        )
      )
      output$raila_table <- renderTable(
        data.table::data.table(Estimate= c('Garnered','Lead',"> 25%"),
                               Votes=c(as.character(raila_votes_2017),paste(rao_led2017," counties"),paste(rao_quorum_2017,"counties")),
                               Percent=c(as.character(paste(raila_p_2017,'%')),"","")
        )
      )
      output$uhuru_table <-renderTable(
        data.table::data.table(Estimate= c('Garnered','Lead',"> 25%"),
                               Votes=c(as.character(uhuru_votes_2017),paste(uhuru_led2017," counties"),paste(uhuru_quorum_2017,"counties")),
                               Percent=c(as.character(paste(uhuru_p_2017,'%')),"","")
        )
      )
      #overall graph
      output$graph <- renderEcharts4r({
        data2017 |> 
          group_by(CANDIDATE) |> 
          e_chart(COUNTRY) |>
          e_bar(PERCENTAGE) |>
          e_animation(duration = 4000)|>
          e_axis_labels(x='',y = '% VOTES GARNERED')|> 
          e_tooltip(trigger='item')|>
          e_toolbox_feature(feature = "saveAsImage")|>
          e_color(my_colors2017)
      })
      #tables for 2013
    }else if(input$years_2%in%"2013"){
      pass_mark2 <- ((0.5*voter_turn_out2013)+1)%>%round(0)
      output$overall <- renderTable(
        data.table::data.table(Estimate= c('Total cast','Pass Mark','Rejected',"Others"), 
                               Votes=c(as.character(voter_turn_out2013),as.character(pass_mark2),as.character(rejected_votes2013),""),
                               Percent= c(paste(voter_p_turn_out2013,"%"),"50% + 1","",paste(others_overall_2013,'%'))
        )
      )
      output$raila_table <- renderTable(
        data.table::data.table(Estimate= c('Garnered','Lead',"> 25%"),
                               Votes=c(as.character(raila_votes_2013),paste(rao_led2013," counties"),paste(rao_quorum_2013,"counties")),
                               Percent=c(as.character(paste(raila_p_2013,'%')),"","")
        )
      )
      output$uhuru_table <-renderTable(
        data.table::data.table(Estimate= c('Garnered','Lead',"> 25%"),
                               Votes=c(as.character(uhuru_votes_2013),paste(uhuru_led2013," counties"),paste(uhuru_quorum_2013,"counties")),
                               Percent=c(as.character(paste(uhuru_p_2013,'%')),"","")
        )
      )
      #overall graph
      output$graph <- renderEcharts4r({
        data2013 |> 
          group_by(CANDIDATE) |> 
          e_chart(COUNTRY) |>
          e_bar(PERCENTAGE) |>
          e_animation(duration = 4000)|>
          e_axis_labels(x='',y = '% VOTES GARNERED')|> 
          e_tooltip(trigger='item')|>
          e_toolbox_feature(feature = "saveAsImage")|>
          e_color(my_colors2013)
      })
    } else {
      return()
    }
  })
  #click of back button
  observeEvent(input$back,{
    jqui_hide('#vote_tables1', effect = 'fade')
    jqui_hide('#years2', effect = 'fade')
    jqui_show('#years1', effect = 'fade')
    jqui_show('#home', effect = 'fade')
    jqui_hide('#counties1', effect = 'fade')
    jqui_hide('#counties2', effect = 'fade')
    jqui_hide('#counties3', effect = 'fade')
    jqui_show("#livemap1", effect="fade")
    jqui_hide("#livemap", effect="fade")
    
    if(input$years_1%in%"2022"){
    output$race_title <- renderText(
      ' GENERAL ELECTIONS 2022'
    )
    output$race_date <- renderText(
      paste('DATE:',' 09-08-2022')
    )
    leafletProxy("livemap1") %>%
      clearShapes() %>% 
      setView(lng=37.9083,lat=0.1769,zoom = 6)%>%
      addPolygons(data =county_shp,
                  color = "brown",
                  layerId =county_shp$name,
                  weight = 1,
                  smoothFactor = 0.5,
                  opacity = 3,
                  fillOpacity = 2,
                  fillColor = county_shp$col2022,
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
        layerId="key",
        position = "topright",
        colors=c('blue','yellow'),
        labels = c('AZIMIO-OKA','KENYA KWANZA'),
        opacity = 3,
        title ='POLITICAL PARTY',
        className = "info legend")
    }else if(input$years_1%in%"2017"){
      output$race_title <- renderText(
        ' GENERAL ELECTIONS 2017'
      )
      output$race_date <- renderText(
        paste('DATE:',' 09-08-2017')
      )
      leafletProxy("livemap1") %>%
        clearShapes() %>% 
        setView(lng=37.9083,lat=0.1769,zoom = 6)%>%
        addPolygons(data =county_shp,
                    color = "yellow",
                    layerId =county_shp$name,
                    weight = 1,
                    smoothFactor = 0.5,
                    opacity = 3,
                    fillOpacity = 2,
                    fillColor = county_shp$col2017,
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
          layerId="key",
          position = "topright",
          colors=c('red','darkblue'),
          labels = c('JUBILEE','NASA'),
          opacity = 3,
          title ='POLITICAL PARTY',
          className = "info legend")
      
    }else if(input$years_1%in%"2013"){
      output$race_title <- renderText(
        ' GENERAL ELECTIONS 2013'
      )
      output$race_date <- renderText(
        paste('DATE:',' 09-08-2013')
      )
      leafletProxy("livemap1") %>%
        clearShapes() %>% 
        setView(lng=37.9083,lat=0.1769,zoom = 6)%>%
        addPolygons(data =county_shp,
                    color = "grey",
                    layerId =county_shp$name,
                    weight = 1,
                    smoothFactor = 0.5,
                    opacity = 3,
                    fillOpacity = 2,
                    fillColor = county_shp$col2013,
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
          layerId="key",
          position = "topright",
          colors=c('red','SKYblue'),
          labels = c('JUBILEE','CORD'),
          opacity = 3,
          title ='POLITICAL PARTY',
          className = "info legend")
    }else {
      return()
    }
  })
  #click from county shape
  observeEvent(input$back2,{
    jqui_show('#years2', effect = 'fade')
    jqui_show('#vote_tables1',effect = 'fade')
    jqui_hide('#years3', effect = 'fade')
    jqui_hide('#single1', effect = 'fade')
    jqui_show('#counties1', effect = 'fade')
    jqui_show('#counties2', effect = 'fade')
    jqui_show('#counties3', effect = 'fade')
    jqui_hide("#check_it", effect="fade")
    
    if(input$years_2%in%"2022"){
      output$race_title <- renderText(
        ' GENERAL ELECTIONS 2022'
      )
      output$race_date <- renderText(
        paste('DATE:',' 09-08-2022')
      )
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
                    fillColor = county_shp$col2022,
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
          layerId="key",
          position = "topright",
          colors=c('blue','yellow'),
          labels = c('AZIMIO-OKA','KENYA KWANZA'),
          opacity = 3,
          title ='POLITICAL PARTY',
          className = "info legend")
      
    }else if(input$years_2%in%"2017"){
      output$race_title <- renderText(
        ' GENERAL ELECTIONS 2017'
      )
      output$race_date <- renderText(
        paste('DATE:',' 09-08-2017')
      )
      leafletProxy("livemap") %>%
        clearShapes() %>%
        setView(lng=37.9083,lat=0.1769,zoom = 6) %>%
        addPolygons(data =county_shp,
                    color = "yellow",
                    layerId = county_shp$name,
                    weight = 1,
                    smoothFactor = 0.5,
                    opacity = 3,
                    fillOpacity = 2,
                    fillColor = county_shp$col2017,
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
          layerId="key",
          position = "topright",
          colors=c('red','darkblue'),
          labels = c('JUBILEE','NASA'),
          opacity = 3,
          title ='POLITICAL PARTY',
          className = "info legend")
      
    }else if(input$years_2%in%"2013"){
      output$race_title <- renderText(
        ' GENERAL ELECTIONS 2013'
      )
      output$race_date <- renderText(
        paste('DATE:',' 09-08-2013')
      )
      leafletProxy("livemap") %>%
        clearShapes() %>% 
        setView(lng=37.9083,lat=0.1769,zoom = 6) %>%
        addPolygons(data =county_shp,
                    color = "grey",
                    layerId = county_shp$name,
                    weight = 1,
                    smoothFactor = 0.5,
                    opacity = 3,
                    fillOpacity = 2,
                    fillColor = county_shp$col2013,
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
          layerId="key",
          position = "topright",
          colors=c('red','SKYblue'),
          labels = c('JUBILEE','CORD'),
          opacity = 3,
          title ='POLITICAL PARTY',
          className = "info legend")
      
    }else {
      return()
    }
  })
  
  #when a single county is clicked
  #checkbox ticked
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
   output$county1 <- renderText(
     paste(cnt1$county_code,toupper(name1),"COUNTY",sep = " ",input$years_3)
   )
   jqui_hide('#counties1', effect = 'fade')
   jqui_hide('#counties2', effect = 'fade')
   jqui_hide('#counties3', effect = 'fade')
   jqui_show("#check_it", effect="fade")
   
   if(input$years_3%in%"2022"){
     output$race_title <- renderText(
       ' GENERAL ELECTIONS 2022'
     )
     output$race_date <- renderText(
       paste('DATE:',' 09-08-2022')
     )
     if(isTruthy(input$check)) {
  leafletProxy("livemap")%>% 
     clearShapes() %>% 
    addPolygons(data = county_shp,
                 color = "brown",
                 layerId= county_shp$name,
                 weight = 1,
                 smoothFactor = 0.5,
                 opacity = 3,
                 fillOpacity = 2,
                 fillColor = county_shp$col2022,
                 highlightOptions = highlightOptions(color = "black",
                                                     weight = 2,
                                                     bringToFront = TRUE),
       label = paste(
         "<strong>Serial code:</strong>",cnt1$county_code,
         "<br>",
         "<strong>County:</strong>",cnt1$name,
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
               zoom = county_shp$zoom)%>% addLegend(
                 layerId="key",
                 position = "topright",
                 colors=c('blue','yellow'),
                 labels = c('AZIMIO-OKA','KENYA KWANZA'),
                 opacity = 3,
                 title ='POLITICAL PARTY',
                 className = "info legend"
               )
     }
  output$overallA <- renderTable(
    data.table::data.table(
      Estimate= c('Registered','Total cast','Raila Vote','Ruto Vote','Undecided Vote'),
      Votes=c(as.character(cnt1$reg_voters_2022), as.character(round(cnt1$projected_cast_2022),0),as.character(round(cnt$rao_county_votes),0),as.character(round(cnt$ruto_county_votes),0),as.character(round(cnt$undecided_county_votes),0)),
      Percentage =c('',paste(round(cnt1$projected_p_cast_2022,2),'%'),paste(cnt$rao_p_per,'%'),paste(cnt$ruto_p_per,'%'),paste(cnt$undecided_p_per,'%'))
      )
  )
  data_county <- data.frame(COUNTY=c("% VOTES","% VOTES","% VOTES"),
                      CANDIDATE= c('RAILA','RUTO','UNDECIDED'),
                      PERCENTAGE= c(round(county_shp$rao_p_per,2),round(county_shp$ruto_p_per,2),round(county_shp$undecided_p_per,2))
                      )
  #selected county graph
  output$graph1 <- renderEcharts4r({
    data_county |> 
      group_by(CANDIDATE) |> 
      e_chart(COUNTY) |>
      e_bar(PERCENTAGE) |>
      e_animation(duration = 4000)|>
      e_axis_labels(x='',y = '% VOTES GARNERED')|> 
      e_tooltip(trigger='item')|>
      e_toolbox_feature(feature = "saveAsImage")|>
      e_color(my_colors2022)
  })
  #selected county 2017
   } else if (input$years_3%in%"2017"){
     output$race_title <- renderText(
       ' GENERAL ELECTIONS 2017'
     )
     output$race_date <- renderText(
       paste('DATE:',' 08-08-2017')
     )
     if(isTruthy(input$check)) {
     leafletProxy("livemap")%>% 
       clearShapes() %>% 
       addPolygons(data = county_shp,
                   color = "yellow",
                   layerId= county_shp$name,
                   weight = 1,
                   smoothFactor = 0.5,
                   opacity = 3,
                   fillOpacity = 2,
                   fillColor = county_shp$col2017,
                   highlightOptions = highlightOptions(color = "black",
                                                       weight = 2,
                                                       bringToFront = TRUE),
                   label = paste(
                     "<strong>Serial code:</strong>",cnt1$county_code,
                     "<br>",
                     "<strong>County:</strong>",cnt1$name,
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
               zoom = county_shp$zoom)%>%addLegend(
                 layerId="key",
                 position = "topright",
                 colors=c('red','darkblue'),
                 labels = c('JUBILEE','NASA'),
                 opacity = 3,
                 title ='POLITICAL PARTY',
                 className = "info legend")
     }
     
     output$overallA <- renderTable(
       data.table::data.table(
         Estimate= c('Registered','Total cast','Raila Vote','Uhuru Vote','Others Vote'),
         Votes=c(as.character(cnt1$reg_voters_2017),as.character(round(cnt1$cast_votes_2017,0)),as.character(round(cnt1$raila_votes_2017,0)),as.character(round(cnt1$uhuru_votes_2017,0)),as.character(round(cnt1$others_votes_2017,0))),
         Percentage =c('',paste(round(cnt1$cast_p_2017,2),'%'),paste(round(cnt1$raila_p_2017,2),'%'),paste(round(cnt1$uhuru_p_2017,2),'%'),paste(round(cnt1$others_p_2017,2),'%'))
       )
     )
     data_county <- data.frame(COUNTY=c("% VOTES","% VOTES","% VOTES"),
                               CANDIDATE= c('RAILA','UHURU','OTHERS'),
                               PERCENTAGE= c(round(cnt1$raila_p_2017,2),round(cnt1$uhuru_p_2017,2),round(cnt1$others_p_2017,2))
     )
     #selected county graph
     output$graph1 <- renderEcharts4r({
       data_county |> 
         group_by(CANDIDATE) |> 
         e_chart(COUNTY) |>
         e_bar(PERCENTAGE) |>
         e_animation(duration = 4000)|>
         e_axis_labels(x='',y = '% VOTES GARNERED')|> 
         e_tooltip(trigger='item')|>
         e_toolbox_feature(feature = "saveAsImage")|>
         e_color(my_colors2017)
     })
     #selected county in 2013
   } else if(input$years_3%in%"2013"){
     output$race_title <- renderText(
       ' GENERAL ELECTIONS 2013'
     )
     output$race_date <- renderText(
       paste('DATE:',' 04-03-2013')
     )
     if(isTruthy(input$check)) {
     leafletProxy("livemap")%>% 
       clearShapes() %>% 
       addPolygons(data = county_shp,
                   color = "yellow",
                   layerId= county_shp$name,
                   weight = 1,
                   smoothFactor = 0.5,
                   opacity = 3,
                   fillOpacity = 2,
                   fillColor = county_shp$col2013,
                   highlightOptions = highlightOptions(color = "black",
                                                       weight = 2,
                                                       bringToFront = TRUE),
                   label = paste(
                     "<strong>Serial code:</strong>",cnt1$county_code,
                     "<br>",
                     "<strong>County:</strong>",cnt1$name,
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
               zoom = county_shp$zoom)%>%addLegend(
                 layerId="key",
                 position = "topright",
                 colors=c('red','SKYblue'),
                 labels = c('JUBILEE','CORD'),
                 opacity = 3,
                 title ='POLITICAL PARTY',
                 className = "info legend")
     }
     output$overallA <- renderTable(
       data.table::data.table(
         Estimate= c('Registered','Total cast','Raila Vote','Uhuru Vote','Others Vote'),
         Votes=c(as.character(cnt1$reg_voters_2013),as.character(round(cnt1$cast_votes_2013,0)),as.character(round(cnt1$raila_votes_2013,0)),as.character(round(cnt1$uhuru_votes_2013,0)),as.character(round(cnt1$others_votes_2013,0))),
         Percentage =c('',paste(round(cnt1$cast_p_2013,2),'%'),paste(round(cnt1$raila_p_2013,2),'%'),paste(round(cnt1$uhuru_p_2013,2),'%'),paste(round(cnt1$others_p_2013,2),'%'))
       )
     )
     data_county <- data.frame(COUNTY=c("% VOTES","% VOTES","% VOTES"),
                               CANDIDATE= c('RAILA','UHURU','OTHERS'),
                               PERCENTAGE= c(round(cnt1$raila_p_2013,2),round(cnt1$uhuru_p_2013,2),round(cnt1$others_p_2013,2))
     )
     #selected county graph
     output$graph1 <- renderEcharts4r({
       data_county |> 
         group_by(CANDIDATE) |> 
         e_chart(COUNTY) |>
         e_bar(PERCENTAGE) |>
         e_animation(duration = 4000)|>
         e_axis_labels(x='',y = '% VOTES GARNERED')|> 
         e_tooltip(trigger='item')|>
         e_toolbox_feature(feature = "saveAsImage")|>
         e_color(my_colors2013)
     })
   }else {
     return()
   }
  })
  #leading county lists
  #raila counties
 names_1<- county_shp@data%>%filter(col2022%in%'blue')%>%select(name)
 names_2<-county_shp@data%>%filter(col2017%in%'darkblue')%>%select(name)
 names_3<-county_shp@data%>%filter(col2013%in%'skyblue')%>%select(name)
    colnames(names_1)[colnames(names_1) == "name"] <- "RAILA LED COUNTIES"
    colnames(names_2)[colnames(names_2) == "name"] <- "RAILA LED COUNTIES"
    colnames(names_3)[colnames(names_3) == "name"] <- "RAILA LED COUNTIES"
    n3<-0.5*nrow(names_1)
    n4<-0.5*nrow(names_2)
    n5<-0.5*nrow(names_3)
    
    names1_1<- head(names_1,n3)
    names1_2<- head(names_2,n4)
    names1_3<- head(names_3,n5)
    
    names2_1<- tail(names_1,n3)
    names2_2<- tail(names_2,n4)
    names2_3<- tail(names_3,n5)
    
    #ruto counties
     names<- county_shp@data%>%filter(col2022%in%'yellow')%>%select(name)
     colnames(names)[colnames(names) == "name"] <- "RUTO LED COUNTIES"
     n<-0.5*nrow(names)
     names1<- head(names,n)
     names2<- tail(names,n)
    #uhuru led counties
     names_5_1<- county_shp@data%>%filter(col2017%in%'red')%>%select(name)
     names_5_2<-county_shp@data%>%filter(col2013%in%'red')%>%select(name)
     colnames(names_5_1)[colnames(names_5_1) == "name"] <- "UHURU LED COUNTIES"
     colnames(names_5_2)[colnames(names_5_2) == "name"] <- "UHURU LED COUNTIES"
     n1<-0.5*nrow(names_5_1)
     n2<-0.5*nrow(names_5_2)

     names5_1<- head(names_5_1,n1)
     names5_2<- head(names_5_2,n2)

     names6_1<- tail(names_5_1,n1)
     names6_2<- tail(names_5_2,n2)
     
     observeEvent(input$button_home, {
       jqui_show('#controls', effect = 'fade')
       jqui_show('#election_years', effect = 'fade')
       jqui_show('#home_button2', effect = 'fade')
       jqui_hide('#home_button', effect = 'fade')
       jqui_show('#timer', effect = 'fade')
       jqui_hide('#timer_center', effect = 'fade')
       jqui_show('#socials', effect = 'fade')
       jqui_hide('#socials_1', effect = 'fade')
       jqui_hide('#logo1', effect = 'fade')
       jqui_hide('#logo2', effect = 'fade')
       jqui_show("#livemap1", effect="fade")
       jqui_hide("#livemap", effect="fade")
       jqui_show("#years1",effect="fade")
       jqui_hide('#years2', effect = 'fade')
       jqui_show("#home", effect='fade')
       jqui_hide('#years3', effect = 'fade')
       jqui_hide('#vote_tables1', effect = 'fade')
       jqui_hide('#single1', effect = 'fade')
       jqui_hide('#counties1', effect = 'fade')
       jqui_hide('#counties2', effect = 'fade')
       jqui_hide('#counties3', effect = 'fade')
       
       if(input$years_1%in%"2022"){
         leafletProxy("livemap1") %>%
           clearShapes() %>% 
           setView(lng=37.9083,lat=0.1769,zoom = 6)%>%
           addPolygons(data =county_shp,
                       color = "brown",
                       layerId =county_shp$name,
                       weight = 1,
                       smoothFactor = 0.5,
                       opacity = 3,
                       fillOpacity = 2,
                       fillColor = county_shp$col2022,
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
             layerId="key",
             position = "topright",
             colors=c('blue','yellow'),
             labels = c('AZIMIO-OKA','KENYA KWANZA'),
             opacity = 3,
             title ='POLITICAL PARTY',
             className = "info legend")%>% 
           addControl(title, position = "topleft", className="map-title")
       } else if(input$years_1%in%"2017"){
         leafletProxy("livemap1") %>%
           clearShapes() %>% 
           setView(lng=37.9083,lat=0.1769,zoom = 6)%>%
           addPolygons(data =county_shp,
                       color = "yellow",
                       layerId =county_shp$name,
                       weight = 1,
                       smoothFactor = 0.5,
                       opacity = 3,
                       fillOpacity = 2,
                       fillColor = county_shp$col2017,
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
             layerId="key",
             position = "topright",
             colors=c('red','darkblue'),
             labels = c('JUBILEE','NASA'),
             opacity = 3,
             title ='POLITICAL PARTY',
             className = "info legend")%>% 
           addControl(title, position = "topleft", className="map-title")
         
       } else if(input$years_1%in%"2013"){
         leafletProxy("livemap1") %>%
           clearShapes() %>% 
           setView(lng=37.9083,lat=0.1769,zoom = 6)%>%
           addPolygons(data =county_shp,
                       color = "grey",
                       layerId =county_shp$name,
                       weight = 1,
                       smoothFactor = 0.5,
                       opacity = 3,
                       fillOpacity = 2,
                       fillColor = county_shp$col2013,
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
             layerId="key",
             position = "topright",
             colors=c('red','SKYblue'),
             labels = c('JUBILEE','CORD'),
             opacity = 3,
             title ='POLITICAL PARTY',
             className = "info legend")%>% 
           addControl(title, position = "topleft", className="map-title")
         
       }else{
         return()
       }
     })
     observeEvent(input$button_home2, {
       jqui_hide('#controls', effect = 'fade')
       jqui_hide('#election_years', effect = 'fade')
       jqui_hide('#home_button2', effect = 'fade')
       jqui_show('#home_button', effect = 'fade')
       jqui_hide('#timer', effect = 'fade')
       jqui_show('#timer_center', effect = 'fade')
       jqui_show('#socials_1', effect = 'fade')
       jqui_hide('#socials', effect = 'fade')
       jqui_show('#logo1', effect = 'fade')
       jqui_show('#logo2', effect = 'fade')
       jqui_hide("#livemap", effect="fade")
       jqui_show("#livemap1", effect="fade")
       jqui_hide("#check_it", effect="fade")
       jqui_hide('#counties1', effect = 'fade')
       jqui_hide('#counties2', effect = 'fade')
       jqui_hide('#counties3', effect = 'fade')
       
       leafletProxy("livemap1") %>%
         clearShapes() %>% clearControls()%>%
         setView(lng=37.9083,lat=0.1769,zoom = 6) %>%
         addPolygons(data=county_shp,
                     color ="white",
                     layerId= county_shp$name,
                     smoothFactor = 0.5,
                     weight = 1, opacity = 1.0,
                     fillOpacity = 1.0,
                     fillColor = "olive",
                     highlightOptions = highlightOptions(
                       color = "brown",
                       weight = 1,
                       bringToFront = TRUE),
                     label = paste(
                       "<strong>ONE KENYA:</strong>","ONE NATION"
                     ) %>%
                       lapply(htmltools::HTML),
                     labelOptions = labelOptions( 
                       style = list("font-weight" = "normal", 
                                    padding = "3px 8px"), 
                       textsize = "13px", direction = "auto")
         )
     })
     #when a single county is clicked
     #when check box is not selected
     observe({
       if(!isTruthy(input$check)) {
         click <- input$livemap_shape_click
         if(is.null(click))
           return()
         idx <- which(county_shp$name == click$id)
         name1 <-county_shp$name[[idx]]
         cnt <- county_shp@data%>%filter(name%in%name1)
         cnt1<- counties_kenya%>%filter(name%in%name1)
       
       if(input$years_3%in%"2022"){
         col <- cnt |> 
           select(col2022)
         col_name <- col[[1]]
         county_shp@data <- county_shp@data |> 
           mutate (cols=case_when(name%in%name1 ~ col_name, TRUE~'white' ))
         
         leafletProxy("livemap")%>% 
           clearShapes() %>% 
           setView(lng=37.9083,lat=0.1769,zoom = 6)%>%
           addPolygons(data = county_shp,
                       color = "brown",
                       layerId= county_shp$name,
                       weight = 1,
                       smoothFactor = 0.5,
                       opacity = 3,
                       fillOpacity = 2,
                       fillColor = county_shp$cols,
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
           )%>%
            addLegend(
                     layerId="key",
                     position = "topright",
                     colors=c('blue','yellow'),
                     labels = c('AZIMIO-OKA','KENYA KWANZA'),
                     opacity = 3,
                     title ='POLITICAL PARTY',
                     className = "info legend"
                   )
         
         #selected county 2017
       } else if (input$years_3%in%"2017"){
         col <- cnt |> 
           select(col2017)
         col_name <- col[[1]]
         county_shp@data <- county_shp@data |> 
           mutate (cols=case_when(name%in%name1 ~ col_name, TRUE~'white' ))
         
         leafletProxy("livemap")%>% 
           clearShapes() %>% 
           setView(lng=37.9083,lat=0.1769,zoom = 6)%>%
           addPolygons(data = county_shp,
                       color = "brown",,
                       layerId= county_shp$name,
                       weight = 1,
                       smoothFactor = 0.5,
                       opacity = 3,
                       fillOpacity = 2,
                       fillColor = county_shp$cols,
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
           )%>%
           addLegend(
                     layerId="key",
                     position = "topright",
                     colors=c('red','darkblue'),
                     labels = c('JUBILEE','NASA'),
                     opacity = 3,
                     title ='POLITICAL PARTY',
                     className = "info legend")
         
         
         #selected county in 2013
       } else if(input$years_3%in%"2013"){
         col <- cnt |> 
           select(col2013)
         col_name <- col[[1]]
         county_shp@data <- county_shp@data |> 
           mutate (cols=case_when(name%in%name1 ~ col_name, TRUE~'white' ))
         
         leafletProxy("livemap")%>% 
           clearShapes() %>% 
           setView(lng=37.9083,lat=0.1769,zoom = 6)%>%
           addPolygons(data = county_shp,
                       color = "brown",
                       layerId= county_shp$name,
                       weight = 1,
                       smoothFactor = 0.5,
                       opacity = 3,
                       fillOpacity = 2,
                       fillColor = county_shp$cols,
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
           )%>%
           addLegend(
                     layerId="key",
                     position = "topright",
                     colors=c('red','SKYblue'),
                     labels = c('JUBILEE','CORD'),
                     opacity = 3,
                     title ='POLITICAL PARTY',
                     className = "info legend")
       }
       }
     })
}

shinyApp(ui, server)
  
  