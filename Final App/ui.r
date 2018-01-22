library(shiny)
library(shinysky)

my_autocomplete_list <- c("John Doe","Ash","Ajay sharma","Ken Chong","Will Smith","Neo")

country_list <- c("Argentina","Austria","Australia","Belgium","Bolivia (Plurinational State of)","Brazil","Canada","Switzerland","Chile","Colombia","Costa Rica","Cyprus","Czech Republic","Germany","Denmark","Dominican Republic","Ecuador","Estonia","Spain","Finland","France","United Kingdom of Great Britain and Northern Ireland","Greece","Guatemala","Hong Kong","Honduras","Hungary")


shinyUI(fluidPage(
  tags$head(tags$style(HTML("body{
                            background-image: url(http://wallportal.com/uploads/posts/backgrounds-for-music/backgrounds_for_music_010.jpg);
                            }"))
            ,tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.css")),
  titlePanel("ROCKNROLLA"),
   
	  mainPanel(
	    
	  tabsetPanel(
	  
        tabPanel("RecommendationEngine",
              hr(),   
              fluidPage(   
              #
              sidebarLayout(fluid=TRUE,
                #choices=list("Heard","Not Heard","Gimme All, man!")
                sidebarPanel(
                  textInput(inputId="UserName", label= "User Name", value = "U1"),
                  selectInput(inputId="className", label="Mood", choices=list("Dinner","Sleep","Party","Workout")),
                  radioButtons(inputId="Song_played", label = "You Know this", choiceNames = list(
                    "Heard",
                    "Not Heard"
                  ),
                  choiceValues = list(
                    1, 0 
                  )),
				          actionButton("user_Button", label = "Schneiderize!",
                                                  style="color: #fff; background-color: #337ab7; border-color: #2e6da4") # action button
                  #submitButton("Click for Top 5", icon("refresh"))
                  
                ),
                
                mainPanel(
                  #textOutput('prob'),
                  fluidRow(
                      tableOutput("Top5")
                    
                  )
                )
              ))
              #
				 ), 
				 
        tabPanel("Enterprise",
                 hr(),
                 fluidPage(
                   sidebarLayout(
                     sidebarPanel(
                       fluidRow(
                         textInput.typeahead(id="Region",
                                             placeholder="Country Name",
                                             local=data.frame(country=c(country_list)),
                                             valueKey = "country",
                                             tokens=c(1:length(country_list)),
                                             template = HTML("<p class='repo-language'>{{info}}</p> <p class='repo-name'>{{country}}</p><style>h1 {color:red;}p {color:DarkSlateBlue;}</style>")
                         )),
                       br(),
                       br(),
                       sliderInput("acousticness", "acousticness",
                                   min = 0, max = 1, value = 0.5,
                                   ticks = FALSE, round = TRUE, step = 0.01),
                       sliderInput("danceability", "danceability",
                                   min = 0, max = 1, value = 0.5,
                                   ticks = FALSE, round = TRUE, step = 0.01),
                       sliderInput("speechiness", "speechiness",
                                   min = 0, max = 1, value = 0.5,
                                   ticks = FALSE, round = TRUE, step = 0.01),
                       sliderInput("energy", "energy",
                                   min = 0, max = 1, value = 0.5,
                                   ticks = FALSE, round = TRUE, step = 0.01),
                       sliderInput("liveness", "liveness",
                                   min = 0, max = 1, value = 0.5,
                                   ticks = FALSE, round = TRUE, step = 0.01),
                       actionButton("producer_Button", label = "Schneiderize!")
                     ),
                     mainPanel(
                       dataTableOutput('song_attributes'),			 
                       tableOutput('estimated_length'),
                       fluidRow(
                           plotOutput("plotPut",width="100%")
                         )
                     #)
                     )
                   )
                 ),
                 
                 hr()
                  # action button,
				 ) 
	  )
	)
 )
)