#install.packages("shiny")
#install.packages("ggplot2")
#install.packages("plotly")
#install.packages("leaflet")
#install.packages("dplyr")
#install.packages("tidyr")
#install.packages("readr")
#install.packages("geojsonio")
#install.packages("plyr")
#install.packages("GGally")
#install.packages("scales")
#install.packages("viridis")
#install.packages("shinythemes")

library(shiny)
library(ggplot2)
library(plotly)
library(leaflet)
library(readr)
library(dplyr)
library(shinythemes)

state_data <- read_csv("EDAV-State-Data.csv")

variables <- c("Gun Owner Percentage" = "Gun_Owner_Pct",
               "State Population" = "Population",
               "Poverty Rate" = "Poverty_Rate",
               "Gini Index"="Gini",
               "Benefits_Vs_Tax"="Benefits_Vs_Tax",
               "Percent of Caucasian"="Pct_White",
               "Percent of White Evangelical"="Pct_White_Evangelical",
               "Percent of Nonreligious"="Pct_Nonreligious", 
               "Percent of Urban Population"="Pct_Urban",
               "Percent of Obese"="Pct_Obese",
               "Life Expectancy"="Life_Exp",
               "Percent of Union Worker"="Pct_Union",
               #"Right to work"="RTW",
               "Percent of Business Owner"="Pct_Biz_Owner",
               "Smoker Percentage"="Pct_Smoker",
               "% High School or lower"="Pct_Highschoolbelow",
               "% Some College Education"="Pct_Somecollege",
               "% College Graduate"="Pct_College_Graduate",
               "% Post Graduate"="Pct_Post_Graduate",
               "No_money_for_healthcare"="No_money_for_healthcare",
               "No_money_for_food"="No_money_for_food",
               "Democratic Vote Lean"="Democratic_Vote")

shinyUI(fluidPage(theme = shinytheme("slate"),
  #Basic framework of webpage
  #titlePanel(title = "Election Analysis"),
  navbarPage("Election Analysis",
        
        tabPanel("Data",tableOutput("str")),
        
        navbarMenu("Report",
                   tabPanel("Barchart Plot",plotlyOutput("hist1",height = 600),tags$p("\n"),
                            textOutput("h_com1"),tags$p("\n"),
                            plotlyOutput("hist2",height = 600),tags$p("\n"),
                            textOutput("h_com2"),tags$p("\n"),
                            plotlyOutput("hist3",height = 600),tags$p("\n"),
                            textOutput("h_com3"),tags$p("\n")),
                   tabPanel("Correlation Plot",plotOutput("cor1",height = 600),tags$p("\n"),
                            textOutput("com_c1"),tags$p("\n"),
                               plotOutput("cor2",height = 600),tags$p("\n"),
                               textOutput("com_c2"),tags$p("\n")),
                   tabPanel("Scatter Plot", plotlyOutput("scatter3"),tags$p("\n"),
                            textOutput("com_s3"), tags$p("\n"),
                            plotlyOutput("scatter4"),tags$p("\n"),
                            textOutput("com_s4"),tags$p("\n"),
                            plotlyOutput("scatter5"),tags$p("\n"),
                            textOutput("com_s5"),tags$p("\n"),
                            plotlyOutput("scatter6"),tags$p("\n"),
                            textOutput("com_s6"),tags$p("\n")),
                   tabPanel("Heatmap", plotlyOutput("heat1",height = 660),tags$p("\n"),
                            textOutput("com_h1"),tags$p("\n"),
                            plotlyOutput("heat2", height = 660),tags$p("\n"),
                            textOutput("com_h2"),tags$p("\n"))
                   
        ),
        tabPanel("Map",div(class="outer",
                           leafletOutput("map",height = 700),
                           absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                         draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                         width = 340, height = "auto",
                                         plotlyOutput("small"),
                                         h2("Election Result Map"),
                                         selectInput("vars", "Variable", variables, selected = "Democratic_Vote"),
                                         checkboxInput("BR","Political Map",FALSE)
                           ))),

        navbarMenu("More",
                   tabPanel("Data Source", textOutput("ds_com1"),tags$p("\n"),textOutput("ds_com2"),tags$p("\n"),tableOutput("datasource")
                            ),
                   tabPanel("Team",h3("Team name: Moon Illusion"),tableOutput("team1"), h4("If you have any questions about 
                                                                              our project, please feel free to 
                                                                              contact with us (=ↀωↀ=)✧")
    ,verbatimTextOutput("try"))
  
  )
)
)
)

