# STAT40830
library(shinydashboard)
library(shiny)
library(xlsx)
library(ggplot2)
library(shinythemes)
library(DT)
library(dashboardthemes)
library(shinyalert)
library(plotly)
library(readxl)
library(dplyr)
library(knitr)
library(ggrepel)
library(magrittr)
# ui ---------------------------------------------------------------------------
ui <- dashboardPage(
    skin = "black",
    #set up dashboard
    dashboardHeader(title = span(tagList(icon("fa-regular fa-book-open-cover"), "Assignment 2"))),
    #set up sidebar space
    dashboardSidebar(
        # Sidebar tabs -----------------------------------------------------------------
        sidebarMenu(id = "sidebarid",
                    #upload data menu
                    menuItem(text = "File Upload", tabName = "data", icon = icon("file-upload")),
                    #table menu
                    menuItem("Table", tabName = "table", icon = icon("table")),
                    conditionalPanel('input.sidebarid == "table"'),
                    # boxplot menu
                    menuItem("Math results", tabName = "boxplot", icon = icon("fa-regular fa-square")),
                    conditionalPanel('input.sidebarid == "boxplot"'),
                    # scatterplot menu
                    menuItem("VLE usage", tabName = "scatterplot", icon = icon("fa-regular fa-chart-line")),
                    conditionalPanel('input.sidebarid == "scatterplot"'),
                    # histogram menu
                    menuItem("Big Five personality factors", tabName = "histogram", icon = icon("bar-chart-o")),
                    conditionalPanel('input.sidebarid == "histogram"'),
                    #histogram menu
                    menuItem("Approaches to Learning", tabName = "biggs", icon = icon("bar-chart-o")),
                    conditionalPanel('input.sidebarid == "biggs"')
        )
    ),
    dashboardBody(
        # set theme
        shinyDashboardThemes(
            theme = "blue_gradient"
        ),
        # tab menus
        tabItems(
            tabItem(tabName = "data", 
                    box(title = "Upload File", width = 12, height = NULL, solidHeader = TRUE, fileInput("file1", "Please upload Maths Ed dataset", accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
                        tags$hr())),
            # page 1 ----
            tabItem(tabName = "table", 
                    box(title = "Table content", width = 12, height = NULL, solidHeader = TRUE, DT::dataTableOutput("table"))),
            # page 2 ----
            tabItem(tabName = "boxplot", 
                    #graph box
                    box(title =  "Marks achieved for males and females in maths", width = 8, height = NULL, plotlyOutput("boxplot")), 
                    #input box
                    box(title = "Plot Menu", width = 4, height = NULL, 
                        collapsible = TRUE,
                        collapsed = FALSE,
                        #input
                        textInput("Boxtit", "Input title for violinplot", ""), 
                        textInput("BoxX", "Input title for X axis", ""), 
                        textInput("BoxY", "Input title for Y axis", ""))),
            # page 3 ----
            tabItem(tabName = "scatterplot",
                    #graph box
                    box(title =  "Virtual Learning Environment usage", width = 8, height = NULL, plotlyOutput("plot")), 
                    #input box
                    box(title = "Plot Menu", width = 4, height = NULL,
                        collapsible = TRUE,
                        collapsed = FALSE,
                        #input
                        textInput("scattit", "Input title for Mini-IPIP plot", ""), 
                        textInput("scattX", "Input title for X axixs", ""), 
                        textInput("scattY", "Input title for Y axis", ""), 
                        #select input from range
                        selectInput("choice", label = "Gender", 
                                    choices = c("Male","Female"), 
                                    selected = "Male"),
                    )),
            # page 4 ----
            tabItem(tabName = "histogram",
                    #graph box
                    box(title =  "Big Five personality factors (measured using Mini-IPIP scales - scores out of 20)", width = 8, height = NULL, plotlyOutput("histagram")), 
                    #input box
                    box(title = "Plot Menu", width = 4, height = NULL,
                        collapsible = TRUE,
                        collapsed = FALSE,
                        # input
                        textInput("bartit", "Input title for histogram", ""), 
                        textInput("barX", "Input title for X axis", ""),
                        #select input from range
                        selectInput("Bchoice", label = "Gender", 
                                    choices = c("Male","Female"), 
                                    selected = "Male"),
                        selectInput("bin", label = "Big Five personality factors", 
                                    choices = c("Extraversion","Agreeableness","Conscientiousness","Neuroticism","Openness"), 
                                    selected = "Extraversion"))),
            # page 5 ----
            tabItem(tabName = "biggs", 
                    #graph box
                    box(title =  "Approaches to Learning (measured using Biggs Revised Two Factor Study Process Questionnaire - scale of 5-25)", 
                        width = 8, height = NULL, plotlyOutput("biggs")), 
                    #input box
                    box(title = "Plot Menu", width = 4, height = NULL,
                        collapsible = TRUE,
                        collapsed = FALSE,
                        #input
                        textInput("bartit2", "Input title for histogram", ""), 
                        textInput("barX2", "Input title for X axis", ""),
                        #select input from range
                        selectInput("Bchoice2", label = "Gender", 
                                    choices = c("Male","Female"), 
                                    selected = "Male"),
                        selectInput("bin2", label = "Approaches to Learning", 
                                    choices = c("DeepMotive","DeepStrategy","SurfaceMotive","SurfaceStrategy"), 
                                    selected = "DeepMotive")))
            
        )
    )
)
