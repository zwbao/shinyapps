library(shiny)
library(RColorBrewer)
library(reshape2)
library(ggpubr)
library(colourpicker)
library(colorspace)
library(shinycssloaders)
library(shinydashboard)
library(reactable)

body <- dashboardBody(
    fluidRow(
        column(width = 4,
               box(
                   title = "Upload", status = "primary", solidHeader = TRUE,
                   collapsible = TRUE,width = NULL,
                   h5("Upload tab-delimited text files."),
                   fileInput("counts",
                             "Count file",
                             multiple = FALSE,
                             accept = c(".txt")),
                   fileInput("groups",
                             "Group file",
                             multiple = FALSE,
                             accept = c(".txt")),
                   fileInput("colors",
                             "Color file",
                             multiple = FALSE,
                             accept = c(".txt")),
                   tags$div("The example data is available in",
                            tags$a(href = "https://github.com/zwbao/shinyapps/tree/main/stacked_bar/www", "Github"),
                            "."
                   )
               ),
               box(
                   title = "Customize", status = "warning", solidHeader = TRUE,
                   collapsible = TRUE,width = NULL,
                   sliderInput("xfontsize", "X axis label font size",
                               min = 0, max = 30, value = 15
                   ),
                   textInput("ylabel", "Y axis label", value = "Relative Abundance"),
                   sliderInput("yfontsize", "Y axis label font size",
                               min = 0, max = 30, value = 15
                   ),
                   sliderInput("tyfontsize", "Y axis title font size",
                               min = 0, max = 30, value = 15
                   ),
                   selectizeInput('colpal', "Choose a color palette (Plot 2)",
                                  selected = "Set3",
                                  choices = rownames(brewer.pal.info[brewer.pal.info$category=="qual",])),
                   sliderInput("plotheight", "Height (pixels)",
                               min = 400, max = 1000, value = 460, step = 20
                   ),
                   sliderInput("plotwidth", "Width (pixels)",
                               min = 400, max = 1000, value = 400, step = 20
                   ),
                   checkboxInput("customcol", "Custom colors for each taxon group"),
                   uiOutput("colourpickers"),
                   actionButton("run", label = "Plot", icon = icon("paper-plane"))
               )
               ),
        
        column(width = 8,
               tabBox(
                   title = "Input Data",
                   id = "tabset1",width = NULL,
                   tabPanel("Count", reactableOutput("ct_table")),
                   tabPanel("Group", reactableOutput("gp_table")),
                   tabPanel("Color", reactableOutput("cl_table"))
               ),
               uiOutput("ui"),
               uiOutput("textanno")
        )
    )
)


shinyUI(
    dashboardPage(
        dashboardHeader(title = "Stacked bar chart"),
        dashboardSidebar(
            disable = TRUE),
        body
    )
)

