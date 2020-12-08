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
                             "Choose count file(.tsv)",
                             multiple = FALSE,
                             accept = c(".txt")),
                   fileInput("groups",
                             "Choose group file(.tsv)",
                             multiple = FALSE,
                             accept = c(".txt")),
                   fileInput("colors",
                             "Choose color file(.tsv)",
                             multiple = FALSE,
                             accept = c(".txt")),
                   checkboxInput("default", "Example data",
                                 value = TRUE)
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
                   selectizeInput('colpal', "Choose a color palette",
                                  selected = "Set3",
                                  choices = rownames(brewer.pal.info[brewer.pal.info$category=="qual",])),
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
               uiOutput("ui")
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

