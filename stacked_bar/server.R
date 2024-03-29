#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(RColorBrewer)
library(reshape2)
library(ggpubr)
library(colourpicker)
library(colorspace)
library(shinycssloaders)
library(shinydashboard)
library(reactable)

color_lighten <- function(cc,num){
    tmp <- c()
    ln <- 0.8/num
    for (i in seq(num)) {
        tmp <- c(tmp,lighten(cc, i*ln))
    }
    return(rev(tmp))
}

color_list = rownames(brewer.pal.info[brewer.pal.info$category=="seq",])

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    td <- tempdir()

    counts <- reactive({
        ifelse(is.null(input$counts),
               data <- read.table("./www/counts.txt",header = TRUE,sep = "\t",row.names = 1,check.names=FALSE),
               data <- read.table(input$counts$datapath,header = TRUE,sep = "\t",row.names = 1,check.names=FALSE)
               )
        data
    })
    
    colors <- reactive({
        ifelse(is.null(input$colors),
               data <- read.table("./www/colors.txt",header = TRUE,sep = "\t",check.names=FALSE),
               data <- read.table(input$colors$datapath,header = TRUE,sep = "\t",check.names=FALSE)
               )
        colnames(data) <- c("taxa","color")
        data
    })
    
    groups <- reactive({
        ifelse(is.null(input$groups),
               data <- read.table("./www/group.txt",header = TRUE,sep="\t",check.names=FALSE),
               data <- read.table(input$groups$datapath,header = TRUE,sep = "\t",check.names=FALSE)
               )
        colnames(data) <- c("sample","group")
        data
    })
    
    output$ct_table <- renderReactable({
        validate(
            need(try(counts() != ""),"Please upload count file")
        )
        reactable(counts())
    })
    
    output$gp_table <- renderReactable({
        validate(
            need(try(groups() != ""),"Please upload group file")
        )
        reactable(groups())
    })
    
    output$cl_table <- renderReactable({
        validate(
            need(try(colors() != ""),"Please upload color file")
        )
        reactable(colors())
    })
    
    output$downloadp1 <- downloadHandler(
        filename <- function() {
            paste("p1", "pdf", sep=".")
        },
        content <- function(file) {
            file.copy(paste0(td,"/p1.pdf"), file)
        })
    
    output$downloadp1png <- downloadHandler(
        filename <- function() {
            paste("p1", "png", sep=".")
        },
        content <- function(file) {
            file.copy(paste0(td,"/p1.png"), file)
        })
    
    output$downloadp2 <- downloadHandler(
        filename <- function() {
            paste("p2", "pdf", sep=".")
        },
        content <- function(file) {
            file.copy(paste0(td,"/p2.pdf"), file)
        })
    output$downloadp2png <- downloadHandler(
        filename <- function() {
            paste("p2", "png", sep=".")
        },
        content <- function(file) {
            file.copy(paste0(td,"/p2.png"), file)
        })
    
    output$downloadp3 <- downloadHandler(
        filename <- function() {
            paste("p3", "pdf", sep=".")
        },
        content <- function(file) {
            file.copy(paste0(td,"/p3.pdf"), file)
        })
    output$downloadp3png <- downloadHandler(
        filename <- function() {
            paste("p3", "png", sep=".")
        },
        content <- function(file) {
            file.copy(paste0(td,"/p3.png"), file)
        })
    
    output$colourpickers <- renderUI({
        if(input$customcol){
            if(!is.null(colors())){
                pvars <- length(unique(colors()$color))
            }else{
                validate(
                    need(input$colors,"Please upload color file")
                )
            }
            
            pvars <- length(unique(colors()$color))
            lapply(seq(pvars), function(i) {
                colourInput(paste0("col", i), paste0("Select colour ", i),"#D42424")
            })
            
        }
    })
    
    observeEvent(input$rep,{
        colors <- colors()
        groups <- groups()
        counts <- counts()
        
        counts[is.na(counts)] <- 0
        color_l <- sample(1:18, length(unique(colors$color)), replace = FALSE)
        
        tmp <- c()
        for (i in 1:length(color_l)) {
            tmp<- c(tmp,colorRampPalette(brewer.pal(9,color_list[color_l[i]])[c(3,5,7)])(data.frame(table(colors$color))$Freq[i]))
        }
        
        colors$my_color <- tmp
        counts$group <- groups$group
        tmp <- melt(counts,id.vars="group")
        
        tmp$variable <- factor(tmp$variable,
                               levels = colors$taxa)
        
        p1 <- ggplot(tmp,aes(group,value,fill=variable)) +
            geom_bar(stat="identity",position = "fill",width = 0.8,size=0.25) +
            xlab("") + ylab(input$ylabel) + 
            scale_y_continuous(labels = scales::percent) +
            scale_fill_manual(values = colors$my_color) +
            guides(fill=guide_legend(title=NULL)) +
            theme(axis.text.x=element_text(size=input$xfontsize),
                  axis.text.y=element_text(size=input$yfontsize),
                  axis.title.y=element_text(size=input$tyfontsize),
                  panel.grid = element_blank(), 
                  panel.background = element_rect(color = 'black', 
                                                  fill = 'transparent'))
        
        ggsave(paste0(td,"/p1.pdf"),plot = p1,
               width = input$plotwidth/72,
               height = input$plotheight/72,
               dpi=300)
        ggsave(paste0(td,"/p1.png"),plot = p1,
               width = input$plotwidth/72,
               height = input$plotheight/72,
               dpi=300)
        
        output$stp1 <- renderPlot(p1)
    })
    
    observeEvent(input$run,{
        
        colors <- colors()
        groups <- groups()
        counts <- counts()
        
        counts[is.na(counts)] <- 0
        color_l <- sample(1:18, length(unique(colors$color)), replace = FALSE)
        
        tmp <- c()
        for (i in 1:length(color_l)) {
            tmp<- c(tmp,colorRampPalette(brewer.pal(9,color_list[color_l[i]])[c(3,5,7)])(data.frame(table(colors$color))$Freq[i]))
        }
        
        colors$my_color <- tmp
        counts$group <- groups$group
        tmp <- melt(counts,id.vars="group")
        
        tmp$variable <- factor(tmp$variable,
                               levels = colors$taxa)
        
        p1 <- ggplot(tmp,aes(group,value,fill=variable)) +
            geom_bar(stat="identity",position = "fill",width = 0.8,size=0.25) +
            xlab("") + ylab(input$ylabel) + 
            scale_y_continuous(labels = scales::percent) +
            scale_fill_manual(values = colors$my_color) +
            guides(fill=guide_legend(title=NULL)) +
            theme(axis.text.x=element_text(size=input$xfontsize),
                  axis.text.y=element_text(size=input$yfontsize),
                  axis.title.y=element_text(size=input$tyfontsize),
                  panel.grid = element_blank(), 
                  panel.background = element_rect(color = 'black', 
                                                  fill = 'transparent'))
        ggsave(paste0(td,"/p1.pdf"),
               plot = p1,
               width = input$plotwidth/72,
               height = input$plotheight/72,dpi=300)
        ggsave(paste0(td,"/p1.png"),
               plot = p1,
               width = input$plotwidth/72,
               height = input$plotheight/72,dpi=300)
        colors$my_color <- colorRampPalette(brewer.pal(8, input$colpal))(length(unique(colors$taxa)))
        
        p2 <- ggplot(tmp,aes(group,value,fill=variable)) +
            geom_bar(stat="identity",position = "fill",width = 0.8,size=0.25) +
            xlab("") + ylab(input$ylabel) + 
            scale_y_continuous(labels = scales::percent) +
            scale_fill_manual(values = colors$my_color) +
            guides(fill=guide_legend(title=NULL)) +
            theme(axis.text.x=element_text(size=input$xfontsize),
                  axis.text.y=element_text(size=input$yfontsize),
                  axis.title.y=element_text(size=input$tyfontsize),
                  panel.grid = element_blank(), 
                  panel.background = element_rect(color = 'black', 
                                                  fill = 'transparent'))
        
        ggsave(paste0(td,"/p2.pdf"),plot = p2,
               width = input$plotwidth/72,
               height = input$plotheight/72,dpi=300)
        ggsave(paste0(td,"/p2.png"),plot = p2,
               width = input$plotwidth/72,
               height = input$plotheight/72,dpi=300)
        
        output$stp1 <- renderPlot(p1)
        output$stp2 <- renderPlot(p2)
        
        if(input$customcol){
            output$ui <- renderUI({
                tabBox(
                    title = "Plot Area",
                    id = "plotarea",width = NULL,
                    tabPanel("Plot 1", 
                             h4("Random colors for each taxon group"),
                             splitLayout(cellWidths = c("70%","30%"),
                                         tags$div(plotOutput("stp1",
                                                             width = paste0(input$plotwidth, "px"),
                                                             height = paste0(input$plotheight, "px")) %>% withSpinner()),
                                         tags$div(actionButton("rep", label = "Re-generate"),
                                                  tags$br(),
                                                  tags$br(),
                                                  downloadButton("downloadp1", "Save PDF", icon = icon("download")),
                                                  tags$br(),
                                                  tags$br(),
                                                  downloadButton("downloadp1png", "Save PNG", icon = icon("download")))
                             )),
                    tabPanel("Plot 2",
                             h4("Random colors for each taxon"),
                             splitLayout(cellWidths = c("70%","30%"),
                                         tags$div(plotOutput("stp2",
                                                             width = paste0(input$plotwidth, "px"),
                                                             height = paste0(input$plotheight, "px")) %>% withSpinner()),
                                         tags$div(downloadButton("downloadp2", "Save PDF", icon = icon("download")),
                                                  tags$br(),
                                                  tags$br(),
                                                  downloadButton("downloadp2png", "Save PNG", icon = icon("download")))
                             )),
                    tabPanel("Plot 3",
                             h4("Custom colors for each taxon group"),
                             splitLayout(cellWidths = c("70%","30%"),
                                         tags$div(plotOutput("stp3",
                                                             width = paste0(input$plotwidth, "px"),
                                                             height = paste0(input$plotheight, "px")) %>% withSpinner()),
                                         tags$div(downloadButton("downloadp3", "Save PDF", icon = icon("download")),
                                                  tags$br(),
                                                  tags$br(),
                                                  downloadButton("downloadp3png", "Save PNG", icon = icon("download")))
                             ))
                )
            })
            
            output$textanno <- renderUI({
                tags$div(
                    tags$h4("Plot1: Random colors for each taxon group"), 
                    tags$h4("Plot2: Random colors for each taxon"),
                    tags$h4("Plot3: Custom colors for each taxon group")
                )
            })
            
            
            
            custom_colors <- c()
            for (i in seq(length(unique(colors$color)))) {
                custom_colors <-c(custom_colors,
                                  color_lighten(eval(parse(text = paste0("input$col", i))),
                                                data.frame(table(colors$color))$Freq[i]))
            }
            
            colors$my_color <- custom_colors
            
            p3 <- ggplot(tmp,aes(group,value,fill=variable)) +
                geom_bar(stat="identity",position = "fill",width = 0.8,size=0.25) +
                xlab("") + ylab(input$ylabel) + 
                scale_y_continuous(labels = scales::percent) +
                scale_fill_manual(values = colors$my_color) +
                guides(fill=guide_legend(title=NULL)) +
                theme(axis.text.x=element_text(size=input$xfontsize),
                      axis.text.y=element_text(size=input$yfontsize),
                      axis.title.y=element_text(size=input$tyfontsize),
                      panel.grid = element_blank(), 
                      panel.background = element_rect(color = 'black', 
                                                      fill = 'transparent'))
            ggsave(paste0(td,"/p3.pdf"),plot = p3,
                   width = input$plotwidth/72,
                   height = input$plotheight/72,dpi=300)
            ggsave(paste0(td,"/p3.png"),plot = p3,
                   width = input$plotwidth/72,
                   height = input$plotheight/72,dpi=300)
            output$stp3 <- renderPlot(p3)
        }
        else{
            output$textanno <- renderUI({
                tags$div(
                    tags$h4("Plot1: Random colors for each taxon group"), 
                    tags$h4("Plot2: Random colors for each taxon")
                )
            })
            
            output$ui <- renderUI({
                tabBox(
                    title = "Plot Area",
                    id = "plotarea",width = NULL,
                    tabPanel("Plot 1", 
                             h4("Random colors for each taxon group"),
                             splitLayout(cellWidths = c("70%","30%"),
                                         tags$div(plotOutput("stp1",
                                                             width = paste0(input$plotwidth, "px"),
                                                             height = paste0(input$plotheight, "px")) %>% withSpinner()),
                                         tags$div(actionButton("rep", label = "Re-generate"),
                                                  tags$br(),
                                                  tags$br(),
                                                  downloadButton("downloadp1", "Save PDF", icon = icon("download")),
                                                  tags$br(),
                                                  tags$br(),
                                                  downloadButton("downloadp1png", "Save PNG", icon = icon("download")))
                                         )
                             ),
                    tabPanel("Plot 2",
                             h4("Random colors for each taxon"),
                             splitLayout(cellWidths = c("70%","30%"),
                                         tags$div(plotOutput("stp2",
                                                             width = paste0(input$plotwidth, "px"),
                                                             height = paste0(input$plotheight, "px")) %>% withSpinner()),
                                         tags$div(downloadButton("downloadp2", "Save PDF", icon = icon("download")),
                                                  tags$br(),
                                                  tags$br(),
                                                  downloadButton("downloadp2png", "Save PNG", icon = icon("download")))
                             ))
                )
            })
        }
    })
})
