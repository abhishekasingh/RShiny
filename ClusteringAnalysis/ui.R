library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinythemes)
library(datasets)
library(plotly)
library(ggplot2)
library(naniar)
library(caTools)
library(dplyr)
library(tidyverse)
library(corrplot)
library(cluster)
library(factoextra)
library(clustertend)
library(NbClust)
library(fpc)
library(clValid)
library(pvclust)
library(dendextend)
library(gplots)
library(pheatmap)
library(d3heatmap)

theme_set(theme_bw())

# Define UI for application that draws a histogram
shinyUI(
    dashboardPage(
        
        # Application title
        dashboardHeader(title = "Demystify Data"),
        
        # Sidebar with a slider input for number of bins 
        dashboardSidebar( 
            sidebarMenu(
                menuItem("Dashboard", tabName = 'dashboard', icon = icon('dashboard')),
                menuItem(text = 'Clustering',
                         tabName = "clustering",
                         icon = icon("clipboard"),
                         menuSubItem(text = "Partitional Clustering", 
                                     tabName = 'partitionalclustering',
                                     icon = icon('line-chart')),
                         menuSubItem(text = "Hierarchical Clustering", 
                                     tabName = 'hierarchicalclustering',
                                     icon = icon('line-chart'))
                )
                
            )
            , collapsed = TRUE
        ),
        dashboardBody(
            tags$head(
                tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
            ),
            tabItems(
                tabItem(tabName = "dashboard",
                        
                        fluidRow(
                            valueBox(2, "Number of Algorithms", icon = icon("bezier-curve"), color = "purple"),
                            valueBox(15 * 2, "Coffee", icon = icon("coffee"), color = "yellow"),
                            valueBox(5, "Team Members", icon = icon("users")),
                        ),
                        column(12,
                               tags$img(src="images/dashboard.jpg", width="50%", style="margin:0 auto; display:block")
                        ),
                        tags$div('style' = "clear:both;padding-top: 10px;padding-bottom: 10px;"),
                        helpText("Note: The tool is in the beta version and does not support null values"),
                        helpText("Please refresh the tool if you find any discrepancy.")
                        
                ),
                
                
                ### Hierarchical Clustering starts here
                tabItem(tabName = "hierarchicalclustering",
                        div(class = "well cluster", "Hierarchical Clustering"),
                        fluidRow(class="uploadfeature",
                                 # column allocation for widgets
                                 column(6,
                                        fileInput("hcfile","Upload the file"),
                                        helpText("Default max. file size is 5MB"),
                                 ),
                                 column(3,
                                        h5(helpText("Select the read.table parameters below")),
                                        checkboxInput(inputId = 'hcheader', label = 'Header', value = TRUE),
                                        checkboxInput(inputId = "hcstringAsFactors", "stringAsFactors", FALSE),
                                 ),
                                 column(3,
                                        radioButtons(inputId = 'hcsep', label = 'Separator', choices = c(Comma=',',Semicolon=';',Tab='\t', Space=''), selected = ','),
                                 )
                        ),
                        
                        conditionalPanel(
                            condition = "output.hcfileUploaded",
                            hr(),
                            fluidRow(
                                class="selectfeature",
                                # column allocation for widgets
                                column(6,
                                       uiOutput("hcx")
                                       
                                ),
                                column(3,
                                       h5(helpText("Scale the dataset")),
                                       checkboxInput(inputId = 'hcscale', label = 'Scale', value = TRUE)
                                ),
                                
                                column(2,
                                       actionButton("hcnumberclusters", "Compute the number of clusters"),
                                ),
                                
                                column(12,
                                       uiOutput("hcvarx")
                                )
                            )
                        ),
                        
                        conditionalPanel(
                            condition = "input.hcnumberclusters > 0",
                            hr(),
                            div(class = "well cluster", "Optimal Number of clusters"),
                            fluidRow(
                                
                                box(title = "The Best Number Of Clusters",solidHeader = TRUE, width = 12,
                                    plotOutput("hcnbclust", height = 350)
                                ),
                                box(title = "Elbow method",solidHeader = TRUE,
                                    plotOutput("hcelbowmethood", height = 350)
                                ),
                                
                                box(title = "Silhouette Methood",solidHeader = TRUE,
                                    plotOutput("hcsilhouettemethood", height = 350)
                                )
                            ),
                            
                            tags$div('style' = "clear:both;margin-top: 10px;margin-bottom: 10px;") ,
                            
                            fluidRow(
                                class="hcinput",
                                column(4,
                                       
                                       textInput("hcnumberofclusters", "Enter the number of clusters"),
                                ),
                                
                                column(4,
                                       actionButton("hcnumberofclustersaction", "Run Model"),
                                ),
                            ),
                            tags$div('style' = "clear:both;margin-top: 10px;margin-bottom: 10px;")
                            
                        ),
                        
                        conditionalPanel(
                            condition = ("input.hcnumberofclustersaction > 0"),
                            hr(),
                            fluidRow(
                                class="hcoutput",
                                # column allocation for widgets
                                
                                ### Single Linkage
                                div(class = "well cluster", "Single Linkage"),
                                box(title = "Dendrogram - Single Linkage", width = 6, solidHeader = TRUE,
                                    plotOutput("singlelinkagedendrogram", height = 350)
                                ),
                                box(title = "Cluster Plot - Single Linkage", width = 6, solidHeader = TRUE,
                                    plotOutput("singlelinkagecluster", height = 350)
                                ),
                                tags$div('style' = "clear:both;margin-top: 10px;margin-bottom: 10px;"),
                                
                                ### Complete Linkage
                                div(class = "well cluster", "Complete Linkage"),
                                box(title = "Dendrogram - Complete Linkage", width = 6, solidHeader = TRUE,
                                    plotOutput("completelinkagedendrogram", height = 350)
                                ),
                                box(title = "Cluster Plot - Complete Linkage", width = 6, solidHeader = TRUE,
                                    plotOutput("completelinkagecluster", height = 350)
                                ),
                                tags$div('style' = "clear:both;margin-top: 10px;margin-bottom: 10px;"),
                                
                                ### Average Linkage
                                div(class = "well cluster", "Average Linkage"),
                                box(title = "Dendrogram - Average Linkage", width = 6, solidHeader = TRUE,
                                    plotOutput("averagelinkagedendrogram", height = 350)
                                ),
                                box(title = "Cluster Plot - Average Linkage", width = 6, solidHeader = TRUE,
                                    plotOutput("averagelinkagecluster", height = 350)
                                ),
                                tags$div('style' = "clear:both;margin-top: 10px;margin-bottom: 10px;"),
                                
                                ### Ward Linkage
                                div(class = "well cluster", "Ward Linkage"),
                                box(title = "Dendrogram - Ward Linkage", width = 6, solidHeader = TRUE,
                                    plotOutput("wardlinkagedendrogram", height = 350)
                                ),
                                box(title = "Cluster Plot - Ward Linkage", width = 6, solidHeader = TRUE,
                                    plotOutput("wardlinkagecluster", height = 350)
                                ),
                                tags$div('style' = "clear:both;margin-top: 10px;margin-bottom: 10px;")
                                
                            )
                        ),
                        
                        
                ),
                ### Hierarchical Clustering starts here
                
                ### Partitional Clustering starts here
                tabItem(tabName = "partitionalclustering",
                            div(class = "well cluster", "Partitional Clustering"),
                            fluidRow(class="uploadfeature",
                                # column allocation for widgets
                                column(6,
                                       fileInput("kmeansfile","Upload the file"),
                                       helpText("Default max. file size is 5MB"),
                                ),
                                column(3,
                                       h5(helpText("Select the read.table parameters below")),
                                       checkboxInput(inputId = 'kmeansheader', label = 'Header', value = TRUE),
                                       checkboxInput(inputId = "kmeansstringAsFactors", "stringAsFactors", FALSE),
                                ),
                                column(3,
                                       radioButtons(inputId = 'kmeanssep', label = 'Separator', choices = c(Comma=',',Semicolon=';',Tab='\t', Space=''), selected = ','),
                                )
                            ),
                            conditionalPanel(
                                condition = "output.kmeansfileUploaded",
                                hr(),
                                fluidRow(
                                    class="selectfeature",
                                    # column allocation for widgets
                                    column(6,
                                           uiOutput("kmeansx")
                                           
                                    ),
                                    
                                    column(3,
                                           h5(helpText("Scale the dataset")),
                                           checkboxInput(inputId = 'kmeansscale', label = 'Scale', value = TRUE)
                                    ),
                                    
                                    column(2,
                                           actionButton("kmeansevaluate", "Evaluate the dataset"),
                                    ),
                                    column(12,
                                           uiOutput("kmeansvarx")
                                    )
                                    
                                ),
                            ),
                            conditionalPanel(
                                condition = ("input.kmeansevaluate == 1"),
                            hr(),
                                fluidRow(
                                    # column allocation for widgets
                                    column(6,
                                           uiOutput("Hopkins")
                                    ),
                                    column(6,
                                           plotOutput("kmeansheatmap")
                                    ),
                                ),
                                
                            ),
                           
                        conditionalPanel(
                            condition = ("input.getclusterbutton > 0"),
                            hr(),
                            fluidRow(
                                class="getnumberofcluster",
                                # column allocation for widgets
                                div(class = "well cluster", "Optimal Number of clusters"),
                                       box(title = "Elbow method",solidHeader = TRUE,
                                           plotOutput("elbowmethood", height = 250)
                                       ),
                                
                                       box(title = "Silhouette Methood",solidHeader = TRUE,
                                           plotOutput("silhouettemethood", height = 250)
                                       ),
                            ),
                                       
                            tags$div('style' = "clear:both;margin-top: 10px;margin-bottom: 10px;") ,
                            fluidRow(
                                    class="kmeansinput",
                                    column(12,
                                           helpText("Enter the details for K-Means"),
                                    ),
                                column(4,
                                       
                                       textInput("numberofclusters", "Enter the number of clusters"),
                                ),
                                column(4,
                                       textInput("numberofnstarts", "Enter the number of nstart "),
                                ),
                                column(4,
                                       actionButton("numberofclustersaction", "Run K-Means Model"),
                                ),
                                ),
                                tags$div('style' = "clear:both;margin-top: 10px;margin-bottom: 10px;") ,
                                fluidRow(
                                    class="kmedoidinput",
                                    column(12,
                                    helpText("Enter the details for K-Medoid"),
                                    ),
                                    column(4,
                                       textInput("kmedoidnumberofclusters", "Enter the number of clusters"),
                                ),
                                
                                column(4,
                                       actionButton("kmedoidnumberofclustersaction", "Run K-Medoids Model"),
                                )
                                ),
                            ),
                        
                        conditionalPanel(
                            condition = ("input.numberofclustersaction > 0"),
                            hr(),
                            fluidRow(
                                class="knnoutput",
                                # column allocation for widgets
                                div(class = "well cluster", "K-Means"),
                                
                                column(12,
                                       uiOutput("kmeansoutput")
                                ),
                                
                                column(6,
                                       verbatimTextOutput("knnoutputsummary") 
                                ),
                                box(title = "K-Means: Cluster Plot", width = 6, solidHeader = TRUE,
                                    plotOutput("knnoutputplot", height = 350)
                                ),
                                       
                            )
                        ),
                        conditionalPanel(
                            condition = ("input.kmedoidnumberofclustersaction > 0"),
                            hr(),
                            fluidRow(
                                class="knnoutput",
                                # column allocation for widgets
                                div(class = "well cluster", "K-Medoids"),
                                column(6,
                                       verbatimTextOutput("kmedoidsummary") 
                                ),
                                box(title = "K-Medoids: Cluster Plot", width = 6, solidHeader = TRUE,
                                    plotOutput("kmedoidplot", height = 350)
                                ),
                                
                            )
                        )
                        
                        
                        )
                
                ### Partitional Clustering ends here
                
            ),
            
            
        )
    )
)