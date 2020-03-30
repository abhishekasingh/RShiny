library(shiny)
library(ggplot2)
# Define server logic required to draw a histogram
shinyServer(function(input, output, session){
    
    #### Hierarchical Clustering starts here
    
    hcdata <- reactive({
        file1 <- input$hcfile
        if(is.null(file1)){return()} 
        df <- read.csv(file=file1$datapath, 
                       sep=input$hcsep, 
                       header = input$hcheader, 
                       stringsAsFactors = input$hcstringAsFactors)
        return(df)
    })
    
    output$hcfileUploaded <- reactive({
        return(!is.null(hcdata()))
    })
    
    outputOptions(output, "hcfileUploaded", suspendWhenHidden=FALSE)
    
    # Pulling the list of variable for choice of variable x
    output$hcx <- renderUI({
        selectInput("hcvarx",
                    "Select the inputs Variable",
                    choices=names(hcdata()),
                    multiple = T)
    })
    
    output$hcvarx <- renderUI({
        hcxvals <- paste(input$hcvarx, collapse = ", ")
        tags$p(paste("Your ideal(s) value(s) are", hcxvals), class="")
    })
    
    output$hcelbowmethood <- renderPlot({
        if(input$hcscale == TRUE)
            df.data <- scale(hcdata()[,input$hcvarx])
        else
            df.data <- hcdata()[,input$hcvarx]
        
        fviz_nbclust(df.data, kmeans, method = "wss")
        
    })
    
    output$hcsilhouettemethood <- renderPlot({
        if(input$hcscale == TRUE)
            df.data <- scale(hcdata()[,input$hcvarx])
        else
            df.data <- hcdata()[,input$hcvarx]
        
        fviz_nbclust(df.data, pam, method = "silhouette")
        
    })
    
    output$hcnbclust <- renderPlot({
        if(input$hcscale == TRUE)
            df.data <- scale(hcdata()[,input$hcvarx])
        else
            df.data <- hcdata()[,input$hcvarx]
        
        nb.res <- NbClust(df.data, distance = "euclidean", min.nc = 2,
                          max.nc = 10, method = "kmeans")
        
        fviz_nbclust(nb.res)
        
    })
    
    #### Single Linkage Dendrogram
    output$singlelinkagedendrogram <- renderPlot({
        if(input$hcscale == TRUE)
            df.data <- scale(hcdata()[,input$hcvarx])
        else
            df.data <- hcdata()[,input$hcvarx]
        
        res.dist <- dist(df.data, method = "euclidean")
        
        single.res.hc <- hclust(d = res.dist, method = "single")
        
        fviz_dend(single.res.hc, k = as.numeric(input$hcnumberofclusters),
                  cex = 0.5,
                  k_colors = "jco",
                  color_labels_by_k = TRUE,
                  rect = TRUE
        )
        
    })
    
    #### Single Linkage Cluster
    output$singlelinkagecluster <- renderPlot({
        if(input$hcscale == TRUE)
            df.data <- scale(hcdata()[,input$hcvarx])
        else
            df.data <- hcdata()[,input$hcvarx]
        
        res.dist <- dist(df.data, method = "euclidean")
        
        single.res.hc <- hclust(d = res.dist, method = "single")
        
        fviz_cluster(list(data = df.data, cluster = cutree(single.res.hc, k = as.numeric(input$hcnumberofclusters))),
                     palette = "jco", 
                     ellipse.type = "convex",
                     repel = TRUE,
                     show.clust.cent = FALSE,
                     ggtheme = theme_minimal()
        )
        
    })
    
    #### Complete Linkage Dendrogram
    output$completelinkagedendrogram <- renderPlot({
        if(input$hcscale == TRUE)
            df.data <- scale(hcdata()[,input$hcvarx])
        else
            df.data <- hcdata()[,input$hcvarx]
        
        res.dist <- dist(df.data, method = "euclidean")
        
        complete.res.hc <- hclust(d = res.dist, method = "complete")
        
        fviz_dend(complete.res.hc, k = as.numeric(input$hcnumberofclusters),
                  cex = 0.5,
                  k_colors = "jco",
                  color_labels_by_k = TRUE,
                  rect = TRUE
        )
        
    })
    
    #### Complete Linkage Cluster
    output$completelinkagecluster <- renderPlot({
        if(input$hcscale == TRUE)
            df.data <- scale(hcdata()[,input$hcvarx])
        else
            df.data <- hcdata()[,input$hcvarx]
        
        res.dist <- dist(df.data, method = "euclidean")
        
        complete.res.hc <- hclust(d = res.dist, method = "complete")
        
        fviz_cluster(list(data = df.data, cluster = cutree(complete.res.hc, k = as.numeric(input$hcnumberofclusters))),
                     palette = "jco", 
                     ellipse.type = "convex",
                     repel = TRUE,
                     show.clust.cent = FALSE,
                     ggtheme = theme_minimal()
        )
        
    })
    
    #### Average Linkage Dendrogram
    output$averagelinkagedendrogram <- renderPlot({
        if(input$hcscale == TRUE)
            df.data <- scale(hcdata()[,input$hcvarx])
        else
            df.data <- hcdata()[,input$hcvarx]
        
        res.dist <- dist(df.data, method = "euclidean")
        
        average.res.hc <- hclust(d = res.dist, method = "average")
        
        fviz_dend(average.res.hc, k = as.numeric(input$hcnumberofclusters),
                  cex = 0.5,
                  k_colors = "jco",
                  color_labels_by_k = TRUE,
                  rect = TRUE
        )
        
    })
    
    #### Average Linkage Cluster
    output$averagelinkagecluster <- renderPlot({
        if(input$hcscale == TRUE)
            df.data <- scale(hcdata()[,input$hcvarx])
        else
            df.data <- hcdata()[,input$hcvarx]
        
        res.dist <- dist(df.data, method = "euclidean")
        
        average.res.hc <- hclust(d = res.dist, method = "average")
        
        fviz_cluster(list(data = df.data, cluster = cutree(average.res.hc, k = as.numeric(input$hcnumberofclusters))),
                     palette = "jco", 
                     ellipse.type = "convex",
                     repel = TRUE,
                     show.clust.cent = FALSE,
                     ggtheme = theme_minimal()
        )
        
    })
    
    #### ward.D2 Linkage Dendrogram
    output$wardlinkagedendrogram <- renderPlot({
        if(input$hcscale == TRUE)
            df.data <- scale(hcdata()[,input$hcvarx])
        else
            df.data <- hcdata()[,input$hcvarx]
        
        res.dist <- dist(df.data, method = "euclidean")
        
        ward.res.hc <- hclust(d = res.dist, method = "ward.D2")
        
        fviz_dend(ward.res.hc, k = as.numeric(input$hcnumberofclusters),
                  cex = 0.5,
                  k_colors = "jco",
                  color_labels_by_k = TRUE,
                  rect = TRUE
        )
        
    })
    
    #### ward.D2 Linkage Cluster
    output$wardlinkagecluster <- renderPlot({
        if(input$hcscale == TRUE)
            df.data <- scale(hcdata()[,input$hcvarx])
        else
            df.data <- hcdata()[,input$hcvarx]
        
        res.dist <- dist(df.data, method = "euclidean")
        
        ward.res.hc <- hclust(d = res.dist, method = "ward.D2")
        
        fviz_cluster(list(data = df.data, cluster = cutree(ward.res.hc, k = as.numeric(input$hcnumberofclusters))),
                     palette = "jco", 
                     ellipse.type = "convex",
                     repel = TRUE,
                     show.clust.cent = FALSE,
                     ggtheme = theme_minimal()
        )
        
    })
    
    
    #### Hierarchical Clustering Ends here
    
    #### Partitional Clustering starts here
    kmeansdata <- reactive({
        file1 <- input$kmeansfile
        if(is.null(file1)){return()} 
        df <- read.csv(file=file1$datapath, 
                       sep=input$kmeanssep, 
                       header = input$kmeansheader, 
                       stringsAsFactors = input$kmeansstringAsFactors)
        return(df)
    })
    
    output$kmeansfileUploaded <- reactive({
        return(!is.null(kmeansdata()))
    })
    
    outputOptions(output, "kmeansfileUploaded", suspendWhenHidden=FALSE)
    
    # Pulling the list of variable for choice of variable x
    output$kmeansx <- renderUI({
        selectInput("kmeansvarx",
                    "Select the inputs Variable",
                    choices=names(kmeansdata()),
                    multiple = T)
    })
    
    output$kmeansvarx <- renderUI({
        kmeansxvals <- paste(input$kmeansvarx, collapse = ", ")
        tags$p(paste("Your ideal(s) value(s) are", kmeansxvals), class="")
    })
    
    output$Hopkins <- renderUI({
        req(kmeansdata())
        if(input$kmeansevaluate == 0)
            return()
        else
            if(input$kmeansscale == TRUE)
                df.data <- scale(kmeansdata()[,input$kmeansvarx])
            else
                df.data <- kmeansdata()[,input$kmeansvarx]
                
            res <- get_clust_tendency(df.data, n = nrow(df.data)-1, graph = FALSE)
            
            
            if(res$hopkins_stat > 0.5){
                withTags({
                    div(
                        p("The null and the alternative hypotheses are defined as follow:"),
                        ul(
                            li("Null hypothesis: the data set D is uniformly distributed (i.e., no meaningful clusters)"),
                            li("Alternative hypothesis: the data set D is not uniformly distributed (i.e., contains meaningful clusters)")
                            
                        ),
                        p(paste("The Hopkins statistic states ", round(res$hopkins_stat, 3)), class = "hopkinssuccess"),
                        actionButton("getclusterbutton", "Compute the number of clusters", class="getclusterbutton")
                    )
                    
                })
            }
            else{
                withTags({
                    div(
                        p("The null and the alternative hypotheses are defined as follow:"),
                        ul(
                            li("Null hypothesis: the data set D is uniformly distributed (i.e., no meaningful clusters)"),
                            br(),
                            li("Alternative hypothesis: the data set D is not uniformly distributed (i.e., contains meaningful clusters)")
                            
                        ),
                        br(),
                        p(paste("The Hopkins statistic states ", round(res$hopkins_stat, 3)), class = "hopkinerror")
                    )
                    
                })
                
            }
    })
    
    output$kmeansheatmap <- renderPlot({
        if(input$kmeansscale == TRUE)
            df.data <- scale(kmeansdata()[,input$kmeansvarx])
        else
            df.data <- kmeansdata()[,input$kmeansvarx]
        
        fviz_pca_ind(prcomp(df.data), title = "PCA - Dataset",
                     geom = "point", ggtheme = theme_classic(),
                     legend = "bottom")
        
    })
    
    output$elbowmethood <- renderPlot({
        if(input$kmeansscale == TRUE)
            df.data <- scale(kmeansdata()[,input$kmeansvarx])
        else
            df.data <- kmeansdata()[,input$kmeansvarx]
        
        fviz_nbclust(df.data, kmeans, method = "wss")
            
    })
    
    output$silhouettemethood <- renderPlot({
        if(input$kmeansscale == TRUE)
            df.data <- scale(kmeansdata()[,input$kmeansvarx])
        else
            df.data <- kmeansdata()[,input$kmeansvarx]
        
        fviz_nbclust(df.data, pam, method = "silhouette")
        
    })
    
    output$kmeansoutput <- renderUI({
        req(kmeansdata())
        if(input$kmeansevaluate == 0)
            return()
        else
            if(input$kmeansscale == TRUE)
                df.data <- scale(kmeansdata()[,input$kmeansvarx])
            else
                df.data <- kmeansdata()[,input$kmeansvarx]
            
            
            km.res <- kmeans(df.data, as.numeric(input$numberofclusters), nstart = as.numeric(input$numberofnstarts) )
            
            kmeans.size <- paste(km.res$size, collapse = ", ")
            
            print("K means results are as follows:")
            
            withTags({
                div(style = "",
            ul(
                li(paste("Size:", kmeans.size)),
                li(paste("Total within clusters: ", round(km.res$tot.withinss),3))
                
            ),
            
            )
            })
            
                
            
    })
    
    output$knnoutputsummary <- renderPrint({
        if(input$kmeansscale == TRUE)
            df.data <- scale(kmeansdata()[,input$kmeansvarx])
        else
            df.data <- kmeansdata()[,input$kmeansvarx]
        
            
        km.res <- kmeans(df.data, as.numeric(input$numberofclusters), nstart = as.numeric(input$numberofnstarts) )
        
        print(km.res)
        
    })
    
    output$knnoutputplot <- renderPlot({
        if(input$kmeansscale == TRUE)
            df.data <- scale(kmeansdata()[,input$kmeansvarx])
        else
            df.data <- kmeansdata()[,input$kmeansvarx]
        
        km.res <- kmeans(df.data, as.numeric(input$numberofclusters), nstart = as.numeric(input$numberofnstarts) )
        
        fviz_cluster(km.res, data = df.data,
                     palette = 'jco', 
                     ellipse.type = "euclid", # Concentration ellipse
                     star.plot = TRUE, # Add segments from centroids to items
                     repel = TRUE, # Avoid label overplotting (slow)
                     ggtheme = theme_minimal()
        )
        
    })
    
    output$kmedoidsummary <- renderPrint({
        if(input$kmeansscale == TRUE)
            df.data <- scale(kmeansdata()[,input$kmeansvarx])
        else
            df.data <- kmeansdata()[,input$kmeansvarx]
        
        
        pam.res <- pam(df.data, as.numeric(input$kmedoidnumberofclusters))
        
        print(pam.res)
        
    })
    
    output$kmedoidplot <- renderPlot({
        if(input$kmeansscale == TRUE)
            df.data <- scale(kmeansdata()[,input$kmeansvarx])
        else
            df.data <- kmeansdata()[,input$kmeansvarx]
        
        pam.res <- pam(df.data, as.numeric(input$kmedoidnumberofclusters))
        
        fviz_cluster(pam.res, 
                     palette = "jco", # color palette
                     ellipse.type = "t", # Concentration ellipse
                     repel = TRUE, # Avoid label overplotting (slow),
                     ggtheme = theme_classic()
        )
        
    })
    
    #### Partitional Clustering ends here
    
})