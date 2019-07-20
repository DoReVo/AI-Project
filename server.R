source("helperScripts/models.R")
library(ggplot2)

# GENERATE GRAPH LIST
frameList = generatePrediction()

# EXTRACT DATA FRAMES FROM LIST
graphDataFrame1 = data.frame(frameList[1])
graphDataFrame2 = data.frame(frameList[2])
graphDataFrame3 = data.frame(frameList[3])
graphDataFrame4 = data.frame(frameList[4])


renderedDataFrame = graphDataFrame1
graphAccuracy = c()
graphErrorRate = c()
summaryText = NULL

 masterGraph = function(x,shownCol,keyCol,renderedDataFrame){        

    #  Graph Object
    graph = ggplot(renderedDataFrame,aes(x=1:nrow(renderedDataFrame),y=strength)) +    
    labs(x="Row",y="Actual Strength Vs Predicted Strength") +            
    theme(legend.position="none") +
    xlim(x[1],x[2])

    # Control key columns to be rendered on graphs // Strength & Predicted Strength
    if(!is.null(keyCol)){
        for(x in keyCol){
            switch(x,
            "strength"={
                graph = graph + geom_line() +  geom_point()                
            },
            "predictedStr" = {
                graph = graph + geom_line(aes(y=PredictedStrength,col="#5fb56c")) +  geom_point(aes(y=PredictedStrength),col="#5fb56c")
            })
        }
    }

    # Control optional columns to be rendered on graphs // Columns in csv file
    if(!is.null(shownCol)){        
        for(x in shownCol){
            switch(x,
            "cement"={
                graph = graph + geom_line(aes(y=cement,col="#4b6d97")) +  geom_point(aes(y=cement),col="#4b6d97")                
            },
            "slag"={
                graph = graph + geom_line(aes(y=slag,col="#71a4a1")) +  geom_point(aes(y=slag),col="#71a4a1")                
            },
            "ash"={
                graph = graph + geom_line(aes(y=ash,col="#d0dde0")) +  geom_point(aes(y=ash),col="#d0dde0")                
            },
            "water"={
                graph = graph + geom_line(aes(y=water,col="#867a90")) +  geom_point(aes(y=water),col="#867a90")                
            },
            "superPlastic"={
                graph = graph + geom_line(aes(y=superplastic,col="#b762dd")) +  geom_point(aes(y=superplastic),col="#b762dd")                
            },
            "coarseagg"={
                graph = graph + geom_line(aes(y=coarseagg,col="#5137a5")) +  geom_point(aes(y=coarseagg),col="#5137a5")                
            },
            "fineagg"={
                graph = graph + geom_line(aes(y=fineagg,col="#54aadf")) +  geom_point(aes(y=fineagg),col="#54aadf")                
            },
            "age"={
                graph = graph + geom_line(aes(y=age,col="black")) +  geom_point(aes(y=age),col="black")                
            }
            )
        }
    }

return (graph)            
}


server = function(input, output) {
        
    # Observe which tab is currently opened and control rendred graphs dataframe
    observeEvent(input$tabSet,
        {
            # Assign data frame to be rendered on graphs
            switch(input$tabSet,
                "tab1" = {
                    renderedDataFrame = graphDataFrame1                                                    
                },
                "tab2" = {
                    renderedDataFrame = graphDataFrame2                    
                },
                "tab3" = {
                    renderedDataFrame = graphDataFrame3                    
                },
                "tab4" = {
                    renderedDataFrame = graphDataFrame4                    
                }
            )
            # Set graph accuracy text
            graphAccuracy = format(cor(renderedDataFrame$PredictedStrength,concrete_test$strength),digits=4)
            # Set graph error rate text
            graphErrorRate = format(1-cor(renderedDataFrame$PredictedStrength,concrete_test$strength),digits=4)

            # summaryText = toString(input$shownCol)
            # Ui output for accuracy
            output$accuracyRate = renderText(
                    {
                        paste("Accuracy Rate: <mark>" , graphAccuracy,"</mark>")
                        
                    }
            )
            # Ui output for error rate
            output$errorRate = renderText(
                {
                    paste("Error Rate: <mark>", graphErrorRate,"</mark>")
                }        
            )

            # Render summary text
            # output$summaryText = renderText(
                # paste("hello ",summaryText,"<br>")
                # paste(typeof(input$shownCol))
                # paste(input$shownCol,input$keyCol)
                
                # summaryText
                # paste("Graph columns",input$shownCol)
                # paste(input$shownCol)
            # )
            # Render the graph
            output$myGraph = renderPlot(
                {
                    masterGraph(input$xControl,input$shownCol,input$keyCol,renderedDataFrame)    
                }                
            )
        }
    )


    observeEvent(
        input$shownCol,{
            output$summaryText = renderText(
                # paste("Columns shown are",toString(input$shownCol))
                paste("Graph shown is prediction of an ANN model with")
            )
            
            
        }
    )
    output$debug = renderText(
        {
            # nrow(renderedDataFrame)
        }
    )

    output$reactiveWord = renderText(
        {
            #  paste0("x=", input$clickEvent$x, "\ny=", input$clickEvent$y)
            input$xControl[1]
        }
    )
    
  
}