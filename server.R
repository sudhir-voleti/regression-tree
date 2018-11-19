###########################################################
#         Regression Tree App (server)              #
###########################################################
library(shiny)
library(rpart)
library(pastecs)
library(dplyr)
library(Hmisc)
library("hydroGOF")
require(party)
require(partykit)

shinyServer(function(input, output,session) {
  
  #------------------------------------------------#
  
  readdata <- reactive({
    if (is.null(input$file)) { return(NULL) }
    else{
      readdata <- as.data.frame(read.csv(input$file$datapath ,header=TRUE, sep = ","))
      return(readdata)
    }
  })

  pred.readdata <- reactive({
    if (is.null(input$filep)) { return(NULL) }
    else{
      readdata <- as.data.frame(read.csv(input$filep$datapath ,header=TRUE, sep = ","))
      return(readdata)
    }
  })

    
  # Select variables:
  output$yvarselect <- renderUI({
    if (is.null(input$file)) {return(NULL)}
    
    selectInput("yAttr", "Select Y variable",
                colnames(readdata()), colnames(readdata())[1])
    
  })
  
  output$xvarselect <- renderUI({
    if (identical(readdata(), '') || identical(readdata(),data.frame())) return(NULL)
    
    checkboxGroupInput("xAttr", "Select X variables",
                       setdiff(colnames(readdata()),input$yAttr), setdiff(colnames(readdata()),input$yAttr))
    
  })

  readdata.temp = reactive({
    mydata = readdata()[,c(input$yAttr,input$xAttr)]
  })

    
  output$fxvarselect <- renderUI({
    if (identical(readdata.temp(), '') || identical(readdata.temp(),data.frame())) return(NULL)
    
    checkboxGroupInput("fxAttr", "Select factor variable in Data set",
                       colnames(readdata.temp()) )
    
  })
  
  
  Dataset = reactive({
    mydata = readdata()[,c(input$yAttr,input$xAttr)]
    
    if (length(input$fxAttr) >= 1){
      for (j in 1:length(input$fxAttr)){
        mydata[,input$fxAttr[j]] = as.factor(mydata[,input$fxAttr[j]])
      }
    }
    return(mydata)
    
  })
  
  
  Dataset.Predict <- reactive({
    fxc = setdiff(input$fxAttr, input$yAttr)
    mydata = pred.readdata()[,c(input$xAttr)]
    
    if (length(fxc) >= 1){
      for (j in 1:length(fxc)){
        mydata[,fxc[j]] = as.factor(mydata[,fxc[j]])
      }
    }
    return(mydata)
  })
  
  # a = c('a','b','c')
  # b = ('c')
  # setdiff(a,b)
    #------------------------------------------------#
  
  out = reactive({
    data = Dataset()
    Dimensions = dim(data)
    Head = head(data)
    Tail = tail(data)
    Class = NULL
    for (i in 1:ncol(data)){
      c1 = class(data[,i])
      Class = c(Class, c1)
    }
    
    nu = which(Class %in% c("numeric","integer"))
    fa = which(Class %in% c("factor","character"))
    nu.data = data[,nu] 
    fa.data = data[,fa] 
    Summary = list(Numeric.data = round(stat.desc(nu.data)[c(4,5,6,8,9,12,13),] ,4), factor.data = describe(fa.data))
    
    a = seq(from = 0, to=200,by = 4)
    j = length(which(a < ncol(nu.data)))
    out = list(Dimensions = Dimensions,Summary =Summary ,Tail=Tail,fa.data,nu.data,a,j)
    return(out)
  })
  
  output$summarydata = renderPrint({
    if (is.null(input$file)) {return(NULL)}
    else {
      out()[1:2]
    }
  })
  
  testsample =  reactive({
  set.seed(5898)
  sample(1:nrow(Dataset()), round(nrow(Dataset())*((input$sample)/100)))
         })

  train_data = reactive({
      Dataset()[-testsample(),]
  })
  
  test_data = reactive({
    Dataset()[testsample(),]
  })
  
  
  
  #------------------------------------------------#
  #----------Random classification tree------------#
  #------------------------------------------------#
  fit.rt = reactive({
  if (identical(Dataset(), '') || identical(Dataset(),data.frame())) return(NULL)
    
  x = setdiff(colnames(Dataset()), input$Attr)
  y = input$yAttr
  # formula1 =
  ## mean predictions
  
  if (class(train_data()[,c(input$yAttr)]) == "factor"){
  fit.rt <- rpart(as.formula(paste(y, paste( x, collapse = ' + '), sep=" ~ ")),
                  cp = input$cp,
                  method="class",   # use "class" for classification trees
                data=train_data())
  pr <- as.party(fit.rt)    # thus, we use same object 'rp' from the raprt package
  val = predict(pr, newdata = test_data(),type="response")
  
  imp = round(fit.rt$variable.importance/sum(fit.rt$variable.importance),2)
  
  } else {
  fit.rt <- rpart(as.formula(paste(y, paste( x, collapse = ' + '), sep=" ~ ")),
                  cp = input$cp,
                  method="anova",   # use "class" for classification trees
                  data=train_data())
  pr <- as.party(fit.rt)    # thus, we use same object 'rp' from the raprt package
   val = predict(pr, newdata = test_data())
   imp = round(fit.rt$variable.importance/sum(fit.rt$variable.importance),2)
  }
  
  out = list(model = fit.rt, validation = val, imp = imp)
    })

  #------------------------------------------------------#
  #-------------Random forest trees----------------------#
  #------------------------------------------------------#
  
  fit.rf = reactive({
    if (identical(Dataset(), '') || identical(Dataset(),data.frame())) return(NULL)
    
    x = setdiff(colnames(Dataset()), input$Attr)
    y = input$yAttr
    # formula1 = 
    ## mean predictions
    
    if (class(train_data()[,c(input$yAttr)]) == "factor"){
      fit.randf <- randomForest(as.factor(train_data()[,c(input$yAttr)]) ~ .,train_data(),ntree = 500,mtry = 4,nodesize = 5,importance = TRUE)
      print(class(fit.randf))
      val <- predict(fit.randf,test_data())
    } else {
      fit.randf <- randomForest(as.factor(train_data()[,c(input$yAttr)]) ~ .,train_data(),ntree = 500,mtry = 4,nodesize = 5,importance = TRUE)
      print(class(fit.randf))
      val <- predict(fit.randf,test_data())
    }
    
    out <- list(model = fit.randf,validation = val)
    return(out)
  })
  #------------------------------------------------------#
  output$validation = renderPrint({
    if (is.null(input$file)) {return(NULL)}
    
    if (class(train_data()[,c(input$yAttr)]) == "factor"){
      y = test_data()[,input$yAttr]
      yhat = fit.rt()$validation
    confusion_matrix = table(y,yhat)
    accuracy = (sum(diag(confusion_matrix))/sum(confusion_matrix))*100
    out = list(Confusion_matrix_of_Validation = confusion_matrix, Accuracy_of_Validation = accuracy)
    } else {
    dft = data.frame(scale(data.frame(y = test_data()[,input$yAttr], yhat = fit.rt()$validation)))
    mse.y = mse(dft$y,dft$yhat)
    out = list(Mean_Square_Error_of_Standardized_Response_in_Validation = mse.y)
    } 
    out
       })

  
  #------------------------------------------------#
  output$results = renderPrint({
    
    if (is.null(input$file)) {return(NULL)}
     printcp(fit.rt()$model) # display the results
    # formula.mod()
  })
  
  
  #------------------------------------------------#
  output$summary = renderPrint({
    if (is.null(input$file)) {return(NULL)}
    
    summary(fit.rt()$model) # detailed summary of splits  
  })
  
  
  #------------------------------------------------#
  output$imp = renderPrint({
    if (is.null(input$file)) {return(NULL)}
    
    fit.rt()$imp
  })
  
  #------------------------------------------------#
  output$plot1 = renderPlot({
    
    if (is.null(input$file)) {return(NULL)}
    
    plotcp(fit.rt()$model) # visualize cross-validation results   
  })
  
  
  #------------------------------------------------#
  output$plot2 = renderPlot({
    if (is.null(input$file)) {return(NULL)}
    
    title1 = paste("Decision Nodes for", input$yAttr)
    
    fit.rt1 = fit.rt()$model     
    fit.rt1$frame$yval = as.numeric(rownames(fit.rt()$model$frame))    
    
    # create attractive postcript plot of tree 
    prp(fit.rt1,type=1,extra=1,under=TRUE,split.font=1,varlen=-10)
    
    # post(fit.rt1, 
    #      # file = "tree2.ps", 
    #      filename = "",   # will print to console
    #      use.n = FALSE,
    #      compress = TRUE,
    #      title = title1) 
    
  })
  
  output$plot3 = renderPlot({
    if (is.null(input$file)) {return(NULL)}
    
    title1 = paste("Decision Tree for", input$yAttr)
    
    prp(fit.rt()$model,type=1,extra=1,under=TRUE,split.font=1,varlen=-10)
    
  # post(fit.rt()$model, 
  #      # file = "tree2.ps", 
  #      filename = "",   # will print to console
  #      use.n = TRUE,
  #      compress = TRUE,
  #      title = title1) 
  })
  
  output$plot4 = renderPlot({
    if (is.null(input$file)) {return(NULL)}
    
    title1 = paste("Random forest for", input$yAttr)
    varImpPlot(fit.rf()$model,type = 1)
    
    # confusionMatrix(fit.rf$validation,test_data()[,c(input$yAttr)])
    
    
    # post(fit.rt()$model, 
    #      # file = "tree2.ps", 
    #      filename = "",   # will print to console
    #      use.n = TRUE,
    #      compress = TRUE,
    #      title = title1) 
  })
  
  
  #------------------------------------------------#
  nodes1 =  reactive({
    
  tree_nodes = as.data.frame(fit.rt()$model$where)
  colnames(tree_nodes) <- "node_number"
  # tree_nodes %>% head()
    
  a0 = as.numeric(rownames(fit.rt()$model$frame)); a0
  a1 = seq(1:nrow(fit.rt()$model$frame)); a1 
  a2 = as.vector(fit.rt()$model$where)
  node_num = a2
  for (i1 in 1:nrow(tree_nodes)){
    node_num[i1] = a0[a2[i1]]
  }
  
  tree_nodes1 <- fit.rt()$model$where %>% as.data.frame() %>% 
  cbind(node_num) %>% dplyr::select("node_num")
  tree_nodes1
  
  })

  # my edits below
  # output$nodesout = renderPrint({
  #  head(nodes1(),15)
  # })
  
 #  output$nodesout = renderTable({
 #   head(data.frame(nodes1(), train_data()), min(50, nrow(train_data())))     })

output$nodesout <- renderDataTable({  	
       data.frame(nodes1(), train_data())
	}, options = list(lengthMenu = c(10, 30, 50), pageLength = 100))  # my edits here
	  
  output$downloadData3 <- downloadHandler(
    filename = function() { "Nodes Info.csv" },
    content = function(file) {
      if (identical(Dataset(), '') || identical(Dataset(),data.frame())) return(NULL)
      dft = data.frame(nodes1(), train_data());   # data.frame(row_numer = row.names(nodes1()), nodes1())
      write.csv(dft, file, row.names=F, col.names=F)
    }
  )
  
  prediction = reactive({
    if (class(train_data()[,c(input$yAttr)]) == "factor"){
      
      fit.rt <- fit.rt()$model
      pr <- as.party(fit.rt)    # thus, we use same object 'rp' from the raprt package
      val = predict(pr, newdata = Dataset.Predict(),type="response")
      
    } 
    else {
      fit.rt <- fit.rt()$model
      pr <- as.party(fit.rt)    # thus, we use same object 'rp' from the raprt package
      val = predict(pr, newdata = Dataset.Predict())
      
    }
    
    out = data.frame(Yhat = val, pred.readdata())
    return(out)    
    
  })
  
  output$prediction =  renderPrint({
    if (is.null(input$filep)) {return(NULL)}
    head(prediction(),10)
  })
  
  #------------------------------------------------#
  output$downloadData1 <- downloadHandler(
    filename = function() { "Predicted Data.csv" },
    content = function(file) {
      if (identical(Dataset(), '') || identical(Dataset(),data.frame())) return(NULL)
      write.csv(prediction(), file, row.names=F, col.names=F)
    }
  )
  output$downloadData <- downloadHandler(
    filename = function() { "beer data.csv" },
    content = function(file) {
      write.csv(read.csv("data/beer data.csv"), file, row.names=F, col.names=F)
    }
  )
  
  output$downloadData2 <- downloadHandler(
    filename = function() { "beer data - prediction sample.csv" },
    content = function(file) {
      write.csv(read.csv("data/beer data - prediction sample.csv"), file, row.names=F, col.names=F)
    }
  )
  
  })
