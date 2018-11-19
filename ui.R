###########################################################
#           Regression Tree App (ui)                #
###########################################################
library("shiny")
# library("randomForest")

shinyUI(
  fluidPage(
    
    titlePanel("Regression Tree"),
    
    sidebarLayout(
      
      sidebarPanel(
        # Upload data:
        h4(p(" Data Input")),
        fileInput("file", "Upload Model Training data in csv"),
        sliderInput('sample','Validation Sample Proportion',10,50,30),
        # h4(p("Select Response Variable")),
        sliderInput('cp','Complexity Parameter',0,1,0.01),
        fileInput("filep", "Upload Prediction data in csv"),
        htmlOutput("yvarselect"),
        htmlOutput("xvarselect"),
        htmlOutput("fxvarselect")
      ),   # end of sidebar panel
      
    mainPanel(
        tabsetPanel(type = "tabs",
                    tabPanel("Overview",
                             h4(p("How to use this shiny application")),
                             p("This shiny application require one data input from the user. To do so, click on the Browse (in left side-bar panel) and upload the csv data input file.
                           Note that this application can read only csv file (comma delimited file), so if you don't have csv input data file, first convert your data in csv format 
                           and then proceed. Make sure you have top row as variable names.",align="justify"),
                             p("Once csv file is uploaded successfully, variables in the data file will reflect in left-side Data Selection Panel. Now you can select 
                            dependent variable (Y Variable) from drop-down menu. By default all other remaining variables will be selected as explanatory variables (X variables). 
                              If you want to drop any variable from explanatory variables, just uncheck that variable and it will be dropped from the model.
                            ",align="justify"),
                             p('You can also adjust the complexity parameter in regression tree model. Default value of complexity parameter is "0.01". You can adjust the validation sample proportion from the slider in left sidebar panel. Validation sample will be selected from the input data set. If you have a similar data set on which you want to make the prediction based on regression tree, You can upload that data set in left side bar panel. Please note that prediction data should have all explanatory variables similar to model data.',align="justify"),
                             br(),
                             h4(p("Download Sample Input Files")),
                             # br(),
                             downloadButton('downloadData', 'Download model training input file (works only in browsers)'),
                             br(),
                             br(),
                             downloadButton('downloadData2', 'Download prediction input file (works only in browsers)'),
                             h5("Description of variables in sample file is as follows-"),
h5('week_num    =    week serial number'),
h5('sku_num    =    SKU serial number'),
h5('beer_brand    =    brand names of beers'),
h5('sku_size_oz    =    SKU size in fluid ounces'),
h5('bottle    =    whether bottle or can'),
h5('light_color    =    Whether Light colored beer'),
h5('amber_color    =    Whether Amber colored beer'),
h5('golden_color    =    Whether Golden colored beer'),
h5('lite_beer    =    Whether low calorie beer'),
h5('regular_beer    =    Whether regular beer'),
h5('ad_spend    =    ad spend in $000'),
h5('price_per_oz    =    Price in $ per oz'),
h5('beer_distbn    =    Weighted distribution'),
h5('promo    =    Weighted Promotional activities'),
h5('beer_sales_vol    =    volume sold in that week'),
h5('month    =    month number 1 to 12'),

                             br(),
                             p("Please note that download will not work with RStudio interface. Download will work only in web-browsers. So open this app in a web-browser and then download the example file. For opening this app in web-browser click on \"Open in Browser\" as shown below -"),
                             img(src = "example1.png") #, height = 280, width = 400
                             ),
                    tabPanel("Data Summary",verbatimTextOutput('summarydata')),
                    tabPanel("Model Output",
                             h4('Validation Result Summary'),
                             verbatimTextOutput("validation"),
                             h4('Model Result Summary'),
                             verbatimTextOutput("results"),
                             h4('Variable importance'),
                             verbatimTextOutput('imp'),
                            h4('Detailed summary of splits'),
                            verbatimTextOutput("summary")),
                    tabPanel("Decision Tree",
                             # h4('Visualize cross-validation results'),
                             # plotOutput("plot1",height = 600, width = 850),
                             # h4('Regression Tree'),
                             plotOutput("plot3",height = 600, width = 850)),
                    tabPanel("Random Forest",
                             # h4('Visualize cross-validation results'),
                             # plotOutput("plot1",height = 600, width = 850),
                             # h4('Regression Tree'),
                             plotOutput("plot4",height = 600, width = 850)),
                    
                    tabPanel("Node labels",
                             plotOutput("plot2",height = 600, width = 850),
                             h4("First column is assigned node number for each obsrvn in model training data"),
                             # verbatimTextOutput("nodesout"),
                             dataTableOutput("nodesout"),
                             br(),
                             h4("Download nodes data from model training data"),
                             br(),
                             downloadButton('downloadData3','Download nodes data (Works only in browser)')
                             ),
                    
                    # tabPanel("Random Forest",verbatimTextOutput('rfimp')),
                    tabPanel("Prediction",br(),
                             h4("First 10 rows of predicted data"),
                             p('"Yhat" column is the predicted value.'),
                             verbatimTextOutput('prediction'),
                             h4("Download Predicted data"),
                             downloadButton('downloadData1', 'Download Predicted data (Works only in browser)')
                             )
                             
        ) # end of tabsetPanel
      )# end of main panel
    ) # end of sidebarLayout
  )  # end if fluidPage
) # end of UI



