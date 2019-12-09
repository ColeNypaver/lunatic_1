#####
#Bugger App
#####

#Initializing the required packages
library(shiny)
library(ggplot2)
library(purrr)
library(dplyr)
library(DT)

#Supplying example datan options
data(diamonds)
data(mtcars)
data(iris)

#This will let ggplot2 control the plotting (color, fill) based on type
values <- sapply(mtcars,function(x){length(unique(x))})
mtcars <- map_if(mtcars,values<4,as.factor) %>%
  as.data.frame()
diamonds <- as.data.frame(diamonds)

#Setting up the theme for ggplot
.theme <- theme(
  axis.line = element_line(colour = 'gray', size = 1),
  panel.background = element_blank(),
  plot.background = element_blank()
)

# UI for app
ui <- (fluidPage(
  
  # This is the title of our app
  headerPanel("The Bugger Data Visualization App"),
  
  #input
  sidebarPanel(
    # Panel discription for the Data Visualization tab
    conditionalPanel(condition = "$('li.active a').first().html()==='Data Visualization'",
                     h2("About this App:"),
                     p("This app can take your (or sample) data, displays a series of ggplot objects of your choosing, preform an ANOVA test, and construct linear model of your data."),
                     p("If you would like to test the app before using your own data, you can choose from two sample options: mtcars, iris & diamonds."),
                     p("You also have the option of selecting from three plot types: Boxplot, Histogram, Bar graph, and a Scatter plot"),
                     br()
    ),
  
    #Panel discription for the Plot tab
    conditionalPanel(condition = "$('li.active a').first().html()==='Plot'",
                     h2("Plots, plots, and more plots!!:"),
                     p("In this tab, you can see the output based on the plot you chose."),
                     br()
    ),
    
    #Panel discription for the Linear Model tab
    conditionalPanel(condition = "$('li.active a').first().html()==='Linear Model'",
                     h2("Linear Model:"),
                     p("Here, you can see a linear model output based on the variables you chose."),
                     br()
    ),
    #Panel discription for the ANOVA tab
    conditionalPanel(condition = "$('li.active a').first().html()==='ANOVA'",
                     h2("ANOVA:"),
                     p("You have the option to run a one-way or two-way ANOVA. To run a one-way ANOVA, make the same selection for both independent variables. For a two-way ANOVA, choose different values."),
                     br()
    ),
    
    # Input: Select a file ----
    fileInput("file1", "Choose CSV File",
              multiple = TRUE,
              accept = c("text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv")),
    # Input: Checkbox if file has header ----
    checkboxInput("header", "Header", TRUE),
    
    # Input: Select separator ----
    radioButtons("sep", "Separator",
                 choices = c(Semicolon = ";",
                             Comma = ",",
                             Tab = "\t",
                             Dash ="-"),
                 selected = ","),

    # Input: Select what to display
    selectInput("dataset","Select Data:",
                choices =list(diamonds = "diamonds", mtcars = "mtcars", iris = "iris",
                              uploaded_file = "inFile"), selected=NULL),
    selectInput("variable1","Dependent Variable:", choices = NULL),
    selectInput("variable2","Independent Variable 1:", choices = NULL),
    selectInput("variable3","Independent Variable 2:", choices = NULL),
    selectInput("plot.type","Plot Type:",
                list(boxplot = "boxplot", histogram = "histogram", bar = "bar", scatter = "scatter")
    ),
  ),
  
  # output
  mainPanel(
    tabsetPanel(
      tabPanel("Data Visualization", DTOutput('datavis')),
      tabPanel('Plot', plotOutput('plots')), # The name "Scatter Plot" is the name of the tab. 'scatter' is referenced to tell R where to display the output
      tabPanel('Linear Model', verbatimTextOutput('linear')),
      tabPanel('ANOVA', tableOutput('aovSummary'))
    ),
    uiOutput("plot") # depends on input
  )
))


# shiny server side code for each call
server <- (function(input, output, session){
  
  #update group and
  #variables based on the data
  observe({
    #browser()
    if(!exists(input$dataset)) return() #This checks to see if the uploaded data exists
    var.opts<-colnames(get(input$dataset))
    updateSelectInput(session, "variable1", choices = var.opts)
    updateSelectInput(session, "variable2", choices = var.opts)
    updateSelectInput(session, "variable3", choices = var.opts)
  })
  
  output$caption<-renderText({
    switch(input$plot.type,
           "boxplot" 	= 	"Boxplot",
           "histogram" =	"Histogram",
           "bar" 		=	"Bar graph",
           "scatter" = "Scatter Plot")
  })
  
  output$plot <- renderUI({
    plotOutput("p")
  })
  
  #get data object
  get_data <- reactive({
    
    if(!exists(input$dataset)) return() # if no upload
    
    check<-function(x){is.null(x) || x==""}
    if(check(input$dataset)) return()
    
    obj <- list(data=get(input$dataset),
              variable1=input$variable1,
              variable2=input$variable2,
              variable3=input$variable3
    )
    
    #require all to be set to proceed
    if(any(sapply(obj,check))) return()
    #make sure choices had a chance to update
    check <- function(obj){
      !all(c(obj$variable1, obj$variable2, obj$variable3) %in% colnames(obj$data))
    }
    
    if(check(obj)) return()
    obj
  })
  
  #printing data to first tab
  output$datavis <- renderDT({
    data.obj <- as.data.frame(get_data()) 
    datatable(data.obj)})
  
  #Plotting
  output$plots <- renderPlot({
    
    plot.obj <- get_data()
    
    #conditions for plotting
    if(is.null(plot.obj)) return()
    
    #make sure variable and group have loaded
    if(plot.obj$variable1 == "" | plot.obj$variable2 =="") return()
    
    #plot types
    plot.type <- switch(input$plot.type,
                      "boxplot" 	= geom_boxplot(),
                      "histogram" =	geom_histogram(alpha=0.5,position="identity"),
                      "bar" 		=	geom_bar(position="dodge",),
                      "scatter" = geom_point()
    )
    
    if(input$plot.type=="boxplot" || input$plot.type=="scatter")	{
      p <- ggplot(plot.obj$data,
                aes_string(
                  x 		= plot.obj$variable2,
                  y 		= plot.obj$variable1,
                  fill 	= plot.obj$variable2 # let type determine plotting
                )
      ) + plot.type
      
      {p <- p + geom_point(color='black',alpha=0.5, position = 'dodge')
      }
      
    } else {
      
      p <- ggplot(plot.obj$data,
                aes_string(
                  x 		= plot.obj$variable1,
                  fill 	= plot.obj$variable2,
                  variable2 	= plot.obj$variable2,
                  color 	= as.factor(plot.obj$variable2)
                )
      ) + plot.type
    }
    
    p <- p+labs(
      fill 	= input$variable2,
      x 		= input$variable2,
      y 		= input$variable1
    )  +
      .theme
    print(p)
  })
  
  #Linear model of data
  
  output$linear <- renderPrint({
    
    lm.obj <- get_data()
    
    fit <- lm(lm.obj$data[,input$variable1] ~ lm.obj$data[,input$variable2])
    names(fit$coefficients) <- c("Intercept", input$variable2)
    summary(fit)
    
  })
  
  # ANOVA
  output$aovSummary = renderTable({
    
  an.obj <- get_data()
  Variable1 <- an.obj$data[,input$variable1]
  Variable2 <- an.obj$data[,input$variable2]
  Variable3 <- an.obj$data[,input$variable3]
  rev.aov <- anova(lm(Variable1 ~ Variable2 + Variable3 + Variable2:Variable3))
  rev.aov
   }, rownames = TRUE, colnames = TRUE)
 
  # set uploaded file
  upload_data <- reactive({
    
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    #could also store in a reactiveValues
    read.csv(inFile$datapath,
             header = input$header,
             sep = input$sep)
  })
  
  observeEvent(input$file1,{
    inFile <<-upload_data()
  })
})

# Create Shiny app ----
shinyApp(ui, server)

