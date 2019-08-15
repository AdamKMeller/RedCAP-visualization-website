#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(REDCapR)
library(RCurl)
library(dplyr)
library(shinyWidgets)
library(car)

#counter9 <- 0 
#Communcate with REDCap
#uri='https://redcap.research.cchmc.org/api/'
uri='https://redcap.vanderbilt.edu/api/'
token = '8E66DB6844D58E990075AFB51658A002'
set <- redcap_read(redcap_uri=uri, token=token)$data

numericSet <- set[ , purrr::map_lgl(set, is.numeric)]

showII <- FALSE
colHeaders = colnames(set)
numColHeaders = colnames(numericSet)

#All the code to show the data #############################################################
#Set up set

setSet <- function(str1,str2)
{
  uri2 <- str1
  token2 <- str2
  
  setTEMP <- redcap_read(redcap_uri=uri2, token=token2)$data

  return(setTEMP)
}

#BarPlot
barPlotIt <- function(FOI,UseSet=set, title = NA) #Field of Intrest
{
  set2 <- UseSet %>%
    select(FOI)
  
  set2 <-filter(set2,!is.na(set2[1]))
  
  #set2 <- gsub(",", "", set2)   # remove comma
  set2 <- set2[,1]   # turn into numbers
  # generate bins based of set2 (histogramifies the dataset)
  set3 <- rep(0, max(set2,na.rm=TRUE)+1)
  for (x in set2)
  {
    for(y in c(0:max(set2,na.rm=TRUE)))
    {
      if(x==y)
        set3[y+1] <- set3[y+1] + 1 
    }
  }
  
  #For the names vector
  setNames1 <- rep(0:max(set2,na.rm=TRUE))
  hold = 0
  for(z in setNames1)
  {
    z = hold
    hold = hold +1
  }
  
  # draw the barplot with the specified number of values
  myBarPlot <- barplot(set3, names.arg = setNames1, col = 'blue', border = 'black', main = title)
  return(myBarPlot)
  
}

####Pie Plot
piePlotIt <- function(FOI,UseSet=set, title = NA) #Field of Intrest
{
  set2 <- UseSet %>%
    select(FOI)
  
  set2 <-filter(set2,!is.na(set2[1]))
  
  #set2 <- gsub(",", "", set2)   # remove comma
  set2 <- set2[,1]   # turn into numbers
  # generate bins based of set2 (histogramifies the dataset)
  set3 <- rep(0, max(set2,na.rm=TRUE)+1)
  for (x in set2)
  {
    for(y in c(0:max(set2,na.rm=TRUE)))
    {
      if(x==y)
        set3[y+1] <- set3[y+1] + 1 
    }
  }
  
  #For the names vector
  setNames1 <- rep(0:max(set2,na.rm=TRUE))
  hold = 0
  for(z in setNames1)
  {
    z = hold
    hold = hold +1
  }
  
  # draw the barplot with the specified number of values
  myPiePlot <- pie(set3, labels = setNames1, border = 'black', main = title)
  return(myPiePlot)
  
}


#Scatterplot

scatterPlotIt <- function(xA, yA, UseSet = set, myTitle = NA, ADV = FALSE )
{
  set2 <- UseSet %>%
    select(xA,yA)%>%
    filter(!is.na(xA) & !is.na(yA))
  
  set2 <- filter(set2, !is.na(set2[1]))
  set2 <- filter(set2, !is.na(set2[2]))
  
  
  #set2 <- gsub(",", "", set2)   # remove comma
  set3 <- set2[,1]   # turn into numbers
  set4 <- set2[,2] 
  if(ADV==FALSE)
    myScatterPlot <- plot(set3,set4, xlab = xA, ylab = yA, col = 'blue', pch = 19, main = myTitle)
  else
  {
    myScatterPlot<-plot(set3,set4, xlab = xA, ylab = yA, col = 'blue', pch = 19, main = myTitle)
    lines(lowess(set3,set4), col="blue")
    #abline(set3,set4, col="red")
  }
  
  return(myScatterPlot)
}

## Line Graph 
linePlotIt <- function(xA,yA,UseSet=set, title = NA) #Field of Intrest
{ 
  set2 <- UseSet %>%
    select(xA,yA)
  
  
  set2 <- filter(set2, !is.na(set2[1]))
  set2 <- filter(set2, !is.na(set2[2]))
  
  set3 <- set2[,1]   # turn into numbers
  set4 <- set2[,2] 
  if(isDate(set4[1])==TRUE)
  {
    
    
    set5 <- c(1:length(set4))
    for(g in c(1:length(set4)))
    {
      c <- strsplit(set4[g],"-")
      c <- as.data.frame(sapply(c, as.numeric))
      Fcounter <- as.integer(0)
      Fvalue <- 0
      for(s in c)
      {
        Fvalue <- (s[1]-1000)*365 + s[2]*30 +s[3]
      }
      set5[g] <- Fvalue
    }
    
    
    sortData <- data.frame(set3,set5)
    
    sortData<- filter(sortData, !is.na(sortData[2]))
    
    sortedData <- sortData[order(sortData[2]),]
    
    
    set3 <- sortedData[,1]
    set5 <- sortedData[,2]
    
    
    minTime <- min(set5)
    for(g in c(1:length(set5)))
    {
      #set3[g] <- round(set3[g])
      set5[g] <- set5[g]-minTime
    }
    
  }
  else
  {
    set5 <- set4
    sortData <- data.frame(set3,set5)
    
    sortData<- filter(sortData, !is.na(sortData[2]))
    
    sortedData <- sortData[order(sortData[2]),]
    
    
    set3 <- sortedData[,1]
    set5 <- sortedData[,2]
  }
  
  myLinePlot <- plot(set5,set3)
  lines(set5,set3,type = "o")
  return(myLinePlot)
}

#DateIdentifier
isDate <- function(foi)
{
  if(is.null(foi)==FALSE & is.array(foi)==FALSE)
  {
  if(!is.null(foi[1]) & length(foi) > 0 & nchar(foi[1])==10)
  {
    if(substr(foi,5,5)=='-' & substr(foi,8,8)=='-')
      return(TRUE)
    else
      return(FALSE)
  }
  else
    return(FALSE)
  }
  else 
    return(FALSE)
}


#Remove constrained 
rConstrained <- function(foi, workSet = set, bot, top)
{
  rSet <- workSet
  rSet <-  filter(rSet, !is.na(!!as.symbol(foi)) )
  rSet <-  filter(rSet, (!!as.symbol(foi)) >= bot)
  rSet <-  filter(rSet, (!!as.symbol(foi)) <= top)
  return(rSet)
}

####Descion Making Alg  for plot 1
plotOptions <- function(input1)
{
    myInput<- input1
    set2 <- set %>%
      select(myInput)
    if(typeof(set2[,1])=="integer")
    {
      if(!is.na(max(set2[,1],na.rm=TRUE)))
      {
        if(max(set2[,1],na.rm=TRUE)>0)
        {
          myPlotOptions <- c("bar plot", "pie chart", "histogram", "scatter plot", "line graph", "density plot")
        }
      }
    }
    else if(typeof(set2[,1])=="double")
    {
      if(!is.na(max(set2[,1],na.rm=TRUE)))
      {
        if(max(set2[,1],na.rm=TRUE)>0)
        {
          myPlotOptions <- c("histogram","scatter plot", "line graph", "density plot")
        }
      }
    }
    else if(isDate(set2[,1]))
    {
      myPlotOptions <- c("timeline")
    }
}


###########################################################################################
#testing <- c("a","b","c")
#Set the UI
    ui <- fluidPage(
      
      # Application title
      titlePanel("REDCap auto-Visualization project"),
      
      
        sidebarPanel(
          textInput("text1", label = h3("Controls"), value = "Enter domain"),
          textInput("text2", label = "", value = "Enter API token"),
          actionButton('apiS', label = 'Submit Token'),
          
      tags$div(id = 'placeholder'), 
      uiOutput("ui0"),
      uiOutput("ui"),
      uiOutput("ui2"),
      uiOutput("ui3"),
      uiOutput("ui4"),
      uiOutput("ui5"),
      uiOutput("ui6"),
      uiOutput("ui7"),
      uiOutput("ui8")
      
    
      
    
    ),
    
        
        # Show a plot of the generated distribution
        mainPanel(
          plotOutput("distPlot")
        )
      )
    


###############################################################################################
#Here is all the Rshiny code for the Server



#set the server
server <- function(input, output, session) {
  #set up set
  observeEvent(input$apiS,{
    set <<- setSet(input$text1,input$text2)
    numericSet <<- set[ , purrr::map_lgl(set, is.numeric)]
    colHeaders <<- colnames(set)
    showII <<- TRUE
    numColHeaders <<- colnames(numericSet)
    insertUI(
      selector = '#placeholder',
      where = "beforeBegin",
      ui = pickerInput(inputId = 'inputCol1',
                       label = 'First Variable ',
                       choices = colHeaders,
                       options = list(`style` = "btn-info"))
      
      )
    
    
  }
  )
  
  #observe({
    
    # Depending on input$input_type, we'll generate a different
    # UI component and send it to the client.
   # if(showII == TRUE)
   # {
   # updatePickerInput(session, inputId = 'inputCol2',
     #                                                 label = 'Graph Type',
     #                                                 choices = plotOptions(input$inputCol1),
     #                                                 options = list(`style` = "btn-warning")
     #                                                 )
      
   # }
    
  #})
           
  output$ui0 <- renderUI({
    if (is.null(input$inputCol1))
      return()
    
    pickerInput(inputId = 'inputCol2',
                label = 'Graph Type ',
                #choices = c("Pick a  valid input column"),
                choices = plotOptions(input$inputCol1),
                options = list(`style` = "btn-warning"))
    
  })
  
  output$ui <- renderUI({
    if (is.null(input$inputCol2))
      return()
    
    # Depending on input$input_type, we'll generate a different
    # UI component and send it to the client.
    switch(input$inputCol2,
           "histogram" = sliderInput(inputId = 'bins',
                                  label = "Number of bins for histograms:",
                                  value = 5,
                                  min = 1, max = 50),
           "scatter plot" = pickerInput(inputId = 'inputCol3',
                                        label = 'Crossed column ',
                                        choices = colHeaders,
                                        options = list(`style` = "btn-info")),
           "line graph" = pickerInput(inputId = 'inputCol3',
                                      label = 'Date column ',
                                      choices = colHeaders,
                                      options = list(`style` = "btn-info")),
           "timeline" = pickerInput(inputId = 'inputCol3',
                                      label = 'Value column ',
                                      choices = colHeaders,
                                      options = list(`style` = "btn-info"))
           )
  })
  
  
  output$ui2 <- renderUI({
    if (is.null(input$inputCol3))
      return()
    
    switch(input$inputCol2,
           "scatter plot" =  checkboxInput("spAdv", "Advanced", FALSE)
              )
  })
           
  output$ui3 <- renderUI({
    if (is.null(input$inputCol2))
      return()
    else
      checkboxInput("conId", "Add Constraint", FALSE)
    
  })        
  
  output$ui4 <- renderUI({
    if (is.null(input$conId))
      return()
    
    switch(input$conId,
           "TRUE" =  pickerInput(inputId = 'inputCol4',
                               label = 'Constraining Variable ',
                               choices = numColHeaders,
                               options = list(`style` = "btn-info")),
           "FALSE" = return()
           
    )
    
  })
  
  
  output$ui5 <- renderUI({
    if (is.null(input$inputCol4) || input$conId == FALSE)
      return()
    else
    {
    setUI <- set %>%
      select(input$inputCol4)
    
    setUI <- filter(setUI, !is.na(setUI[1]))
    vUI <- setUI[,1]
    
    sliderInput("range", "Range:",
                min = min(vUI), max = max(vUI),
                value = c(min(vUI),max(vUI)))
    }
    
  })
    
    
    output$ui6 <- renderUI({
      if (is.null(input$inputCol4) || input$conId == FALSE)
        return()
      else
    checkboxInput("conId2", "Add Second Constraint", FALSE)
    
  })
    
    
    
    output$ui7 <- renderUI({
      if (is.null(input$conId2))
        return()
      
      switch(input$conId2,
             "TRUE" =  pickerInput(inputId = 'inputCol5',
                                   label = 'Constraining Variable 2',
                                   choices = numColHeaders,
                                   options = list(`style` = "btn-info")),
             "FALSE" = return()
             
      )
      
    })
    
    
    output$ui8 <- renderUI({
      if (is.null(input$inputCol5) || input$conId2 == FALSE)
        return()
      else
      {
        setUI <- set %>%
          select(input$inputCol5)
        
        setUI <- filter(setUI, !is.na(setUI[1]))
        vUI <- setUI[,1]
        
        sliderInput("range2", "Range:",
                    min = min(vUI), max = max(vUI),
                    value = c(min(vUI),max(vUI)))
      }
      
    })
    
    
    
  
  output$distPlot <- renderPlot({
    
    if(input$conId == TRUE)
    {
      conSet <- rConstrained(input$inputCol4,workSet = set, as.numeric(input$range[1]),as.numeric(input$range[2]))
        if(input$conId2 == TRUE)
        {
          conSet <- rConstrained(input$inputCol5,workSet = conSet, as.numeric(input$range2[1]),as.numeric(input$range2[2]))
        }
   ##Now This 
    if (input$inputCol2 == "bar plot")
    {
        barPlotIt(input$inputCol1,UseSet = conSet, title=input$inputCol1)
    }
    if (input$inputCol2 == "pie chart")
    {
      piePlotIt(input$inputCol1,UseSet = conSet, title=input$inputCol1)
    }
    if (input$inputCol2 == "density plot")
    {
      UseSet <- conSet
      FOI <- input$inputCol1
      set2 <- UseSet %>%
        select(FOI)
      
      set2 <-filter(set2,!is.na(set2))
      
      #set2 <- gsub(",", "", set2)   # remove comma
      set2 <- set2[,1]   # turn into numbers
      myDP <- density(set2)
      plot(myDP)
    }
    if (input$inputCol2 == "histogram")
    {
      UseSet <- conSet
      FOI <- input$inputCol1
      set2 <- UseSet %>%
        select(FOI)
      
      set2 <-filter(set2,!is.na(set2))
      
      #set2 <- gsub(",", "", set2)   # remove comma
      set2 <- set2[,1]   # turn into numbers
      
      bins <- seq(min(set2), max(set2), length.out=input$bins +1)
      
      # draw the histogram with the specified number of bins
      hist(set2, breaks = bins, col = 'blue', border = 'black', main= input$inputCol1, xlab = FOI)
    }
    if (input$inputCol2 == "scatter plot")
    {
      if(input$spAdv==FALSE)
      {

          scatterPlotIt(input$inputCol1, input$inputCol3, UseSet = conSet, myTitle= paste(input$inputCol1, input$inputCol3, sep = " vs ", collapse = NULL))
      }
         else
        scatterPlotIt(input$inputCol1, input$inputCol3, UseSet = conSet, ADV=TRUE, myTitle= paste(input$inputCol1, input$inputCol3, sep = " vs ", collapse = NULL))
    }
    if (input$inputCol2 == "line graph")
    {
      linePlotIt(input$inputCol1, input$inputCol3, UseSet = conSet)
    }
    if (input$inputCol2 == "timeline")
    {
      linePlotIt(input$inputCol3, input$inputCol1, UseSet = conSet)
    }
    }
    
    
    
    
    
    else #This is if Useset is not Conset 
    {
      if (input$inputCol2 == "bar plot")
      {
        barPlotIt(input$inputCol1, title=input$inputCol1)
      }
      if (input$inputCol2 == "pie chart")
      {
        piePlotIt(input$inputCol1, title=input$inputCol1)
      }
      if (input$inputCol2 == "density plot")
      {
        UseSet <- set
        FOI <- input$inputCol1
        set2 <- UseSet %>%
          select(FOI)
        
        set2 <-filter(set2,!is.na(set2))
        
        #set2 <- gsub(",", "", set2)   # remove comma
        set2 <- set2[,1]   # turn into numbers
        myDP <- density(set2, main = FOI)
        plot(myDP)
      }
      if (input$inputCol2 == "histogram")
      {
        UseSet <- set
        FOI <- input$inputCol1
        set2 <- UseSet %>%
          select(FOI)
        
        set2 <-filter(set2,!is.na(set2))
        
        #set2 <- gsub(",", "", set2)   # remove comma
        set2 <- set2[,1]   # turn into numbers
        
        bins <- seq(min(set2), max(set2), length.out=input$bins +1)
        
        # draw the histogram with the specified number of bins
        hist(set2, breaks = bins, col = 'blue', border = 'black', main=FOI, xlab = FOI)
      }
      if (input$inputCol2 == "scatter plot")
      {
        if(input$spAdv==FALSE)
        {
          
          scatterPlotIt(input$inputCol1, input$inputCol3, myTitle= paste(input$inputCol1, input$inputCol3, sep = " vs ", collapse = NULL))
        }
        else
          scatterPlotIt(input$inputCol1, input$inputCol3, ADV=TRUE, myTitle= paste(input$inputCol1, input$inputCol3, sep = " vs ", collapse = NULL))
      }
      if (input$inputCol2 == "line graph")
      {
        linePlotIt(input$inputCol1, input$inputCol3)
      }
      if (input$inputCol2 == "timeline")
      {
        linePlotIt(input$inputCol3, input$inputCol1)
      }
      

    }
    
    })
  

}
# Run the application 
shinyApp(ui = ui, server = server)
