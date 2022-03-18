library(shiny)
library(maps)
library(leaflet)
library(ggplot2)
library(tidyverse)
library(scales)
#Load data stuff
campus <- read.csv("data/campus_mapping.csv")
vote <- read.csv("data/vote_data.csv")
cost_living <- read.csv("data/cost_living.csv")
Salary <- c("12 Month", "9 Month^")
Amount <- c(2294.777778 * 12, 2294.777778 * 9)
cost_sals <- data.frame(Salary, Amount)
#Global functions
vote_chart <- function(Data, title = "Across all UC Campuses") {
  pie <-
    ggplot(Data, aes(x = '', y = Percent, fill = Vote)) + geom_bar(width = 1, stat =
                                                                     "identity") + coord_polar("y", start = 0) + theme_void() + geom_text(data =
                                                                                                                                            Data,
                                                                                                                                          aes(
                                                                                                                                            y = lab.ypos,
                                                                                                                                            label = percent(Percent),
                                                                                                                                            x = 1.7
                                                                                                                                          ),
                                                                                                                                          size = 6) + ggtitle(title) + theme(
                                                                                                                                            legend.position = c(0.95, 0.85),
                                                                                                                                            text = element_text(size = 20),
                                                                                                                                            plot.title = element_text(hjust = 0.5)
                                                                                                                                          ) + geom_text(
                                                                                                                                            label = paste("Number of Votes = ", sum(Data$Vote.Count)),
                                                                                                                                            y = 0,
                                                                                                                                            x = -.8,
                                                                                                                                            size = 6
                                                                                                                                          )
  return(pie)
}
expy <- function(x,
                 n,
                 percent,
                 mult = 1,
                 add = 0,percent2=0) {
  return(mult * ((n * exp((
    x - 2018
  ) * percent)) + (add*exp((x-2018)*percent2))))
}

bar_graph <- function(Data, status, Data2) {
  graph <-
    ggplot(Data, aes(x = Campus, y = Cost, fill = Expense)) + geom_bar(width =
                                                                         0.8,
                                                                       stat = "identity",
                                                                       alpha = 0.9) + coord_flip() + theme_classic() + ggtitle(paste("Living Expenses Across all UC campuses for ", status, sep =
                                                                                                                                       "")) + theme(text = element_text(size = 20),
                                                                                                                                                    plot.title = element_text(hjust = 0.5)) + geom_hline(
                                                                                                                                                      color = "black",
                                                                                                                                                      size = 2,
                                                                                                                                                      aes(linetype = Salary, yintercept = Amount),
                                                                                                                                                      data = Data2
                                                                                                                                                    ) + scale_y_continuous(label = dollar_format()) + ylab("Yearly Expenses") +
    scale_linetype_manual(values = "dashed")
  return(graph)
}

graph_project <- function(funcs, housing) {
  #parms<-list=(n,percent,mult,add)
  #sharedhousing<-list(1017,min=3.09,av=6.38,max=9.811)
  #studio<-list(1428,min=1.108,av=9.9997,max=20.404)
  #1br<-list(2079,min=7.337,av=12.483,max=22.510)
  #2br<-list(2977,min=2.06,av=8.264,max=11.33)
  p <-
    ggplot(data = data.frame(x = 0), mapping = aes(x = x)) + theme_classic() +
    xlab("Years") + ylab("Projected Rent or Salary") + theme(text = element_text(size =
                                                                                   25)) + labs(color = "") + scale_y_continuous(label = dollar_format()) +
    ggtitle(paste(
      "Projections of Monthly Rent for a ",
      housing,
      " and TA Salary",
      sep = ""
    )) + scale_x_continuous(breaks = seq(2018, 2038, 5),
                            limits = c(2018, 2038))
  if (length(funcs) == 2) {
    p <-
      p + stat_function(
        fun = expy,
        args = (funcs$Rent),
        mapping = aes(color = "Rent"),
        size = 1.5
      ) + stat_function(
        fun = expy,
        args = (funcs[[2]]),
        mapping = aes(color = names(funcs)[2]),
        size = 1.5
      )
    
  } else if (length(funcs) == 3) {
    p <-
      p + stat_function(
        fun = expy,
        args = (funcs$Rent),
        mapping = aes(color = "Rent"),
        size = 1.5
      ) + stat_function(
        fun = expy,
        args = (funcs[[2]]),
        mapping = aes(color = names(funcs)[2]),
        size = 1.5
      ) + stat_function(
        fun = expy,
        args = (funcs[[3]]),
        mapping = aes(color = names(funcs)[3]),
        size = 1.5
      )
    
  } else if (length(funcs) == 4) {
    p <-
      p + stat_function(
        fun = expy,
        args = (funcs$Rent),
        mapping = aes(color = "Rent"),
        size = 1.5
      ) + stat_function(
        fun = expy,
        args = (funcs[[2]]),
        mapping = aes(color = names(funcs)[2]),
        size = 1.5
      ) + stat_function(
        fun = expy,
        args = (funcs[[3]]),
        mapping = aes(color = names(funcs)[3]),
        size = 1.5
      ) + stat_function(
        fun = expy,
        args = (funcs[[4]]),
        mapping = aes(color = names(funcs)[4]),
        size = 1.5
      )
    
  } else if (length(funcs) == 5) {
    p <-
      p + stat_function(
        fun = expy,
        args = (funcs$Rent),
        mapping = aes(color = "Rent"),
        size = 1.5
      ) + stat_function(
        fun = expy,
        args = (funcs[[2]]),
        mapping = aes(color = names(funcs)[2]),
        size = 1.5
      ) + stat_function(
        fun = expy,
        args = (funcs[[3]]),
        mapping = aes(color = names(funcs)[3]),
        size = 1.5
      ) + stat_function(
        fun = expy,
        args = (funcs[[4]]),
        mapping = aes(color = names(funcs)[4]),
        size = 1.5
      ) + stat_function(
        fun = expy,
        args = (funcs[[5]]),
        mapping = aes(color = names(funcs)[5]),
        size = 1.5
      )
  } else{
    p <-
      p + stat_function(
        fun = expy,
        args = (funcs$Rent),
        mapping = aes(color = "Rent"),
        size = 1.5
      )
  }
  
  return(p)
}


# User interface ----
ui <- fluidPage(
  HTML('<meta name="viewport" content="width=1024">'),
  navbarPage("",tabPanel("Homepage",
  titlePanel(
    h1("UCSC Graduate Students are on Strike!", align = "center")
  ),sidebarPanel( br(),
                 imageOutput("logo"),
                 br(),
                 br(),
                 br(),
                 br(),
                 br(),
                 br(),
                 p("For more information on the strike go to the official ",a("website.", href = "https://payusmoreucsc.com/")),br(),br(),p("If you have any questions or suggestions (i.e. bugs, typos, other data to include, etc.) let me know by shooting me an email: thatmewartsuk(AT)gmail(DOT)com")),mainPanel(fluidRow(h2("Background:"),br(),p("University of California, Santa Cruz graduate students are on a wildcat grading strike for a cost of living adjustment (COLA). This web app is meant to provide some of the data explaining the context of why this strike is happening. You can explore some of the data sets using the tabs above. Below is a short description of each data set."),br(),h4("2018 Contract Voting:"),br(),p("The current TA contract was negotiated by the UAW2865 and was ratified by a UC wide vote in Fall 2018. However, when looking at specific UC campuses, UC Santa Cruz voted strongly against the current contract."),br(),h4("Cost of Living:"),br(),p("The current TA contract applies to graduate students at all UC's depsite different costs of living between different UC's. This data set allows you to explore how cost of living varies across the different campuses."),br(),h4("Trends of Rent Costs in Santa Cruz:"),br(),p("Cost of rent in Santa Cruz is rising. These graphs allow you to visualize how both rent and TA salary have increased over the past 4 years."),br(),h4("Rent Projections Santa Cruz:"),br(),p("You can explore how the guranteed 3% yearly TA salary increase compares to various rent projections over the next 20 years with or without the requested COLA of $1412 per month.")))),
  tabPanel("2018 Contract Voting",titlePanel(h1("2018 UC TA Contract Vote Results", align = "center")),
           sidebarPanel(actionButton("Total", "Results Across All Campuses")),
           mainPanel(fluidRow(splitLayout(cellWidths = c("40%", "60%"),leafletOutput("map"),plotOutput("plot"))))),
  tabPanel("Trends of Rent Costs in Santa Cruz", titlePanel(h1("Trends of Rent Increase in Santa Cruz 2014 - 2018", align = "center")),
           sidebarPanel(fluidRow(radioButtons("rentdisp",h3("Data to Display:"),choices = list("Monthly Trends" = "Rent+Historic_salary.png", "Trends in Percent" = "Percent_change_rent_salary.png","Change in Rent Burden Over the Past 4 Years"="Change_rent_burden.png"),selected = "Rent+Historic_salary.png"))),
           mainPanel(imageOutput("image"))),
  tabPanel("Cost of Living",titlePanel(h1("Graduate Student Cost of Living Across All UC's (UCOP Survey 2017)",align = "center")),
           sidebarPanel(fluidRow(radioButtons("Status",h3("Graduate Student Family Status:"),choices = list("Single" = "Single Students","Has a Partner" = "Students with a Partner","Has Dependents" = "Students with Dependents"),selected = "Single Students"),radioButtons("Salary",h3("Salary:"),choices = list("12 Month" = "12 Month", "9 Month^" = "9 Month^"),selected = "12 Month"))),
           mainPanel(plotOutput("cost_plot"),verbatimTextOutput("info_cost"))),
  tabPanel("Rent Projections Santa Cruz",
           sidebarPanel(fluidRow(sliderInput("rate",h3("Percent Rate of Rent Increase:"),min = 0,max = 24,value = 6.8,step = 0.1),radioButtons("Sal_Month",h3("Funding Situation (Many graduate students only recieve 9 months of funding):"),choices=list("9 Month"="9","12 Month"="12"),selected="9"),checkboxGroupInput("lines",h3("Proportion of salary to be displayed:"),choices = list("30% (Rent Burden)" = 0.3,"50% (Severe Rent Burden)" = 0.5,"75%" = 0.75,"Full" = 1),selected = 0.3),sliderInput("COLA",h3("Starting COLA (cost of living adjustment; $1412 per month is the proposed value):"),min = 0,max = 1412,value = 0,step = 1),radioButtons("COLA_p",h3("Percent Rate of COLA Increase:"),choices=list("None"=0,"3% (same as current salary contract)"=0.03,"With the Rate of Rent Increase"="rent"),selected=0),radioButtons("housing",h3("Housing data to graph"),choices = list("Room in Shared House" = "Room in House","Studio" = "Studio","1 Bed Room Apt" = "1 Bed Room Apt","1 Bed Room House" = "1 Bed Room House","2 Bed Room Apt" = "2 Bed Room Apt","2 Bed Room House" = "2 Bed Room House"),selected = "Room in House"))),
           mainPanel(plotOutput("fun_plot"),verbatimTextOutput("info")))))


# Server logic ----
server <- function(input, output) {
  #California map
  cal_map <-
    leaflet() %>% addTiles() %>% addMarkers(
      data = campus,
      lng =  ~ long,
      lat =  ~ lat,
      label =  ~ Campus,
      layerId = ~ Campus
    )
  flag <- reactiveValues(clickedMarker = NULL)
  observeEvent(input$map_marker_click, {
    flag$clickedMarker <- 1
  })
  data_set <- reactive({
    cam <- substring(input$map_marker_click$id, 4)
    vote[vote$Campus == cam, ]
  })
  observeEvent(input$Total, {
    flag$clickedMarker <- NULL
  })
  #plot for 2018 votes
  output$plot <- renderPlot({
    if (is.null(flag$clickedMarker)) {
      vote_chart(vote[vote$Campus == "Total", ])
    } else{
      vote_chart(data_set(), paste("Poll Results from UC", unique(data_set()$Campus)))
    }
  })
  #Map plots
  output$map <- renderLeaflet({
    cal_map
  })
  #logo
  output$logo <- renderImage({
    file_l <- normalizePath(file.path('./data', "Logo.jpg"))
    list(src = file_l,
         align = "center",
         width = "100%")
  }, deleteFile = FALSE)
  #Rent Increase
  output$image <- renderImage({
    file <- normalizePath(file.path('./data', input$rentdisp))
    list(
      src = file,
      align = "center",
      width = 907.706,
      height = 522.0703
    )
  }, deleteFile = FALSE)
  
  #working with other shit
  #sharedhousing<-list(1017,min=3.09,av=6.38,max=9.811)
  #studio<-list(1428,min=1.108,av=9.9997,max=20.404)
  #1brapt<-list(2079,min=7.337,av=12.483,max=22.510)
  #1brhouse<-list(1995,min=-2.927,av=7.608,max=14.392)
  #2brapt<-list(2977,min=2.06,av=8.264,max=11.33)
  #2brhouse<-list(2775,min=3.559,av=9.927,max=13.54)
  #example<-list("Rent"=list(n=1017,percent=0.06),"30% Salary"=list(n=2363.667,percent=0.03,mult=0.3),"Full Salary"=list(n=2363.667,percent=0.03),"50% Salary"=list(n=2363.667,percent=0.03,mult=0.5),"75% Salary"=list(n=2363.667,percent=0.03,mult=0.75))
  parm_list <- reactive({
    if (input$housing == "Room in House") {
      init = 1017
    } else if (input$housing == "Studio") {
      init = 1428
    } else if (input$housing == "1 Bed Room Apt") {
      init = 2079
    } else if (input$housing == "2 Bed Room Apt") {
      init = 2977
    } else if (input$housing == "1 Bed Room House") {
      init = 1995
    } else if (input$housing == "2 Bed Room House") {
      init = 2775
    }
    parms <- list("Rent" = list(n = init, percent = input$rate / 100))
    col = input$COLA
    #now figure out what salarys
    for (i in input$lines) {
      if (i == "0.3") {
        name = "30% Salary"
      } else if (i == "0.5") {
        name = "50% Salary"
      } else if (i == "0.75") {
        name = "75% Salary"
      } else if (i == "1") {
        name = "Full Salary"
      }
      parms[[name]] <-
        list(
          n = ifelse(input$Sal_Month=="9", (2363.667*9)/12,2363.667),
          percent = 0.03,
          percent2=ifelse(input$COLA_p=="rent",input$rate/100,as.numeric(input$COLA_p)),
          add = col,
          mult = as.numeric(i)
        )
    }
    return(parms)
  })
  information <- reactive({
    if (input$housing == "Room in House") {
      statement = "NOTE: Salary is before taxes.\n\nFrom 2014-2018 the rent for a room in shared house increased on average by 6.4% each year.\n\nThe smallest increase was 3.1% and the largest was 9.8%.\n\nData is from https://communityrentals.ucsc.edu/cost/5-year.html"
    } else if (input$housing == "Studio") {
      statement = "NOTE: Salary is before taxes.\n\nFrom 2014-2018 the rent for a studio increased on average by 10% each year.\n\nThe smallest increase was 1.1% and the largest was 20.4%.\n\nData is from https://communityrentals.ucsc.edu/cost/5-year.html"
    } else if (input$housing == "1 Bed Room Apt") {
      statement = "NOTE: Salary is before taxes.\n\nFrom 2014-2018 the rent for a 1 Bed Room Apartment increased on average by 12.5% each year.\n\nThe smallest increase was 7.3% and the largest was 22.5%.\n\nData is from https://communityrentals.ucsc.edu/cost/5-year.html"
    } else if (input$housing == "2 Bed Room Apt") {
      statement = "NOTE: Salary is before taxes.\n\nFrom 2014-2018 the rent for a 2 Bed Room Apartment increased on average by 8.3% each year.\n\nThe smallest increase was 2.1% and the largest was 11.3%.\n\nData is from https://communityrentals.ucsc.edu/cost/5-year.html"
    } else if (input$housing == "1 Bed Room House") {
      statement = "NOTE: Salary is before taxes.\n\nFrom 2014-2018 the rent for a 1 Bed Room House increased on average by 7.6% each year.\n\nThe smallest change was a 3% decrease and the largest change was a 14.4% increase.\n\nData is from https://communityrentals.ucsc.edu/cost/5-year.html"
    } else if (input$housing == "2 Bed Room House") {
      statement = "NOTE: Salary is before taxes.\n\nFrom 2014-2018 the rent for a 2 Bed Room House increased on average by 9.9% each year.\n\nThe smallest increase was 3.6% and the largest was 13.5%.\n\nData is from https://communityrentals.ucsc.edu/cost/5-year.html"
    }
    return(statement)
  })
  output$fun_plot <-
    renderPlot({
      graph_project(parm_list(), input$housing)
    })
  output$info <- renderText({
    information()
  })
  
  #Cost of living
  output$info_cost <-
    renderText(
      "NOTE: Salary is before taxes\n\n*Personal Expenses include: health costs not covered by insurance, recreation, and other expenses.\n\n**Transportation expenses include: car insurance, public transport, gas, and regular vehicle maintenance.\n\n***Utilities include: housing utilities, phone, and internet bills.\n\n^Some Graduate students are only payed for 9 months out of the year.\n\nNOTE: Costs do not include child care. On average across UC campuses monthly child care was $580 for one dependent.\n\nData is from a survey conducted by University of California in 2017, that had a sample size of 35% of the total graduate student population.\n\nLink to data set: https://www.ucop.edu/student-affairs/_files/GCOAS%20Report%202017.pdf (Table 12)"
    )
  output$cost_plot <-
    renderPlot({
      bar_graph(cost_living[cost_living$Status == input$Status, ], input$Status, cost_sals[cost_sals$Salary ==
                                                                                             input$Salary, ])
    })
}

# Run app ----
shinyApp(ui, server)
#substring(hi,4)
