########################################################
#   qplanr - Shiny App 
#   Author: Dean Marchiori
#######################################################


# Notes -------------------------------------------------------------------
#' This is a shiny app to launch the qplanr functions. qplanr is a call centre
#' queue resourcing algorithm. It calcualtes the number agents required to 
#' staff an MMC queue model using the Erlang-C formula.


# Environment Setup -------------------------------------------------------
library(shiny)
library(ggplot2)
library(ggthemes)


# Required Functions ------------------------------------------------------

# MAIN FUNCTION FOR ERLANG
agents_required <- function(demandqty, serviceduration, targetservetime, targetservicelevel) {
  
  # Declare Variables
  servicelevel <- 0L
  s            <- NULL
  arrivalrate  <- demandqty/ 1800
  intensity    <- arrivalrate*serviceduration
  numagents    <- ceiling(intensity)
  
  # Iterate over number of agents until service level above target
  while(servicelevel < targetservicelevel){
    for (i in seq(numagents)) {
      s[i]<- (intensity^(i-1))/factorial(i-1)
    }
    occupancy <-intensity/numagents
    ErlangC <-(intensity^numagents)/factorial(numagents)/(((intensity^numagents)/factorial(numagents))+(1-occupancy)*sum(s))
    servicelevel <- 1- (ErlangC * exp(-(numagents-intensity)*(targetservetime/serviceduration)))
    numagents <- numagents+1
  }
  return(
    c(
      (numagents-1), # Number of Agents
      servicelevel,  # GOS
      occupancy,     # Occupancy
      ErlangC*100,   # Prob wait
      ((ErlangC * serviceduration) / (numagents * (1 - occupancy)))  # ASA
    )
  ) 
}

# SUPPLIMENTARY FUNCTION TO CALCULATE SERVICE LEVEL VECTOR FOR CHART
service_levels <- function(demandqty,numagents, serviceduration, targetservetime) {
  # Declare Variables
  s            <-NULL
  arrivalrate  <-demandqty/ 1800
  intensity    <-arrivalrate*serviceduration
  
  for (i in seq(numagents)) {
    s[i]<- (intensity^(i-1))/factorial(i-1)
  }
  occupancy <-intensity/numagents
  ErlangC<-(intensity^numagents)/factorial(numagents)/(((intensity^numagents)/factorial(numagents))+(1-occupancy)*sum(s))
  servicelevel <- 1- (ErlangC * exp(-(numagents-intensity)*(targetservetime/serviceduration)))
}


# Shiny UI ----------------------------------------------------------------

ui <- fluidPage(
  navbarPage("QplanR",
             tabPanel("Calculator",
                      titlePanel("Call Centre Resourcing Tool"),
                      sidebarPanel(
                        sliderInput("GOS",
                                    label = h4("Target Service Level (GOS)"),
                                    min = 20,
                                    max= 99,
                                    value = 80),
                        numericInput("NumCalls",
                                     label = h5("Number of Calls per half hour"),
                                     value = 15),
                        numericInput("CallDur",
                                     label = h5("Ave Call Duration (sec)"),
                                     value = 300),
                        numericInput("ASA",
                                     label = h5("Target Speed of Answer (sec)"),
                                     value = 60),
                        p("")
                      ),
                      mainPanel(
                        h4(textOutput("statement")),
                        br(),
                        h4("Results"),
                        tableOutput("results"),
                        plotOutput("chart")
                      )
             ),
             tabPanel("About", 
                      h4("About this App"),
                      p("The QplanR tool will calculate the number of call centre agents required to staff a call centre queue using the Erlang-C traffic formula."),
                      h4("Assumptions"),
                      p("The Erlang C traffic model is an analytical model for estimating the performance of telecommunications systems which incorporate queueing.  Queuing applications include switchboard operators and ACD Call Centre agents. The model makes the following assumptions:"),
                      p("- Calls are offered randomly and follow a Poisson arrival distribution"), 
                      p("- Users wait if they find the system busy and do not leave the queue"),
                      p("- Service times follow a negative exponential distribution"),
                      p("- Users are served in the order of arrival"),
                      p("- Users are directed to the first available server (agent)"),
                      p("- Queue sizes are unlimited."),
                      h4("Inputs"),
                      p("- Target GOS: The desired minimum acceptable service level: % calls answered within target answer time"),
                      p("- Number of Calls per half hour: The expected number of inbound calls to the call centre"),
                      p("- Ave Call Duration (sec): The average handling time of the inbound calls"),
                      p("- Target Speed of Answer (sec): The target answer time for all calls"),
                      h4("Outputs"),
                      p("The calculator will provide the number of agents required to keep service levels within target based on the user inputs and the assumptions above."),
                      p("Using the modelled number of agents, key indicators are estimated such as probability of calls queueing and ASA. A chart is also produced to show the imapct on GOS for over/under resourcing the queue."),
                      br(),
                      h4("Author and Maintainer:"),
                      p("Dean Marchiori"),
                      p("https://github.com/deanmarchiori/qplanr")
             )
  )
)


# Shiny Server ------------------------------------------------------------

server <- function(input, output){
  
  # Reactive- calculating agents required
  agents <- reactive({
    agents_required(demandqty = input$NumCalls, 
                    serviceduration = input$CallDur,
                    targetservetime =  input$ASA, 
                    targetservicelevel =  input$GOS/100)
  })
  
  # Reactive - calculating service level vector for chart
  servicelev <- reactive({
    agentvec <- seq(from = agents()[1] -3, to = agents()[1] +3)
    agentvec[agentvec <=0] <- 1
    sapply(X = agentvec, 
           FUN = function(x) service_levels(demandqty = input$NumCalls, 
                                            serviceduration = input$CallDur, 
                                            targetservetime = input$ASA, 
                                            numagents = x))
  })
  
  # Output chunks
  output$statement <- renderText({
    paste("With", input$NumCalls, " calls being recieved in a half-hour interval,", agents()[1] ," agents will be required to acheive", sprintf("%.0f%%", input$GOS), "GOS target.")
  })
  
  output$results <- renderTable({ 
    data.frame(
      Item = c("Agents Required", 
               "Estimated GOS", 
               "Agent Occupancy", 
               "Probability of being queued", 
               "Average Wait Time"),
      Stat = c(sprintf("%.0f", agents()[1]),
               sprintf("%.0f%%", agents()[2]*100),
               sprintf("%.0f%%", agents()[3]*100),
               sprintf("%.0f%%", agents()[4]),
               sprintf("%.0f sec", agents()[5]))
    )}, colnames = FALSE)
  
  # Output Chart
  output$chart <- renderPlot({
    # Adjusting some inputs for dataframe for ggplot
    agentvec <- seq(from = agents()[1] -3, to = agents()[1] +3)
    agentvec[agentvec <=0] <- 1
    servicelevel_vec <- servicelev()
    servicelevel_vec[servicelevel_vec < 0] <- 0
    servicelevel_vec <- servicelevel_vec*100
    
    x <- data.frame(cbind(agentvec,servicelevel_vec * 100))
    
    ggplot(x, aes(agentvec,servicelevel_vec)) + 
      geom_line(size = 0.8, alpha = 0.6, colour = "blue") + 
      labs(title = "Grade of Service curve", 
           subtitle = "This shows the impact on service levels if we over/under staff the queue",
           x = "Number of Agents in the queue", 
           y = "GOS (%)" )+
      geom_vline(xintercept = agentvec[4], linetype="dotted", size = 1) +
      theme(legend.position = "none") +
      scale_x_continuous(breaks = agentvec) +
      scale_y_continuous(breaks = seq(from = 0, to = 100, by = 10))
  }, width = 800)
  
}


# Run ---------------------------------------------------------------------

shinyApp(ui, server)



