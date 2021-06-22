library(shiny)
library(tidyverse)
library(ggforce)
library(shinyjs)


# Setting up colour values
NHL_red <- "#FFCCD8" # Use #C8102E for original red in the rules, #FFE6EB for lighter hue
NHL_blue <- "#CCE1FF" # Use #0033A0 for original blue in the rules, #E6EFFF for lighter hue
NHL_light_blue <- "#CCF5FF" # Use #41B6E6 for original crease blue in the rules, #E6F9FF for lighter hue

nhl_rink_plot <- function () {
  
  # Plotting an NHL rink completely following the NHL rule book:
  # https://cms.nhl.bamgrid.com/images/assets/binary/308893668/binary-file/file.pdf
  # Line widths, lengths, colours, all followed as closely as possible
  
  rin <- ggplot() +
    
    # Faceoff circles
    geom_circle(aes(x0 = 0, y0 = 0, r = 15), colour = NHL_blue, size = 2 / 12) + # Centre
    geom_circle(aes(x0 = 69, y0 = 22, r = 15), colour = NHL_red, size = 2 / 12) + # Top-Right
    geom_circle(aes(x0 = 69, y0 = -22, r = 15), colour = NHL_red, size = 2 / 12) + # Bottom-Right
    geom_circle(aes(x0 = -69, y0 = 22, r = 15), colour = NHL_red, size = 2 / 12) + # Top-Left
    geom_circle(aes(x0 = -69, y0 = -22, r = 15), colour = NHL_red, size = 2 / 12) + # Bottom-Left
    
    # Hash marks in T-R/B-R/T-L/B-R order, groups of four
    geom_tile(aes(x = 66.125, y = 37.77, width = 2 / 12, height = 2), fill = NHL_red) +
    geom_tile(aes(x = 66.125, y = 6.23, width = 2 / 12, height = 2), fill = NHL_red) +
    geom_tile(aes(x = 71.875, y = 37.77, width = 2 / 12, height = 2), fill = NHL_red) +
    geom_tile(aes(x = 71.875, y = 6.23, width = 2 / 12, height = 2), fill = NHL_red) +
    geom_tile(aes(x = 66.125, y = -37.77, width = 2 / 12, height = 2), fill = NHL_red) +
    geom_tile(aes(x = 66.125, y = -6.23, width = 2 / 12, height = 2), fill = NHL_red) +
    geom_tile(aes(x = 71.875, y = -37.77, width = 2 / 12, height = 2), fill = NHL_red) +
    geom_tile(aes(x = 71.875, y = -6.23, width = 2 / 12, height = 2), fill = NHL_red) +
    geom_tile(aes(x = -66.125, y = 37.77, width = 2 / 12, height = 2), fill = NHL_red) +
    geom_tile(aes(x = -66.125, y = 6.23, width = 2 / 12, height = 2), fill = NHL_red) +
    geom_tile(aes(x = -71.875, y = 37.77, width = 2 / 12, height = 2), fill = NHL_red) +
    geom_tile(aes(x = -71.875, y = 6.23, width = 2 / 12, height = 2), fill = NHL_red) +
    geom_tile(aes(x = -66.125, y = -37.77, width = 2 / 12, height = 2), fill = NHL_red) +
    geom_tile(aes(x = -66.125, y = -6.23, width = 2 / 12, height = 2), fill = NHL_red) +
    geom_tile(aes(x = -71.875, y = -37.77, width = 2 / 12, height = 2), fill = NHL_red) +
    geom_tile(aes(x = -71.875, y = -6.23, width = 2 / 12, height = 2), fill = NHL_red) +
    
    # Centre line
    geom_tile(aes(x = 0, y = 0, width = 1, height = 85), fill = NHL_red) + # Centre line
    
    # Faceoff dots - Plot AFTER centre lines for centre ice circle to show up above
    geom_circle(aes(x0 = 0, y0 = 0, r = 6 / 12), colour = "#FF99B4", fill = "#FF99B4", size = 0) + # Centre dot with unique red
    geom_circle(aes(x0 = 69, y0 = 22, r = 1), colour = NHL_red, fill = NHL_red, size = 0) + # Top-Right
    geom_circle(aes(x0 = 69, y0 = -22, r = 1), colour = NHL_red, fill = NHL_red, size = 0) + # Bottom-Right
    geom_circle(aes(x0 = -69, y0 = 22, r = 1), colour = NHL_red, fill = NHL_red, size = 0) + # Top-Left
    geom_circle(aes(x0 = -69, y0 = -22, r = 1), colour = NHL_red, fill = NHL_red, size = 0) + # Bottom-Left
    
    geom_circle(aes(x0 = 20.5, y0 = 22, r = 1), colour = NHL_red, fill = NHL_red, size = 0) + # Neutral Top-Right
    geom_circle(aes(x0 = 20.5, y0 = -22, r = 1), colour = NHL_red, fill = NHL_red, size = 0) + # Neutral Bottom-Right
    geom_circle(aes(x0 = -20.5, y0 = 22, r = 1), colour = NHL_red, fill = NHL_red, size = 0) + # Neutral Top-Left
    geom_circle(aes(x0 = -20.5, y0 = -22, r = 1), colour = NHL_red, fill = NHL_red, size = 0) + # Neutral Bottom-Left
    
    # Ells surrounding faceoff dots
    geom_tile(aes(x = 65, y = 22.83, width = 4, height = 2 / 12), fill = NHL_red) + # Top-Right
    geom_tile(aes(x = 73, y = 22.83, width = 4, height = 2 / 12), fill = NHL_red) +
    geom_tile(aes(x = 65, y = 21.17, width = 4, height = 2 / 12), fill = NHL_red) +
    geom_tile(aes(x = 73, y = 21.17, width = 4, height = 2 / 12), fill = NHL_red) +
    geom_tile(aes(x = 66.92, y = 24.25, width = 2 / 12, height = 3), fill = NHL_red) +
    geom_tile(aes(x = 71.08, y = 24.25, width = 2 / 12, height = 3), fill = NHL_red) +
    geom_tile(aes(x = 66.92, y = 19.75, width = 2 / 12, height = 3), fill = NHL_red) +
    geom_tile(aes(x = 71.08, y = 19.75, width = 2 / 12, height = 3), fill = NHL_red) +
    
    geom_tile(aes(x = 65, y = -22.83, width = 4, height = 2 / 12), fill = NHL_red) + # Bottom-Right
    geom_tile(aes(x = 73, y = -22.83, width = 4, height = 2 / 12), fill = NHL_red) +
    geom_tile(aes(x = 65, y = -21.17, width = 4, height = 2 / 12), fill = NHL_red) +
    geom_tile(aes(x = 73, y = -21.17, width = 4, height = 2 / 12), fill = NHL_red) +
    geom_tile(aes(x = 66.92, y = -24.25, width = 2 / 12, height = 3), fill = NHL_red) +
    geom_tile(aes(x = 71.08, y = -24.25, width = 2 / 12, height = 3), fill = NHL_red) +
    geom_tile(aes(x = 66.92, y = -19.75, width = 2 / 12, height = 3), fill = NHL_red) +
    geom_tile(aes(x = 71.08, y = -19.75, width = 2 / 12, height = 3), fill = NHL_red) +
    
    geom_tile(aes(x = -65, y = 22.83, width = 4, height = 2 / 12), fill = NHL_red) + # Top-Left
    geom_tile(aes(x = -73, y = 22.83, width = 4, height = 2 / 12), fill = NHL_red) +
    geom_tile(aes(x = -65, y = 21.17, width = 4, height = 2 / 12), fill = NHL_red) +
    geom_tile(aes(x = -73, y = 21.17, width = 4, height = 2 / 12), fill = NHL_red) +
    geom_tile(aes(x = -66.92, y = 24.25, width = 2 / 12, height = 3), fill = NHL_red) +
    geom_tile(aes(x = -71.08, y = 24.25, width = 2 / 12, height = 3), fill = NHL_red) +
    geom_tile(aes(x = -66.92, y = 19.75, width = 2 / 12, height = 3), fill = NHL_red) +
    geom_tile(aes(x = -71.08, y = 19.75, width = 2 / 12, height = 3), fill = NHL_red) +
    
    geom_tile(aes(x = -65, y = -22.83, width = 4, height = 2 / 12), fill = NHL_red) + # Bottom-Left
    geom_tile(aes(x = -73, y = -22.83, width = 4, height = 2 / 12), fill = NHL_red) +
    geom_tile(aes(x = -65, y = -21.17, width = 4, height = 2 / 12), fill = NHL_red) +
    geom_tile(aes(x = -73, y = -21.17, width = 4, height = 2 / 12), fill = NHL_red) +
    geom_tile(aes(x = -66.92, y = -24.25, width = 2 / 12, height = 3), fill = NHL_red) +
    geom_tile(aes(x = -71.08, y = -24.25, width = 2 / 12, height = 3), fill = NHL_red) +
    geom_tile(aes(x = -66.92, y = -19.75, width = 2 / 12, height = 3), fill = NHL_red) +
    geom_tile(aes(x = -71.08, y = -19.75, width = 2 / 12, height = 3), fill = NHL_red) +
    
    # Referee crease
    geom_arc(aes(x0 = 0, y0 = -42.5, start = -pi / 2, end = pi / 2, r = 10), colour = NHL_red) +
    
    # Left goalie crease
    geom_tile(aes(x = -86.75, y = 0, width = 4.5, height = 8), fill = NHL_light_blue) +
    geom_arc_bar(aes(x0 = -89, y0 = 0, start = atan(4.5/4) - 0.01, end = pi - atan(4.5 / 4) + 0.01, r0 = 4, r = 6), fill = NHL_light_blue, colour = NHL_light_blue, size = 1 / 12) + # manually adjusted arc
    geom_tile(aes(x = -86.75, y = -4, width = 4.5, height = 2 / 12), fill = NHL_red) +
    geom_tile(aes(x = -86.75, y = 4, width = 4.5, height = 2 / 12), fill = NHL_red) +
    geom_arc(aes(x0 = -89, y0 = 0, start = atan(4.5/4) - 0.01, end = pi - atan(4.5 / 4) + 0.01, r = 6), colour = NHL_red, size = 2 / 12) + # manually adjusted arc
    geom_tile(aes(x = -85, y = 3.75, width = 2 / 12, height = 0.42), fill = NHL_red) +
    geom_tile(aes(x = -85, y = -3.75, width = 2 / 12, height = 0.42), fill = NHL_red) +
    
    # Right goalie crease
    geom_tile(aes(x = 86.75, y = 0, width = 4.5, height = 8), fill = NHL_light_blue) +
    geom_arc_bar(aes(x0 = 89, y0 = 0, start = -atan(4.5/4) + 0.01, end = -pi + atan(4.5 / 4) - 0.01, r0 = 4, r = 6), fill = NHL_light_blue, colour = NHL_light_blue, size = 1 / 12) + # manually adjusted arc
    geom_tile(aes(x = 86.75, y = -4, width = 4.5, height = 2 / 12), fill = NHL_red) +
    geom_tile(aes(x = 86.75, y = 4, width = 4.5, height = 2 / 12), fill = NHL_red) +
    geom_arc(aes(x0 = 89, y0 = 0, start = -atan(4.5/4) + 0.01, end = -pi + atan(4.5 / 4) - 0.01, r = 6), colour = NHL_red, size = 2 / 12) + # manually adjusted arc
    geom_tile(aes(x = 85, y = 3.75, width = 2 / 12, height = 0.42), fill = NHL_red) +
    geom_tile(aes(x = 85, y = -3.75, width = 2 / 12, height = 0.42), fill = NHL_red) +
    
    # Goalie nets placed as rectangles
    geom_tile(aes(x = -90.67, y = 0, width = 3.33, height = 6), fill = "#E5E5E3") + # Left # with grey fills
    geom_tile(aes(x = 90.67, y = 0, width = 3.33, height = 6), fill = "#E5E5E3") + # Right
    
    # Trapezoids
    geom_polygon(aes(x = c(-100, -100, -89, -89), y = c(10.92, 11.08, 7.08, 6.92)), fill = NHL_red) + # Left
    geom_polygon(aes(x = c(-100, -100, -89, -89), y = c(-10.92, -11.08, -7.08, -6.92)), fill = NHL_red) + # Left 
    geom_polygon(aes(x = c(100, 100, 89, 89), y = c(10.92, 11.08, 7.08, 6.92)), fill = NHL_red) + # Right
    geom_polygon(aes(x = c(100, 100, 89, 89), y = c(-10.92, -11.08, -7.08, -6.92)), fill = NHL_red) + # Right
    
    # Lines
    geom_tile(aes(x = -25.5, y = 0, width = 1, height = 85), fill = NHL_blue) + # Left Blue line
    geom_tile(aes(x = 25.5, y = 0, width = 1, height = 85),  fill = NHL_blue) + # Right Blue line
    geom_tile(aes(x = -89, y = 0, width = 2 / 12, height = 73.50), fill = NHL_red) + # Left goal line (73.5 value is rounded from finding intersect of goal line and board radius)
    geom_tile(aes(x = 89, y = 0, width = 2 / 12, height = 73.50), fill = NHL_red) + # Right goal line
    
    # Borders as line segments - plotted last to cover up line ends, etc.
    geom_line(aes(x = c(-72, 72), y = c(42.5, 42.5))) + # Top
    geom_line(aes(x = c(-72, 72), y = c(-42.5, -42.5))) + # Bottom
    geom_line(aes(x = c(-100, -100), y = c(-14.5, 14.5))) + # Left
    geom_line(aes(x = c(100, 100), y = c(-14.5, 14.5))) + # Right
    geom_arc(aes(x0 = 72, y0 = 14.5, start = pi / 2, end = 0, r = 28)) + # Top-Right
    geom_arc(aes(x0 = 72, y0 = -14.5, start = pi, end =  pi / 2, r = 28)) + # Bottom-Right
    geom_arc(aes(x0 = -72, y0 = 14.5, start = - pi / 2, end = 0, r = 28)) + # Top-Left
    geom_arc(aes(x0 = -72, y0 = -14.5, start = pi, end =  3 * pi / 2, r = 28)) + # Bottom-Left
    # Fixed scale for the coordinate system  
    coord_fixed()
  return(rin)
}

outputDir <- "responses"

saveData <- function(data) {
  #data <- t(data)
  # Create a unique file name
  #fileName <- sprintf("%s_%s.csv", as.integer(Sys.time()), digest::digest(data))
  # Write the file to the local system
  write.csv(
    x = data,
    file = 'asdfasdf.csv'
  )
}


roster_data <- wrangleRoster(roster)

ui <- basicPage(
  plotOutput("plot1", click = "plot_click"),
  verbatimTextOutput("info"),
  actionButton("updateplot", "Update Plot:"),
  actionButton("submit", "Save Data"),
  radioButtons("type", "Shot type:",
               c("Shot" = "shot",
                 "Goal" = "goal")),
  radioButtons("player", "Player",
               roster_data$roster_1)
  
  
)

##---------------Testing fluid sidebars----------------------#
ui <- fluidPage(
  titlePanel("Inputs"),
  sidebarLayout(
    sidebarPanel(
      verbatimTextOutput("info"),
      actionButton("updateplot", "Update Plot:"),
      actionButton("submit", "Save Data"),
      radioButtons("type", "Shot type:",
                   c("Shot" = "shot",
                     "Goal" = "goal")),
      radioButtons("player", "Player",
                   roster_data$roster_1)
    ),
    mainPanel(
      plotOutput("plot1", click = "plot_click")
    )
  )
)
##---------------Testing fluid sidebars----------------------#


##---------------Testing fluid sidebars----------------------#
# With UI on side
ui <- fluidPage(
  
  titlePanel("Inputs"),
  sidebarLayout(
    sidebarPanel(
      verbatimTextOutput("info"),
      actionButton("updateplot", "Update Plot:"),
      actionButton("submit", "Save Data"),
      radioButtons("type", "Shot type:",
                   c("Shot" = "shot",
                     "Goal" = "goal")),
    ),

    mainPanel(
      plotOutput("plot1", click = "plot_click"),
      fluidRow(
        column(4, 
               radioButtons("player", "Player",
                            roster_data$roster_1)
        ),
        column(8, 
               radioButtons("player", "Player",
                            roster_data$roster_1)
        )
      )
      )
  )
)

# With all settings on bottom
ui <- fluidPage(
  
  titlePanel("Inputs"),
  useShinyjs(),
  fluidRow(
    column(2,
           verbatimTextOutput("info"),
           actionButton("updateplot", "Update Plot"),
           actionButton("undo", "Undo last event"),
           actionButton("submit", "Save Data")
    ),
    column(10,
      plotOutput("plot1", click = "plot_click")
    )
    ),
      fluidRow(
        column(2, 
        ),
        column(4, 
               radioButtons("player1", "Player",
                            roster_data$roster_1)
        ),
        column(2, 
               numericInput("period", "Period:",1,min = 1, max = 3),
               numericInput("cmin", "Clock minutes:",19),
               numericInput("csec", "Clock seconds:",59),
               radioButtons("type", "Shot type:",
                            c("Shot" = "shot",
                              'Block' = 'Block',
                              'Miss' = 'Miss',
                              'Deflection' = 'Deflection',
                              "Goal" = "Goal"))
        ),
        column(4, 
               radioButtons("player2", "Player",
                            roster_data$roster_2,
                            selected = character(0))
        )
      )
    
  
)
shinyApp(ui, server)

##---------------Testing fluid sidebars----------------------#


server <- function(input, output,session) {
  
  ##
  roster <- reactiveVal("no selection")
  
  onclick("player2", {
    updateRadioButtons(session, "player1", choices = roster_data$roster_1, 
                       selected = character(0))
    roster(input$player2)
  })
  onclick("player1",{
    updateRadioButtons(session, "player2", 
                       choices = roster_data$roster_2, 
                       selected = character(0))
    roster(input$player1)
  })
  
  ##
  
  
  vals <- reactiveValues(
    d_x = c(),
    d_y = c(),
    shot_type = c(),
    players = c(),
    period = c(),
    mins = c(),
    secs = c(),
    events = 0
  )
  
  # Get radio button input
  #shot_type <- reactive ({ input$type})
  
  observeEvent(input$plot_click,{
    vals$d_x <- append( vals$d_x, input$plot_click$x)
    vals$d_y <- append( vals$d_y, input$plot_click$y)
    vals$shot_type <-append( vals$shot_type, input$type)
    vals$players <-append( vals$players, roster())
    vals$period <-append( vals$period, input$period)
    vals$mins <-append( vals$mins, input$cmin)
    vals$secs <-append( vals$secs, input$csec)
    vals$events <- vals$events +1
  })
  
  observeEvent(input$undo,{
    if(vals$events > 0){
      vals$d_x <- vals$d_x[1:(length(vals$d_x)-1)]
      vals$d_y <- vals$d_y[1:(length(vals$d_y)-1)]
      vals$shot_type <- vals$shot_type[1:(length(vals$shot_type)-1)]
      vals$players <-vals$players[1:(length(vals$players)-1)]
      vals$period <-vals$period[1:(length(vals$period)-1)]
      vals$mins <-vals$mins[1:(length(vals$mins)-1)]
      vals$secs <-vals$secs[1:(length(vals$secs)-1)]
      vals$events <- vals$events - 1
    }
  })

  output$plot1 <- renderPlot({
    input$updateplot
    isolate({
      #temp<- data.frame(x = vals$d_x, y = vals$d_y, color =factor(vals$shot_type) )
      nhl_rink_plot()+geom_point(aes(vals$d_x, vals$d_y,color = factor(vals$shot_type)))+scale_color_manual(values=c("Red", "darkgreen"))
    })
  })
  
  saveData <- function(data) {
    fileName <- sprintf("123_drive_time.csv")
    
    # Write the data to a temporary file locally
    filePath <- file.path('C:\\Users\\Reece\\123_drive_time.csv')
    write.csv(data, filePath, row.names = F, quote = TRUE)
  }
  
  observeEvent(input$submit, {    saveData(data.frame(x = vals$d_x, y = vals$d_y, color =factor(vals$shot_type) ))})
  
  ####
  output$info <- renderText({
    input$plot_click
    isolate({
      print(tail(vals$d_x, n =1))
      paste0("x = ", tail(vals$d_x, n =1), 
             "\ny = ", tail(vals$d_y,n=1), 
             '\nType =', tail(vals$shot_type, n =1), 
             '\nPlayer =', tail(vals$players, n =1),
             '\nClock =','P',tail(vals$period,n=1),' ', tail(vals$mins,n=1),':',tail(vals$secs,n=1))
    })
  })
  
  
}

shinyApp(ui, server)


d <- c('asdf')
append(d,'asdffff')
