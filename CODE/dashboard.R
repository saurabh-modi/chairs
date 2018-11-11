library(shiny)
library(dplyr)

dat <- read.csv("mamidat.csv", header = TRUE)
dat$DATE <- as.character(dat$DATE)

ui <- fluidPage(

    titlePanel("Screening at MAMI 2018!"),
    
    sidebarLayout(
        sidebarPanel(
            selectInput("Date", 
                        label = "Date",
                        choices = dat$DATE,
                        selected = NULL,
                        multiple = TRUE),
            selectInput("Time", 
                        label = "Time",
                        choices = dat$TIMESLOT,
                        selected = NULL,
                        multiple = TRUE),
            selectInput("Location", 
                        label = "Location",
                        choices = dat$LOCATION,
                        selected = NULL,
                        multiple = TRUE),
            selectInput("Movie", 
                        label = "Movie",
                        choices = dat$MOVIE,
                        selected = NULL,
                        multiple = TRUE),
        selectInput("Category", 
                    label = "Category",
                    choices = dat$CATEGORY,
                    selected = NULL,
                    multiple = TRUE)
        ),
        
        mainPanel(
            tabsetPanel(
                tabPanel("Here you go:", tableOutput("table")),
                tabPanel("MAMI schedule", tableOutput("mainTable"))
      )
      )
    )
)

rm(server)
server <- function(input, output) {
    
    samples.df <- data.frame(ID = paste0("ID", 1:nrow(dat)),
                             CATEGORY = as.factor(dat$CATEGORY),
                             TIMESLOT = as.factor(dat$TIMESLOT),
                             DATE = as.factor(dat$DATE),
                             LOCATION = as.factor(dat$LOCATION),
                             MULTIPLEX = as.factor(dat$MULTIPLEX),
                             MOVIE = as.factor(dat$MOVIE),
                             TIMINGS = as.factor(dat$TIMINGS))    
    
    ## samples.df <- data.frame(ID = paste0("ID",
    ##                                     as.character(round(runif(nrow(dat),
    ##                                                               min = 0,
    ##                                                               max = 100 * nrow(dat))))), 
    ##           Category = as.factor(dat$CATEGORY),
    ##           Time = as.factor(dat$TIMESLOT),
    ##           Date = as.factor(dat$DATE))
    
    samples.df$ID <- as.character(samples.df$ID)
    
    ## values.df <- subset(dat, select = c("TIMINGS", "MOVIE",
    ##                                     "MULTIPLEX", "LOCATION",
    ##                                     "CATEGORY", "TIMESLOT", "DATE"))
    
    ## values.df <- merge(values.df, samples.df, by = c("DATE",
    ##                                                   "TIMESLOT",
    ##                                                  "CATEGORY"))
    ## values.df <- values.df[order(values.df$ID),] 
    
    values.df <- cbind(paste0("Feature", 1:649), 
                       as.data.frame(dat, nrow = 649))
    colnames(values.df) <- c("ID", colnames(dat))
    
    values.df$ID <- samples.df$ID
    
    vs.values <- reactive({
        if (is.null(input$Date)) {
            return(dat$DATE)
        } else {
            return(input$Date)
        } 
    })
    
    carb.values <- reactive({
        if (is.null(input$Time)) {
            return(dat$TIMESLOT)
        } else {
            return(input$Time)
        } 
    })
    
    gear.values <- reactive({
        if (is.null(input$Category)) {
        return(dat$CATEGORY)
        } else {
            return(input$Category)
        } 
    })
    
    loc.values <- reactive({
        if (is.null(input$Location)) {
            return(dat$LOCATION)
        } else {
            return(input$Location)
        } 
    })
    mov.values <- reactive({
        if (is.null(input$Movie)) {
            return(dat$MOVIE)
        } else {
            return(input$Movie)
        } 
    })
    
    
    filtered.samples.df <- reactive({
        return(samples.df %>% filter(dat$CATEGORY %in% gear.values(),
                                     dat$DATE %in% vs.values(),
                                     dat$TIME %in% carb.values(),
                                     dat$LOCATION %in% loc.values(),
                                     dat$MOVIE %in% mov.values()))
    })
    
    filtered.values.df <- reactive({
        selected.samples <-  c("ID", "DATE", "TIMESLOT", "MULTIPLEX",
                               "CATEGORY", "MOVIE", "TIMINGS",
                               names(values.df)[names(values.df) %in%
                                                filtered.samples.df()$ID])
        return(values.df %>% select(selected.samples))
  })
    
    output$mainTable <- renderTable({
        filtered.values.df()
    })
    
    output$table <- renderTable({
        filtered.samples.df()
    })
    
    
}

shinyApp(ui = ui, server = server)
runApp("MyApp", display.mode = "showcase")
