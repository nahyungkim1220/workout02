library(shiny)
library(ggplot2)
library(reshape)

future_value <- function (amount = 100, rate = 0.05, years = 1) {
  amount * (1 + rate) ^ years
}
annuity <- function(contrib = 100, rate = 0.05, years = 1) {
  contrib * (((1 + rate) ^ years - 1) / rate)
}
growing_annuity <- function(contrib = 100, rate = 0.05, growth = 0.03, years = 1) {
  contrib * (((1 + rate) ^ years - (1 + growth) ^ years) / (rate - growth))
}

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Investing Models"),
  
  # Sidebar with a slider input for number of bins 
  flowLayout(
    column(12,
           sliderInput(inputId = "a",
                       label =  "Initial Amount",
                       min = 0,
                       max = 100000,
                       step = 500,
                       pre = "$",
                       sep = ",",
                       value = 1000),
           sliderInput(inputId = "d",
                       label = "Annual Contribution",
                       min = 0,
                       max = 50000,
                       step = 500,
                       pre = "$",
                       sep = ",",
                       value = 2000)
    ),
    column(12,
           sliderInput(inputId = "b",
                       label = "Return Rate (in %)",
                       min = 0,
                       max = 20,
                       step = 0.1,
                       value = 5),
           sliderInput(inputId = "e",
                       label = "Growth Rate (in %)",
                       min = 0,
                       max = 20,
                       step = 0.1,
                       value = 2)
    ),
    column(12,
           sliderInput(inputId = "c",
                       label = "Years",
                       min = 0,
                       max = 50,
                       step = 1,
                       value = 10),
           selectInput(inputId = "f",
                       label = "Facet?",
                       choices = c("Yes", "No"),
                       selected = "No")
    )),
  
  # Show a plot of the generated distribution
  mainPanel(hr(),
            h4("Timelines"),
            plotOutput("line"),
            h4("Balances"),
            verbatimTextOutput("modalitiestable")
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  modalities <- reactive({
    modalities_df <- data.frame("year" = 0: input$c, "no_contrib" = 0 : input$c, "fixed_contrib" = 0 : input$c, "growing_contrib" = 0 : input$c)
    for (i in 1: nrow(modalities_df)) {
      modalities_df[i, 2] <- future_value(amount = input$a, rate = (input$b)/100, years = i - 1)
      modalities_df[i, 3] <- future_value(amount = input$a, rate = (input$b)/100, years = i - 1) + annuity(contrib = input$d, rate = (input$b)/100, years = i - 1)
      modalities_df[i, 4] <- future_value(amount = input$a, rate = (input$b)/100, years = i - 1) + growing_annuity(contrib = input$d, rate = (input$b)/100, growth = (input$e)/100, years = i - 1)
    }
    modalities_df
  })
  melted_modalities <- reactive({
    melted <- melt(modalities(), id = "year")
    melted
  })
  output$line <- renderPlot({
    if (input$f == "No") {
      ggplot(modalities()) + 
        geom_point(aes(x = year, y = no_contrib, color = "a")) + 
        geom_line(aes(x = year, y = no_contrib, color = "a")) + 
        geom_point(aes(x = year, y = fixed_contrib, color = "b")) + 
        geom_line(aes(x = year, y = fixed_contrib, color = "b")) +
        geom_point(aes(x = year, y = growing_contrib, color = "c")) + 
        geom_line(aes(x = year, y = growing_contrib, color = "c")) +
        scale_color_manual(name = "modality", 
                           labels = c("a" = "no_contrib", 
                                      "b" = "fixed_contrib", 
                                      "c" = "growing_contrib"),
                           values = c("a" = "#D35400",
                                      "b" = "#229954", 
                                      "c" = "#3498DB")) +
        ggtitle("Three modes of investing") + 
        xlab("year") + 
        ylab("value") 
    } else if (input$f == "Yes") {
      ggplot(melted_modalities(), aes(x = year, y = value, fill = variable)) + 
        geom_point(aes(x = year, y = value, color = variable)) + 
        geom_line(aes(x = year, y = value, color = variable)) +
        geom_area(alpha=0.6) +
        facet_wrap(~ variable) +
        ggtitle("Three modes of investing") + 
        xlab("year") + 
        ylab("value") +
        theme_gray()
    }
  })
  output$modalitiestable <- renderPrint(modalities())
}

# Run the application 
shinyApp(ui = ui, server = server)