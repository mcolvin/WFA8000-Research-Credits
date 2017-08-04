
shinyUI(
    fluidPage(
        #DT::dataTableOutput("responses", width = 300), tags$hr(),
        textInput("name", "Name", ""),
        checkboxInput("used_shiny", "I've built a Shiny app in R before", FALSE),
        sliderInput("r_num_years", "Number of years using R", 0, 25, 2, ticks = FALSE),
        actionButton("submit", "Submit")
      )
)