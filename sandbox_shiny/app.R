library(shiny)
# Define UI ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Scores de bien-être par groupe ou sexe au temps 2"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Selector for variable to plot against mpg ----
      selectInput("variable", "Variable:",
                  c("Groupe" = "group",
                    "Sexe" = "sex")),
      
      # Input: Checkbox for whether outliers should be included ----
      checkboxInput("outliers", "voir les outliers", TRUE)
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Formatted text for caption ----
      h3(textOutput("caption")),
      
      # Output: Plot of the requested variable against mpg ----
      plotOutput("cripsPlot")
      
    )
  )
)

# import des données (un custom de crips 2019 pour l'expérience)----
library(rio)
d <- import("data.csv")

# Define server logic to plot various variables against score be ----
# On veut voir le bien-être comparé des groupes entre eux ou des sexes entre eux.
server <- function(input, output) {
  
  # Compute the formula text ----
  # This is in a reactive expression since it is shared by the
  # output$caption and output$cripsPlot functions
  formulaText <- reactive({
    paste("hbs_sco ~", input$variable)
  })
  
  # Return the formula text for printing as a caption ----
  output$caption <- renderText({
    formulaText()
  })
  
  # Generate a plot of the requested variable against score be ----
  # and only exclude outliers if requested
  output$cripsPlot <- renderPlot({
    boxplot(as.formula(formulaText()),
            data = d,
            outline = input$outliers,
            col = "#75AADB", pch = 19)
  })
  
}
  
shinyApp(ui, server)

# Jouer plus loin :
# Choisir les classes qu'on veut comparer à la moyenne de toutes les classes.
# Choisir simplement la classe que je veux voir.
# Choisir la classe et si je veux les données profs ou les données élèves...
