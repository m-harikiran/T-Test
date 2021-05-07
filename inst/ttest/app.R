#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ADVTTEST)
library(shinyFeedback)

# Define UI for application that takes two samples and prints results of hypothesis testing for diference in means.
ui <- fluidPage(
    # Setting up user feedback in UI
    useShinyFeedback(),
    # Application title
    titlePanel("Difference in population means"),

    # Sidebar layout with input and output definitions
    sidebarLayout(
        # Sidebar panel for inputs ----
        sidebarPanel(
            # Input: Select the random distribution type ----
            radioButtons(
                "sampType",
                "Samples Type:",
                c("Unpaired Samples" = "unp",
                  "Paired Samples" = "pair"),
                selected = "unp"
            ),

            # introduce extra vertical spacing
            br(),

            h5('Randomly generating nornally distributed samples using rnorm'),

            br(),

            # Header SAMPLE 1
            h4('SAMPLE 1'),

            # Input: Slider numeric for the number of observations to generate
            sliderInput(
                'n1',
                'Number of observations',
                min = 10,
                max = 1000,
                value = 30
            ),

            # Input: Numeric input for the mean of sample to be generated
            numericInput(
                "m1",
                "Mean of sample data:",
                value = 10,
                min = 1,
                max = 1000
            ),

            # Input: Numeric input for the standard deviation of sample to be generated
            numericInput(
                "sd1",
                "Standard Deviation of sample data:",
                value = 15,
                min = 1,
                max = 1000
            ),

            br(),

            # Header SAMPLE 2
            h4('SAMPLE 2'),

            # Input: Slider numeric for the number of observations to generate
            sliderInput(
                'n2',
                'Number of observations',
                min = 10,
                max = 1000,
                value = 30
            ),

            # Input: Numeric input for the mean of sample to be generated
            numericInput(
                "m2",
                "Mean of sample data:",
                value = 8,
                min = 1,
                max = 1000
            ),

            # Input: Numeric input for the standard deviation of sample to be generated
            numericInput(
                "sd2",
                "Standard Deviation of sample data:",
                value = 15,
                min = 1,
                max = 1000
            ),

            br(),

            # Header
            h4('Hypothesis Testing'),

            br(),

            sliderInput(
                'alpha',
                'Significance Level',
                min = 0,
                max = 1,
                value = 0.05,
                step = 0.01,
                animate = animationOptions(interval = 200, loop = TRUE)
            )
        ),

        # For displaying the summary of Hypothesis Testing and other results
        mainPanel(# Output tabs
            tabsetPanel(
                type = 'tabs',
                tabPanel('Samples',
                         fluidRow(
                             column(5, offset = 2, h4('SAMPLE 1'), tableOutput('sample1')),
                             column(5, h4('SAMPLE 2'), tableOutput('sample2'))
                         )),
                tabPanel('Plot'),
                tabPanel('Summary', verbatimTextOutput('summary'))
            ))
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    x <- reactive({
        sPaired <- input$sampType == 'pair'

        feedbackWarning('n1',
                        (((
                            input$n1 != input$n2
                        ) && sPaired)),
                        "Number of observations must be same for paired samples")
        feedbackWarning('n2',
                        (((
                            input$n1 != input$n2
                        ) && sPaired)),
                        "Number of observations must be same for paired samples")

        if (sPaired) {
            req((input$n1 == input$n2), cancelOutput = TRUE)
        }

        set.seed(32)
        rnorm(input$n1, mean = input$m1, sd = input$sd1)
    })

    y <- reactive({
        sPaired <- input$sampType == 'pair'
        if (sPaired) {
            req((input$n1 == input$n2), cancelOutput = TRUE)

            set.seed(35)
            x() + rnorm(input$n2, mean = input$m2, sd = input$sd2)
        }
        else{
            set.seed(35)
            rnorm(input$n2, mean = input$m2, sd = input$sd2)
        }
    })


    testSummary <- reactive(ADVTTEST::myttest(x(), y(), alpha = input$alpha, paired = (input$sampType == 'pair')))


    output$sample1 <- renderTable({
        x()
    })

    output$sample2 <- renderTable({
        y()
    })

    output$summary <- renderPrint({
        print(testSummary())

    })
}

# Run the application
shinyApp(ui = ui, server = server)
