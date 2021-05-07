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
                value = 30,
                step = 1,
                animate = animationOptions(interval = 200, loop = TRUE)
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
                value = 30,
                step = 1,
                animate = animationOptions(interval = 200, loop = TRUE)
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

            #Slider Input
            sliderInput(
                'alpha',
                'Significance Level (alpha)',
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
                         br(),
                         fluidRow(
                             column(5, offset = 2, h4('SAMPLE 1'), tableOutput('sample1')),
                             column(5, h4('SAMPLE 2'), tableOutput('sample2'))
                         )),

                tabPanel(
                    'Summary',
                    br(),
                    h4('Type of Test Performed'),
                    h5(textOutput('testType')),

                    br(),
                    h4('Test Statistic'),
                    h5(textOutput('testStastic')),

                    br(),
                    h4('Degree of Freedom'),
                    h5(textOutput('testdf')),

                    br(),
                    h4('P-Value'),
                    h5(textOutput('testPval')),

                    br(),
                    h4('Significance Level (alpha)'),
                    h5(textOutput('testSlev')),

                    br(),
                    h4(textOutput('confLev')),
                    h5(textOutput('confInt')),

                    br(),
                    h4('Sample Estimates'),
                    h5(textOutput('testMean')),

                    br(),
                    h4('Test Conclusion'),
                    h5(textOutput('conclusion')),

                    br(),
                    h4('Test Summary'),
                    verbatimTextOutput('summary')
                ),
                tabPanel(
                    'Plot',
                    br(),
                    h4('Box Plot'),
                    br(),
                    plotOutput('boxPlot', width = '50%')
                )

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


    testSummary <-
        reactive(ADVTTEST::myttest(
            x(),
            y(),
            alpha = input$alpha,
            paired = (input$sampType == 'pair')
        ))

    confLevel <- reactive(100 - input$alpha * 100)

    testConclusion <- reactive({
        if (testSummary()$Test_Conclusion == 'Y') {
            'Since p-value is less than significance level we reject NULL Hypothesis, i.e., true difference in means is not equal to 0'
        }
        else {
            'Since p-value is greater than significance level we fail to reject NULL Hypothesis, i.e., true difference in means is equal to 0'
        }
    })

    testPlot <- reactive(plot(testSummary()))


    output$sample1 <- renderTable({
        x()
    })

    output$sample2 <- renderTable({
        y()
    })

    output$testType <-
        renderText(testSummary()$Test_Summary$method)

    output$testStastic <-
        renderText(paste0('t-statistic = ', testSummary()$Test_Summary$statistic))

    output$testdf <-
        renderText(paste0('df = ', testSummary()$Test_Summary$parameter))

    output$testPval <-
        renderText(testSummary()$Test_Summary$p.value)

    output$testSlev <- renderText(reactive(input$alpha)())

    output$confLev <-
        renderText(paste0(confLevel(), ' Percent Confidence Interval'))

    output$confInt <-
        renderText(
            paste0(
                '[',
                testSummary()$Test_Summary$conf.int[1],
                ' , ',
                testSummary()$Test_Summary$conf.int[2],
                ']'
            )
        )

    output$testMean <-
        renderText(
            paste0(
                'Mean of Sample 1 : ',
                testSummary()$Test_Summary$estimate[1],
                '\nMean of Sample 2 : ',
                testSummary()$Test_Summary$estimate[2]
            )
        )

    output$conclusion <- testConclusion

    output$summary <- renderPrint({
        print(testSummary()$Test_Summary)
    })

    output$boxPlot <- renderPlot(testPlot())


}

# Run the application
shinyApp(ui = ui, server = server)
