#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(CHOIRBM)
library(plotly)
library(magrittr)

# Define UI for application with panels for data table and plotting
ui <- fluidPage(

    # Application title
    titlePanel("CHOIR Body Map Plotter"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            # Input: Select a file ----
            fileInput("file1", "Choose CSV File",
                      multiple = FALSE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv"))

            # Horizontal line ----
            , tags$hr()

            # Input: Checkbox if file has header ----
            , checkboxInput("header", "Header", TRUE)

            # Input: Select separator ----
            , radioButtons("sep", "Separator",
                         choices = c(Comma = ",",
                                     Semicolon = ";",
                                     Tab = "\t"),
                         selected = ",")

            # Input: Select quotes ----
            , radioButtons("quote", "Quote",
                         choices = c(None = "",
                                     "Double Quote" = '"',
                                     "Single Quote" = "'"),
                         selected = '"')

            # Horizontal line ----
            , tags$hr()

            # add an action button for plotting
            , actionButton("goButton", "Draw Body Map")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            # Output: Tabset w/ plot, summary, and table ----
            tabsetPanel(
                type = "tabs",
                tabPanel(
                    "Overview/Instructions"
                    , br()
                    , HTML("<p>The CHOIR Body Map (CBM) is a instrument for
                    assessing the distribution of a patient’s pain, and has
                    been validated in a paper  published in
                    <a href=\"https://doi.org/10.1097/pr9.0000000000000880\">
                    Pain Reports (Scherrer et al 2021)</a>. Patients use the different
                    segments of the CBM to indicate the location of their
                           pain and its level/intensity.</p>"
                           )
                    , h3("Example male and female CBMs:")
                    , img(src = "cbm-journal.jpeg", align = "center", width = "480px")
                    , br()
                    , br()
                    , HTML("<p>The CBM has been integrated with several
                           clincal research tools such as
                           <a href=\"https://www.project-redcap.org/\">REDCap</a>
                           (see GitHub repo
                           <a href=\"https://github.com/susom/redcap-em-imagemap-fork\">here</a>).
                           An  additional R package for customizable visualization
                           has been made available on GitHub as well
                           (<a href=\"https://github.com/emcramer/CHOIRBM\">here</a>).</p>")
                    , h3("Instructions")
                    , HTML("<ol>
                            <li>Use the \"Browse\" button to find your data set (a csv file).</li>
                            <li>Check your data in the \"Table\" tab. Make certain that
                                all of the IDs for the segments are present, are grouped
                                according to front and back, and that the column you
                                wish to plot is named \'value\'.</li>
                            <li>If there is an issue, go back to the file to fix it.
                                Otherwise, click \"Draw Body Map\" in the side panel.</li>
                            <li>To save your plot as a png file, hover your mouse
                                over the body map and click the grey camera icon
                                near the top right corner.</li>
                           </ol>")
                    , p("The plot is interactive. You may zoom, pan, hover, and select
                        areas. To reset your axes and return to the default view, click
                        the house/home button in the top right of the plot.")
                    , HTML("<p><strong>*</strong>The \"Table\" tab will be blank until data is added,
                        and the \"Body Map\" tab will appear blank the user
                        clicks draw.</p>")
                    , h3("Example Input/Output (with random data)")
                    , p("Five random rows of the input data file...")
                    , tableOutput("ex_data")
                    , p("Resulting plot from the randomly generated data.")
                    , plotlyOutput("ex_cbm", height = "480px")
                )
                , tabPanel(
                    "Table"
                    , dataTableOutput("contents")
                )  # show data
                , tabPanel(
                    "Body Map"
                    , br()
                    , plotlyOutput("cbm", height = "780px")
                ) # pca plot
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    # examples...
    output$ex_data <- renderTable({
        gen_example_data()[sample(1:74, 5), ]
    })
    output$ex_cbm <- renderPlotly({
        (plot_male_choirbm(gen_example_data(), "value") +
                theme(legend.position = "top") +
                scale_fill_gradient(
                    low = "white"
                    , high = "red"
                    , space = "lab"
                ) +
                labs(fill = "Percent") +
                coord_fixed(ratio = 0.8)
        ) %>%
            ggplotly()
    })

    # get the data set
    dataSet <- reactive({
        # check if data is uploaded...
        req(input$file1)

        # load the dataset

        # input$file1 will be NULL initially. After the user selects
        # and uploads a file, head of that data file by default,
        # or all rows if selected, will be shown.

        req(input$file1)

        # when reading semicolon separated files,
        # having a comma separator causes `read.csv` to error
        tryCatch(
            {
                df <- read.csv(input$file1$datapath,
                               header = input$header,
                               sep = input$sep,
                               quote = input$quote)
            },
            error = function(e) {
                # return a safeError if a parsing error occurs
                stop(safeError(e))
            }
        )
        return(df)
    })

    # loading and rendering the uploaded data
    output$contents <- renderDataTable({
        # generate a DT of the uploaded data
        req(input$file1)
        dataSet()
    })

    # render the CHOIR body map as a plotly
    output$cbm <- renderPlotly({
        req(input$goButton) # reload on the goButton

        # wait until the button is clicked...
        isolate({
            (plot_male_choirbm(dataSet(), "value") +
                theme(legend.position = "top") +
                scale_fill_gradient(
                    low = "white"
                    , high = "red"
                    , space = "lab"
                ) +
                labs(fill = "Percent") +
                coord_fixed(ratio = 0.8)
             ) %>%
            ggplotly()
        })
    })
}

# Run the application
shinyApp(ui = ui, server = server)
