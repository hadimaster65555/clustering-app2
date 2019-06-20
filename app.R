# library
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(dplyr)
library(openxlsx)
library(factoextra)
        
# halaman dashboard
ui <- dashboardPagePlus(
    # Membuat judul website
    title = "LUSTRO",
    
    # bagian header
    header = dashboardHeaderPlus(
        # Membuat judul untuk header
        title = tagList(span(class = "logo-lg", "Cluster App"),
                        span("CA")),
        enable_rightsidebar = T,
        rightSidebarIcon = "user",
        tags$li(
            class = "dropdown",
            uiOutput(outputId = "logout_button_1"),
            uiOutput(outputId = "render_login")
        ),
        tags$li(
            class = "dropdown",
            tags$script(
                HTML(
                    "window.addEventListener('DOMContentLoaded', (event) => {
                          document.getElementsByTagName('a')[1].setAttribute('id', 'iseng');
                          document.getElementById('iseng').click();
                          $('.sidebar-toggle').click();
                    });"
                )
            ),
            tags$script(
                "Shiny.addCustomMessageHandler('background-color', function(message) {
                    console.log(message);
                    document.getElementById('iseng').click();
                    document.getElementById('iseng').style.visibility = 'hidden'
                    document.getElementById('render_login').style.visibility = 'hidden'
                    $('.sidebar-toggle').click();
                });"
            ),
            tags$script(
                "Shiny.addCustomMessageHandler('logout-handler', function(message) {
                    console.log(message);
                    document.getElementById('iseng').style.visibility = 'visible'
                    document.getElementById('render_login').style.visibility = 'visible'
                    document.getElementById('iseng').click();
                    $('.sidebar-toggle').click();
                });"
            ),
            tags$style(
                HTML(
                    "
                    #username_input {
                        position: relative;
                        bottom: -6px;
                    }
                    
                    #password_input {
                        position:relative;
                        bottom: -6px;
                    }
                    
                    #login_button {
                        position: relative;
                        bottom: -6px;
                    }
                    
                    #logout_button {
                        position: relative;
                        bottom: -6px;
                        --display: none;
                    }
                    
                    .control-sidebar-dark .nav-tabs.control-sidebar-tabs {
                        border-bottom: #1c2529;
                        display: none;
                    }
                    "
                )
            )
        )
    ),
    
    rightsidebar = rightSidebar(
        textInput(inputId = "username", label = "Username"),
        passwordInput(inputId = "password", label = "Password"),
        actionButton(inputId = "login_button", label = "Login")
    ),
    # bagian sidebar
    sidebar = dashboardSidebar(sidebarMenu(
        menuItem(
            text = "Tabel Data",
            tabName = "showing_table",
            icon = icon("table")
        ),
        menuItem(
            text = "Visualisasi Klaster",
            tabName = "visualize_cluster",
            icon = icon("window-restore")
        ),
        menuItem(
            text = "Analisa Klaster",
            tabName = "cluster_analysis",
            icon = icon("gears")
        )
    )),
    # bagian body
    body = dashboardBody(tabItems(
        tabItem(
            tabName = "showing_table",
            uiOutput(outputId = "judul_data_mentah"),
            uiOutput(outputId = "input_pilih_data"),
            uiOutput(outputId = "value_box_raw_data"),
            uiOutput(outputId = "raw_data_box")
        ),
        
        tabItem(
            tabName = "visualize_cluster",
            uiOutput(outputId = "judul_visualisasi_cluster"),
            uiOutput(outputId = "visualisasi_cluster_input_box"),
            uiOutput(outputId = "visualisasi_cluster_plot_box"),
            uiOutput(outputId = "visualisasi_cluster_suggestion")
        ),
        
        tabItem(
            tabName = "cluster_analysis",
            uiOutput(outputId = "judul_cluster_analysis"),
            uiOutput(outputId = "hasil_cluster_analysis")
        )
    ))
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    # token holder
    token_login <- reactiveValues(username = NULL, password = NULL)
    
    # login
    observeEvent(input$login_button, {
        if (nchar(input$username) != 0 & nchar(input$password) != 0) {
            token_login$username <- input$username
            token_login$password <- input$password
            updateTextInput(session = session,
                            inputId = "username",
                            value = "")
            updateTextInput(session = session,
                            inputId = "password",
                            value = "")
            
            session$sendCustomMessage("background-color", message = "login")
        }
    })
    
    output$render_login <- renderUI({
        if (is.null(token_login$username)) {
            tags$b("Login", style = "position: relative; bottom: -14px; color: white;")
        }
    })
    
    output$logout_button_1 <- renderUI({
        req(token_login$username)
        if (is.null(token_login$username)) {
            return()
        } else {
            actionButton("logout_button", label = "logout")
        }
    })
    
    observeEvent(input$logout_button, {
        token_login$username <- NULL
        session$sendCustomMessage("logout-handler", message = "logout")
    })
    
    output$judul_data_mentah <- renderUI({
        req(token_login$username)
        h1("Raw Data")
    })
    
    output$input_pilih_data <- renderUI({
        req(token_login$username)
        fluidRow(box(
            selectInput(
                inputId = "pilih_tahun",
                label = "Masukkan Tahun",
                choices = c(
                    "2014" = "2014",
                    "2015" = "2015",
                    "2016" = "2016",
                    "2017" = "2017"
                )
            )
        ))
    })
    
    raw_data <- reactive({
        req(token_login$username, input$pilih_tahun)
        read.xlsx(xlsxFile = "data/data-2014.xlsx",
                  sheet = input$pilih_tahun)
    })
    
    output$value_box_raw_data <- renderUI({
        req(token_login$username, raw_data())
        fluidRow(
            valueBox(
                value = nrow(raw_data()),
                subtitle = "Banyak Data",
                color = "lime",
                width = 6
            ),
            valueBox(
                value = ncol(raw_data()),
                subtitle = "Banyak Feature",
                color = "teal",
                width = 6
            )
        )
    })
    
    output$raw_data_box <- renderUI({
        req(token_login$username)
        fluidRow(box(
            title = "Data Mentah",
            DT::dataTableOutput(outputId = "tabel_data_mentah"),
            width = 12
        ))
    })
    
    output$tabel_data_mentah <- DT::renderDataTable({
        req(token_login$username)
        DT::datatable(
            raw_data(),
            extensions = 'FixedColumns',
            options = list(scrollX = TRUE, fixedColumns = TRUE)
        )
    })
    
    output$judul_visualisasi_cluster <- renderUI({
        req(token_login$username)
        h1("Visualisasi Cluster")
    })
    
    data_to_model <- reactive({
        df <- raw_data()[, -1]
        rownames(df) <- raw_data()[, 1]
        df
    })
    
    output$visualisasi_cluster_input_box <- renderUI({
        req(token_login$username, data_to_model())
        fluidRow(box(
            numericInput(
                # memberi id 'nilai_cluster'
                inputId = "nilai_cluster",
                # memberi label
                label = "Masukkan Jumlah Klaster",
                # masukkan nilai awal
                value = 2,
                # masukkan nilai minimum
                min = 2,
                # masukkan nilai maksimum
                max = 10
            ),
            width = 12
        ))
    })
    
    output$visualisasi_cluster_plot_box <- renderUI({
        req(token_login$username, data_to_model())
        fluidRow(box(
            title = "Visualiasi Klaster",
            plotOutput(outputId = "plot_data"),
            width = 12
        ))
    })
    
    model_cluster <- reactive({
        req(token_login$username,
            data_to_model(),
            input$nilai_cluster)
        kmeans(x = data_to_model(), centers = input$nilai_cluster)
    })
    
    output$plot_data <- renderPlot({
        req(token_login$username,
            data_to_model(),
            input$nilai_cluster)
        set.seed(1)
        fviz_cluster(object = model_cluster(),
                     data = data_to_model(),
                     repel = T) + theme_light()
    })
    
    output$visualisasi_cluster_suggestion <- renderUI({
        req(token_login$username, data_to_model())
        fluidRow(
            box(
                title = "Plot WSS",
                plotOutput(outputId = "wss_plot"),
                width = 4
            ),
            box(
                title = "Plot Silhouette",
                plotOutput(outputId = "silhouette_plot"),
                width = 4
            ),
            box(
                title = "Plot Gap Statistics",
                plotOutput(outputId = "gap_plot"),
                width = 4
            )
        )
    })
    
    output$wss_plot <- renderPlot(expr = {
        req(token_login$username, data_to_model())
        fviz_nbclust(
            x = data_to_model()[, -5],
            FUNcluster = kmeans,
            k.max = 10,
            method = "wss"
        )
    })
    
    output$silhouette_plot <- renderPlot(expr = {
        req(token_login$username, data_to_model())
        fviz_nbclust(
            x = data_to_model()[, -5],
            FUNcluster = kmeans,
            k.max = 10,
            method = "silhouette"
        )
    })
    
    output$gap_plot <- renderPlot(expr = {
        req(token_login$username, data_to_model())
        fviz_nbclust(
            x = data_to_model()[, -5],
            FUNcluster = kmeans,
            k.max = 10,
            method = "gap"
        )
    })
    
    output$judul_cluster_analysis <- renderUI({
        req(token_login$username)
        h1("Analisa Cluster")
    })
    
    output$hasil_cluster_analysis <- renderUI({
        req(token_login$username, model_cluster())
        fluidRow(box(renderPrint({
            model_cluster()
        }),
        width = 12))
    })
}

# Run the application
shinyApp(ui = ui, server = server)
