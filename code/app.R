#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Package
library(shiny)
library(tidyverse)
library(ggthemes)
library(shinythemes)
library(png)
#library(shinydashboard)
#library(org.Hs.eg.db)
library(MetBrewer)
#library(ggrepel)
#library(Homo.sapiens)
library(plotly)
#library(shinyjs)
#options(rsconnect.packrat = TRUE)

# Functions


# Define UI for application 
home_pages = tabPanel(
    title = "Home",
    icon = icon("house", class = NULL, lib = "font-awesome"),
    titlePanel(
        h1(HTML("<b>PassengerDepMap</b>"), 
           align = "center",
           style = "font-size:50px;")
    ),   
    h2("A web-tool to explore collateral dependencies associated with passenger gene co-amplifications",
       align = "center",
       style = "font-size:25px;"),
    br(),
    br(),
    #img(src="https://www.skoda-dobron.pl/assets/AKOL/1470/1b671b3ff2/4960013_z2.jpg",height=300,width=300,style="display: block; margin-left: auto; margin-right: auto; margin-top: 10px;"),
    plotOutput(outputId = "png"),
    br(),
    fluidRow(
        column(6, align="center", offset = 3,
        actionButton("reset", "Use cases", style="simple; align:center;", size="sm", color = "btn-warning",icon = icon("book", class = NULL, lib = "font-awesome")),
        verbatimTextOutput(outputId = "text"),
        tags$style(type='text/css', "#button { vertical-align- middle; height- 50px; width- 100%; font-size- 30px;}")
        ))
    
    #actionButton("reset", "RESET", style="simple; align:center;", size="sm", color = "warning",icon = icon("book", class = NULL, lib = "font-awesome")),
    #verbatimTextOutput(outputId = "text")
    
)

analysis_page = tabPanel(
    title = "Analysis",
    icon = icon("sliders", class = NULL, lib = "font-awesome"),
    br(),
    sidebarPanel(
        tabsetPanel(
            tabPanel(title = "Input", 
                     textInput("gene", label = h3("Type in an oncogene", style = "font-size:15px;"), 
                               value = ""),
                     actionButton("analysis1", "Go", style="simple; align:center;", size="sm", color = "btn-warning"),
                     textInput("passenger", label = h3("Type in a passenger gene", style = "font-size:15px;"), 
                               value = ""),
                     actionButton("analysis2", "Go", style="simple; align:center;", size="sm", color = "btn-warning"),
            ),
            
            tabPanel(
                "Help", 
                br(),
                tags$li("Type in a oncogene name(e.g., 'MYCN') in the textbox in the top-left and click Go button"),
                tags$li("Gene symbols matched with your query are highlighted in the plot with its nearby co-amplified passenger genes.
                        If that oncogene is without amplified case, it will say no-amplification "),
                tags$li("Type in a passenger gene name(e.g., 'DDX1') in the textbox in the bottom-left and click Go button"),
                tags$li("If there is dependency data, there will show the plot and result in the right panel")
                )
            
        )),
    # Show a plot of the generated distribution
    sidebarPanel(
        tabsetPanel(
            id = "tabs2",
            tabPanel(
                title = "Passenger genes",
                plotOutput("plot1"),
                textOutput("text1")
            ),
            
            tabPanel(
                title = "Passenger genes table",
                tableOutput("table1")
            ))
    ),
    
    
    
    
    sidebarPanel(
        tabsetPanel(
            id = "tabs3",
            tabPanel(
                title = "Passenger dependency",
                plotlyOutput("plot2"),
                textOutput("text2")
            ),
            
            tabPanel(
                title = "Dependency genes list",
                dataTableOutput("table2")
        ))
    )
    
)

about_page = tabPanel(
    title = "About",
    icon = icon("universal-access", class = NULL, lib = "font-awesome"),
    titlePanel(
        h1(HTML("<b>About</b>"), 
           style = "font-size:30px;")
    ),   
    
    #describe the main function of website
    titlePanel(
        h2(HTML("<b>PassengerDepMap</b>"), 
           style = "font-size:20px;")
    ),
    tags$li("PassengerDepMap is a web-tool to explore the collateral vulnerable targets of co-amplified passenger genes on the oncogenic amplicons"),
    tags$li("The CRISPR dependency dataset is from DepMap(version 21Q1)"),
    tags$li("The Cell line copy number dataset is from CCLE"),
    tags$li("The tumor patients copy number dataset is from PCAWG and TARGETS"),
    
    # the usage of the the web tool
    titlePanel(
        h3(HTML("<b>Usage</b>"), 
           style = "font-size:20px;")
    ),
    tags$line("This web-tool can be used for the following purposes."),
    br(),
    br(),
    tags$li("Identify the potential co-amplified passenger genes in the vicility of oncogene"),
    tags$li("Predict the most likely collateral vulnerable targets to the co-amplified passenger genes"),
    tags$li("Predict the most likely collateral vulnerable targets to co-amplified passenger genes"),
    
    # add publication
    titlePanel(
        h4(HTML("<b>Publication</b>"), 
           style = "font-size:20px;")
    ),
    "If you find PassengerDepMap useful in your research, please consider citing the following publication:",
    br(),
    tags$line("Yi Bei, Luca Bramé, Marieluise Kirchner,et al., Amplicon structure creates collateral therapeutic vulnerability in cancer. 
              BioRxiv 2022"),
    tags$a("doi: 10.1101/2022.09.08.506647",
           href = "https://www.biorxiv.org/content/10.1101/2022.09.08.506647v2"),
    
    #Inquiries
    titlePanel(
        h5(HTML("<b>Inquiries</b>"), 
           style = "font-size:20px;")
    ),
    "Please contact ",
    tags$a(" henssenlab@gmail.com "),
    "for inquiries"
    
)

github_page = tabPanel(
    title = "Github",
    icon = icon("github", class = NULL, lib = "font-awesome"),
    br(),
    br()
)


ui <- tagList(
    tags$head(tags$style("body{overflow:hidden;}")),
    tags$head(tags$script(type="text/javascript", src = "code.js")),
    navbarPage(
        #title = div("PassengerDepMap"),
        #title = div("PassengerDep", div(plotOutput(outputId = "logo"))),
        #titlePanel(title = span(img(src="https://www.skoda-dobron.pl/assets/AKOL/1470/1b671b3ff2/4960013_z2.jpg",height=30,style=" margin-left: auto; margin-right: auto; margin-top: auto;"), "test")),
        tags$line(
            class = "dropdown",
            img(
                src = "https://raw.githubusercontent.com/yeebae1118/PassengerDepMap/main/www/Passenger_icon.png",
                style= 'margin-right:10000px',
                width="65",height="45"
            ),
            "PassengerDepMap "
        ),
        theme = shinytheme('united'),
        tabsetPanel(
            id = "tabs",
            home_pages,
            analysis_page,
            about_page,
            github_page
        ) 
    )
       
    )
    #title = div(plotOutput(outputId = "logo",width="20px", height="20px"), "PassengerDep"),
   
    #title = div("PassengerDepMap"),
    
    #title = "PassengerDep",
    #theme = shinytheme('united'),
    #home_pages,
    #analysis_page,
    #about_page,
    #github_page


# Define server logic required to draw a histogram
server <- function(input, output, session) {

    
    output$png <- renderPlot({
        pic = readPNG('www/hypothesis.png')
        plot.new()
        grid::grid.raster(pic)
        
    })
   
    
    # add url for Github tab
    observeEvent(input$tabs, { #URL for the github of Yi Bei
        print(input$tabs)
        if( input$tabs == "Github" ) {
            utils::browseURL(url="https://github.com/yeebae1118/PassengerDepMap")
          #js$browseURl("https://github.com/yeebae1118/PassengerDepMap")
        }
    })
    
    
    observeEvent(input$reset, {
        # display a modal dialog with a header, textinput and action buttons
        showModal(modalDialog(
            title = "Collateral vulnerable finder",
            tags$line("PassengerDepMap first asks which oncogene interests you in cancer. If this oncogene
                    is amplified in cancer, it will show you the co-amplified passenger in the proximity.
                    Then, it will ask which co-amplified passenger interests you more. After your input,
                      it will show you the potentially collateral vulnerable targets to the co-amplified passenger
                      gene."),
            
            tags$line("There are 10 oncogenes with co-amplified passenger gene dependency data from DepMap."),
            tags$li("Those oncogenes are: CCND1, MYCN, MYC, MDM2, EGFR, HMGA2, MET, KAT6A, RAD21, MITF"),
            titlePanel(
                h2(HTML("<b>Case 1. DDX1 is commonly co-amplified passenger gene in neuroblastoma</b>"), 
                   style = "font-size:15px;")
            ),
            tags$line(
              class = "dropdown",
              img(
                src = "https://raw.githubusercontent.com/yeebae1118/PassengerDepMap/main/www/DDX1_MYCN.png",
                #style= 'margin-right:10000px',
                width="500",height="200"
              )),
          
            
        ))
    })
    
  #insert action 1
    observeEvent(input$analysis1,{
      # laod data
      co_amp_df = readRDS("amp_data/patient_count.RDS")
    
      # input gene
      gene_input = as.character(input$gene)
      # convert to ENTREZID
      if(gene_input %in% names(co_amp_df)){
        Result = reactive({
          
          onco_coamp = co_amp_df[[gene_input]]
          
          # upset plot
          library(UpSetR)
          colname = colnames(onco_coamp)
          upset(as.data.frame(onco_coamp), sets = c(colname[1:3],gene_input, colname[5:7]), sets.bar.color = "dodgerblue3",
                order.by = "freq")
        })
        
        output$plot1 = renderPlot(
          Result()
        )
        
        Result1 = reactive({
          
          onco_coamp = co_amp_df[[gene_input]]
          colname = colnames(onco_coamp)
        })
        
        output$table1 = renderTable(
          Result1()
        )
       
      }
      else{
        Result = reactive({
          reply = "It is not an oncogene"
        })
        output$text1 = renderText(
          Result()
        )
      }
     
      
     
    })
   
    #insert action 2
    observeEvent(input$analysis2,{
      passenger_input = as.character(input$passenger)
      df_co_amp = readRDS("amp_data/co-amp.RDS")
      co_gene = names(df_co_amp)
      if(passenger_input %in% co_gene){
        Result = reactive({
          df = df_co_amp[[passenger_input]]
          library(plotly)
          df %>% 
            mutate(color1 = ifelse(p_value <= 0.05 & diff_score >0.2 & co_mediam_df < -0.5, "black", "blue")) %>%
            ggplot(aes(x = diff_score, y = -log10(p_value))) +
            geom_point(aes(color = color1, label = gene)) +
            theme_minimal() +
            theme(legend.position = "none")+
            labs(x = "∆median(co-amplification vs. no co-amplification)")+
            
            scale_color_manual(values = c("#E76355","grey")) -> P
             
          ggplotly(P)
        })
        
        output$plot2 = renderPlotly(
          Result()
        )
        Result2 = reactive({
          df = df_co_amp[[passenger_input]]
          df %>% 
            filter(diff_score >=0.2 & p_value <0.05 & co_mediam_df < -0.5) %>% 
            dplyr::select(gene,diff_score,p_value)  -> df_flt
        })
        
        output$table2 = renderDataTable(
          {Result2()}
        )
      }else{
        Result = reactive({
          reply = "There is no dependency data"
        })
        output$text2 = renderText(
          Result()
        )
      }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

