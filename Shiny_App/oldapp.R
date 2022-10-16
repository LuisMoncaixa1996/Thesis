## add sub menu items in shinydashboard sidebar

# load the required packages
require(devtools)
library(shiny)
library(shinydashboard)
library(DT)
library(Comp2ROC)
library(plotly)
library(shinyWidgets)
library(dplyr)
library(shinycssloaders)
library(readxl)
library(geepack)
library(MESS)
library(shinyjs)

options(shiny.fullstacktrace=TRUE)


tweaks <- 
  list(tags$head(tags$style(HTML("
                                 .multicol { 
                                   height: 250px;
                                   -webkit-column-count: 4; /* Chrome, Safari, Opera */ 
                                   -moz-column-count: 4;    /* Firefox */ 
                                   column-count: 4; 
                                   -moz-column-fill: auto;
                                   -column-fill: auto;
                                 } 
                                 ")) 
  ))


#Interface da aplica??o
ui <- shinyUI(
    dashboardPage(skin = 'black',
        dashboardHeader(title = "GEE Models", titleWidth = 300,tags$li(class = "dropdown",
                                                                     dropMenu(
                                                                         dropdownButton("Info", circle = TRUE, status = 'primary', icon = icon('info'),size = "sm", up = FALSE),
                                                                         h3(strong('Information')),
                                                                         br(),
                                                                         h5(div(includeMarkdown("Info.Rmd"), 
                                                                                align="justify")),
                                                                         placement = "right",
                                                                         arrow = TRUE) )),
        dashboardSidebar(
            width = 350,
            sidebarMenu(id = 'sidebarmenu',
                        # Menu correspondente ao Upload dos datasets
                        menuItem("Preparacao do modelo", tabName = "Prep", icon = icon("upload"),
                                 menuSubItem("Upload Dataset", tabName = "Upload", icon = icon("upload")),
                                 menuSubItem("Escolha das variaveis", tabName = "Select")
                                 ),
                        
                        # Menus de compara??o entre curvas dependendo do tipo de vari?vel
                        menuItem("GEE Models", icon = icon("clipboard"),
                                 tabName = "Gee_List"),
                                             
                        menuItem("Results",
                                 icon = icon("clipboard"),
                                 menuSubItem("Choose Predictions",
                                             tabName = "Escolher_Variaveis_Ind",
                                             icon = icon("edit")),
                                 menuSubItem("Graphic Results",
                                             tabName = "Resultados_Graficos_Ind",
                                             icon = icon("chart-area")),
                                 menuSubItem("Statistical Results",
                                             tabName = "Resultados_Estatisticos_Ind",
                                             icon = icon("list-ol"))),
                        
                        #Menu informativo com refer?ncias acerca da aplica??o
                        menuItem("About", tabName = "Sobre", icon = icon("book"))
                        )),
        
        
        dashboardBody(shinyjs::useShinyjs(),
            tabItems(
                # Toda a interface do menu Upload
                tabItem("Upload", sidebarLayout(
                    sidebarPanel(
                        fileInput("file1",
                                  "Pick your Data",
                                  multiple = TRUE,
                                  accept = c("text/csv","test/comma-separated-values, text/plain",".csv", ".xlsx")),
                        tags$hr(),
                        checkboxInput("header","Header", TRUE),
                        radioButtons("sep","Separator",choices = c(Comma = ",",
                                                                   Semicolon = ";",
                                                                   Tab = "\t",
                                                                   selected = ",")),
                        tags$hr(),
                        radioButtons("quote","Quote",choices = c(None = "" ,
                                                                 "Double Quote" = '"',
                                                                 "Single Quote" = "'",
                                                                 selected = '"')),
                        tags$hr(),
                        radioButtons("disp","Display",choices = c(Head = "head",
                                                                  ALL = "all",
                                                                  selected = "head")),
                        
                        actionButton("submitbutton", "Submit", class = "btn btn-primary")
                    ),
                    
                    mainPanel(
                        DT::dataTableOutput("contents"), style = "height:800px;overflow-x: scroll;"
                    )
                )),
                tabItem("Select", tabBox(
                  title = "Variable Selection for GEE Model",
                  id = "selectvariable", height = "800px", width = "1000px",
                  selected = "Model Definitions",
                  tabPanel("Variables",
                           box(solidHeader = TRUE,
                               collapsible = TRUE, width = "1000px",
                               selectInput("VR","Predictor Variable",""), style="font-size:100%; font-family:Arial Black;",
                               column(2,style="font-size:100%; font-family:Arial Black;",  
                                      "Variable Type: "),
                               column(4, style = "display: inline-block; margin-left: 150px; vertical-align: -20px; font-size:100%; font-family:Times New Roman;", 
                                      prettyCheckbox(
                                        inputId = "Ordinal", label = "Ordinal", icon = icon("thumbs-up"), 
                                        status = "default", shape = "curve", animation = "pulse"
                                      )),
                               column(4, style = "display: inline-block; vertical-align: -20px;font-size:100%; font-family:Times New Roman;",
                                      prettyCheckbox(
                                        inputId = "Literal", label = "Literal", icon = icon("thumbs-up"), 
                                        status = "default", shape = "curve", animation = "pulse"
                                      )),
                               column(12,style = "text-align: right;",
                                      actionButton("VRbutton", "Submit", icon("paper-plane"), 
                                                      style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))
                               
                               
                           ),
                           box(solidHeader = TRUE,
                               collapsible = TRUE, width = "1000px",
                               column(12,style="height: 40px;font-size:100%; font-family:Arial Black;",  
                                      "Covariables: "),
                               fluidPage(tweaks,fluidRow(column(12, list(tags$div(align = 'left', 
                                                                                           class = 'multicol', 
                                                                                           checkboxGroupInput(inputId  = 'Variables', 
                                                                                                              label    = NULL, 
                                                                                                              choices  = c(),
                                                                                                              inline   = FALSE))))
                                
                               )),column(12,style = "text-align: right;",
                                        actionButton("Covariablebutton", "Submit", icon("paper-plane"), 
                                                     style="color: #fff; background-color: #337ab7; border-color: #2e6da4")))
                           ),
                  tabPanel("Family/ID",
                           box(solidHeader = TRUE,
                               collapsible = TRUE, width = "1000px",
                               column(12,style="height: 40px;font-size:100%; font-family:Arial Black;",  
                                      "Identification Column:  "),
                               column(12,style="font-size:100%; font-family:Arial Black;",
                                      selectInput("ID",NULL,"")),
                               column(12,style = "text-align: right;",
                                      actionButton("IDbutton", "Submit", icon("paper-plane"), 
                                                   style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))
                               ),
                           box(solidHeader = TRUE, collapsible = TRUE, width = "1000px",
                               column(12,style="height: 40px;font-size:100%; font-family:Arial Black;",  
                                      "Distribution Family:  "),
                               column(12,style="font-size:100%; font-family:Times New Roman;",
                                      selectInput("Family",NULL,c('binomial','gaussian','Gamma',
                                                              'inverse.gaussian','poisson','quasi',
                                                              'quasibinomial','quasipoisson'))),
                               column(12,style = "text-align: right;",
                                      actionButton("Familybutton", "Submit", icon("paper-plane"), 
                                                   style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))
                               )),
                  tabPanel("Correlation Structure",
                           column(5,style="display: inline-block;margin-top:50px;margin-right:0px;font-size:120%; font-family:Arial Black;",  
                                  "Choose your correlation structure:  "),
                           column(4,style="display: inline-block;margin-top:45px; font-size:100%; font-family:Times New Roman;",
                                  selectInput("CS",NULL,c('Independence','Exchangeable','AR-1',
                                                          'Unstructured'))),
                           column(7,style="display: inline-block;margin-left: 50px;margin-right:0px;font-size:90%; font-family:Arial;",  
                                  "Display QIC values:  "),
                           column(4, style= "display: inline-block;margin-left: 0px;", prettyCheckbox(
                             inputId = "QIC_Show", label = NULL, icon = icon("thumbs-up"), 
                             status = "default", shape = "curve", animation = "pulse")),
                           column(12,DT::dataTableOutput('QIC_Table'))
                           
                           ),
                  tabPanel("Model Definitions",
                           column(4,style="display: inline-block;margin-top:50px;margin-right:0px;font-size:120%; font-family:Arial Black;",  
                                  "Name your Model:  "),
                           column(4,style="display: inline-block;margin-top:45px; font-size:100%; font-family:Times New Roman;",
                                  textInput("Model_name", NULL, "Enter your model name")),
                           column(5,style="margin-top:50px;font-size:120%; font-family:Arial Black;",  
                                  "Describe your model:  "),
                           column(4,style="display: inline-block;height: 200px;margin-top:45px; font-size:100%; font-family:Times New Roman;",
                                  textAreaInput("Model_description", NULL, "Opcional")),
                           column(12,style = "text-align: right;",
                                  actionButton("MDbutton", "Submit", icon("paper-plane"), 
                                               style="color: #fff; background-color: #337ab7; border-color: #2e6da4")))
                  
                  
                )),
                #Interface do segundo menu (Dependentes) -> Escolha das vari?veis
                tabItem("Gee_List", box(id = "Model_1",solidHeader = TRUE, width = "1000px",
                                        column(5,style="margin-top:0px;font-size:120%; font-family:Arial Black;",  
                                               textOutput('values')),
                                        column(10,style="margin-top:10px;font-size:80%; font-family:Arial;",  
                                               textOutput('des')),
                                        column(12,style="margin-top:10px;font-size:100%; font-family:Arial;",  
                                               htmlOutput('vr')),
                                        column(12,style="margin-top:10px;font-size:100%; font-family:Arial;",  
                                               htmlOutput('co')),
                                        column(12,style="margin-top:10px;font-size:100%; font-family:Arial;",  
                                               htmlOutput('id')),
                                        column(12,style="margin-top:10px;font-size:100%; font-family:Arial;",  
                                               htmlOutput('family')),
                                        column(12,style="margin-top:10px;font-size:100%; font-family:Arial;",  
                                               htmlOutput('cs')),
                                        column(12, style = "text-align: right; margin-top: -120px; font-size:100%; font-family:Times New Roman;", 
                                               prettyCheckbox(
                                                 inputId = "Select_Model", label = NULL, icon = icon("thumbs-up"), 
                                                 status = "default", shape = "curve", animation = "pulse"
                                               ))),
                        
                        box(id = "Model_2",solidHeader = TRUE, width = "2000px",
                            column(5,style="margin-top:0px;font-size:120%; font-family:Arial Black;",  
                                   textOutput('values_2')),
                            column(10,style="margin-top:10px;font-size:80%; font-family:Arial;",  
                                   textOutput('des_2')),
                            column(12,style="margin-top:10px;font-size:100%; font-family:Arial;",  
                                   htmlOutput('vr_2')),
                            column(12,style="margin-top:10px;font-size:100%; font-family:Arial;",  
                                   htmlOutput('co_2')),
                            column(12,style="margin-top:10px;font-size:100%; font-family:Arial;",  
                                   htmlOutput('id_2')),
                            column(12,style="margin-top:10px;font-size:100%; font-family:Arial;",  
                                   htmlOutput('family_2')),
                            column(12,style="margin-top:10px;font-size:100%; font-family:Arial;",  
                                   htmlOutput('cs_2')),
                            column(12, style = "text-align: right; margin-top: -70px; font-size:100%; font-family:Times New Roman;", 
                                   prettyCheckbox(
                                     inputId = "Select_Model_2", label = NULL, icon = icon("thumbs-up"), 
                                     status = "default", shape = "curve", animation = "pulse"
                                   ))),
                        box(id = "Model_3",solidHeader = TRUE, width = "2000px",
                            column(5,style="margin-top:0px;font-size:120%; font-family:Arial Black;",  
                                   textOutput('values_3')),
                            column(10,style="margin-top:10px;font-size:80%; font-family:Arial;",  
                                   textOutput('des_3')),
                            column(12,style="margin-top:10px;font-size:100%; font-family:Arial;",  
                                   htmlOutput('vr_3')),
                            column(12,style="margin-top:10px;font-size:100%; font-family:Arial;",  
                                   htmlOutput('co_3')),
                            column(12,style="margin-top:10px;font-size:100%; font-family:Arial;",  
                                   htmlOutput('id_3')),
                            column(12,style="margin-top:10px;font-size:100%; font-family:Arial;",  
                                   htmlOutput('family_3')),
                            column(12,style="margin-top:10px;font-size:100%; font-family:Arial;",  
                                   htmlOutput('cs_3')),
                            column(12, style = "text-align: right; margin-top: -80px; font-size:100%; font-family:Times New Roman;", 
                                   prettyCheckbox(
                                     inputId = "Select_Model_3", label = NULL, icon = icon("thumbs-up"), 
                                     status = "default", shape = "curve", animation = "pulse"
                                   ))),
                        div(style="display:inline-block;float:right;padding:10px;font-size:80%;",actionButton("Analysisbutton", "Run", icon("play"), 
                                                                      style="color: #fff; background-color: #337ab7; border-color: #2e6da4")),
                        div(style="display:inline-block;float:right;padding:10px;font-size:80%;",actionButton("Editbutton", "Edit", icon("pencil"), 
                                                                      style="color: #fff; background-color: #337ab7; border-color: #2e6da4")),
                        div(style="display:inline-block;float:right;padding:10px;font-size:80%;",actionButton("ADDbutton", "Add", icon("plus"), 
                                                                                                              style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))
                    ),
                #Interface do segundo menu (Dependentes) -> Resultados Gr?ficos
                tabItem('Resultados_Graficos',
                        plotOutput("plotOut")%>% withSpinner(color="#0dc5c1"), downloadButton("Download_Plot", label = "Download"), plotOutput("plotOut2")%>% withSpinner(color="#0dc5c1"),downloadButton("Download_Plot_2", label = "Download")),
                
                #Interface do segundo menu (Dependentes) -> Resultados Estat?sticos
                tabItem('Resultados_Estatisticos', sidebarLayout(
                    sidebarPanel(
                        checkboxGroupInput("Resultados", "Show Results:",c("Prediction 1","Prediction 2","Statistical Overall"), selected =c("Prediction 1","Prediction 2","Statistical Overall"))),
                    mainPanel(
                        tags$label(h2("Statistical Results")),
                        verbatimTextOutput("OutputDependentes"),
                        
                    ))),
                #Interface do terceiro menu (Independentes) -> Escolha das vari?veis
                tabItem("Escolher_Variaveis_Ind", h4("Choose your independent predictions"),sidebarLayout(
                    sidebarPanel(
                        selectInput("IPred1","Prediction 1",""),
                        selectInput("IResult1","Result 1",""),
                        checkboxInput("Direction_1", "Direction_1", value = T),
                        selectInput("IPred2","Prediction 2",""),
                        selectInput("IResult2","Result 2",""),
                        checkboxInput("Direction_2", "Direction_2", value = T),
                        actionButton("Isubmitbutton", "Submit", class = "btn btn-primary")),
                    mainPanel(
                        DT::dataTableOutput("Independentes"), style = "height:800px;overflow-x: scroll;"
                    ))),
                #Interface do terceiro menu (Independentes) -> Resultados Gr?ficos
                    tabItem('Resultados_Graficos_Ind', 
                            plotOutput("IplotOut") %>% withSpinner(color="#0dc5c1"), downloadButton("IDownload_Plot", label = "Download"), plotOutput("IplotOut2") %>% withSpinner(color="#0dc5c1"),downloadButton("IDownload_Plot_2", label = "Download")),
                
                #Interface do terceiro menu (Independentes) -> Resultados Estat?sticos 
                    tabItem('Resultados_Estatisticos_Ind', sidebarLayout(
                        sidebarPanel(
                            checkboxGroupInput("IResultados", "Show Results:",c("Prediction 1","Prediction 2","Statistical Overall"), selected =c("Prediction 1","Prediction 2","Statistical Overall"))),
                        mainPanel(
                            tags$label(h2("Statistical Results")),
                            verbatimTextOutput("OutputIndependentes"),
                            ))),
                
                #Menu informativo
                    tabItem("Sobre",titlePanel("About"),
                            br(),
                            div(includeMarkdown("About.Rmd"), 
                                align="justify")
                     
                    )
                
            )
        )
    )
)
server <- function(input, output, session) {
    
    #Tabela resultante do dataset importado pelo utilizador
    output$contents <- renderDataTable({
        if (input$submitbutton > 0) {
            req(input$file1)
            ext <- tools::file_ext(input$file1$name)
            if(ext == "csv"){
              df <- read.csv(input$file1$datapath,header = input$header, sep = input$sep,
                             quote = input$quote)
            }
            else{
              df <- read_excel(input$file1$datapath)
            }
            
            if(input$disp == "head"){
                observeEvent(df, {
                  updateSelectInput(session, "VR", choices = colnames(df))
                })
                observeEvent(df, {
                  updateCheckboxGroupInput(session, "Variables", choices = colnames(df))
                })
                observeEvent(df, {
                  updateSelectInput(session, "ID", choices = colnames(df))
                })
                return(head(df))
                
            }
            else{
                observeEvent(df, {
                  updateSelectInput(session, "VR", choices = colnames(df))
                })
                observeEvent(df, {
                  updateCheckboxGroupInput(session, "Variables", choices = colnames(df))
                })
                observeEvent(df, {
                  updateSelectInput(session, "ID", choices = colnames(df))
                })
                return(df)
            }
            isolate(datasetInput())
        }
    })
    observeEvent(input$ADDbutton, {
      updateTabsetPanel(session, 'sidebarmenu',
                        selected = "Select")
    })
    
    #Table with QIC values
      newtab <- reactive({
          req(input$file1)
          ext <- tools::file_ext(input$file1$name)
          if(ext == "csv"){
            df <- read.csv(input$file1$datapath,header = input$header, sep = input$sep,
                           quote = input$quote)
          }
          else{
            df <- read_excel(input$file1$datapath)
          }
          if (input$QIC_Show){
            s = ""
            for (x in 1:length(input$Variables)){
              if (x == 1){
                s = paste('(',s,input$Variables[1])
                
              }
              else if (x == length(input$Variables)){
                s = paste(s,'+',input$Variables[x], ')')
                
              }
              else{
                s = paste(s,'+',input$Variables[x])
                
              }
            }
            if ((input$VRbutton > 0) &  (input$Covariablebutton > 0) & (input$IDbutton >0) & (input$Familybutton >0)){
              form = as.formula(paste(input$VR, '~' , s, collapse = ""))
              identification = input$ID
              gee_Ind = geeglm(form,data = df,id = df[[identification]],family = input$Family,corstr = "independence")
              qic_ind = MESS::QIC(gee_Ind)[1]
              qicu_ind = MESS::QIC(gee_Ind)[2]
              names(qic_ind) <- NULL
              names(qicu_ind) <- NULL
              gee_Exch = geeglm(formula = form,data = df,id = df[[identification]],family = input$Family,corstr = "exchangeable")
              qic_Exch = MESS::QIC(gee_Exch)[1]
              qicu_Exch = MESS::QIC(gee_Exch)[2]
              names(qic_Exch) <- NULL
              names(qicu_Exch) <- NULL
              gee_Ar = geeglm(formula = form,data = df,id = df[[identification]],family = input$Family,corstr = "ar1")
              qic_Ar = MESS::QIC(gee_Ar)[1]
              qicu_Ar = MESS::QIC(gee_Ar)[2]
              names(qic_Ar) <- NULL
              names(qicu_Ar) <- NULL
              #gee_Un = geeglm(formula = form,data = df,id = df[[identification]],family = input$Family,corstr = "unstructured")
              #qic_Un = MESS::QIC(gee_Un)
              #names(qic_Un) <- NULL
              nwe <- data.frame(QIC = c(qic_ind,qic_Exch,qic_Ar), 
                                QICu = c(qicu_ind,qicu_Exch,qicu_Ar))
              rownames(nwe) <- c('Independence','Exchangeable','AR-1')
              return(nwe)
            }
            }
          
        })
      output$QIC_Table <- DT::renderDataTable(datatable(newtab(),options = list(dom = 't')))
      
      #Functions to print the output boxes:
      result_s <- function(co){
        s = ""
        for (x in 1:length(co)){
          if (x == 1){
            s = paste(s,co[1])
            
          }
          else if (x == length(co)){
            s = paste(s,'+',co[x])
            
          }
          else{
            s = paste(s,'+',co[x])
            
          }
        }
        return(s)
      }
      
      
      
      #Values for 1st model
      boxvalues1 <- reactiveValues(mn = NULL, md = NULL, vr = NULL, co = NULL,
                                   id = NULL, fm = NULL,  cs = NULL)
      
      boxvalues2 <- reactiveValues(mn = NULL, md = NULL, vr = NULL, co = NULL,
                                   id = NULL, fm = NULL,  cs = NULL)
      
      boxvalues3 <- reactiveValues(mn = NULL, md = NULL, vr = NULL, co = NULL,
                                   id = NULL, fm = NULL,  cs = NULL)
      
      lastboxcreated <- reactiveValues(boxname= NULL)
      
      observeEvent(input$MDbutton,{
        if (input$MDbutton == 1){
          shinyjs::hide(id = "Model_2")
          shinyjs::hide(id = "Model_3")
          boxvalues1$mn <- input$Model_name
          boxvalues1$md <- input$Model_description
          boxvalues1$vr <- input$VR
          boxvalues1$co <- input$Variables
          boxvalues1$id <- input$ID
          boxvalues1$fm <- input$Family
          boxvalues1$cs <- input$CS
          output$values <- renderText({
            input$Model_name
          })
          output$des <- renderText({
            input$Model_description
          })
          output$vr <- renderText({
            paste('<b>Response Variable: ',"</b>", input$VR)
          })
          output$co <- renderText({
            paste('<b>Covariates: ' ,"</b>", result_s(input$Variables) )
          })
          output$id <- renderText({
            paste('<b>ID: ' , "</b>", input$ID)
          })
          output$family <- renderText({
            paste('<b>Family: ', "</b>", input$Family)
          })
          output$cs <- renderText({
            paste('<b>Correlation Structure: ' ,"</b>", input$CS)
          })
          updatePrettyCheckbox(session,"Select_Model", value = TRUE)
          lastboxcreated$boxname = 'box1'
        }
        
      })
      
      
      #ADD button functions
      observeEvent(input$ADDbutton,{
        if (input$ADDbutton == 1){
          shinyjs::show("Model_2")
          output$values <- renderText({
            boxvalues1$mn
          })
          output$des <- renderText({
            boxvalues1$md
          })
          output$vr <- renderText({
            paste('<b>Response Variable: ',"</b>", boxvalues1$vr)
          })
          output$co <- renderText({
            paste('<b>Covariates: ' ,"</b>", result_s(boxvalues1$co) )
          })
          output$id <- renderText({
            paste('<b>ID: ' , "</b>", boxvalues1$id)
          })
          output$family <- renderText({
            paste('<b>Family: ', "</b>", boxvalues1$fm)
          })
          output$cs <- renderText({
            paste('<b>Correlation Structure: ' ,"</b>", boxvalues1$cs)
          })
          output$values_2 <- renderText({
            input$Model_name
          })
          output$des_2 <- renderText({
            input$Model_description
          })
          output$vr_2 <- renderText({
            paste('<b>Response Variable: ',"</b>", input$VR)
          })
          output$co_2 <- renderText({
            paste('<b>Covariates: ' ,"</b>", result_s(input$Variables) )
          })
          output$id_2 <- renderText({
            paste('<b>ID: ' , "</b>", input$ID)
          })
          output$family_2 <- renderText({
            paste('<b>Family: ', "</b>", input$Family)
          })
          output$cs_2 <- renderText({
            paste('<b>Correlation Structure: ' ,"</b>", input$CS)
          })
          updatePrettyCheckbox(session,"Select_Model", value = FALSE)
          updatePrettyCheckbox(session,"Select_Model_2", value = TRUE)
          lastboxcreated$boxname = 'box2'
        }
        else if(input$ADDbutton == 2){
          shinyjs::show("Model_3")
          shinyjs::disable("ADDbutton")
          boxvalues2$mn <- input$Model_name
          boxvalues2$md <- input$Model_description
          boxvalues2$vr <- input$VR
          boxvalues2$co <- input$Variables
          boxvalues2$id <- input$ID
          boxvalues2$fm <- input$Family
          boxvalues2$cs <- input$CS
          output$values_2 <- renderText({
            boxvalues2$mn
          })
          output$des_2 <- renderText({
            boxvalues2$md
          })
          output$vr_2 <- renderText({
            paste('<b>Response Variable: ',"</b>", boxvalues2$vr)
          })
          output$co_2 <- renderText({
            paste('<b>Covariates: ' ,"</b>", result_s(boxvalues2$co) )
          })
          output$id_2 <- renderText({
            paste('<b>ID: ' , "</b>", boxvalues2$id)
          })
          output$family_2 <- renderText({
            paste('<b>Family: ', "</b>", boxvalues2$fm)
          })
          output$cs_2 <- renderText({
            paste('<b>Correlation Structure: ' ,"</b>", boxvalues2$cs)
          })
          output$values_3 <- renderText({
            input$Model_name
          })
          output$des_3 <- renderText({
            input$Model_description
          })
          output$vr_3 <- renderText({
            paste('<b>Response Variable: ',"</b>", input$VR)
          })
          output$co_3 <- renderText({
            paste('<b>Covariates: ' ,"</b>", result_s(input$Variables) )
          })
          output$id_3 <- renderText({
            paste('<b>ID: ' , "</b>", input$ID)
          })
          output$family_3 <- renderText({
            paste('<b>Family: ', "</b>", input$Family)
          })
          output$cs_3 <- renderText({
            paste('<b>Correlation Structure: ' ,"</b>", input$CS)
          })
          updatePrettyCheckbox(session,"Select_Model_2", value = FALSE)
          updatePrettyCheckbox(session,"Select_Model_3", value = TRUE)
          lastboxcreated$boxname = 'box3'
          
        }
      })
      
      observe({
        if((input$Select_Model)|| (input$Select_Model_2) || (input$Select_Model_3)){
          if((input$Select_Model) && (input$Select_Model_2) && (input$Select_Model_3)){
            shinyjs::disable("Editbutton")
          }
          else if(((input$Select_Model) && (input$Select_Model_2)) || 
                  ((input$Select_Model_3) && (input$Select_Model))|| ((input$Select_Model_2) && (input$Select_Model_3))){
            shinyjs::disable("Editbutton")
          }
          else{
            shinyjs::enable("Editbutton")
          }
        }
        else {
          shinyjs::disable("Editbutton")
        }
      })
      
      lastbox <- reactiveValues(lastBox = NULL, currentBox = NULL)
      lastboxcreated <- reactiveValues(boxname= NULL)
      
      observeEvent(input$Select_Model, {
        if (input$Select_Model) {
          lastbox$lastBox = lastbox$currentBox
          lastbox$currentBox = "box1"
          print(lastbox$lastBox)
        }
      })
      observeEvent(input$Select_Model_2, {
        if (input$Select_Model_2) {
          lastbox$lastBox = lastbox$currentBox
          lastbox$currentBox = "box2"
          print(lastbox$lastBox)
        }
      })
      observeEvent(input$Select_Model_3, {
        if (input$Select_Model_3) {
          lastbox$lastBox = lastbox$currentBox
          lastbox$currentBox = "box3"
          print(lastbox$lastBox)
        }
      })
      
      #Edit button functions
      observeEvent(input$Editbutton,{
        updateTabsetPanel(session, 'sidebarmenu',
                          selected = "Select")
        if (input$Editbutton == 1){
          boxvalues3$mn <- input$Model_name
          boxvalues3$md <- input$Model_description
          boxvalues3$vr <- input$VR
          boxvalues3$co <- input$Variables
          boxvalues3$id <- input$ID
          boxvalues3$fm <- input$Family
          boxvalues3$cs <- input$CS
        }
        if (input$Select_Model){
          output$values_2 <- renderText({
            boxvalues2$mn
          })
          output$des_2 <- renderText({
            boxvalues2$md
          })
          output$vr_2 <- renderText({
            paste('<b>Response Variable: ',"</b>", boxvalues2$vr)
          })
          output$co_2 <- renderText({
            paste('<b>Covariates: ' ,"</b>", result_s(boxvalues2$co) )
          })
          output$id_2 <- renderText({
            paste('<b>ID: ' , "</b>", boxvalues2$id)
          })
          output$family_2 <- renderText({
            paste('<b>Family: ', "</b>", boxvalues2$fm)
          })
          output$cs_2 <- renderText({
            paste('<b>Correlation Structure: ' ,"</b>", boxvalues2$cs)
          })
          print(boxvalues3$mn)
          output$values_3 <- renderText({
            boxvalues3$mn
          })
          output$des_3 <- renderText({
            boxvalues3$md
          })
          output$vr_3 <- renderText({
            paste('<b>Response Variable: ',"</b>", boxvalues3$vr)
          })
          output$co_3 <- renderText({
            paste('<b>Covariates: ' ,"</b>", result_s(boxvalues3$co) )
          })
          output$id_3 <- renderText({
            paste('<b>ID: ' , "</b>", boxvalues3$id)
          })
          output$family_3 <- renderText({
            paste('<b>Family: ', "</b>", boxvalues3$fm)
          })
          output$cs_3 <- renderText({
            paste('<b>Correlation Structure: ' ,"</b>", boxvalues3$cs)
          })
          #BOX1
          output$values <- renderText({
            input$Model_name
          })
          output$des <- renderText({
            input$Model_description
          })
          output$vr <- renderText({
            paste('<b>Response Variable: ',"</b>", input$VR)
          })
          output$co <- renderText({
            paste('<b>Covariates: ' ,"</b>", result_s(input$Variables) )
          })
          output$id <- renderText({
            paste('<b>ID: ' , "</b>", input$ID)
          })
          output$family <- renderText({
            paste('<b>Family: ', "</b>", input$Family)
          })
          output$cs <- renderText({
            paste('<b>Correlation Structure: ' ,"</b>", input$CS)
          })
          boxvalues1$mn <- input$Model_name
          boxvalues1$md <- input$Model_description
          boxvalues1$vr <- input$VR
          boxvalues1$co <- input$Variables
          boxvalues1$id <- input$ID
          boxvalues1$fm <- input$Family
          boxvalues1$cs <- input$CS
        }
        else if(input$Select_Model_2){
          output$values <- renderText({
            boxvalues1$mn
          })
          output$des <- renderText({
            boxvalues1$md
          })
          output$vr <- renderText({
            paste('<b>Response Variable: ',"</b>", boxvalues1$vr)
          })
          output$co <- renderText({
            paste('<b>Covariates: ' ,"</b>", result_s(boxvalues1$co) )
          })
          output$id <- renderText({
            paste('<b>ID: ' , "</b>", boxvalues1$id)
          })
          output$family <- renderText({
            paste('<b>Family: ', "</b>", boxvalues1$fm)
          })
          output$cs <- renderText({
            paste('<b>Correlation Structure: ' ,"</b>", boxvalues1$cs)
          })
          output$values_3 <- renderText({
            boxvalues3$mn
          })
          output$des_3 <- renderText({
            boxvalues3$md
          })
          output$vr_3 <- renderText({
            paste('<b>Response Variable: ',"</b>", boxvalues3$vr)
          })
          output$co_3 <- renderText({
            paste('<b>Covariates: ' ,"</b>", result_s(boxvalues3$co) )
          })
          output$id_3 <- renderText({
            paste('<b>ID: ' , "</b>", boxvalues3$id)
          })
          output$family_3 <- renderText({
            paste('<b>Family: ', "</b>", boxvalues3$fm)
          })
          output$cs_3 <- renderText({
            paste('<b>Correlation Structure: ' ,"</b>", boxvalues3$cs)
          })
          #BOX2
          output$values_2 <- renderText({
            input$Model_name
          })
          output$des_2 <- renderText({
            input$Model_description
          })
          output$vr_2 <- renderText({
            paste('<b>Response Variable: ',"</b>", input$VR)
          })
          output$co_2 <- renderText({
            paste('<b>Covariates: ' ,"</b>", result_s(input$Variables) )
          })
          output$id_2 <- renderText({
            paste('<b>ID: ' , "</b>", input$ID)
          })
          output$family_2 <- renderText({
            paste('<b>Family: ', "</b>", input$Family)
          })
          output$cs_2 <- renderText({
            paste('<b>Correlation Structure: ' ,"</b>", input$CS)
          })
          boxvalues2$mn <- input$Model_name
          boxvalues2$md <- input$Model_description
          boxvalues2$vr <- input$VR
          boxvalues2$co <- input$Variables
          boxvalues2$id <- input$ID
          boxvalues2$fm <- input$Family
          boxvalues2$cs <- input$CS
        }
        else if (input$Select_Model_3){
          output$values_2 <- renderText({
            boxvalues2$mn
          })
          output$des_2 <- renderText({
            boxvalues2$md
          })
          output$vr_2 <- renderText({
            paste('<b>Response Variable: ',"</b>", boxvalues2$vr)
          })
          output$co_2 <- renderText({
            paste('<b>Covariates: ' ,"</b>", result_s(boxvalues2$co) )
          })
          output$id_2 <- renderText({
            paste('<b>ID: ' , "</b>", boxvalues2$id)
          })
          output$family_2 <- renderText({
            paste('<b>Family: ', "</b>", boxvalues2$fm)
          })
          output$cs_2 <- renderText({
            paste('<b>Correlation Structure: ' ,"</b>", boxvalues2$cs)
          })
          output$values <- renderText({
            boxvalues1$mn
          })
          output$des <- renderText({
            boxvalues1$md
          })
          output$vr <- renderText({
            paste('<b>Response Variable: ',"</b>", boxvalues1$vr)
          })
          output$co <- renderText({
            paste('<b>Covariates: ' ,"</b>", result_s(boxvalues1$co) )
          })
          output$id <- renderText({
            paste('<b>ID: ' , "</b>", boxvalues1$id)
          })
          output$family <- renderText({
            paste('<b>Family: ', "</b>", boxvalues1$fm)
          })
          output$cs <- renderText({
            paste('<b>Correlation Structure: ' ,"</b>", boxvalues1$cs)
          })
          #BOX3
          output$values_3 <- renderText({
            input$Model_name
          })
          output$des_3 <- renderText({
            input$Model_description
          })
          output$vr_3 <- renderText({
            paste('<b>Response Variable: ',"</b>", input$VR)
          })
          output$co_3 <- renderText({
            paste('<b>Covariates: ' ,"</b>", result_s(input$Variables) )
          })
          output$id_3 <- renderText({
            paste('<b>ID: ' , "</b>", input$ID)
          })
          output$family_3 <- renderText({
            paste('<b>Family: ', "</b>", input$Family)
          })
          output$cs_3 <- renderText({
            paste('<b>Correlation Structure: ' ,"</b>", input$CS)
          })
          boxvalues3$mn <- input$Model_name
          boxvalues3$md <- input$Model_description
          boxvalues3$vr <- input$VR
          boxvalues3$co <- input$Variables
          boxvalues3$id <- input$ID
          boxvalues3$fm <- input$Family
          boxvalues3$cs <- input$CS
        }
        
      })

      
    #Dataset gerado apenas com as colunas selecionadas pelo utilizador
    output$Dependentes <- renderDataTable({
        req(input$file1)
        df <- read.csv(input$file1$datapath,header = input$header, sep = input$sep,
                               quote = input$quote)
      
        if (input$Dsubmitbutton > 0) {
                depen <- df[,c(input$DPred1, input$DPred2, input$DResult)]
                return(depen)
                isolate(datasetInput())
                }})
    
    #Data das vari?veis dependentes preparado para utilizar no package COMP2ROC
    data2 = reactive ({
            req (input$file1 )
            inFile2 = input$file1
            df2 = Comp2ROC:: read.file (inFile2$datapath, header.status = input$header, sep = input$sep,
                                            '.',input$DPred1,input$Direction1,input$DPred2, input$Direction2,
                                            input$DResult, T)
            return ( df2 ) })
    
    #Representa??es gr?ficas das curvas ROC emp?ricas e ?reas abaixo da curva e respectivo download    
    output$plotOut <- renderPlot({
            par(mfrow = c(1,2))
            roc.curves.boot_v2(data2(),1000, 0.05,name="Results",input$DPred1,input$DPred2,TRUE)
                })
    output$Download_Plot <-downloadHandler(
            filename=function(){
            paste("graph","png",sep=".")
            },content=function(file){
                png(file, height = 500, width = 1000)
                par(mfrow = c(1,2))
                roc.curves.boot_v2(data2(),1000, 0.05,name="Results",input$DPred1,input$DPred2,TRUE)
                dev.off() 
                }
            )
    #Objeto com os resultados estaisticos gerados.
    result <- reactive ({
            result = Comp2ROC :: roc.curves.boot(data2(),1000, 0.05,name="Results",input$DPred1,input$DPred2,TRUE)
            return(result)})
    
    #Reprenta??o gr?fica do histograma e Quantis.        
    output$plotOut2 <- renderPlot({
            result()
                
            })
    output$Download_Plot_2 <-downloadHandler(
            filename=function(){
                paste("hist","png",sep=".")
                },content=function(file){
                png(file, height = 500, width = 1000)
                Comp2ROC :: roc.curves.boot(data2(),1000, 0.05,name="Results",input$DPred1,input$DPred2,TRUE)
                dev.off() 
                }
            )
        # Representa??o dos resultados estatisticos das vari?veis selecionadas    
        output$OutputDependentes <- renderPrint({
                result <- result()
                if (all(c("Prediction 1", "Prediction 2", "Statistical Overall") %in% input$Resultados)) {
                    rocboot.summary(result,input$DPred1,input$DPred2)
                } else if ("Prediction 2" %in% input$Resultados) {
                    cat("\n")
                    cat("------------------------------------------------\n")
                    cat(input$DPred2, "\n")
                    cat("------------------------------------------------\n")
                    cat("Area:                               ", result$Area1, "\n")
                    cat("Standard Error:                     ", result$SE1, "\n")
                    cat("Area through Trapezoidal Method:    ", result$TrapArea1, "\n")
                    cat("CI Upper bound (Percentil Method):  ", result$ICUB1, "\n")
                    cat("CI Lower bound (Percentil Method):  ", result$ICLB1, "\n")
                } else if ("Prediction 1" %in% input$Resultados) {
                    cat("\n")
                    cat("------------------------------------------------\n")
                    cat(input$DPred1, "\n")
                    cat("------------------------------------------------\n")
                    cat("Area:                               ", result$Area1, "\n")
                    cat("Standard Error:                     ", result$SE1, "\n")
                    cat("Area through Trapezoidal Method:    ", result$TrapArea1, "\n")
                    cat("CI Upper bound (Percentil Method):  ", result$ICUB1, "\n")
                    cat("CI Lower bound (Percentil Method):  ", result$ICLB1, "\n")
                } else if ("Statistical Overall" %in% input$Resultados){
                    cat("\n")
                    cat("Correlation Coefficient between areas:  ", result$CorrCoef, "\n")
                    cat("\n")
                    cat("TEST OF DIFFERENCES\n")
                    cat("Z stats:  ", result$zstats,"\n")
                    cat("p-value:  ", result$pvalue1,"\n")
                    cat("\n")
                    cat("Sum of Global Areas Differences (TS):  ", result$diff, "\n")
                    cat("CI Upper bound (Percentil Method):  ", result$ICUBDiff, "\n")
                    cat("CI Lower bound (Percentil Method):  ", result$ICLBDiff, "\n")
                    cat("\n")
                    cat("Number of Crossings:  ", result$nCross, "\n")
                }
                
            })
            
            
        #Data set das com as vari?veis independentes selecionadas
            output$Independentes <- renderDataTable({
                req(input$file1)
                df <- read.csv(input$file1$datapath,header = input$header, sep = input$sep,
                               quote = input$quote)
                if (input$Isubmitbutton > 0) {
                    ind <- df[,c(input$IPred1, input$IResult1, input$IPred2, input$IResult2)]
                    return(ind)
                    isolate(datasetInput())
                }
                
            })
        #Data gerada e preparada para utilizar no package COMP2ROC    
            data3 = reactive ({
                req (input$file1 )
                inFile3 = input$file1
                df3 = Comp2ROC::read.file ( inFile3$datapath , header.status = input$header , sep = input$sep , ".", input$IPred1 ,
                                            input$Direction_1 , input$IPred2 , input$Direction_2 , input$IResult1 , FALSE , input$IResult2 )
                return ( df3 ) })
            
            #Representa??es gr?ficas das curvas ROC emp?ricas e ?reas abaixo da curva e respectivo download
            output$IplotOut <- renderPlot({
                par(mfrow = c(1,2))
                roc.curves.boot_v2(data3(),1000, 0.05,name="Results",input$IPred1,input$IPred2,FALSE)
                
                
            })
            output$IDownload_Plot <-downloadHandler(
                filename=function(){
                    paste("graph","png",sep=".")
                },content=function(file){
                    png(file, height = 500, width = 1000)
                    par(mfrow = c(1,2))
                    roc.curves.boot_v2(data3(),1000, 0.05,name="Results",input$IPred1,input$IPred2,FALSE)
                    dev.off() 
                }
            )
            #Objeto criado com os resultados estatisticos obtidos
            Ind_result <- reactive ({
                result = Comp2ROC :: roc.curves.boot(data3(),1000, 0.05,name="Results",input$IPred1,input$IPred2,FALSE)
                return(result)})
            
            #Representa??o gr?fica do histograma e Quantis
            output$IplotOut2 <- renderPlot({
                Ind_result()
                
            })
            output$IDownload_Plot_2 <-downloadHandler(
                filename=function(){
                    paste("hist","png",sep=".")
                },content=function(file){
                    png(file, height = 500, width = 1000)
                    Comp2ROC :: roc.curves.boot(data3(),1000, 0.05,name="Results",input$IPred1,input$IPred2,FALSE)
                    dev.off() 
                }
            )
            #Resultados estatisticos das vari?veis independentes selecionadas
            output$OutputIndependentes <- renderPrint({
                result <- Ind_result()
                if (all(c("Prediction 1", "Prediction 2", "Statistical Overall") %in% input$IResultados)) {
                    rocboot.summary(result,input$IPred1,input$IPred2)
                } else if ("Prediction 2" %in% input$IResultados) {
                    cat("\n")
                    cat("------------------------------------------------\n")
                    cat(input$IPred2, "\n")
                    cat("------------------------------------------------\n")
                    cat("Area:                               ", result$Area1, "\n")
                    cat("Standard Error:                     ", result$SE1, "\n")
                    cat("Area through Trapezoidal Method:    ", result$TrapArea1, "\n")
                    cat("CI Upper bound (Percentil Method):  ", result$ICUB1, "\n")
                    cat("CI Lower bound (Percentil Method):  ", result$ICLB1, "\n")
                } else if ("Prediction 1" %in% input$IResultados) {
                    cat("\n")
                    cat("------------------------------------------------\n")
                    cat(input$IPred1, "\n")
                    cat("------------------------------------------------\n")
                    cat("Area:                               ", result$Area1, "\n")
                    cat("Standard Error:                     ", result$SE1, "\n")
                    cat("Area through Trapezoidal Method:    ", result$TrapArea1, "\n")
                    cat("CI Upper bound (Percentil Method):  ", result$ICUB1, "\n")
                    cat("CI Lower bound (Percentil Method):  ", result$ICLB1, "\n")
                } else if ("Statistical Overall" %in% input$IResultados){
                    cat("\n")
                    cat("Correlation Coefficient between areas:  ", result$CorrCoef, "\n")
                    cat("\n")
                    cat("TEST OF DIFFERENCES\n")
                    cat("Z stats:  ", result$zstats,"\n")
                    cat("p-value:  ", result$pvalue1,"\n")
                    cat("\n")
                    cat("Sum of Global Areas Differences (TS):  ", result$diff, "\n")
                    cat("CI Upper bound (Percentil Method):  ", result$ICUBDiff, "\n")
                    cat("CI Lower bound (Percentil Method):  ", result$ICLBDiff, "\n")
                    cat("\n")
                    cat("Number of Crossings:  ", result$nCross, "\n")
                }
                
            })
    
}
shinyApp(ui = ui, server = server)