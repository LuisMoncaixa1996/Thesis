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
                                 menuSubItem("Dataset changes", tabName = "DataChange", icon = icon("database"))),
                                 
                        menuItem('Escolha das variaveis', tabName = "Select", icon = icon("edit")),
                        # Menus de compara??o entre curvas dependendo do tipo de vari?vel
                        menuItem("GEE Models", icon = icon("clipboard"),
                                 tabName = "Gee_List"),
                        
                        menuItem("Validation and Results", tabName = "Results",
                                 menuSubItem("ROC Analysis", tabName = "ROC"),
                                 menuSubItem("GEE Results", tabName = "GEE_results")),
                        
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
                tabItem('DataChange', tabBox(
                  title = "Dataset Changes", id = 'Changes', height = "800px", width = "600px",
                  tabPanel("Columns Names", column(12,style="height: 40px;font-size:100%; font-family:Arial Black;",  
                                                   "Columns: "),fluidPage(tweaks,fluidRow(column(12, list(tags$div(align = 'left', 
                                                                                                                   class = 'multicol', 
                                                                                                                   prettyRadioButtons(inputId  = 'Columns', 
                                                                                                                                      label    = NULL, 
                                                                                                                                      choices  = ""
                                                                                                                                      ))))
                                                                                          
                                                   )),
                           column(6,style="margin-top:-100px;font-size:120%; font-family:Arial Black;",  
                                  "New column name:   "),
                           column(4,style="margin-top:-100px;margin-left:200px; font-size:10px; font-family:Times New Roman;",
                                  textAreaInput("NewColumName", NULL,height = '40px', width = '1000px')),
                           column(12,style = "text-align: right;margin-left:-200px",
                                  actionButton("Colbutton", "Submit", icon("paper-plane"), 
                                               style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))),
                  tabPanel("Factors",column(12,style="height: 40px;font-size:100%; font-family:Arial Black;",  
                                            "Columns: "),fluidPage(tweaks,fluidRow(column(12, list(tags$div(align = 'left', 
                                                                                        class = 'multicol', 
                                                                                        checkboxGroupInput(inputId  = 'Facol', 
                                                                                                           label    = NULL, 
                                                                                                           choices  = c(),
                                                                                                           inline   = FALSE))))
                                                               
                  )),column(12,style = "text-align: right;",
                            actionButton("Facbtn", "Submit", icon("paper-plane"), 
                                         style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))),
                  tabPanel("Data Visualization", mainPanel(
                    DT::dataTableOutput("newdata"), style = "height:1000px;width:1000px"
                  ) )
                    
                )),
                tabItem("Select", tabBox(
                  title = "Variable Selection for GEE Model",
                  id = "selectvariable", height = "800px", width = "1000px",
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
                tabItem("ROC",tabBox(
                  title = "Validation: ROC Analysis",
                  id = "ROC_Analysis", height = "800px", width = "1000px",
                  tabPanel("Data Split", column(4,style="display: inline-block;margin-top:50px;margin-right:0px;font-size:120%; font-family:Arial Black;",  
                                           "Define your data split for ROC validation: "),
                        column(8,style="display: inline-block;margin-top:45px; font-size:100%; font-family:Times New Roman;",
                               selectInput("Split_data",NULL,c('Train: 70% - Test: 30%','Train: 75% - Test: 25%','Train: 80% - Test: 20%',
                                                       'Default'), selected = "Default", width = "600px")),
                        column(12,style = "text-align: right; margin-top: 50px",
                               actionButton("Splitbtn", "Submit", icon("paper-plane"), 
                                            style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))),
                  tabPanel("ROC: Graph Visualization",
                           plotOutput("ROCplot")%>% withSpinner(color="#0dc5c1"), downloadButton("Download_Plot", label = "Download"),
                           withSpinner(plotOutput("ROCplot2"),color="#0dc5c1"),downloadButton("Download_Plot_2", label = "Download")),
                  tabPanel("ROC: Results",
                           box(id = "ROC_Model1", solidHeader = TRUE,collapsible = TRUE, width = "2000px",
                               column(5,style="margin-top:0px;font-size:120%; font-family:Arial Black;",  
                                      textOutput('ROC_model_name_1')),
                               column(12,style="display: inline-block;margin-top:50px;margin-right:0px;font-size:150%; font-family:Arial Black;",  
                                      "Accuraccy: "),
                               column(12,style="display: inline-block;margin-top:50px;margin-right:0px;font-size:150%; font-family:Arial Black;",  
                                      verbatimTextOutput("ROC_ACC_1")),
                               column(12,style="display: inline-block;margin-top:50px;margin-right:0px;font-size:150%; font-family:Arial Black;",  
                                      "AUC: "),
                               column(12,style="display: inline-block;margin-top:50px;margin-right:0px;font-size:150%; font-family:Arial Black;",  
                                      verbatimTextOutput("ROC_AUC_1"))),
                           box(id = "ROC_Model2", solidHeader = TRUE,collapsible = TRUE, width = "2000px",
                               column(5,style="margin-top:0px;font-size:120%; font-family:Arial Black;",  
                                      textOutput('ROC_model_name_2')),
                               column(12,style="display: inline-block;margin-top:50px;margin-right:0px;font-size:150%; font-family:Arial Black;",  
                                      "Accuraccy: "),
                               column(12,style="display: inline-block;margin-top:50px;margin-right:0px;font-size:150%; font-family:Arial Black;",  
                                      verbatimTextOutput("ROC_ACC_2")),
                               column(12,style="display: inline-block;margin-top:50px;margin-right:0px;font-size:150%; font-family:Arial Black;",  
                                      "AUC: "),
                               column(12,style="display: inline-block;margin-top:50px;margin-right:0px;font-size:150%; font-family:Arial Black;",  
                                      verbatimTextOutput("ROC_AUC_2"))),
                           box(id = "ROC_Model3", solidHeader = TRUE,collapsible = TRUE, width = "2000px",
                               column(5,style="margin-top:0px;font-size:120%; font-family:Arial Black;",  
                                      textOutput('ROC_model_name_3')),
                               column(12,style="display: inline-block;margin-top:50px;margin-right:0px;font-size:150%; font-family:Arial Black;",  
                                      "Accuraccy: "),
                               column(12,style="display: inline-block;margin-top:50px;margin-right:0px;font-size:150%; font-family:Arial Black;",  
                                      verbatimTextOutput("ROC_ACC_3")),
                               column(12,style="display: inline-block;margin-top:50px;margin-right:0px;font-size:150%; font-family:Arial Black;",  
                                      "AUC: "),
                               column(12,style="display: inline-block;margin-top:50px;margin-right:0px;font-size:150%; font-family:Arial Black;",  
                                      verbatimTextOutput("ROC_AUC_3"))))
                        )),
                
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
    
  
    lasttable = reactiveValues(tab1 = NULL)
    currentable = reactiveValues(tab1 = NULL)
    
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
                observeEvent(df, {
                  updatePrettyRadioButtons(session, "Columns", choices = colnames(df))
                })
                observeEvent(df, {
                  updateCheckboxGroupInput(session, "Facol", choices = colnames(df))
                })
                lasttable$tab1 <- df
                currentable$tab1 <- df
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
                observeEvent(df, {
                  updatePrettyRadioButtons(session, "Columns", choices = colnames(df))
                })
                observeEvent(df, {
                  updateCheckboxGroupInput(session, "Facol", choices = colnames(df))
                })
                lasttable$tab1 <- df
                currentable$tab1 <- df
                return(df)
            }
            isolate(datasetInput())
        }
    })
    
    #Change the data columns by user and update the data
    observeEvent(input$Colbutton, {
      df = currentable$tab1
      names(df)[names(df) == input$Columns] <- input$NewColumName
      updateTextAreaInput(session,"NewColumName",value = NULL)
      observeEvent(df, {
        updateSelectInput(session, "VR", choices = colnames(df))
      })
      observeEvent(df, {
        updateCheckboxGroupInput(session, "Variables", choices = colnames(df))
      })
      observeEvent(df, {
        updateSelectInput(session, "ID", choices = colnames(df))
      })
      observeEvent(df, {
        updateCheckboxGroupInput(session, "Facol", choices = colnames(df))
      })
      observeEvent(df, {
        updatePrettyRadioButtons(session, "Columns", choices = colnames(df))
      })
      lasttable$tab1 = currentable$tab1
      currentable$tab1 = df
    })
    
    #Show the data with or without changes made by the user
    output$newdata <- renderDataTable({
      return(currentable$tab1)
      
    })
    
    #Turn certain collumns as factor and update the data
    observeEvent(input$Facbtn,{
      df = currentable$tab1
      for (i in input$Facol){
        df[, i] <- as.factor(df[,i])
      }
      lasttable$tab1 = currentable$tab1
      currentable$tab1 = df
    })
    
    
    observeEvent(input$ADDbutton, {
      updateTabsetPanel(session, 'sidebarmenu',
                        selected = "Select")
    })
    
    #Table with QIC values
      newtab <- reactive({
          df = currentable$tab1
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
      lastbox <- reactiveValues(lastBox = NULL, currentBox = NULL)
      
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
          print(lastboxcreated$boxname)
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
          updateCheckboxGroupInput(session, "Variables", selected = character(0))
          updateTextInput(session, "Model_name", value = "")
          updateTextAreaInput(session,"Model_description",value = "")
          updatePrettyCheckbox(session,"QIC_Show", value = FALSE)
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
          print(lastboxcreated$boxname)
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
          updateCheckboxGroupInput(session, "Variables", selected = character(0))
          updateTextInput(session, "Model_name", value = "")
          updateTextAreaInput(session,"Model_description",value = "")
          updatePrettyCheckbox(session,"QIC_Show", value = FALSE)
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
          print(lastboxcreated$boxname)
          
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
      
      observe({
        if ((input$Select_Model == FALSE) && (input$Select_Model_2 == FALSE) && 
            (input$Select_Model_3 == FALSE)){
          shinyjs::disable("Analysisbutton")
        }
        else{
          shinyjs::enable("Analysisbutton")
        }
      })
      
      observeEvent(input$Select_Model, {
        if (input$Select_Model) {
          lastbox$lastBox = lastbox$currentBox
          lastbox$currentBox = "box1"
          print(lastbox$lastBox)
          print(input$Select_Model)
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
        if(lastboxcreated$boxname == lastbox$currentBox){
          if(lastbox$currentBox == "box1"){
            updateCheckboxGroupInput(session, "Variables", selected = boxvalues1$co)
            updateTextInput(session, "Model_name", value = boxvalues1$mn)
            updateTextAreaInput(session,"Model_description",value = boxvalues1$md)
            updatePrettyCheckbox(session,"QIC_Show", value = FALSE)
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
            lastboxcreated$boxname = "box1"
          }
          else if (lastbox$currentBox == 'box2'){
            updateCheckboxGroupInput(session, "Variables", selected = boxvalues2$co)
            updateTextInput(session, "Model_name", value = boxvalues2$mn)
            updateTextAreaInput(session,"Model_description",value = boxvalues2$md)
            updatePrettyCheckbox(session,"QIC_Show", value = FALSE)
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
            lastboxcreated$boxname = "box2"
          }
          else if (lastbox$currentBox == 'box3'){
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
            lastboxcreated$boxname = "box3"
          }
            
        }
        else if(input$"Select_Model"){
          updateCheckboxGroupInput(session, "Variables", selected = boxvalues1$co)
          updateTextInput(session, "Model_name", value = boxvalues1$mn)
          updateTextAreaInput(session,"Model_description",value = boxvalues1$md)
          updatePrettyCheckbox(session,"QIC_Show", value = FALSE)
          if(lastboxcreated$boxname == "box1"){
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
          }
          else if(lastboxcreated$boxname == "box2"){
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
          }
          else if(lastboxcreated$boxname == "box3"){
            boxvalues3$mn <- input$Model_name
            boxvalues3$md <- input$Model_description
            boxvalues3$vr <- input$VR
            boxvalues3$co <- input$Variables
            boxvalues3$id <- input$ID
            boxvalues3$fm <- input$Family
            boxvalues3$cs <- input$CS
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
          }
          lastboxcreated$boxname = "box1"
        }
        else if(input$"Select_Model_2"){
          updateCheckboxGroupInput(session, "Variables", selected = boxvalues2$co)
          updateTextInput(session, "Model_name", value = boxvalues2$mn)
          updateTextAreaInput(session,"Model_description",value = boxvalues2$md)
          updatePrettyCheckbox(session,"QIC_Show", value = FALSE)
          if(lastboxcreated$boxname == "box2"){
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
          }
          else if(lastboxcreated$boxname == "box1"){
            boxvalues1$mn <- input$Model_name
            boxvalues1$md <- input$Model_description
            boxvalues1$vr <- input$VR
            boxvalues1$co <- input$Variables
            boxvalues1$id <- input$ID
            boxvalues1$fm <- input$Family
            boxvalues1$cs <- input$CS
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
          }
          else if(lastboxcreated$boxname == "box3"){
            boxvalues3$mn <- input$Model_name
            boxvalues3$md <- input$Model_description
            boxvalues3$vr <- input$VR
            boxvalues3$co <- input$Variables
            boxvalues3$id <- input$ID
            boxvalues3$fm <- input$Family
            boxvalues3$cs <- input$CS
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
          }
          lastboxcreated$boxname = "box2"
          
        }
        else if(input$"Select_Model_3"){
          updateCheckboxGroupInput(session, "Variables", selected = boxvalues3$co)
          updateTextInput(session, "Model_name", value = boxvalues3$mn)
          updateTextAreaInput(session,"Model_description",value = boxvalues3$md)
          updatePrettyCheckbox(session,"QIC_Show", value = FALSE)
          if(lastboxcreated$boxname == "box3"){
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
          }
          else if(lastboxcreated$boxname == "box1"){
            boxvalues1$mn <- input$Model_name
            boxvalues1$md <- input$Model_description
            boxvalues1$vr <- input$VR
            boxvalues1$co <- input$Variables
            boxvalues1$id <- input$ID
            boxvalues1$fm <- input$Family
            boxvalues1$cs <- input$CS
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
          }
          else if(lastboxcreated$boxname == "box2"){
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
          }
          lastboxcreated$boxname = "box3"
        }
        
        
      })
      
      Model1 <- reactiveValues(mn = NULL, md = NULL, vr = NULL, co = NULL,
                                   id = NULL, fm = NULL,  cs = NULL)
      Model2 <- reactiveValues(mn = NULL, md = NULL, vr = NULL, co = NULL,
                               id = NULL, fm = NULL,  cs = NULL)
      Model3 <- reactiveValues(mn = NULL, md = NULL, vr = NULL, co = NULL,
                               id = NULL, fm = NULL,  cs = NULL)
      
      #Run button functions
      observeEvent(input$Analysisbutton,{
        if((input$Select_Model)|| (input$Select_Model_2) || (input$Select_Model_3)){
          if((input$Select_Model) && (input$Select_Model_2 == FALSE) && (input$Select_Model_3 == FALSE)){
            shinyjs::hide("Download_Plot_2")
            shinyjs::hide("ROC_Model2")
            shinyjs::hide("ROC_Model3")
            if(lastboxcreated$boxname == lastbox$currentBox){
              Model1$mn = input$Model_name
              Model1$md <- input$Model_description
              Model1$vr <- input$VR
              Model1$co <- input$Variables
              Model1$id <- input$ID
              Model1$fm <- input$Family
              Model1$cs <- input$CS
              print(Model1$mn)
              print(Model1$md)
              print(Model1$vr)
              print(Model1$co)
              print(Model1$id)
              print(Model1$fm)
              print(Model1$cs)
            }
            else{
              Model1$mn = boxvalues1$mn
              Model1$md <- boxvalues1$md
              Model1$vr <- boxvalues1$vr
              Model1$co <- boxvalues1$co
              Model1$id <- boxvalues1$id
              Model1$fm <- boxvalues1$fm
              Model1$cs <- boxvalues1$cs
              print(Model1$mn)
              print(Model1$md)
              print(Model1$vr)
              print(Model1$co)
              print(Model1$id)
              print(Model1$fm)
              print(Model1$cs)
            }
            
          }
          else if ((input$Select_Model == FALSE) && (input$Select_Model_2) && (input$Select_Model_3 == FALSE)){
            if(lastboxcreated$boxname == lastbox$currentBox){
              Model2$mn = input$Model_name
              Model2$md <- input$Model_description
              Model2$vr <- input$VR
              Model2$co <- input$Variables
              Model2$id <- input$ID
              Model2$fm <- input$Family
              Model2$cs <- input$CS
              print(Model2$mn)
              print(Model2$md)
              print(Model2$vr)
              print(Model2$co)
              print(Model2$id)
              print(Model2$fm)
              print(Model2$cs)
            }
            else{
              Model2$mn = boxvalues2$mn
              Model2$md <- boxvalues2$md
              Model2$vr <- boxvalues2$vr
              Model2$co <- boxvalues2$co
              Model2$id <- boxvalues2$id
              Model2$fm <- boxvalues2$fm
              Model2$cs <- boxvalues2$cs
              print(Model2$mn)
              print(Model2$md)
              print(Model2$vr)
              print(Model2$co)
              print(Model2$id)
              print(Model2$fm)
              print(Model2$cs)
            }
          }
          else if ((input$Select_Model == FALSE) && (input$Select_Model_2 == FALSE) && (input$Select_Model_3)){
            if(lastboxcreated$boxname == lastbox$currentBox){
              Model3$mn = input$Model_name
              Model3$md <- input$Model_description
              Model3$vr <- input$VR
              Model3$co <- input$Variables
              Model3$id <- input$ID
              Model3$fm <- input$Family
              Model3$cs <- input$CS
              print(Model3$mn)
              print(Model3$md)
              print(Model3$vr)
              print(Model3$co)
              print(Model3$id)
              print(Model3$fm)
              print(Model3$cs)
            }
            else{
              Model3$mn = boxvalues3$mn
              Model3$md <- boxvalues3$md
              Model3$vr <- boxvalues3$vr
              Model3$co <- boxvalues3$co
              Model3$id <- boxvalues3$id
              Model3$fm <- boxvalues3$fm
              Model3$cs <- boxvalues3$cs
              print(Model3$mn)
              print(Model3$md)
              print(Model3$vr)
              print(Model3$co)
              print(Model3$id)
              print(Model3$fm)
              print(Model3$cs)
            }
          }
          else if((input$Select_Model) && (input$Select_Model_2) && (input$Select_Model_3 == FALSE)){
            if ((lastboxcreated$boxname == "box1")){
              Model1$mn = input$Model_name
              Model1$md <- input$Model_description
              Model1$vr <- input$VR
              Model1$co <- input$Variables
              Model1$id <- input$ID
              Model1$fm <- input$Family
              Model1$cs <- input$CS
              print(Model1$mn)
              print(Model1$md)
              print(Model1$vr)
              print(Model1$co)
              print(Model1$id)
              print(Model1$fm)
              print(Model1$cs)
              Model2$mn = boxvalues2$mn
              Model2$md <- boxvalues2$md
              Model2$vr <- boxvalues2$vr
              Model2$co <- boxvalues2$co
              Model2$id <- boxvalues2$id
              Model2$fm <- boxvalues2$fm
              Model2$cs <- boxvalues2$cs
              print(Model2$mn)
              print(Model2$md)
              print(Model2$vr)
              print(Model2$co)
              print(Model2$id)
              print(Model2$fm)
              print(Model2$cs)
            }
            else if ((lastboxcreated$boxname == "box2")){
              Model2$mn = input$Model_name
              Model2$md <- input$Model_description
              Model2$vr <- input$VR
              Model2$co <- input$Variables
              Model2$id <- input$ID
              Model2$fm <- input$Family
              Model2$cs <- input$CS
              print(Model2$mn)
              print(Model2$md)
              print(Model2$vr)
              print(Model2$co)
              print(Model2$id)
              print(Model2$fm)
              print(Model2$cs)
              Model1$mn = boxvalues1$mn
              Model1$md <- boxvalues1$md
              Model1$vr <- boxvalues1$vr
              Model1$co <- boxvalues1$co
              Model1$id <- boxvalues1$id
              Model1$fm <- boxvalues1$fm
              Model1$cs <- boxvalues1$cs
              print(Model1$mn)
              print(Model1$md)
              print(Model1$vr)
              print(Model1$co)
              print(Model1$id)
              print(Model1$fm)
              print(Model1$cs)
            }
            else{
              Model1$mn = boxvalues1$mn
              Model1$md <- boxvalues1$md
              Model1$vr <- boxvalues1$vr
              Model1$co <- boxvalues1$co
              Model1$id <- boxvalues1$id
              Model1$fm <- boxvalues1$fm
              Model1$cs <- boxvalues1$cs
              print(Model1$mn)
              print(Model1$md)
              print(Model1$vr)
              print(Model1$co)
              print(Model1$id)
              print(Model1$fm)
              print(Model1$cs)
              Model2$mn = boxvalues2$mn
              Model2$md <- boxvalues2$md
              Model2$vr <- boxvalues2$vr
              Model2$co <- boxvalues2$co
              Model2$id <- boxvalues2$id
              Model2$fm <- boxvalues2$fm
              Model2$cs <- boxvalues2$cs
              print(Model2$mn)
              print(Model2$md)
              print(Model2$vr)
              print(Model2$co)
              print(Model2$id)
              print(Model2$fm)
              print(Model2$cs)
            }
          }
          else if((input$Select_Model) && (input$Select_Model_2 == FALSE) && (input$Select_Model_3)){
            if ((lastboxcreated$boxname == "box1")){
              Model1$mn = input$Model_name
              Model1$md <- input$Model_description
              Model1$vr <- input$VR
              Model1$co <- input$Variables
              Model1$id <- input$ID
              Model1$fm <- input$Family
              Model1$cs <- input$CS
              print(Model1$mn)
              print(Model1$md)
              print(Model1$vr)
              print(Model1$co)
              print(Model1$id)
              print(Model1$fm)
              print(Model1$cs)
              Model3$mn = boxvalues3$mn
              Model3$md <- boxvalues3$md
              Model3$vr <- boxvalues3$vr
              Model3$co <- boxvalues3$co
              Model3$id <- boxvalues3$id
              Model3$fm <- boxvalues3$fm
              Model3$cs <- boxvalues3$cs
              print(Model3$mn)
              print(Model3$md)
              print(Model3$vr)
              print(Model3$co)
              print(Model3$id)
              print(Model3$fm)
              print(Model3$cs)
            }
            else if ((lastboxcreated$boxname == "box3")){
              Model3$mn = input$Model_name
              Model3$md <- input$Model_description
              Model3$vr <- input$VR
              Model3$co <- input$Variables
              Model3$id <- input$ID
              Model3$fm <- input$Family
              Model3$cs <- input$CS
              print(Model3$mn)
              print(Model3$md)
              print(Model3$vr)
              print(Model3$co)
              print(Model3$id)
              print(Model3$fm)
              print(Model3$cs)
              Model1$mn = boxvalues1$mn
              Model1$md <- boxvalues1$md
              Model1$vr <- boxvalues1$vr
              Model1$co <- boxvalues1$co
              Model1$id <- boxvalues1$id
              Model1$fm <- boxvalues1$fm
              Model1$cs <- boxvalues1$cs
              print(Model1$mn)
              print(Model1$md)
              print(Model1$vr)
              print(Model1$co)
              print(Model1$id)
              print(Model1$fm)
              print(Model1$cs)
            }
            else{
              Model1$mn = boxvalues1$mn
              Model1$md <- boxvalues1$md
              Model1$vr <- boxvalues1$vr
              Model1$co <- boxvalues1$co
              Model1$id <- boxvalues1$id
              Model1$fm <- boxvalues1$fm
              Model1$cs <- boxvalues1$cs
              print(Model1$mn)
              print(Model1$md)
              print(Model1$vr)
              print(Model1$co)
              print(Model1$id)
              print(Model1$fm)
              print(Model1$cs)
              Model3$mn = boxvalues3$mn
              Model3$md <- boxvalues3$md
              Model3$vr <- boxvalues3$vr
              Model3$co <- boxvalues3$co
              Model3$id <- boxvalues3$id
              Model3$fm <- boxvalues3$fm
              Model3$cs <- boxvalues3$cs
              print(Model3$mn)
              print(Model3$md)
              print(Model3$vr)
              print(Model3$co)
              print(Model3$id)
              print(Model3$fm)
              print(Model3$cs)
            }
          }
          else if((input$Select_Model == FALSE) && (input$Select_Model_2) && (input$Select_Model_3)){
            if ((lastboxcreated$boxname == "box2")){
              Model2$mn = input$Model_name
              Model2$md <- input$Model_description
              Model2$vr <- input$VR
              Model2$co <- input$Variables
              Model2$id <- input$ID
              Model2$fm <- input$Family
              Model2$cs <- input$CS
              print(Model2$mn)
              print(Model2$md)
              print(Model2$vr)
              print(Model2$co)
              print(Model2$id)
              print(Model2$fm)
              print(Model2$cs)
              Model3$mn = boxvalues3$mn
              Model3$md <- boxvalues3$md
              Model3$vr <- boxvalues3$vr
              Model3$co <- boxvalues3$co
              Model3$id <- boxvalues3$id
              Model3$fm <- boxvalues3$fm
              Model3$cs <- boxvalues3$cs
              print(Model3$mn)
              print(Model3$md)
              print(Model3$vr)
              print(Model3$co)
              print(Model3$id)
              print(Model3$fm)
              print(Model3$cs)
            }
            else if ((lastboxcreated$boxname == "box3")){
              Model3$mn = input$Model_name
              Model3$md <- input$Model_description
              Model3$vr <- input$VR
              Model3$co <- input$Variables
              Model3$id <- input$ID
              Model3$fm <- input$Family
              Model3$cs <- input$CS
              print(Model3$mn)
              print(Model3$md)
              print(Model3$vr)
              print(Model3$co)
              print(Model3$id)
              print(Model3$fm)
              print(Model3$cs)
              Model2$mn = boxvalues2$mn
              Model2$md <- boxvalues2$md
              Model2$vr <- boxvalues2$vr
              Model2$co <- boxvalues2$co
              Model2$id <- boxvalues2$id
              Model2$fm <- boxvalues2$fm
              Model2$cs <- boxvalues2$cs
              print(Model2$mn)
              print(Model2$md)
              print(Model2$vr)
              print(Model2$co)
              print(Model2$id)
              print(Model2$fm)
              print(Model2$cs)
            }
            else{
              Model2$mn = boxvalues2$mn
              Model2$md <- boxvalues2$md
              Model2$vr <- boxvalues2$vr
              Model2$co <- boxvalues2$co
              Model2$id <- boxvalues2$id
              Model2$fm <- boxvalues2$fm
              Model2$cs <- boxvalues2$cs
              print(Model2$mn)
              print(Model2$md)
              print(Model2$vr)
              print(Model2$co)
              print(Model2$id)
              print(Model2$fm)
              print(Model2$cs)
              Model3$mn = boxvalues3$mn
              Model3$md <- boxvalues3$md
              Model3$vr <- boxvalues3$vr
              Model3$co <- boxvalues3$co
              Model3$id <- boxvalues3$id
              Model3$fm <- boxvalues3$fm
              Model3$cs <- boxvalues3$cs
              print(Model3$mn)
              print(Model3$md)
              print(Model3$vr)
              print(Model3$co)
              print(Model3$id)
              print(Model3$fm)
              print(Model3$cs)
            }
          }
          else if((input$Select_Model) && (input$Select_Model_2) && (input$Select_Model_3)){
            if ((lastboxcreated$boxname == "box1")){
              Model1$mn = input$Model_name
              Model1$md <- input$Model_description
              Model1$vr <- input$VR
              Model1$co <- input$Variables
              Model1$id <- input$ID
              Model1$fm <- input$Family
              Model1$cs <- input$CS
              print(Model1$mn)
              print(Model1$md)
              print(Model1$vr)
              print(Model1$co)
              print(Model1$id)
              print(Model1$fm)
              print(Model1$cs)
              Model3$mn = boxvalues3$mn
              Model3$md <- boxvalues3$md
              Model3$vr <- boxvalues3$vr
              Model3$co <- boxvalues3$co
              Model3$id <- boxvalues3$id
              Model3$fm <- boxvalues3$fm
              Model3$cs <- boxvalues3$cs
              print(Model3$mn)
              print(Model3$md)
              print(Model3$vr)
              print(Model3$co)
              print(Model3$id)
              print(Model3$fm)
              print(Model3$cs)
              Model2$mn = boxvalues2$mn
              Model2$md <- boxvalues2$md
              Model2$vr <- boxvalues2$vr
              Model2$co <- boxvalues2$co
              Model2$id <- boxvalues2$id
              Model2$fm <- boxvalues2$fm
              Model2$cs <- boxvalues2$cs
              print(Model2$mn)
              print(Model2$md)
              print(Model2$vr)
              print(Model2$co)
              print(Model2$id)
              print(Model2$fm)
              print(Model2$cs)
            }
            else if ((lastboxcreated$boxname == "box3")){
              Model3$mn = input$Model_name
              Model3$md <- input$Model_description
              Model3$vr <- input$VR
              Model3$co <- input$Variables
              Model3$id <- input$ID
              Model3$fm <- input$Family
              Model3$cs <- input$CS
              print(Model3$mn)
              print(Model3$md)
              print(Model3$vr)
              print(Model3$co)
              print(Model3$id)
              print(Model3$fm)
              print(Model3$cs)
              Model1$mn = boxvalues1$mn
              Model1$md <- boxvalues1$md
              Model1$vr <- boxvalues1$vr
              Model1$co <- boxvalues1$co
              Model1$id <- boxvalues1$id
              Model1$fm <- boxvalues1$fm
              Model1$cs <- boxvalues1$cs
              print(Model1$mn)
              print(Model1$md)
              print(Model1$vr)
              print(Model1$co)
              print(Model1$id)
              print(Model1$fm)
              print(Model1$cs)
              Model2$mn = boxvalues2$mn
              Model2$md <- boxvalues2$md
              Model2$vr <- boxvalues2$vr
              Model2$co <- boxvalues2$co
              Model2$id <- boxvalues2$id
              Model2$fm <- boxvalues2$fm
              Model2$cs <- boxvalues2$cs
              print(Model2$mn)
              print(Model2$md)
              print(Model2$vr)
              print(Model2$co)
              print(Model2$id)
              print(Model2$fm)
              print(Model2$cs)
            }
            else{
              Model2$mn = input$Model_name
              Model2$md <- input$Model_description
              Model2$vr <- input$VR
              Model2$co <- input$Variables
              Model2$id <- input$ID
              Model2$fm <- input$Family
              Model2$cs <- input$CS
              print(Model2$mn)
              print(Model2$md)
              print(Model2$vr)
              print(Model2$co)
              print(Model2$id)
              print(Model2$fm)
              print(Model2$cs)
              Model1$mn = boxvalues1$mn
              Model1$md <- boxvalues1$md
              Model1$vr <- boxvalues1$vr
              Model1$co <- boxvalues1$co
              Model1$id <- boxvalues1$id
              Model1$fm <- boxvalues1$fm
              Model1$cs <- boxvalues1$cs
              print(Model1$mn)
              print(Model1$md)
              print(Model1$vr)
              print(Model1$co)
              print(Model1$id)
              print(Model1$fm)
              print(Model1$cs)
              Model3$mn = boxvalues3$mn
              Model3$md <- boxvalues3$md
              Model3$vr <- boxvalues3$vr
              Model3$co <- boxvalues3$co
              Model3$id <- boxvalues3$id
              Model3$fm <- boxvalues3$fm
              Model3$cs <- boxvalues3$cs
              print(Model3$mn)
              print(Model3$md)
              print(Model3$vr)
              print(Model3$co)
              print(Model3$id)
              print(Model3$fm)
              print(Model3$cs)
              
            }
          }
          }
      })
      
      observeEvent(input$Splitbtn,{
        df = currentable$tab1
        if ((input$Split_data == "Train: 70% - Test: 30%") || (input$Split_data == "Default")){
          smp_size <- floor(0.70 * nrow(df))
          set.seed(123)
          train_ind <- sample(seq_len(nrow(df)), size = smp_size)
          train_70 <- df[train_ind,]
          test_70 <- df[-train_ind, ]
          if ((input$Select_Model) && (input$Select_Model_2 == FALSE) && (input$Select_Model_3 == FALSE)){
            #Define model
            cov = result_s(Model1$co)
            form = as.formula(paste(Model1$vr,'~',cov,collapse = ""))
            identification = Model1$id
            geemodel = geeglm(form,data = train_70,id = train_70[[identification]],family = Model1$fm,corstr = Model1$cs)
            #ROC Analysis
            pred_70_30 = predict(geemodel,test_70,type="response")
            pred_70_30 = prediction(pred_70_30, test_70$PEX)
            perf_70_30 = performance(pred_70_30, "acc")
            #Plot ROC
            roc_70_30 = performance(pred_70_30,"tpr","fpr")
            output$ROCplot <- renderPlot({
              plot(roc_70_30, colorize = T, lwd = 2, main = Model1$mn)
            })
            output$ROCplot2 <- NULL
            #ACC
            max_ind_70_30 = which.max(slot(perf_70_30, "y.values")[[1]] )
            acc_70_30 = slot(perf_70_30, "y.values")[[1]][max_ind_70_30]
            output$ROC_ACC_1 <- renderPrint({
              acc_70_30
            })
            #AUC
            auc_70_30 = performance(pred_70_30, measure = "auc")
            output$ROC_AUC_1 <- renderPrint({
              auc_70_30@y.values[[1]]
            })
          }
        }
        else if((input$Split_data == "Train: 75% - Test: 25%")){
          smp_size <- floor(0.75 * nrow(df))
          set.seed(123)
          train_ind <- sample(seq_len(nrow(df)), size = smp_size)
          train_75 <- df[train_ind,]
          test_75 <- df[-train_ind, ]
        }
        else if((input$Split_data == "Train: 80% - Test: 20%")){
          smp_size <- floor(0.80 * nrow(df))
          set.seed(123)
          train_ind <- sample(seq_len(nrow(df)), size = smp_size)
          train_80 <- df[train_ind,]
          test_80 <- df[-train_ind, ]
        }
      })
      
    
      
    
}
shinyApp(ui = ui, server = server)