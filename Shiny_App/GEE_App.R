## add sub menu items in shinydashboard sidebar

# load the required packages
require(devtools)
library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(shinyWidgets)
library(dplyr)
library(shinycssloaders)
library(readxl)
library(geepack)
library(MESS)
library(shinyjs)
library(markdown)
library(ROCR)

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
        dashboardHeader(title =div("GEE Models", 
                                         style = "color: black; font-size: 28px;"), titleWidth = 300
                        ),
        dashboardSidebar(
            width = 343,
            sidebarMenu(id = 'sidebarmenu', 
                        # Menu correspondente ao Upload dos datasets
                        menuItem("Preparacao do modelo", tabName = "Prep", icon = icon("upload"),
                                 menuSubItem("Upload Dataset", tabName = "Upload", icon = icon("upload")),
                                 menuSubItem("Dataset changes", tabName = "DataChange", icon = icon("database"))),
                                 
                        menuItem('Escolha das variaveis', tabName = "Select", icon = icon("edit")),
                        # Menus de compara??o entre curvas dependendo do tipo de vari?vel
                        menuItem("GEE Models", icon = icon("clipboard"),
                                 tabName = "Gee_List"),
                        
                        menuItem("Validation and Results", tabName = "Results",icon = icon("chart-line"),
                                 menuSubItem("ROC Analysis", tabName = "ROC", icon = icon("stats",lib = "glyphicon")),
                                 menuSubItem("GEE Results", tabName = "GEE_results", icon = icon("table"))),
                        
                        #Menu informativo com refer?ncias acerca da aplica??o
                        menuItem("About", tabName = "Sobre", icon = icon("book"))
                        )),
        
        
        dashboardBody(shinyjs::useShinyjs(),
                      tags$head(tags$style(HTML('.content-wrapper {background-color:white;}'))),
            tabItems(
                # Toda a interface do menu Upload
                tabItem("Upload", sidebarLayout(
                    sidebarPanel(
                      column(12,style="margin-left: -10px; margin-bottom:30px; font-size:100%; font-weight: bold; font-family:Georgia;",  
                             "Pick your data  ", div(style = " font-family: Georgia; font-weight: normal; text-align: right; margin-top: -20px;",dropdownButton(includeMarkdown("Help_Upload.Rmd"), circle = TRUE, icon = icon('info'),size = "xs",width = "300px", up = FALSE, right = TRUE))),
                        fileInput("file1",
                                  NULL,
                                  multiple = TRUE,
                                  accept = c("text/csv","test/comma-separated-values, text/plain",".csv", ".xlsx")),
                        tags$hr(),
                        checkboxInput("header","Header", TRUE),
                      column(12,style="margin-left: -15px; margin-bottom:10px; font-size:90%;font-weight: bold; font-family:Georgia;",  
                             "Separator"),
                        radioButtons("sep",NULL,choices = c(Comma = ",",
                                                                   Semicolon = ";",
                                                                   Tab = "\t",
                                                                   selected = ",")),
                        tags$hr(),
                      column(12,style="margin-left: -15px; margin-bottom:10px; font-size:90%;font-weight: bold; font-family:Georgia;",  
                             "Quote"),
                        radioButtons("quote",NULL,choices = c(None = "" ,
                                                                 "Double Quote" = '"',
                                                                 "Single Quote" = "'",
                                                                 selected = '"')),
                        tags$hr(),
                      column(12,style="margin-left: -15px; margin-bottom:10px; font-size:90%;font-weight: bold; font-family:Georgia;",  
                             "Display"),
                        radioButtons("disp",NULL,choices = c(Head = "head",
                                                                  ALL = "all",
                                                                  selected = "head")),
                        
                        actionButton("submitbutton", "Upload Dataset", class = "btn btn-primary", icon("upload"),style="width: 250px; color: #fff; background-color: #337ab7; border-color: #2e6da4")
                    ),
                    
                    mainPanel(
                        DT::dataTableOutput("contents"), style = "height:800px;overflow-x: scroll;"
                    )
                )),
                tabItem('DataChange', tabBox(
                  title = "Dataset Changes", id = 'Changes', height = "800px", width = "1000px",
                  tabPanel("Columns Names", column(12,style="height: 40px;font-size:120%; font-weight: bold; font-family:Georgia;",  
                                                   "Columns: ",div(style = " font-family: Georgia; font-weight: normal; text-align: right; margin-top: -20px;",dropdownButton(includeMarkdown("Help_DataChanges.Rmd"), circle = TRUE, icon = icon('info'),size = "xs",width = "300px", up = FALSE, right = TRUE))),fluidPage(tweaks,fluidRow(column(12, list(tags$div(align = 'left', 
                                                                                                                   class = 'multicol', 
                                                                                                                   prettyRadioButtons(inputId  = 'Columns', 
                                                                                                                                      label    = NULL, 
                                                                                                                                      choices  = ""
                                                                                                                                      ))))
                                                                                          
                                                   )),
                           column(6,style="margin-top:-100px;font-size:120%;font-weight: bold; font-family:Georgia;",  
                                  "New column name:   "),
                           column(4,style="margin-top:-100px;margin-left:200px; font-size:10px; font-family:Times New Roman;",
                                  textAreaInput("NewColumName", NULL,height = '30px', width = '2000px')),
                           column(12,style = "text-align: right;margin-left:-150px",
                                  actionButton("Colbutton", "Save", icon("save"), 
                                               style="width: 150px; color: #fff; background-color: #337ab7; border-color: #2e6da4"))),
                  tabPanel("Factors",column(12,style="height: 40px;font-size:120%; font-weight: bold; font-family:Georgia;",  
                                            "Columns: ", div(style = " font-family: Georgia; font-weight: normal; text-align: right; margin-top: -20px;",dropdownButton(includeMarkdown("Help_Factors.Rmd"), circle = TRUE, icon = icon('info'),size = "xs",width = "300px", up = FALSE, right = TRUE))),fluidPage(tweaks,fluidRow(column(12, list(tags$div(align = 'left', 
                                                                                        class = 'multicol', 
                                                                                        checkboxGroupInput(inputId  = 'Facol', 
                                                                                                           label    = NULL, 
                                                                                                           choices  = c(),
                                                                                                           inline   = FALSE))))
                                                               
                  )),column(12,style = "text-align: right; margin-top: -50px",
                            actionButton("Facbtn", "Save", icon("save"), 
                                         style=" width: 150px; color: #fff; background-color: #337ab7; border-color: #2e6da4"))),
                  tabPanel("Data Visualization", mainPanel(
                    DT::dataTableOutput("newdata"), style = "height:600px;width:900px;overflow-x: scroll;"
                  ) ),
                  
                    
                )),
                tabItem("Select", tabBox(
                  title = "Variable Selection for GEE Model",
                  id = "selectvariable", height = "800px", width = "1000px",
                  tabPanel("Variables", div(style = " font-family: Georgia; font-weight: normal; text-align: right; margin-top: 10px; margin-right: 10px",dropdownButton(includeMarkdown("Help_Variaveis.Rmd"), circle = TRUE, icon = icon('info'),size = "xs",width = "300px", up = FALSE, right = TRUE)),
                           box(solidHeader = TRUE,
                               collapsible = TRUE, width = "1000px", height = "200px",
                               column(12,style="height: 40px;font-size:120%; font-weight: bold; font-family:Georgia;",  
                                      "Predictor Variable "),
                               column(12,style="font-size:120%;font-family:Times New Roman;",  
                                      selectInput("VR",NULL,"", width = "1500px")),
                               column(12,style = "text-align: right; margin-top: 20px",
                                      actionButton("VRbutton", "Select", icon("check", lib = "glyphicon"), 
                                                      style="width: 150px; color: #fff; background-color: #337ab7; border-color: #2e6da4"))
                               
                               
                           ),div(style = " font-family: Georgia; font-weight: normal; text-align: right; margin-top: 10px; margin-right: 10px",dropdownButton(includeMarkdown("Help_Covariaveis.Rmd"), circle = TRUE, icon = icon('info'),size = "xs",width = "300px", up = FALSE, right = TRUE)),
                           box(solidHeader = TRUE,
                               collapsible = TRUE, width = "1000px",
                               column(12,style="height: 40px;font-size:120%; font-weight: bold; font-family:Georgia;",  
                                      "Covariables: "),
                               fluidPage(tweaks,fluidRow(column(12,style="font-size:120%;font-family:Times New Roman;", list(tags$div(align = 'left', 
                                                                                           class = 'multicol', 
                                                                                           checkboxGroupInput(inputId  = 'Variables', 
                                                                                                              label    = NULL, 
                                                                                                              choices  = c(),
                                                                                                              inline   = FALSE))))
                                
                               )), column(6,style="margin-top:-50px;font-size:120%;font-weight: bold; font-family:Georgia;",  
                                          "Covariables Expression:   "),
                               column(4,style="margin-top:-50px;margin-left:250px; font-size:10px; font-family:Times New Roman;",
                                      textAreaInput("Coexpression", NULL,height = '30px', width = '2000px')),
                               column(12,style = "text-align: right;",
                                        actionButton("Covariablebutton", "Select", icon("check", lib = "glyphicon"), 
                                                     style="width: 150px;color: #fff; background-color: #337ab7; border-color: #2e6da4")))
                           ),
                  tabPanel("Family/ID", div(style = " font-family: Georgia; font-weight: normal; text-align: right; margin-top: 10px; margin-right: 10px",dropdownButton(includeMarkdown("Help_ID.Rmd"), circle = TRUE, icon = icon('info'),size = "xs",width = "300px", up = FALSE, right = TRUE)),
                           box(solidHeader = TRUE,
                               collapsible = TRUE, width = "1000px",
                               column(12,style="height: 40px;font-size:120%;font-weight: bold; font-family:Georgia;",  
                                      "Identification Column:  "),
                               column(12,style="font-size:120%;font-family:Times New Roman;",
                                      selectInput("ID",NULL,"")),
                               column(12,style = "text-align: right; margin-top: 20px",
                                      actionButton("IDbutton", "Select", icon("check", lib = "glyphicon"), 
                                                   style="width: 150px;color: #fff; background-color: #337ab7; border-color: #2e6da4"))
                               ),div(style = " font-family: Georgia; font-weight: normal; text-align: right; margin-top: 10px; margin-right: 10px",dropdownButton(includeMarkdown("Help_Family.Rmd"), circle = TRUE, icon = icon('info'),size = "xs",width = "300px", up = FALSE, right = TRUE)),
                           box(solidHeader = TRUE, collapsible = TRUE, width = "1000px",
                               column(12,style="height: 40px;font-size:120%;font-weight: bold; font-family:Georgia;",  
                                      "Distribution Family:  "),
                               column(12,style="font-size:120%; font-family:Times New Roman;",
                                      selectInput("Family",NULL,c('binomial','gaussian','Gamma',
                                                              'inverse.gaussian','poisson','quasi',
                                                              'quasibinomial','quasipoisson'))),
                               column(12,style = "text-align: right; margin-top: 20px",
                                      actionButton("Familybutton", "Select", icon("check", lib = "glyphicon"), 
                                                   style="width: 150px;color: #fff; background-color: #337ab7; border-color: #2e6da4"))
                               )),
                  tabPanel("Correlation Structure",div(style = " font-family: Georgia; font-weight: normal; text-align: right; margin-top: 10px; margin-right: 10px",dropdownButton(includeMarkdown("Help_CS.Rmd"), circle = TRUE, icon = icon('info'),size = "xs",width = "300px", up = FALSE, right = TRUE)),
                           column(5,style="display: inline-block;margin-top:50px;margin-right:0px;font-size:120%;font-weight: bold; font-family:Georgia;",  
                                  "Choose your correlation structure:  "),
                           column(5,style="display: inline-block;margin-top:45px; margin-left: -80px; font-size:120%; font-family:Times New Roman;",
                                  selectInput("CS",NULL,c('Independence','Exchangeable','AR-1'),width = "500px")),
                           column(12,style = "text-align: right; margin-top: -50px; margin-left: -20px",
                                  actionButton("CSbtn", "Select", icon("check", lib = "glyphicon"), 
                                               style="width: 150px;color: #fff; background-color: #337ab7; border-color: #2e6da4")),
                           column(7,style="display: inline-block;margin-top: 15px;margin-left: 50px;margin-right:0px;font-size:90%;font-weight: bold; font-family:Georgia;",  
                                  "Display QIC values:  "),
                           column(3, style= "display: inline-block;margin-left: 50px;margin-top: 10px;font-size:150%;", prettyCheckbox(
                             inputId = "QIC_Show", label = NULL, icon = icon("check"), 
                             status = "default", shape = "curve", animation = "jelly")),
                           column(12,DT::dataTableOutput('QIC_Table'))
                           
                           ),
                  tabPanel("Model Definitions",div(style = " font-family: Georgia; font-weight: normal; text-align: right; margin-top: 10px; margin-right: 10px",dropdownButton(includeMarkdown("Help_MD.Rmd"), circle = TRUE, icon = icon('info'),size = "xs",width = "300px", up = FALSE, right = TRUE)),
                           column(4,style="display: inline-block;margin-top:50px;margin-right:0px;font-size:120%;font-weight: bold; font-family:Georgia;",  
                                  "Name your Model:  "),
                           column(5,style="display: inline-block;margin-top:45px;margin-left: -100px; font-size:120%; font-family:Times New Roman;",
                                  textInput("Model_name", NULL, width = "3000px")),
                           column(5,style="margin-top:50px;font-size:120%;font-weight: bold; font-family:Georgia;",  
                                  "Describe your model:  "),
                           column(6,style="display: inline-block;margin-top:45px;margin-left: -180px; font-size:120%; font-family:Times New Roman;",
                                  textAreaInput("Model_description", NULL,height = '50px', width = '500px')),
                           column(12,style = "text-align: right; margin-top: 20px; margin-left: -50px;",
                                  actionButton("MDbutton", "Select", icon("check", lib = "glyphicon"), 
                                               style="width: 150px;color: #fff; background-color: #337ab7; border-color: #2e6da4"),actionButton("Savebutton", "Save", icon("save"), 
                                                                                                                                                style="width: 150px;color: #fff; background-color: #337ab7; border-color: #2e6da4")))
                  
                  
                )),
                #Interface do segundo menu (Dependentes) -> Escolha das vari?veis
                tabItem("Gee_List",div(style = " font-family: Georgia; font-weight: normal; text-align: right; margin-top: 10px; margin-right: 10px",dropdownButton(includeMarkdown("Help_GEEModels.Rmd"), circle = TRUE, icon = icon('info'),size = "xs",width = "300px", up = FALSE, right = TRUE)), box(id = "Model_1",solidHeader = TRUE, width = "1000px",
                                        column(5,style="margin-top:0px;font-size:150%;font-weight: bold; font-family:Georgia;",  
                                               textOutput('values')),
                                        column(10,style="margin-top:10px;font-size:100%; font-family:Times New Roman;",  
                                               textOutput('des')),
                                        column(12,style="margin-top:10px;font-size:120%; font-family:Times New Roman;",  
                                               htmlOutput('vr')),
                                        column(12,style="margin-top:10px;font-size:120%; font-family:Times New Roman;",  
                                               htmlOutput('co')),
                                        column(12,style="margin-top:10px;font-size:120%; font-family:Times New Roman;",  
                                               htmlOutput('id')),
                                        column(12,style="margin-top:10px;font-size:120%; font-family:Times New Roman;",  
                                               htmlOutput('family')),
                                        column(12,style="margin-top:10px;font-size:120%; font-family:Times New Roman;",  
                                               htmlOutput('cs')),
                                        column(12, style = "text-align: right; margin-top: -120px; font-size:150%;", 
                                               prettyCheckbox(
                                                 inputId = "Select_Model", label = NULL, icon = icon("check"), 
                                                 status = "default", shape = "curve", animation = "jelly"
                                               ))),
                        
                        box(id = "Model_2",solidHeader = TRUE, width = "1000px",
                            column(5,style="margin-top:0px;font-size:150%;font-weight: bold; font-family:Georgia;",  
                                   textOutput('values_2')),
                            column(10,style="margin-top:10px;font-size:100%; font-family:Times New Roman;",  
                                   textOutput('des_2')),
                            column(12,style="margin-top:10px;font-size:120%; font-family:Times New Roman;",  
                                   htmlOutput('vr_2')),
                            column(12,style="margin-top:10px;font-size:120%; font-family:Times New Roman;",  
                                   htmlOutput('co_2')),
                            column(12,style="margin-top:10px;font-size:120%; font-family:Times New Roman;",  
                                   htmlOutput('id_2')),
                            column(12,style="margin-top:10px;font-size:120%; font-family:Times New Roman;",  
                                   htmlOutput('family_2')),
                            column(12,style="margin-top:10px;font-size:120%; font-family:Times New Roman;",  
                                   htmlOutput('cs_2')),
                            column(12, style = "text-align: right; margin-top: -120px; font-size:150%;", 
                                   prettyCheckbox(
                                     inputId = "Select_Model_2", label = NULL, icon = icon("check"), 
                                     status = "default", shape = "curve", animation = "jelly"
                                   ))),
                        box(id = "Model_3",solidHeader = TRUE, width = "1000px",
                            column(5,style="margin-top:0px;font-size:150%;font-weight: bold; font-family:Georgia;",  
                                   textOutput('values_3')),
                            column(10,style="margin-top:10px;font-size:100%; font-family:Times New Roman;",  
                                   textOutput('des_3')),
                            column(12,style="margin-top:10px;font-size:120%; font-family:Times New Roman;",  
                                   htmlOutput('vr_3')),
                            column(12,style="margin-top:10px;font-size:120%; font-family:Times New Roman;",  
                                   htmlOutput('co_3')),
                            column(12,style="margin-top:10px;font-size:120%; font-family:Times New Roman;",  
                                   htmlOutput('id_3')),
                            column(12,style="margin-top:10px;font-size:120%; font-family:Times New Roman;",  
                                   htmlOutput('family_3')),
                            column(12,style="margin-top:10px;font-size:120%; font-family:Times New Roman;",  
                                   htmlOutput('cs_3')),
                            column(12, style = "text-align: right; margin-top: -120px; font-size:150%;", 
                                   prettyCheckbox(
                                     inputId = "Select_Model_3", label = NULL, icon = icon("check"), 
                                     status = "default", shape = "curve", animation = "jelly"
                                   ))),
                        div(style="display:inline-block;float:right;padding:10px;font-size:80%;",actionButton("Analysisbutton", "Run", icon("play"), 
                                                                      style="width: 150px;color: #fff; background-color: #337ab7; border-color: #2e6da4")),
                        div(style="display:inline-block;float:right;padding:10px;font-size:80%;",actionButton("Editbutton", "Edit", icon("pencil"), 
                                                                      style="width: 150px;color: #fff; background-color: #337ab7; border-color: #2e6da4")),
                        div(style="display:inline-block;float:right;padding:10px;font-size:80%;",actionButton("ADDbutton", "Add", icon("plus"), 
                                                                                                              style="width: 150px;color: #fff; background-color: #337ab7; border-color: #2e6da4"),actionButton("ADDbutton_3","Add",icon("plus"),style="width: 150px;color: #fff; background-color: #337ab7; border-color: #2e6da4"))
                    ),
                tabItem("ROC",tabBox(
                  title = "Validation: ROC Analysis",
                  id = "ROC_Analysis", height = "1000px", width = "1000px",
                  tabPanel("Data Split",div(style = " font-family: Georgia; font-weight: normal; text-align: right; margin-top: 10px; margin-right: 10px",dropdownButton(includeMarkdown("Help_Data.Rmd"), circle = TRUE, icon = icon('info'),size = "xs",width = "300px", up = FALSE, right = TRUE)), column(4,style="display: inline-block;margin-top:50px;margin-right:0px;font-size:120%;font-weight: bold; font-family:Georgia;",  
                                           "Define your data split for ROC validation: "),
                        column(8,style="display: inline-block;margin-top:45px; font-size:120%; font-family:Times New Roman;",
                               selectInput("Split_data",NULL,c('Train: 70% - Test: 30%','Train: 75% - Test: 25%','Train: 80% - Test: 20%',
                                                       'Default'), selected = "Default", width = "600px")),
                        column(12,style = "text-align: right; margin-top: 50px",
                               actionButton("Splitbtn", "Run ROC", icon("play"), 
                                            style="width: 150px;color: #fff; background-color: #337ab7; border-color: #2e6da4"))),
                  tabPanel("ROC: Graph Visualization",
                           plotOutput("ROCplot")%>% withSpinner(color="#0dc5c1"),downloadButton("Download_Plot", label = "Download",style="width: 150px;color: #fff; background-color: #337ab7; border-color: #2e6da4") ,
                           withSpinner(plotOutput("ROCplot2"),color="#0dc5c1"),column (12,style="text-align: bottom-center;",downloadButton("Download_Plot_2", label = "Download",style="width: 150px;color: #fff; background-color: #337ab7; border-color: #2e6da4"))),
                  tabPanel("ROC: Results",
                           box(id = "ROC_Model1", solidHeader = TRUE,collapsible = TRUE, width = "800px",height = "200px",
                               column(12,style="text-align: center; margin-top:0px;font-size:150%;font-weight: bold; font-family:Georgia;",  
                                      textOutput('ROC_model_name_1')),
                               column(12,style="text-align: center; margin-top:10px;font-size:120%; font-family:Georgia;",  
                                      textOutput('ROC_model_def_1')),
                               column(4,style="margin-top:50px;margin-left:120px;margin-right:0px;font-size:160%;font-weight: bold; font-family:Georgia;",  
                                      "Accuraccy: "),
                               column(5,style="margin-top:55px;margin-left:-150px; font-size:150%;font-weight: bold; font-family:Times New Roman; color: red; ",  
                                      textOutput("ROC_ACC_1")),
                               column(5,style="margin-top:50px;margin-left:-150px;margin-right:0px;font-size:160%;font-weight: bold; font-family:Georgia;",  
                                      "AUC: "),
                               column(2,style="margin-top:55px;margin-left:-280px;font-size:150%;font-weight: bold; font-family:Times New Roman; color: green;",  
                                      textOutput("ROC_AUC_1"))),
                           box(id = "ROC_Model2", solidHeader = TRUE,collapsible = TRUE, width = "800px",height = "200px",
                               column(12,style="text-align: center; margin-top:0px;font-size:150%;font-weight: bold; font-family:Georgia;",  
                                      textOutput('ROC_model_name_2')),
                               column(12,style="text-align: center; margin-top:10px;font-size:120%; font-family:Georgia;",  
                                      textOutput('ROC_model_def_2')),
                               column(4,style="margin-top:50px;margin-left:120px;margin-right:0px;font-size:180%;font-weight: bold; font-family:Georgia;",  
                                      "Accuraccy: "),
                               column(5,style="margin-top:55px;margin-left:-150px; font-size:150%;font-weight: bold; font-family:Times New Roman; color: red; ",  
                                      textOutput("ROC_ACC_2")),
                               column(5,style="margin-top:50px;margin-left:-150px;margin-right:0px;font-size:180%;font-weight: bold; font-family:Georgia;",  
                                      "AUC: "),
                               column(2,style="margin-top:55px;margin-left:-280px;font-size:150%;font-weight: bold; font-family:Times New Roman; color: green;",  
                                      textOutput("ROC_AUC_2"))),
                           box(id = "ROC_Model3", solidHeader = TRUE,collapsible = TRUE, width = "800px",height = "200px",
                               column(12,style="text-align: center; margin-top:0px;font-size:150%;font-weight: bold; font-family:Georgia;",  
                                      textOutput('ROC_model_name_3')),
                               column(12,style="text-align: center; margin-top:10px;font-size:120%; font-family:Georgia;",  
                                      textOutput('ROC_model_def_3')),
                               column(4,style="margin-top:50px;margin-left:120px;margin-right:0px;font-size:180%;font-weight: bold; font-family:Georgia;",  
                                      "Accuraccy: "),
                               column(5,style="margin-top:55px;margin-left:-150px; font-size:150%;font-weight: bold; font-family:Times New Roman; color: red; ",  
                                      textOutput("ROC_ACC_3")),
                               column(5,style="margin-top:50px;margin-left:-150px;margin-right:0px;font-size:180%;font-weight: bold; font-family:Georgia;",  
                                      "AUC: "),
                               column(2,style="margin-top:55px;margin-left:-280px;font-size:150%;font-weight: bold; font-family:Times New Roman; color: green;",  
                                      textOutput("ROC_AUC_3"))))
                        )),
                tabItem("GEE_results", tabBox(
                  title = "GEE Results",
                  id = "Results_GEE", height = "800px", width = "1000px", div(style = " font-family: Georgia; font-weight: normal; text-align: right; margin-top: 10px; margin-right: 10px",dropdownButton(includeMarkdown("Help_GEEResults.Rmd"), circle = TRUE, icon = icon('info'),size = "xs",width = "300px", up = FALSE, right = TRUE)),
                  box(id = "Show_models",solidHeader = TRUE, width = 3, height = "200px",
                      column(12,style="margin-bottom:20px;font-size:120%;font-weight: bold; font-family:Georgia;",  
                             "GEE Models:  "),
                      column(12,style="margin-bottom:20px;font-size:110%; font-family:Times New Roman;",
                             prettyRadioButtons("Models",NULL,choices = c("Model 1", "Model 2", "Model 3"),
                                                icon = icon("check"), status = "default", shape = "curve", animation = "jelly")),
                  ), mainPanel(
                    DT::dataTableOutput("geeresults"), style = " text-align: right; height:500px; width:700px; overflow-x: scroll;"
                  ),
                  div(style="float:right;margin-top: 300px;padding:10px;font-size:80%;",downloadButton("Download_Results", label = "Download",style="width: 150px;color: #fff; background-color: #337ab7; border-color: #2e6da4")))
                ),
                
                #Menu informativo
                    tabItem("Sobre",titlePanel("About"),
                            br(),
                            div(includeMarkdown("Help_About.Rmd"), 
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
              df <- suppressWarnings(read.csv(input$file1$datapath,header = input$header, sep = input$sep,
                             quote = input$quote))
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
    
    #Turn certain columns as factors and update the data
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
    
    observeEvent(input$ADDbutton_3, {
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
              if(input$Coexpression == ""){
                form = as.formula(paste(input$VR, '~' , s, collapse = ""))
              }
              else{
                form = as.formula(paste(input$VR, '~', input$Coexpression, collapse = ""))
              }
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
      
      observe({
        shinyjs:: hide("Savebutton")
        shinyjs:: disable("Savebutton")
        if (input$Editbutton){
          shinyjs:: show("Savebutton")
          shinyjs:: enable("Savebutton")
          shinyjs:: hide("MDbutton")
          shinyjs:: disable("MDbutton")
        }
      })
      
      
      #Values for 1st model
      boxvalues1 <- reactiveValues(mn = NULL, md = NULL, vr = NULL, co = NULL, coex = NULL,
                                   id = NULL, fm = NULL,  cs = NULL)
      
      boxvalues2 <- reactiveValues(mn = NULL, md = NULL, vr = NULL, co = NULL, coex = NULL,
                                   id = NULL, fm = NULL,  cs = NULL)
      
      boxvalues3 <- reactiveValues(mn = NULL, md = NULL, vr = NULL, co = NULL, coex = NULL,
                                   id = NULL, fm = NULL,  cs = NULL)
      
      lastboxcreated <- reactiveValues(boxname= NULL)
      lastbox <- reactiveValues(lastBox = NULL, currentBox = NULL)
      ADDclick = reactiveValues(num = 0)
      ADD3click = reactiveValues(num = 0)
      
      savemodel <- function(){
          boxvalues1$mn <- input$Model_name
          boxvalues1$md <- input$Model_description
          boxvalues1$vr <- input$VR
          if(input$Coexpression == ""){
            boxvalues1$co <- input$Variables
            boxvalues1$coex <- NULL
          }
          else{
            boxvalues1$coex <- input$Coexpression
            boxvalues1$co <- NULL
          }
          boxvalues1$id <- input$ID
          boxvalues1$fm <- input$Family
          boxvalues1$cs <- input$CS
      }
      
      savemodel_2 <- function(){
        boxvalues2$mn <- input$Model_name
        boxvalues2$md <- input$Model_description
        boxvalues2$vr <- input$VR
        if(input$Coexpression == ""){
          boxvalues2$co <- input$Variables
          boxvalues2$coex <- NULL
        }
        else{
          boxvalues2$coex <- input$Coexpression
          boxvalues2$co <- NULL
        }
        boxvalues2$id <- input$ID
        boxvalues2$fm <- input$Family
        boxvalues2$cs <- input$CS
      }
      
      savemodel_3 <- function(){
        boxvalues3$mn <- input$Model_name
        boxvalues3$md <- input$Model_description
        boxvalues3$vr <- input$VR
        if(input$Coexpression == ""){
          boxvalues3$co <- input$Variables
          boxvalues3$coex <- NULL
        }
        else{
          boxvalues3$coex <- input$Coexpression
          boxvalues3$co <- NULL
        }
        boxvalues3$id <- input$ID
        boxvalues3$fm <- input$Family
        boxvalues3$cs <- input$CS
      }
      
      
      
      show_model <- function(use){
          if(use == 'now' ){
            output$values <- renderText({
              input$Model_name
            })
            output$des <- renderText({
              input$Model_description
            })
            output$vr <- renderText({
              paste('<b>Response Variable: ',"</b>", input$VR)
            })
            if(input$Coexpression == ""){
              output$co <- renderText({
                paste('<b>Covariates: ' ,"</b>", result_s(input$Variables) )
              })
            }
            else{
              output$co <- renderText({
                paste('<b>Covariates: ' ,"</b>", input$Coexpression )
              })
            }
            
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
          else if(use == 'old'){
            output$values <- renderText({
              boxvalues1$mn
            })
            output$des <- renderText({
              boxvalues1$md
            })
            output$vr <- renderText({
              paste('<b>Response Variable: ',"</b>", boxvalues1$vr)
            })
            if(is.null(boxvalues1$coex)){
              output$co <- renderText({
                paste('<b>Covariates: ' ,"</b>", result_s(boxvalues1$co) )
              })
            }
            else{
              output$co <- renderText({
                paste('<b>Covariates: ' ,"</b>", boxvalues1$coex )
              })
            }
            
            output$id <- renderText({
              paste('<b>ID: ' , "</b>", boxvalues1$id)
            })
            output$family <- renderText({
              paste('<b>Family: ', "</b>", boxvalues1$fm)
            })
            output$cs <- renderText({
              paste('<b>Correlation Structure: ' ,"</b>", boxvalues1$cs)
            })
          }
      }
          
      
      show_model_2 <- function(use){
        if(use == 'now'){
            output$values_2 <- renderText({
              input$Model_name
            })
            output$des_2 <- renderText({
              input$Model_description
            })
            output$vr_2 <- renderText({
              paste('<b>Response Variable: ',"</b>", input$VR)
            })
            if(input$Coexpression == ""){
              output$co_2 <- renderText({
                paste('<b>Covariates: ' ,"</b>", result_s(input$Variables) )
              })
            }
            else{
              output$co_2 <- renderText({
                paste('<b>Covariates: ' ,"</b>", input$Coexpression )
              })
            }
            
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
        else if (use == 'old'){
          output$values_2 <- renderText({
            boxvalues2$mn
          })
          output$des_2 <- renderText({
            boxvalues2$md
          })
          output$vr_2 <- renderText({
            paste('<b>Response Variable: ',"</b>", boxvalues2$vr)
          })
          if(is.null(boxvalues2$coex)){
            output$co_2 <- renderText({
              paste('<b>Covariates: ' ,"</b>", result_s(boxvalues2$co) )
            })
          }
          else{
            output$co_2 <- renderText({
              paste('<b>Covariates: ' ,"</b>", boxvalues2$coex )
            })
          }
          
          output$id_2 <- renderText({
            paste('<b>ID: ' , "</b>", boxvalues2$id)
          })
          output$family_2 <- renderText({
            paste('<b>Family: ', "</b>", boxvalues2$fm)
          })
          output$cs_2 <- renderText({
            paste('<b>Correlation Structure: ' ,"</b>", boxvalues2$cs)
          })
        }
      }
      
      
      show_model_3 <- function(use){
        if(use == 'now'){
            output$values_3 <- renderText({
              input$Model_name
            })
            output$des_3 <- renderText({
              input$Model_description
            })
            output$vr_3 <- renderText({
              paste('<b>Response Variable: ',"</b>", input$VR)
            })
            if(input$Coexpression == ""){
              output$co_3 <- renderText({
                paste('<b>Covariates: ' ,"</b>", result_s(input$Variables) )
              })
            }
            else{
              output$co_3 <- renderText({
                paste('<b>Covariates: ' ,"</b>", input$Coexpression )
              })
            }
            
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
        else if (use == 'old'){
          output$values_3 <- renderText({
            boxvalues3$mn
          })
          output$des_3 <- renderText({
            boxvalues3$md
          })
          output$vr_3 <- renderText({
            paste('<b>Response Variable: ',"</b>", boxvalues3$vr)
          })
          if(is.null(boxvalues3$coex)){
            output$co_3 <- renderText({
              paste('<b>Covariates: ' ,"</b>", result_s(boxvalues3$co) )
            })
          }
          else{
            output$co_3 <- renderText({
              paste('<b>Covariates: ' ,"</b>", boxvalues3$coex )
            })
          }
          
          output$id_3 <- renderText({
            paste('<b>ID: ' , "</b>", boxvalues3$id)
          })
          output$family_3 <- renderText({
            paste('<b>Family: ', "</b>", boxvalues3$fm)
          })
          output$cs_3 <- renderText({
            paste('<b>Correlation Structure: ' ,"</b>", boxvalues3$cs)
          })
        }
      }
      
      
      
      observeEvent(input$MDbutton,{
        if (input$MDbutton == 1){
          shinyjs::hide(id = "Model_2")
          shinyjs::hide(id = "Model_3")
          savemodel()
          show_model('now')
          updatePrettyCheckbox(session,"Select_Model", value = TRUE)
          lastboxcreated$boxname = 'box1'
          print(lastboxcreated$boxname)
          shinyjs::hide("ADDbutton_3")
          shinyjs::disable("ADDbutton_3")
        }
        if ((ADDclick$num == 1) ){
          shinyjs::show("Model_2")
          print(boxvalues1$mn)
          print(boxvalues1$co)
          show_model('old')
          savemodel_2()
          show_model_2('now')
          updatePrettyCheckbox(session,"Select_Model", value = FALSE)
          updatePrettyCheckbox(session,"Select_Model_2", value = TRUE)
          lastboxcreated$boxname = 'box2'
          shinyjs::hide("ADDbutton")
          shinyjs::disable("ADDbutton")
          shinyjs::show("ADDbutton_3")
          shinyjs::enable("ADDbutton_3")
          ADDclick$num <- 0
          
        }
        if ((ADD3click$num == 1) ){
          shinyjs::show("Model_3")
          shinyjs::disable("ADDbutton_3")
          show_model_2('old')
          show_model_3('now')
          updatePrettyCheckbox(session,"Select_Model_2", value = FALSE)
          updatePrettyCheckbox(session,"Select_Model_3", value = TRUE)
          lastboxcreated$boxname = 'box3'
          savemodel_3()
          ADD3click$num <- 0
          shinyjs::hide("ADDbutton")
          shinyjs::disable("ADDbutton")
          shinyjs::show("ADDbutton_3")
          shinyjs::enable("ADDbutton_3")
          
        }
      })
      
      
      
      #ADD button functions
      observeEvent(input$ADDbutton,{
          shinyjs::show("Model_2")
          shinyjs::hide("Savebutton")
          shinyjs::disable("Savebutton")
          shinyjs::show("MDbutton")
          shinyjs::enable("MDbutton")
          updateCheckboxGroupInput(session, "Variables", selected = character(0))
          updateTextAreaInput(session, "Coexpression", value = "")
          updateTextInput(session, "Model_name", value = "")
          updateTextAreaInput(session,"Model_description",value = "")
          updatePrettyCheckbox(session,"QIC_Show", value = FALSE)
          ADDclick$num <- 1
      })
      
      
      
      
      observeEvent(input$ADDbutton_3,{
        if(input$ADDbutton_3 == 1){
          shinyjs::hide("Savebutton")
          shinyjs::disable("Savebutton")
          shinyjs::show("MDbutton")
          shinyjs::enable("MDbutton")
          updateCheckboxGroupInput(session, "Variables", selected = character(0))
          updateTextAreaInput(session, "Coexpression", value = "")
          updateTextInput(session, "Model_name", value = "")
          updateTextAreaInput(session,"Model_description",value = "")
          updatePrettyCheckbox(session,"QIC_Show", value = FALSE)
          ADD3click$num <- 1
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
            if(is.null(boxvalues1$coex)){
              updateCheckboxGroupInput(session, "Variables", selected = boxvalues1$co)
            }
            else{
              updateTextAreaInput(session, "Coexpression", value = boxvalues1$coex)
            }
            updateTextInput(session, "Model_name", value = boxvalues1$mn)
            updateTextAreaInput(session,"Model_description",value = boxvalues1$md)
            updatePrettyCheckbox(session,"QIC_Show", value = FALSE)
            observeEvent(input$Savebutton,{  
              show_model('now')
              savemodel()
            })
            lastboxcreated$boxname = "box1"
          }
          else if (lastbox$currentBox == 'box2'){
            if(is.null(boxvalues2$coex)){
              updateCheckboxGroupInput(session, "Variables", selected = boxvalues2$co)
            }
            else{
              updateTextAreaInput(session, "Coexpression", value = boxvalues2$coex)
            }
            updateTextInput(session, "Model_name", value = boxvalues2$mn)
            updateTextAreaInput(session,"Model_description",value = boxvalues2$md)
            updatePrettyCheckbox(session,"QIC_Show", value = FALSE)
            observeEvent(input$Savebutton,{
              show_model_2('now')
              savemodel_2()
            })
            lastboxcreated$boxname = "box2"
          }
          else if (lastbox$currentBox == 'box3'){
            if(is.null(boxvalues3$coex)){
              updateCheckboxGroupInput(session, "Variables", selected = boxvalues3$co)
            }
            else{
              updateTextAreaInput(session, "Coexpression", value = boxvalues3$coex)
            }
            updateTextInput(session, "Model_name", value = boxvalues3$mn)
            updateTextAreaInput(session,"Model_description",value = boxvalues3$md)
            updatePrettyCheckbox(session,"QIC_Show", value = FALSE)
            observeEvent(input$Savebutton,{
              show_model_3('now')
              savemodel_3()
            })
            lastboxcreated$boxname = "box3"
          }
            
        }
        else if(input$Select_Model){
          if (is.null(boxvalues1$coex)){
            updateCheckboxGroupInput(session, "Variables", selected = boxvalues1$co)
          }
          else{
            updateTextAreaInput(session, "Coexpression", value = boxvalues1$coex)
          }
          updateTextInput(session, "Model_name", value = boxvalues1$mn)
          updateTextAreaInput(session,"Model_description",value = boxvalues1$md)
          updatePrettyCheckbox(session,"QIC_Show", value = FALSE)
          if(lastboxcreated$boxname == "box1"){
            observeEvent(input$Savebutton,{
              show_model('now')
              savemodel()
            })
          }
          else if(lastboxcreated$boxname == "box2"){
            show_model_2('old')
            observeEvent(input$Savebutton, {
              show_model('now')
              savemodel()
            })
          }
          else if(lastboxcreated$boxname == "box3"){
            show_model_3('old')
            observeEvent(input$Savebutton, {
              show_model('now')
              savemodel()
            })
          }
          lastboxcreated$boxname = "box1"
        }
        else if(input$Select_Model_2){
          if (is.null(boxvalues2$coex)){
            updateCheckboxGroupInput(session, "Variables", selected = boxvalues2$co)
          }
          else{
            updateTextAreaInput(session, "Coexpression", selected = boxvalues2$coex)
          }
          updateTextInput(session, "Model_name", value = boxvalues2$mn)
          updateTextAreaInput(session,"Model_description",value = boxvalues2$md)
          updatePrettyCheckbox(session,"QIC_Show", value = FALSE)
          if(lastboxcreated$boxname == "box2"){
            observeEvent(input$Savebutton,{
              show_model_2('now')
              savemodel_2()
            })
          }
          else if(lastboxcreated$boxname == "box1"){
            show_model('old')
            observeEvent(input$Savebutton,{
              show_model_2('now')
              savemodel_2()
            })
          }
          else if(lastboxcreated$boxname == "box3"){
            show_model_3('old')
            observeEvent(input$Savebutton,{
              show_model_2('now')
              savemodel_2()
            })
          }
          lastboxcreated$boxname = "box2"
        }
        else if(input$"Select_Model_3"){
          if (is.null(boxvalues3$coex)){
            updateCheckboxGroupInput(session, "Variables", selected = boxvalues3$co)
          }
          else{
            updateTextAreaInput(session, "Coexpression", selected = boxvalues3$coex)
          }
          updateTextInput(session, "Model_name", value = boxvalues3$mn)
          updateTextAreaInput(session,"Model_description",value = boxvalues3$md)
          updatePrettyCheckbox(session,"QIC_Show", value = FALSE)
          if(lastboxcreated$boxname == "box3"){
            observeEvent(input$Savebutton,{
              show_model_3('now')
              savemodel_3()
            })
          }
          else if(lastboxcreated$boxname == "box1"){
            show_model('old')
            observeEvent(input$Savebutton,{
              show_model_3('now')
              savemodel_3()
            })
          }
          else if(lastboxcreated$boxname == "box2"){
            show_model_2('old')
            observeEvent(input$Savebutton,{
              show_model_3('now')
              savemodel_3()
            })
          }
          lastboxcreated$boxname = "box3"
        }
        
        
      })
      
      Model1 <- reactiveValues(mn = NULL, md = NULL, vr = NULL, co = NULL, coex = NULL,
                                   id = NULL, fm = NULL,  cs = NULL)
      Model2 <- reactiveValues(mn = NULL, md = NULL, vr = NULL, co = NULL, coex = NULL,
                               id = NULL, fm = NULL,  cs = NULL)
      Model3 <- reactiveValues(mn = NULL, md = NULL, vr = NULL, co = NULL, coex = NULL,
                               id = NULL, fm = NULL,  cs = NULL)
      
      #Run button functions
      observeEvent(input$Analysisbutton,{
          if((input$Select_Model) && (input$Select_Model_2 == FALSE) && (input$Select_Model_3 == FALSE)){
            shinyjs::hide("Download_Plot_2")
            shinyjs::hide("ROC_Model2")
            shinyjs::hide("ROC_Model3")
            if(lastboxcreated$boxname == lastbox$currentBox){
              Model1$mn = input$Model_name
              Model1$md <- input$Model_description
              Model1$vr <- input$VR
              if (input$Coexpression == ""){
                Model1$co <- input$Variables
              }
              else{
                Model1$coex <- input$Coexpression
              }
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
              if (is.null(boxvalues$coex)){
                Model1$co <- boxvalues1$co
              }
              else{
                Model1$coex <-boxvalues1$coex
              }
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
            mychoices <- c(Model1$mn)
            updatePrettyRadioButtons(session, "Models", choices = mychoices,prettyOptions = list(icon = icon("check"), status = "default", shape = "curve", animation = "jelly"))
            observeEvent(input$Models, {
              if (input$Models == Model1$mn){
                df = currentable$tab1
                output$geeresults <- renderDataTable({
                  Gee_Analysis(df,Model1)
                })
                output$Download_Results <-downloadHandler(
                  filename = function(){
                    paste(Model1$mn,".csv",sep="")
                  },
                  content=function(file){
                    write.csv(Gee_Analysis(df,Model1),file, row.names = TRUE)
                  })
              }
            })
          }
          else if ((input$Select_Model == FALSE) && (input$Select_Model_2) && (input$Select_Model_3 == FALSE)){
            shinyjs::hide("Download_Plot_2")
            shinyjs::hide("ROC_Model2")
            shinyjs::hide("ROC_Model3")
            if(lastboxcreated$boxname == lastbox$currentBox){
              Model2$mn = input$Model_name
              Model2$md <- input$Model_description
              Model2$vr <- input$VR
              if (input$Coexpression == ""){
                Model2$co <- input$Variables
              }
              else{
                Model$coex <- input$Coexpression
              }
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
              if (is.null(boxvalues2$coex)){
                Model$co <- boxvalues2$co
              }
              else{
                Model$coex <-boxvalues2$coex
              }
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
            mychoices <- c(Model2$mn)
            updatePrettyRadioButtons(session, "Models", choices = mychoices,prettyOptions = list(icon = icon("check"), status = "default", shape = "curve", animation = "jelly"))
            observeEvent(input$Models, {
              if (input$Models == Model2$mn){
                df = currentable$tab1
                output$geeresults <- renderDataTable({
                  Gee_Analysis(df,Model2)
                })
                output$Download_Results <-downloadHandler(
                  filename = function(){
                    paste(Model2$mn,".csv",sep="")
                  },
                  content=function(file){
                    write.csv(Gee_Analysis(df,Model2),file, row.names = TRUE)
                  })
              }
            })
          }
          else if ((input$Select_Model == FALSE) && (input$Select_Model_2 == FALSE) && (input$Select_Model_3)){
            shinyjs::hide("Download_Plot_2")
            shinyjs::hide("ROC_Model2")
            shinyjs::hide("ROC_Model3")
            if(lastboxcreated$boxname == lastbox$currentBox){
              Model3$mn = input$Model_name
              Model3$md <- input$Model_description
              Model3$vr <- input$VR
              if (input$Coexpression == ""){
                Model3$co <- input$Variables
              }
              else{
                Model3$coex <- input$Coexpression
              }
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
              if (is.null(boxvalues3$coex)){
                Model3$co <- boxvalues3$co
              }
              else{
                Model3$coex <- boxvalues3$coex
              }
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
            mychoices <- c(Model3$mn)
            updatePrettyRadioButtons(session, "Models", choices = mychoices,prettyOptions = list(icon = icon("check"), status = "default", shape = "curve", animation = "jelly"))
            observeEvent(input$Models, {
              if (input$Models == Model3$mn){
                df = currentable$tab1
                output$geeresults <- renderDataTable({
                  Gee_Analysis(df,Model3)
                })
                output$Download_Results <-downloadHandler(
                  filename = function(){
                    paste(Model3$mn,".csv",sep="")
                  },
                  content=function(file){
                    write.csv(Gee_Analysis(df,Model3),file, row.names = TRUE)
                  })
              }
            })
          }
          else if((input$Select_Model) && (input$Select_Model_2) && (input$Select_Model_3 == FALSE)){
            shinyjs::show("Download_Plot_2")
            shinyjs::hide("ROC_Model3")
            if ((lastboxcreated$boxname == "box1")){
              Model1$mn = input$Model_name
              Model1$md <- input$Model_description
              Model1$vr <- input$VR
              if (input$Coexpression == ""){
                Model1$co <- input$Variables
              }
              else{
                Model1$coex <- input$Coexpression
              }
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
              if (is.null(boxvalues2$coex)){
                Model2$co <- boxvalues2$co
              }
              else{
                Model2$coex <- boxvalues2$coex
              }
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
              if (input$Coexpression == ""){
                Model2$co <- input$Variables
              }
              else{
                Model2$coex <- input$Coexpression
              }
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
              if (is.null(boxvalues1$coex)){
                Model1$co <- boxvalues1$co
              }
              else{
                Model1$coex <- boxvalues1$coex
              }
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
              if (is.null(boxvalues1$coex)){
                Model1$co <- boxvalues1$co
              }
              else{
                Model1$coex <- boxvalues1$coex
              }
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
              if (is.null(boxvalues2$coex)){
                Model2$co <- boxvalues2$co
              }
              else{
                Model2$coex <- boxvalues2$coex
              }
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
            mychoices <- c(Model1$mn,Model2$mn)
            updatePrettyRadioButtons(session, "Models", choices = mychoices,selected = Model1$mn,prettyOptions = list(icon = icon("check"), status = "default", shape = "curve", animation = "jelly"))
            observeEvent(input$Models, {
              if (input$Models == Model1$mn){
                df = currentable$tab1
                output$geeresults <- renderDataTable({
                  Gee_Analysis(df,Model1)
                })
                output$Download_Results <-downloadHandler(
                  filename = function(){
                    paste(Model1$mn,".csv",sep="")
                  },
                  content=function(file){
                    write.csv(Gee_Analysis(df,Model1),file, row.names = TRUE)
                  })
              }
              else if (input$Models == Model2$mn){
                df = currentable$tab1
                output$geeresults <- renderDataTable({
                  Gee_Analysis(df,Model2)
                })
                output$Download_Results <-downloadHandler(
                  filename = function(){
                    paste(Model2$mn,".csv",sep="")
                  },
                  content=function(file){
                    write.csv(Gee_Analysis(df,Model2),file, row.names = TRUE)
                  })
              }
            })
          }
          else if((input$Select_Model) && (input$Select_Model_2 == FALSE) && (input$Select_Model_3)){
            shinyjs::show("Download_Plot_2")
            shinyjs::hide("ROC_Model3")
            if ((lastboxcreated$boxname == "box1")){
              Model1$mn = input$Model_name
              Model1$md <- input$Model_description
              Model1$vr <- input$VR
              if (input$Coexpression == ""){
                Model1$co <- input$Variables
              }
              else{
                Model1$coex <- input$Coexpression
              }
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
              if (is.null(boxvalues3$coex)){
                Model3$co <- boxvalues3$co
              }
              else{
                Model3$coex <- boxvalues3$coex
              }
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
              if (input$Coexpression == ""){
                Model3$co <- input$Variables
              }
              else{
                Model3$coex <- input$Coexpression
              }
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
              if (is.null(boxvalues1$coex)){
                Model1$co <- boxvalues1$co
              }
              else{
                Model1$coex <- boxvalues1$coex
              }
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
              if (is.null(boxvalues1$coex)){
                Model1$co <- boxvalues1$co
              }
              else{
                Model1$coex <- boxvalues1$coex
              }
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
              if (is.null(boxvalues3$coex)){
                Model3$co <- boxvalues3$co
              }
              else{
                Model3$coex <- boxvalues3$coex
              }
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
            mychoices <- c(Model1$mn, Model3$mn)
            updatePrettyRadioButtons(session, "Models", choices = mychoices,selected = Model1$mn,prettyOptions = list(icon = icon("check"), status = "default", shape = "curve", animation = "jelly"))
            observeEvent(input$Models, {
              if (input$Models == Model1$mn){
                df = currentable$tab1
                output$geeresults <- renderDataTable({
                  Gee_Analysis(df,Model1)
                })
                output$Download_Results <-downloadHandler(
                  filename = function(){
                    paste(Model1$mn,".csv",sep="")
                  },
                  content=function(file){
                    write.csv(Gee_Analysis(df,Model1),file, row.names = TRUE)
                  })
              }
              else if (input$Models == Model3$mn){
                df = currentable$tab1
                output$geeresults <- renderDataTable({
                  Gee_Analysis(df,Model3)
                })
                output$Download_Results <-downloadHandler(
                  filename = function(){
                    paste(Model3$mn,".csv",sep="")
                  },
                  content=function(file){
                    write.csv(Gee_Analysis(df,Model3),file, row.names = TRUE)
                  })
              }
            })
          }
          else if((input$Select_Model == FALSE) && (input$Select_Model_2) && (input$Select_Model_3)){
            shinyjs::show("Download_Plot_2")
            shinyjs::hide("ROC_Model3")
            if ((lastboxcreated$boxname == "box2")){
              Model2$mn = input$Model_name
              Model2$md <- input$Model_description
              Model2$vr <- input$VR
              if (input$Coexpression == ""){
                Model2$co <- input$Variables
              }
              else{
                Model2$coex <- input$Coexpression
              }
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
              if (is.null(boxvalues3$coex)){
                Model3$co <- boxvalues3$co
              }
              else{
                Model3$coex <- boxvalues3$coex
              }
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
              if (input$Coexpression == ""){
                Model3$co <- input$Variables
              }
              else{
                Model3$coex <- input$Coexpression
              }
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
              if (is.null(boxvalues2$coex)){
                Model2$co <- boxvalues2$co
              }
              else{
                Model2$coex <- boxvalues2$coex
              }
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
              if (is.null(boxvalues2$coex)){
                Model2$co <- boxvalues2$co
              }
              else{
                Model2$coex <- boxvalues2$coex
              }
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
              if (is.null(boxvalues3$coex)){
                Model3$co <- boxvalues3$co
              }
              else{
                Model3$coex <- boxvalues3$coex
              }
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
            mychoices <- c(Model2$mn,Model3$mn)
            updatePrettyRadioButtons(session, "Models", choices = mychoices,selected = Model2$mn,prettyOptions = list(icon = icon("check"), status = "default", shape = "curve", animation = "jelly"))
            observeEvent(input$Models, {
              if (input$Models == Model2$mn){
                df = currentable$tab1
                output$geeresults <- renderDataTable({
                  Gee_Analysis(df,Model2)
                })
                output$Download_Results <-downloadHandler(
                  filename = function(){
                    paste(Model2$mn,".csv",sep="")
                  },
                  content=function(file){
                    write.csv(Gee_Analysis(df,Model2),file, row.names = TRUE)
                  })
              }
              else if (input$Models == Model3$mn){
                df = currentable$tab1
                output$geeresults <- renderDataTable({
                  Gee_Analysis(df,Model3)
                })
                output$Download_Results <-downloadHandler(
                  filename = function(){
                    paste(Model3$mn,".csv",sep="")
                  },
                  content=function(file){
                    write.csv(Gee_Analysis(df,Model3),file, row.names = TRUE)
                  })
              }
            })
          }
          else if((input$Select_Model) && (input$Select_Model_2) && (input$Select_Model_3)){
            shinyjs::show("Download_Plot_2")
            if ((lastboxcreated$boxname == "box1")){
              Model1$mn = input$Model_name
              Model1$md <- input$Model_description
              Model1$vr <- input$VR
              if (input$Coexpression == ""){
                Model1$co <- input$Variables
              }
              else{
                Model1$coex <- input$Coexpression
              }
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
              if (is.null(boxvalues3$coex)){
                Model3$co <- boxvalues3$co
              }
              else{
                Model3$coex <- boxvalues3$coex
              }
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
              if (is.null(boxvalues2$coex)){
                Model2$co <- boxvalues2$co
              }
              else{
                Model2$coex <- boxvalues2$coex
              }
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
              if (input$Coexpression == ""){
                Model3$co <- input$Variables
              }
              else{
                Model3$coex <- input$Coexpression
              }
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
              if (is.null(boxvalues1$coex)){
                Model1$co <- boxvalues1$co
              }
              else{
                Model1$coex <- boxvalues1$coex
              }
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
              if (is.null(boxvalues2$coex)){
                Model2$co <- boxvalues2$co
              }
              else{
                Model2$coex <- boxvalues2$coex
              }
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
              if (input$Coexpression == ""){
                Model2$co <- input$Variables
              }
              else{
                Model2$coex <- input$Coexpression
              }
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
              if (is.null(boxvalues1$coex)){
                Model1$co <- boxvalues1$co
              }
              else{
                Model1$coex <- boxvalues1$coex
              }
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
              if (is.null(boxvalues3$coex)){
                Model3$co <- boxvalues3$co
              }
              else{
                Model3$coex <- boxvalues3$coex
              }
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
            mychoices <- c(Model1$mn,Model2$mn,Model3$mn)
            updatePrettyRadioButtons(session, "Models", choices = mychoices,selected = Model1$mn,prettyOptions = list(icon = icon("check"), status = "default", shape = "curve", animation = "jelly"))
            observeEvent(input$Models, {
              if (input$Models == Model1$mn){
                df = currentable$tab1
                output$geeresults <- renderDataTable({
                  Gee_Analysis(df,Model1)
                })
                output$Download_Results <-downloadHandler(
                  filename = function(){
                    paste(Model1$mn,".csv",sep="")
                  },
                  content=function(file){
                    write.csv(Gee_Analysis(df,Model1),file, row.names = TRUE)
                  })
              }
              else if (input$Models == Model2$mn){
                df = currentable$tab1
                output$geeresults <- renderDataTable({
                  Gee_Analysis(df,Model2)
                })
                output$Download_Results <-downloadHandler(
                  filename = function(){
                    paste(Model2$mn,".csv",sep="")
                  },
                  content=function(file){
                    write.csv(Gee_Analysis(df,Model2),file, row.names = TRUE)
                  })
              }
              else if (input$Models == Model3$mn){
                df = currentable$tab1
                output$geeresults <- renderDataTable({
                  Gee_Analysis(df,Model3)
                })
                output$Download_Results <-downloadHandler(
                  filename = function(){
                    paste(Model3$mn,".csv",sep="")
                  },
                  content=function(file){
                    write.csv(Gee_Analysis(df,Model3),file, row.names = TRUE)
                  })
              }
            })
          }
      })
      
      ROC_Analysis <- function(train,test,model){
        if (is.null(model$coex)){
          cov = result_s(model$co)
        }
        else{
          cov = model$coex
        }
        form = as.formula(paste(model$vr,'~',cov,collapse = ""))
        identification <<- model$id
        geemodel <<- geeglm(form,data = train,id = train[[identification]],family = model$fm,corstr = model$cs)
        pred_70_30 = predict(geemodel,test,type="response")
        pred_70_30 = prediction(pred_70_30, test[[model$vr]])
        perf_70_30 = performance(pred_70_30, "acc")
        roc_70_30 = performance(pred_70_30,"tpr","fpr")
        #ACC
        max_ind_70_30 = which.max(slot(perf_70_30, "y.values")[[1]] )
        acc_70_30 = slot(perf_70_30, "y.values")[[1]][max_ind_70_30]
        auc_70_30 = performance(pred_70_30, measure = "auc")
        auc = auc_70_30@y.values[[1]]
        return(list(roc_70_30,acc_70_30,auc))
      }
      
      observeEvent(input$Splitbtn,{
        df = currentable$tab1
        if ((input$Split_data == "Train: 70% - Test: 30%") || (input$Split_data == "Default")){
          smp_size <- floor(0.70 * nrow(df))
          set.seed(123)
          train_ind <- sample(seq_len(nrow(df)), size = smp_size)
          train_70 <- df[train_ind,]
          test_70 <- df[-train_ind, ]
          if ((input$Select_Model) && (input$Select_Model_2 == FALSE) && (input$Select_Model_3 == FALSE)){
            shinyjs::hide('ROC_Model2')
            shinyjs::hide('ROC_Model3')
            output$ROC_model_name_1 <- renderText({
              Model1$mn
            })
            output$ROC_model_def_1 <- renderText({
              Model1$md
            })
            results = ROC_Analysis(train_70,test_70,Model1)
            output$ROCplot <- renderPlot({
              plot(results[[1]], colorize = T, lwd = 2, main = Model1$mn)
            })
            output$Download_Plot <-downloadHandler(
              filename = function(){
                paste(Model1$mn,".png",sep="")
              },
              content=function(file){
                png(file)
                plot(results[[1]], colorize = T, lwd = 2, main = Model1$mn)
                dev.off()
              })
            output$ROCplot2 <- NULL
            output$ROC_ACC_1 <- renderText({
              results[[2]]
          })
            output$ROC_AUC_1 <- renderText({
              results[[3]]
            })
          }
          else if ((input$Select_Model == FALSE) && (input$Select_Model_2) && (input$Select_Model_3 == FALSE)){
            shinyjs::hide('ROC_Model2')
            shinyjs::hide('ROC_Model3')
            output$ROC_model_name_1 <- renderText({
              Model2$mn
            })
            output$ROC_model_def_1 <- renderText({
              Model2$md
            })
            results = ROC_Analysis(train_70,test_70,Model2)
            output$ROCplot <- renderPlot({
              plot(results[[1]], colorize = T, lwd = 2, main = Model2$mn)
            })
            output$Download_Plot <-downloadHandler(
              filename = function(){
                paste(Model2$mn,".png",sep="")
              },
              content=function(file){
                png(file)
                plot(results[[1]], colorize = T, lwd = 2, main = Model2$mn)
                dev.off()
              })
            output$ROCplot2 <- NULL
            output$ROC_ACC_1 <- renderText({
              results[[2]]
            })
            output$ROC_AUC_1 <- renderText({
              results[[3]]
            })
          }
          else if ((input$Select_Model == FALSE) && (input$Select_Model_2 == FALSE) && (input$Select_Model_3)){
            shinyjs::hide('ROC_Model2')
            shinyjs::hide('ROC_Model3')
            output$ROC_model_name_1 <- renderText({
              Model3$mn
            })
            output$ROC_model_def_1 <- renderText({
              Model3$md
            })
            results = ROC_Analysis(train_70,test_70,Model3)
            output$ROCplot <- renderPlot({
              plot(results[[1]], colorize = T, lwd = 2, main = Model3$mn)
            })
            output$Download_Plot <-downloadHandler(
              filename = function(){
                paste(Model3$mn,".png",sep="")
              },
              content=function(file){
                png(file)
                plot(results[[1]], colorize = T, lwd = 2, main = Model3$mn)
                dev.off()
              })
            output$ROCplot2 <- NULL
            output$ROC_ACC_1 <- renderText({
              results[[2]]
            })
            output$ROC_AUC_1 <- renderText({
              results[[3]]
            })
          }
          else if ((input$Select_Model) && (input$Select_Model_2) && (input$Select_Model_3==FALSE)){
            results = ROC_Analysis(train_70,test_70,Model1)
            results_2 = ROC_Analysis(train_70,test_70,Model2)
            shinyjs::show("ROC_Model2")
            shinyjs::hide('ROC_Model3')
            output$ROC_model_name_1 <- renderText({
              Model1$mn
            })
            output$ROC_model_def_1 <- renderText({
              Model1$md
            })
            output$ROCplot <- renderPlot({
              plot(results[[1]], colorize = T, lwd = 2, main = Model1$mn)
            })
            output$Download_Plot <-downloadHandler(
              filename = function(){
                paste(Model1$mn,".png",sep="")
              },
              content=function(file){
                png(file)
                plot(results[[1]], colorize = T, lwd = 2, main = Model1$mn)
                dev.off()
              })
            output$ROC_model_name_2 <- renderText({
              Model2$mn
            })
            output$ROC_model_def_2 <- renderText({
              Model2$md
            })
            output$ROCplot2 <- renderPlot({
              plot(results_2[[1]], colorize = T, lwd = 2, main = Model2$mn)
            })
            output$Download_Plot_2 <-downloadHandler(
              filename = function(){
                paste(Model2$mn,".png",sep="")
              },
              content=function(file){
                png(file)
                plot(results_2[[1]], colorize = T, lwd = 2, main = Model2$mn)
                dev.off()
              })
            output$ROC_ACC_1 <- renderText({
              results[[2]]
            })
            output$ROC_ACC_2 <- renderText({
              results_2[[2]]
            })
            output$ROC_AUC_1 <- renderText({
              results[[3]]
            })
            output$ROC_AUC_2 <- renderText({
              results_2[[3]]
            })
          }
          else if ((input$Select_Model) && (input$Select_Model_2 == FALSE) && (input$Select_Model_3)){
            results = ROC_Analysis(train_70,test_70,Model1)
            results_3 = ROC_Analysis(train_70,test_70,Model3)
            shinyjs::show("ROC_Model2")
            shinyjs::hide('ROC_Model3')
            output$ROC_model_name_1 <- renderText({
              Model1$mn
            })
            output$ROC_model_def_1 <- renderText({
              Model1$md
            })
            output$ROCplot <- renderPlot({
              plot(results[[1]], colorize = T, lwd = 2, main = Model1$mn)
            })
            output$Download_Plot <-downloadHandler(
              filename = function(){
                paste(Model1$mn,".png",sep="")
              },
              content=function(file){
                png(file)
                plot(results[[1]], colorize = T, lwd = 2, main = Model1$mn)
                dev.off()
              })
            output$ROC_model_name_2 <- renderText({
              Model3$mn
            })
            output$ROC_model_def_2 <- renderText({
              Model3$md
            })
            output$ROCplot2 <- renderPlot({
              plot(results_3[[1]], colorize = T, lwd = 2, main = Model3$mn)
            })
            output$Download_Plot_2 <-downloadHandler(
              filename = function(){
                paste(Model3$mn,".png",sep="")
              },
              content=function(file){
                png(file)
                plot(results_3[[1]], colorize = T, lwd = 2, main = Model3$mn)
                dev.off()
              })
            output$ROC_ACC_1 <- renderText({
              results[[2]]
            })
            output$ROC_ACC_2 <- renderText({
              results_3[[2]]
            })
            output$ROC_AUC_1 <- renderText({
              results[[3]]
            })
            output$ROC_AUC_2 <- renderText({
              results_3[[3]]
            })
          }
          else if ((input$Select_Model == FALSE) && (input$Select_Model_2) && (input$Select_Model_3)){
            results_2 = ROC_Analysis(train_70,test_70,Model2)
            results_3 = ROC_Analysis(train_70,test_70,Model3)
            shinyjs::show("ROC_Model2")
            shinyjs::hide('ROC_Model3')
            output$ROC_model_name_1 <- renderText({
              Model2$mn
            })
            output$ROC_model_def_1 <- renderText({
              Model2$md
            })
            output$ROCplot <- renderPlot({
              plot(results_2[[1]], colorize = T, lwd = 2, main = Model2$mn)
            })
            output$Download_Plot <-downloadHandler(
              filename = function(){
                paste(Model2$mn,".png",sep="")
              },
              content=function(file){
                png(file)
                plot(results_2[[1]], colorize = T, lwd = 2, main = Model2$mn)
                dev.off()
              })
            output$ROC_model_name_2 <- renderText({
              Model3$mn
            })
            output$ROC_model_def_2 <- renderText({
              Model3$md
            })
            output$ROCplot2 <- renderPlot({
              plot(results_3[[1]], colorize = T, lwd = 2, main = Model3$mn)
            })
            output$Download_Plot_2 <-downloadHandler(
              filename = function(){
                paste(Model3$mn,".png",sep="")
              },
              content=function(file){
                png(file)
                plot(results_3[[1]], colorize = T, lwd = 2, main = Model3$mn)
                dev.off()
              })
            output$ROC_ACC_1 <- renderText({
              results_2[[2]]
            })
            output$ROC_ACC_2 <- renderText({
              results_3[[2]]
            })
            output$ROC_AUC_1 <- renderText({
              results_2[[3]]
            })
            output$ROC_AUC_2 <- renderText({
              results_3[[3]]
            })
          }
          else if ((input$Select_Model) && (input$Select_Model_2) && (input$Select_Model_3)){
            results = ROC_Analysis(train_70,test_70,Model1)
            results_2 = ROC_Analysis(train_70,test_70,Model2)
            results_3 = ROC_Analysis(train_70,test_70,Model3)
            shinyjs::show("ROC_Model2")
            shinyjs::show("ROC_Model3")
            output$ROC_model_name_1 <- renderText({
              Model1$mn
            })
            output$ROC_model_def_1 <- renderText({
              Model1$md
            })
            output$ROC_model_name_2 <- renderText({
              Model2$mn
            })
            output$ROC_model_def_2 <- renderText({
              Model2$md
            })
            output$ROCplot <- renderPlot({
              par(mfrow = c(1,2))
              plot(results[[1]], colorize = T, lwd = 2, main = Model1$mn)
              plot(results_2[[1]], colorize = T, lwd = 2, main = Model2$mn)
            })
            output$Download_Plot <-downloadHandler(
              filename = function(){
                paste(Model1$mn,"_",Model2$mn,".png",sep="")
              },
              content=function(file){
                png(file)
                par(mfrow = c(1,2))
                plot(results[[1]], colorize = T, lwd = 2, main = Model1$mn)
                plot(results_2[[1]], colorize = T, lwd = 2, main = Model2$mn)
                dev.off()
              })
            output$ROC_model_name_3 <- renderText({
              Model3$mn
            })
            output$ROC_model_def_3 <- renderText({
              Model3$md
            })
            output$ROCplot2 <- renderPlot({
              plot(results_3[[1]], colorize = T, lwd = 2, main = Model3$mn)
            })
            output$Download_Plot_2 <-downloadHandler(
              filename = function(){
                paste(Model3$mn,".png",sep="")
              },
              content=function(file){
                png(file)
                plot(results_3[[1]], colorize = T, lwd = 2, main = Model3$mn)
                dev.off()
              })
            output$ROC_ACC_1 <- renderText({
              results[[2]]
            })
            output$ROC_ACC_2 <- renderText({
              results_2[[2]]
            })
            output$ROC_ACC_3 <- renderText({
              results_3[[2]]
            })
            output$ROC_AUC_1 <- renderText({
              results[[3]]
            })
            output$ROC_AUC_2 <- renderText({
              results_2[[3]]
            })
            output$ROC_AUC_3 <- renderText({
              results_3[[3]]
            })
          }
          
        }
        else if((input$Split_data == "Train: 75% - Test: 25%")){
          smp_size <- floor(0.75 * nrow(df))
          set.seed(123)
          train_ind <- sample(seq_len(nrow(df)), size = smp_size)
          train_75 <- df[train_ind,]
          test_75 <- df[-train_ind, ]
          if ((input$Select_Model) && (input$Select_Model_2 == FALSE) && (input$Select_Model_3 == FALSE)){
            shinyjs::hide('ROC_Model2')
            shinyjs::hide('ROC_Model3')
            results = ROC_Analysis(train_75,test_75,Model1)
            output$ROC_model_name_1 <- renderText({
              Model1$mn
            })
            output$ROC_model_def_1 <- renderText({
              Model1$md
            })
            output$ROCplot <- renderPlot({
              plot(results[[1]], colorize = T, lwd = 2, main = Model1$mn)
            })
            output$Download_Plot <-downloadHandler(
              filename = function(){
                paste(Model1$mn,".png",sep="")
              },
              content=function(file){
                png(file)
                plot(results[[1]], colorize = T, lwd = 2, main = Model1$mn)
                dev.off()
              })
            output$ROCplot2 <- NULL
            output$ROC_ACC_1 <- renderText({
              results[[2]]
            })
            output$ROC_AUC_1 <- renderText({
              results[[3]]
            })
          }
          else if ((input$Select_Model == FALSE) && (input$Select_Model_2) && (input$Select_Model_3 == FALSE)){
            shinyjs::hide('ROC_Model2')
            shinyjs::hide('ROC_Model3')
            results = ROC_Analysis(train_75,test_75,Model2)
            output$ROC_model_name_1 <- renderText({
              Model2$mn
            })
            output$ROC_model_def_1 <- renderText({
              Model2$md
            })
            output$ROCplot <- renderPlot({
              plot(results[[1]], colorize = T, lwd = 2, main = Model2$mn)
            })
            output$Download_Plot <-downloadHandler(
              filename = function(){
                paste(Model2$mn,".png",sep="")
              },
              content=function(file){
                png(file)
                plot(results[[1]], colorize = T, lwd = 2, main = Model2$mn)
                dev.off()
              })
            output$ROCplot2 <- NULL
            output$ROC_ACC_1 <- renderText({
              results[[2]]
            })
            output$ROC_AUC_1 <- renderText({
              results[[3]]
            })
          }
          else if ((input$Select_Model == FALSE) && (input$Select_Model_2 == FALSE) && (input$Select_Model_3)){
            shinyjs::hide('ROC_Model2')
            shinyjs::hide('ROC_Model3')
            results = ROC_Analysis(train_75,test_75,Model3)
            output$ROC_model_name_1 <- renderText({
              Model3$mn
            })
            output$ROC_model_def_1 <- renderText({
              Model3$md
            })
            output$ROCplot <- renderPlot({
              plot(results[[1]], colorize = T, lwd = 2, main = Model3$mn)
            })
            output$Download_Plot <-downloadHandler(
              filename = function(){
                paste(Model3$mn,".png",sep="")
              },
              content=function(file){
                png(file)
                plot(results[[1]], colorize = T, lwd = 2, main = Model3$mn)
                dev.off()
              })
            output$ROCplot2 <- NULL
            output$ROC_ACC_1 <- renderText({
              results[[2]]
            })
            output$ROC_AUC_1 <- renderText({
              results[[3]]
            })
          }
          else if ((input$Select_Model) && (input$Select_Model_2) && (input$Select_Model_3==FALSE)){
            shinyjs::show('ROC_Model2')
            shinyjs::hide('ROC_Model3')
            results = ROC_Analysis(train_75,test_75,Model1)
            results_2 = ROC_Analysis(train_75,test_75,Model2)
            output$ROC_model_name_1 <- renderText({
              Model1$mn
            })
            output$ROC_model_def_1 <- renderText({
              Model1$md
            })
            output$ROCplot <- renderPlot({
              plot(results[[1]], colorize = T, lwd = 2, main = Model1$mn)
            })
            output$Download_Plot <-downloadHandler(
              filename = function(){
                paste(Model1$mn,".png",sep="")
              },
              content=function(file){
                png(file)
                plot(results[[1]], colorize = T, lwd = 2, main = Model1$mn)
                dev.off()
              })
            output$ROC_model_name_2 <- renderText({
              Model2$mn
            })
            output$ROC_model_def_2 <- renderText({
              Model2$md
            })
            output$ROCplot2 <- renderPlot({
              plot(results_2[[1]], colorize = T, lwd = 2, main = Model2$mn)
            })
            output$Download_Plot_2 <-downloadHandler(
              filename = function(){
                paste(Model2$mn,".png",sep="")
              },
              content=function(file){
                png(file)
                plot(results_2[[1]], colorize = T, lwd = 2, main = Model2$mn)
                dev.off()
              })
            output$ROC_ACC_1 <- renderText({
              results[[2]]
            })
            output$ROC_ACC_2 <- renderText({
              results_2[[2]]
            })
            output$ROC_AUC_1 <- renderText({
              results[[3]]
            })
            output$ROC_AUC_2 <- renderText({
              results_2[[3]]
            })
          }
          else if ((input$Select_Model) && (input$Select_Model_2 == FALSE) && (input$Select_Model_3)){
            shinyjs::show('ROC_Model2')
            shinyjs::hide('ROC_Model3')
            results = ROC_Analysis(train_75,test_75,Model1)
            results_3 = ROC_Analysis(train_75,test_75,Model3)
            output$ROC_model_name_1 <- renderText({
              Model1$mn
            })
            output$ROC_model_def_1 <- renderText({
              Model1$md
            })
            output$ROCplot <- renderPlot({
              plot(results[[1]], colorize = T, lwd = 2, main = Model1$mn)
            })
            output$Download_Plot <-downloadHandler(
              filename = function(){
                paste(Model1$mn,".png",sep="")
              },
              content=function(file){
                png(file)
                plot(results[[1]], colorize = T, lwd = 2, main = Model1$mn)
                dev.off()
              })
            output$ROC_model_name_2 <- renderText({
              Model3$mn
            })
            output$ROC_model_def_2 <- renderText({
              Model3$md
            })
            output$ROCplot2 <- renderPlot({
              plot(results_3[[1]], colorize = T, lwd = 2, main = Model3$mn)
            })
            output$Download_Plot_2 <-downloadHandler(
              filename = function(){
                paste(Model3$mn,".png",sep="")
              },
              content=function(file){
                png(file)
                plot(results_3[[1]], colorize = T, lwd = 2, main = Model3$mn)
                dev.off()
              })
            output$ROC_ACC_1 <- renderText({
              results[[2]]
            })
            output$ROC_ACC_2 <- renderText({
              results_3[[2]]
            })
            output$ROC_AUC_1 <- renderText({
              results[[3]]
            })
            output$ROC_AUC_2 <- renderText({
              results_3[[3]]
            })
          }
          else if ((input$Select_Model == FALSE) && (input$Select_Model_2) && (input$Select_Model_3)){
            shinyjs::show('ROC_Model2')
            shinyjs::hide('ROC_Model3')
            results_2 = ROC_Analysis(train_75,test_75,Model2)
            results_3 = ROC_Analysis(train_75,test_75,Model3)
            output$ROC_model_name_1 <- renderText({
              Model2$mn
            })
            output$ROC_model_def_1 <- renderText({
              Model2$md
            })
            output$ROCplot <- renderPlot({
              plot(results_2[[1]], colorize = T, lwd = 2, main = Model2$mn)
            })
            output$Download_Plot <-downloadHandler(
              filename = function(){
                paste(Model2$mn,".png",sep="")
              },
              content=function(file){
                png(file)
                plot(results_2[[1]], colorize = T, lwd = 2, main = Model2$mn)
                dev.off()
              })
            output$ROC_model_name_2 <- renderText({
              Model3$mn
            })
            output$ROC_model_def_2 <- renderText({
              Model3$md
            })
            output$ROCplot2 <- renderPlot({
              plot(results_3[[1]], colorize = T, lwd = 2, main = Model3$mn)
            })
            output$Download_Plot_2 <-downloadHandler(
              filename = function(){
                paste(Model3$mn,".png",sep="")
              },
              content=function(file){
                png(file)
                plot(results_3[[1]], colorize = T, lwd = 2, main = Model3$mn)
                dev.off()
              })
            output$ROC_ACC_1 <- renderText({
              results_2[[2]]
            })
            output$ROC_ACC_2 <- renderText({
              results_3[[2]]
            })
            output$ROC_AUC_1 <- renderText({
              results_2[[3]]
            })
            output$ROC_AUC_2 <- renderText({
              results_3[[3]]
            })
          }
          else if ((input$Select_Model) && (input$Select_Model_2) && (input$Select_Model_3)){
            results = ROC_Analysis(train_75,test_75,Model1)
            results_2 = ROC_Analysis(train_75,test_75,Model2)
            results_3 = ROC_Analysis(train_75,test_75,Model3)
            output$ROC_model_name_1 <- renderText({
              Model1$mn
            })
            output$ROC_model_def_1 <- renderText({
              Model1$md
            })
            output$ROC_model_name_2 <- renderText({
              Model2$mn
            })
            output$ROC_model_def_2 <- renderText({
              Model2$md
            })
            output$ROCplot <- renderPlot({
              par(mfrow = c(1,2))
              plot(results[[1]], colorize = T, lwd = 2, main = Model1$mn)
              plot(results_2[[1]], colorize = T, lwd = 2, main = Model2$mn)
            })
            output$Download_Plot <-downloadHandler(
              filename = function(){
                paste(Model1$mn,"_",Model2$mn,".png",sep="")
              },
              content=function(file){
                png(file)
                par(mfrow = c(1,2))
                plot(results[[1]], colorize = T, lwd = 2, main = Model1$mn)
                plot(results_2[[1]], colorize = T, lwd = 2, main = Model2$mn)
                dev.off()
              })
            output$ROC_model_name_3 <- renderText({
              Model3$mn
            })
            output$ROC_model_def_3 <- renderText({
              Model3$md
            })
            output$ROCplot2 <- renderPlot({
              plot(results_3[[1]], colorize = T, lwd = 2, main = Model3$mn)
            })
            output$Download_Plot_2 <-downloadHandler(
              filename = function(){
                paste(Model3$mn,".png",sep="")
              },
              content=function(file){
                png(file)
                plot(results_3[[1]], colorize = T, lwd = 2, main = Model3$mn)
                dev.off()
              })
            output$ROC_ACC_1 <- renderText({
              results[[2]]
            })
            output$ROC_ACC_2 <- renderText({
              results_2[[2]]
            })
            output$ROC_ACC_3 <- renderText({
              results_3[[2]]
            })
            output$ROC_AUC_1 <- renderText({
              results[[3]]
            })
            output$ROC_AUC_2 <- renderText({
              results_2[[3]]
            })
            output$ROC_AUC_3 <- renderText({
              results_3[[3]]
            })
          }
        }
        else if((input$Split_data == "Train: 80% - Test: 20%")){
          smp_size <- floor(0.80 * nrow(df))
          set.seed(123)
          train_ind <- sample(seq_len(nrow(df)), size = smp_size)
          train_80 <- df[train_ind,]
          test_80 <- df[-train_ind, ]
          if ((input$Select_Model) && (input$Select_Model_2 == FALSE) && (input$Select_Model_3 == FALSE)){
            shinyjs::hide('ROC_Model2')
            shinyjs::hide('ROC_Model3')
            results = ROC_Analysis(train_80,test_80,Model1)
            output$ROC_model_name_1 <- renderText({
              Model1$mn
            })
            output$ROC_model_def_1 <- renderText({
              Model1$md
            })
            output$ROCplot <- renderPlot({
              plot(results[[1]], colorize = T, lwd = 2, main = Model1$mn)
            })
            output$Download_Plot <-downloadHandler(
              filename = function(){
                paste(Model1$mn,".png",sep="")
              },
              content=function(file){
                png(file)
                plot(results[[1]], colorize = T, lwd = 2, main = Model1$mn)
                dev.off()
              })
            output$ROCplot2 <- NULL
            output$ROC_ACC_1 <- renderText({
              results[[2]]
            })
            output$ROC_AUC_1 <- renderText({
              results[[3]]
            })
          }
          else if ((input$Select_Model == FALSE) && (input$Select_Model_2) && (input$Select_Model_3 == FALSE)){
            shinyjs::hide('ROC_Model2')
            shinyjs::hide('ROC_Model3')
            results = ROC_Analysis(train_80,test_80,Model2)
            output$ROC_model_name_1 <- renderText({
              Model2$mn
            })
            output$ROC_model_def_1 <- renderText({
              Model2$md
            })
            output$ROCplot <- renderPlot({
              plot(results[[1]], colorize = T, lwd = 2, main = Model2$mn)
            })
            output$Download_Plot <-downloadHandler(
              filename = function(){
                paste(Model2$mn,".png",sep="")
              },
              content=function(file){
                png(file)
                plot(results[[1]], colorize = T, lwd = 2, main = Model2$mn)
                dev.off()
              })
            output$ROCplot2 <- NULL
            output$ROC_ACC_1 <- renderText({
              results[[2]]
            })
            output$ROC_AUC_1 <- renderText({
              results[[3]]
            })
          }
          else if ((input$Select_Model == FALSE) && (input$Select_Model_2 == FALSE) && (input$Select_Model_3)){
            shinyjs::hide('ROC_Model2')
            shinyjs::hide('ROC_Model3')
            results = ROC_Analysis(train_80,test_80,Model3)
            output$ROC_model_name_1 <- renderText({
              Model3$mn
            })
            output$ROC_model_def_1 <- renderText({
              Model3$md
            })
            output$ROCplot <- renderPlot({
              plot(results[[1]], colorize = T, lwd = 2, main = Model3$mn)
            })
            output$Download_Plot <-downloadHandler(
              filename = function(){
                paste(Model3$mn,".png",sep="")
              },
              content=function(file){
                png(file)
                plot(results[[1]], colorize = T, lwd = 2, main = Model3$mn)
                dev.off()
              })
            output$ROCplot2 <- NULL
            output$ROC_ACC_1 <- renderText({
              results[[2]]
            })
            output$ROC_AUC_1 <- renderText({
              results[[3]]
            })
          }
          else if ((input$Select_Model) && (input$Select_Model_2) && (input$Select_Model_3==FALSE)){
            shinyjs::hide('ROC_Model3')
            results = ROC_Analysis(train_80,test_80,Model1)
            results_2 = ROC_Analysis(train_80,test_80,Model2)
            output$ROC_model_name_1 <- renderText({
              Model1$mn
            })
            output$ROC_model_def_1 <- renderText({
              Model1$md
            })
            output$ROCplot <- renderPlot({
              plot(results[[1]], colorize = T, lwd = 2, main = Model1$mn)
            })
            output$Download_Plot <-downloadHandler(
              filename = function(){
                paste(Model1$mn,".png",sep="")
              },
              content=function(file){
                png(file)
                plot(results[[1]], colorize = T, lwd = 2, main = Model1$mn)
                dev.off()
              })
            output$ROC_model_name_2 <- renderText({
              Model2$mn
            })
            output$ROC_model_def_2 <- renderText({
              Model2$md
            })
            output$ROCplot2 <- renderPlot({
              plot(results_2[[1]], colorize = T, lwd = 2, main = Model2$mn)
            })
            output$Download_Plot_2 <-downloadHandler(
              filename = function(){
                paste(Model2$mn,".png",sep="")
              },
              content=function(file){
                png(file)
                plot(results_2[[1]], colorize = T, lwd = 2, main = Model2$mn)
                dev.off()
              })
            output$ROC_ACC_1 <- renderText({
              results[[2]]
            })
            output$ROC_ACC_2 <- renderText({
              results_2[[2]]
            })
            output$ROC_AUC_1 <- renderText({
              results[[3]]
            })
            output$ROC_AUC_2 <- renderText({
              results_2[[3]]
            })
          }
          else if ((input$Select_Model) && (input$Select_Model_2 == FALSE) && (input$Select_Model_3)){
            shinyjs::hide('ROC_Model3')
            results = ROC_Analysis(train_80,test_80,Model1)
            results_3 = ROC_Analysis(train_80,test_80,Model3)
            output$ROC_model_name_1 <- renderText({
              Model1$mn
            })
            output$ROC_model_def_1 <- renderText({
              Model1$md
            })
            output$ROCplot <- renderPlot({
              plot(results[[1]], colorize = T, lwd = 2, main = Model1$mn)
            })
            output$Download_Plot <-downloadHandler(
              filename = function(){
                paste(Model1$mn,".png",sep="")
              },
              content=function(file){
                png(file)
                plot(results[[1]], colorize = T, lwd = 2, main = Model1$mn)
                dev.off()
              })
            output$ROC_model_name_2 <- renderText({
              Model3$mn
            })
            output$ROC_model_def_2 <- renderText({
              Model3$md
            })
            output$ROCplot2 <- renderPlot({
              plot(results_3[[1]], colorize = T, lwd = 2, main = Model3$mn)
            })
            output$Download_Plot_2 <-downloadHandler(
              filename = function(){
                paste(Model3$mn,".png",sep="")
              },
              content=function(file){
                png(file)
                plot(results_3[[1]], colorize = T, lwd = 2, main = Model3$mn)
                dev.off()
              })
            output$ROC_ACC_1 <- renderText({
              results[[2]]
            })
            output$ROC_ACC_2 <- renderText({
              results_3[[2]]
            })
            output$ROC_AUC_1 <- renderText({
              results[[3]]
            })
            output$ROC_AUC_2 <- renderText({
              results_3[[3]]
            })
          }
          else if ((input$Select_Model == FALSE) && (input$Select_Model_2) && (input$Select_Model_3)){
            shinyjs::hide('ROC_Model3')
            results_2 = ROC_Analysis(train_80,test_80,Model2)
            results_3 = ROC_Analysis(train_80,test_80,Model3)
            output$ROC_model_name_1 <- renderText({
              Model2$mn
            })
            output$ROC_model_def_1 <- renderText({
              Model2$md
            })
            output$ROCplot <- renderPlot({
              plot(results_2[[1]], colorize = T, lwd = 2, main = Model2$mn)
            })
            output$Download_Plot <-downloadHandler(
              filename = function(){
                paste(Model2$mn,".png",sep="")
              },
              content=function(file){
                png(file)
                plot(results_2[[1]], colorize = T, lwd = 2, main = Model2$mn)
                dev.off()
              })
            output$ROC_model_name_2 <- renderText({
              Model3$mn
            })
            output$ROC_model_def_2 <- renderText({
              Model3$md
            })
            output$ROCplot2 <- renderPlot({
              plot(results_3[[1]], colorize = T, lwd = 2, main = Model3$mn)
            })
            output$Download_Plot_2 <-downloadHandler(
              filename = function(){
                paste(Model3$mn,".png",sep="")
              },
              content=function(file){
                png(file)
                plot(results_3[[1]], colorize = T, lwd = 2, main = Model3$mn)
                dev.off()
              })
            output$ROC_ACC_1 <- renderText({
              results_2[[2]]
            })
            output$ROC_ACC_2 <- renderText({
              results_3[[2]]
            })
            output$ROC_AUC_1 <- renderText({
              results_2[[3]]
            })
            output$ROC_AUC_2 <- renderText({
              results_3[[3]]
            })
          }
          else if ((input$Select_Model) && (input$Select_Model_2) && (input$Select_Model_3)){
            results = ROC_Analysis(train_80,test_80,Model1)
            results_2 = ROC_Analysis(train_80,test_80,Model2)
            results_3 = ROC_Analysis(train_80,test_80,Model3)
            output$ROC_model_name_1 <- renderText({
              Model1$mn
            })
            output$ROC_model_def_1 <- renderText({
              Model1$md
            })
            output$ROC_model_name_2 <- renderText({
              Model2$mn
            })
            output$ROC_model_def_2 <- renderText({
              Model2$md
            })
            output$ROCplot <- renderPlot({
              par(mfrow = c(1,2))
              plot(results[[1]], colorize = T, lwd = 2, main = Model1$mn)
              plot(results_2[[1]], colorize = T, lwd = 2, main = Model2$mn)
            })
            output$Download_Plot <-downloadHandler(
              filename = function(){
                paste(Model1$mn,"_",Model2$mn,".png",sep="")
              },
              content=function(file){
                png(file)
                par(mfrow = c(1,2))
                plot(results[[1]], colorize = T, lwd = 2, main = Model1$mn)
                plot(results_2[[1]], colorize = T, lwd = 2, main = Model2$mn)
                dev.off()
              })
            output$ROC_model_name_3 <- renderText({
              Model3$mn
            })
            output$ROC_model_def_3 <- renderText({
              Model3$md
            })
            output$ROCplot2 <- renderPlot({
              plot(results_3[[1]], colorize = T, lwd = 2, main = Model3$mn)
            })
            output$Download_Plot_2 <-downloadHandler(
              filename = function(){
                paste(Model3$mn,".png",sep="")
              },
              content=function(file){
                png(file)
                plot(results_3[[1]], colorize = T, lwd = 2, main = Model3$mn)
                dev.off()
              })
            output$ROC_ACC_1 <- renderText({
              results[[2]]
            })
            output$ROC_ACC_2 <- renderText({
              results_2[[2]]
            })
            output$ROC_ACC_3 <- renderText({
              results_3[[2]]
            })
            output$ROC_AUC_1 <- renderText({
              results[[3]]
            })
            output$ROC_AUC_2 <- renderText({
              results_2[[3]]
            })
            output$ROC_AUC_3 <- renderText({
              results_3[[3]]
            })
          }
        }
      })
      
      
      Gee_Analysis <- function(data,model){
        if (is.null(model$coex)){
          cov = result_s(model$co)
        }
        else{
          cov = model$coex
        }
        form = as.formula(paste(model$vr, '~', cov, collapse = ""))
        identification = model$id
        geemodel = geeglm(form,data = data, id = data[[identification]], family = model$fm, corstr = model$cs, scale.fix = TRUE)
        ccgeepack = coef(summary(geemodel))
        citab_geepack <- with(as.data.frame(ccgeepack),
                              cbind(lwr=Estimate-1.96*Std.err,
                                    upr=Estimate+1.96*Std.err))
        rownames(citab_geepack) <- rownames(ccgeepack)
        ccgeepack1 = as.data.frame(cbind(ccgeepack,citab_geepack))
        sapply(ccgeepack[,1], exp)
        exp(ccgeepack[1,1])
        
        ccgeepack1$Estimate = exp(ccgeepack1$Estimate)
        ccgeepack1$lwr = exp(ccgeepack1$lwr)
        ccgeepack1$upr = exp(ccgeepack1$upr)
        
        return(ccgeepack1)
      }
      
      

      
}
shinyApp(ui = ui, server = server)