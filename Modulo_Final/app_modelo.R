library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(shinyWidgets)
library(dashboardthemes)
library(shinydashboardPlus)
library(shinyjs)
library(dplyr)
library(DT)
library(waiter)
library(bupaR)
library(edeaR)
library(processmapR)
library(DiagrammeR)
library(processanimateR)
library(processcheckR)
library(formattable)
library(lubridate)
library(kableExtra)

### Funções próprias -----------------------
cria_log_evento <- function(processos, ciclo = NULL,
                            times = NULL){
  if(!is.null(times)){
    log_processos <- activities_to_eventlog(
      activity_log = processos,
      case_id = "Caso",
      activity_id = "Atividade",
      resource_id = "Recurso",
      timestamps = times
    )
  }else{
    if(is.null(ciclo)){
      processos$Ciclo <- "Fim"
      processos <- assign_instance_id(processos, case_id = "Caso",
                                      activity_id = "Atividade",
                                      timestamp = "Tempo",
                                      lifecycle_id = "Ciclo")
    }else{
      processos <- assign_instance_id(processos, case_id = "Caso",
                                      activity_id = "Atividade",
                                      timestamp = "Tempo",
                                      lifecycle_id = ciclo)
    }
    processos$Tempo <- as.POSIXct(processos$Tempo)
    if("Recurso" %in% names(processos)){
      log_processos <- processos %>% 
        eventlog(
          case_id = "Caso",
          activity_id = "Atividade",
          activity_instance_id = "instance",
          lifecycle_id = "Ciclo",
          timestamp = "Tempo",
          resource_id = "Recurso"
        )
    }else{
      processos$Recurso <- NA
      log_processos <- processos %>% 
        eventlog(
          case_id = "Caso",
          activity_id = "Atividade",
          activity_instance_id = "instance",
          lifecycle_id = "Ciclo",
          timestamp = "Tempo",
          resource_id = "Recurso"
        )
    }
  }
  return(log_processos)
}
############### -----------------

header <- dashboardHeader(title = "AuditFlow", titleWidth = 300)
sidebar <- dashboardSidebar(width = 300,
                            sidebarMenu(
                              menuItem("Menu único", tabName = "datum", 
                                       icon = icon("money"))
                            )
)

body <- dashboardBody(useShinyjs(), use_waiter(), use_hostess(),
 tabsetPanel(
   tabPanel("Regras",
            dropdownButton(
              h4("Selecione as regras"),
              selectInput("regras_select", "Regras", selected = "",
                          choices = c("Ocorre n vezes ou mais", 
                                      "Ocorre exatamente n vezes",
                                      "Não ocorre mais do que (n - 1) vezes",
                                      "Não ocorre menos do que n, e não mais do que k vezes",
                                      "Começa com",
                                      "Termina com",
                                      "Duas atividades sempre existem juntas",
                                      "Se a Atividade A ocorreu, B deve ocorrer depois; se a Atividade B ocorreu, A deve ter ocorrido antes",
                                      "Se a Atividade A ocorreu, B deve ocorrer depois",
                                      "Se a Atividade B ocorre, A deve ter ocorrido antes",
                                      "Se a Atividade A ocorre, B ocorreu antes ou ocorrerá depois de A",
                                      "Se a Atividade A existir, B não deve existir; se a Atividade B existe, A não deve existir")),
              conditionalPanel("input.regras_select == 'Ocorre n vezes ou mais'",
                               selectInput("regras_ativ_occur_n_plus","Atividade",
                                           choices = NULL),
                               numericInput("regras_x_occur_n_plus", "Nº vezes", value = 1, min = 1),
                               actionButton("regras_x_occur_n_plus_bt", "Adicionar regra")
                               ),
              conditionalPanel("input.regras_select == 'Ocorre exatamente n vezes'",
                               selectInput("regras_ativ_occur_n","Atividade",
                                           choices = NULL),
                               numericInput("regras_x_occur_n", "Nº vezes", value = 1, min = 1),
                               actionButton("regras_x_occur_n_bt", "Adicionar regra")
              ),
              conditionalPanel("input.regras_select == 'Não ocorre mais do que (n - 1) vezes'",
                               selectInput("regras_ativ_not_occur_n1","Atividade",
                                           choices = NULL),
                               numericInput("regras_x_not_occur_n1", "Nº vezes", value = 1, min = 1),
                               actionButton("regras_x_not_occur_n1_bt", "Adicionar regra")
              ),
              conditionalPanel("input.regras_select == 'Não ocorre menos do que n, e não mais do que k vezes'",
                               selectInput("regras_ativ_contains_between","Atividade",
                                           choices = NULL),
                               numericInput("regras_x_contains_between_min", "Nº mínimo", value = 1, min = 1),
                               numericInput("regras_x_contains_between_max", "Nº máximo", value = 1, min = 1),
                               actionButton("regras_x_contains_between_bt", "Adicionar regra")
              ),
              conditionalPanel("input.regras_select == 'Começa com'",
                               selectInput("regras_ativ_starts","Atividade",
                                           choices = NULL),
                               actionButton("regras_ativ_starts_bt", "Adicionar regra")
              ),
              conditionalPanel("input.regras_select == 'Termina com'",
                               selectInput("regras_ativ_ends","Atividade",
                                           choices = NULL),
                               actionButton("regras_ativ_ends_bt", "Adicionar regra")
              ),
              conditionalPanel("input.regras_select == 'Duas atividades sempre existem juntas'",
                               selectInput("regras_ativ_existA","Atividade A",
                                           choices = NULL),
                               selectInput("regras_ativ_existB","Atividade B",
                                           choices = NULL),
                               actionButton("regras_x_exist_bt", "Adicionar regra")
              ),
              conditionalPanel("input.regras_select == 'Se a Atividade A ocorreu, B deve ocorrer depois; se a Atividade B ocorreu, A deve ter ocorrido antes'",
                               selectInput("regras_ativ_successionA","Atividade A",
                                           choices = NULL),
                               selectInput("regras_ativ_successionB","Atividade B",
                                           choices = NULL),
                               actionButton("regras_x_succession_bt", "Adicionar regra")
              ),
              conditionalPanel("input.regras_select == 'Se a Atividade A ocorreu, B deve ocorrer depois'",
                               selectInput("regras_ativ_responseA","Atividade A",
                                           choices = NULL),
                               selectInput("regras_ativ_responseB","Atividade B",
                                           choices = NULL),
                               actionButton("regras_x_response_bt", "Adicionar regra")
              ),
              conditionalPanel("input.regras_select == 'Se a Atividade B ocorre, A deve ter ocorrido antes'",
                               selectInput("regras_ativ_precedenceA","Atividade A",
                                           choices = NULL),
                               selectInput("regras_ativ_precedenceB","Atividade B",
                                           choices = NULL),
                               actionButton("regras_x_precedence_bt", "Adicionar regra")
              ),
              conditionalPanel("input.regras_select == 'Se a Atividade A ocorre, B ocorreu antes ou ocorrerá depois de A'",
                               selectInput("regras_ativ_responded_existA","Atividade A",
                                           choices = NULL),
                               selectInput("regras_ativ_responded_existB","Atividade B",
                                           choices = NULL),
                               actionButton("regras_x_responded_exist_bt", "Adicionar regra")
              ),
              conditionalPanel("input.regras_select == 'Se a Atividade A existir, B não deve existir; se a Atividade B existe, A não deve existir'",
                               selectInput("regras_ativ_xorA","Atividade A",
                                           choices = NULL),
                               selectInput("regras_ativ_xorB","Atividade B",
                                           choices = NULL),
                               actionButton("regras_x_xor_bt", "Adicionar regra")
              ),
              circle = T, status = "primary", icon = icon("plus"), margin = "10px",
              tooltip = tooltipOptions(title = "Clique aqui para selecionar suas opções")
              ),
            fluidRow(
              column(width = 2,),
              column(width = 10,
                     formattableOutput("regras_tabela", width = "60%")
                     )
            ), br(), br(),
            fluidRow(
              column(width = 6,
                     actionButton("regras_avaliar", "Avaliar")
                     ),
              column(width = 6,
                     actionButton("regras_limpar", "Limpar regras")
              )
            ),
            fluidRow(
              tableOutput("regras_tab_verdade")
            )
          )
 )                     
)

ui <- dashboardPage(header, sidebar, body) # UI

server <- function(input, output, session){
  dados_sei <- reactiveVal() # Usar essa mesma variável
  dados_carregados <- reactiveVal()
  log_evento <- reactiveVal()
  tab_regras <- reactiveVal()
  tab_res_regras <- reactiveVal()
  df_regras <- data.frame()
  tab_regras(df_regras)
  dados <- read.csv2("D:/MATHEUS/Estatística/5º semestre/Tópicos em Estatística/Projeto AuditFlow - SEI/base_2020-09-15 17_43_56-SUPRIMENTO DE FUNDOS-tratada-2.csv")
  names(dados)[c(4,12,17)] <- c("Caso","Tempo","Recurso")
  log <- cria_log_evento(dados)

  observe({
    if(!is.null(log)){
      variaveis <- names(log)
      Atividades <- as.character(unique(log[["Atividade"]]))
      anos <- as.character(unique(year(log[["Tempo"]])))
      recursos <- as.character(unique(log[["Recurso"]]))
      updateSelectInput(session, "regras_ativ_occur_n_plus",
                        choices = Atividades, selected = "")
      updateSelectInput(session, "regras_ativ_occur_n",
                        choices = Atividades, selected = "")
      updateSelectInput(session, "regras_ativ_not_occur_n1",
                        choices = Atividades, selected = "")
      updateSelectInput(session, "regras_ativ_contains_between",
                        choices = Atividades, selected = "")
      updateSelectInput(session, "regras_ativ_starts",
                        choices = Atividades, selected = "")
      updateSelectInput(session, "regras_ativ_ends",
                        choices = Atividades, selected = "")
      updateSelectInput(session, "regras_ativ_existA",
                        choices = Atividades, selected = "")
      updateSelectInput(session, "regras_ativ_existB",
                        choices = Atividades, selected = "")
      updateSelectInput(session, "regras_ativ_successionA",
                        choices = Atividades, selected = "")
      updateSelectInput(session, "regras_ativ_successionB",
                        choices = Atividades, selected = "")
      updateSelectInput(session, "regras_ativ_responseA",
                        choices = Atividades, selected = "")
      updateSelectInput(session, "regras_ativ_responseB",
                        choices = Atividades, selected = "")
      updateSelectInput(session, "regras_ativ_precedenceA",
                        choices = Atividades, selected = "")
      updateSelectInput(session, "regras_ativ_precedenceB",
                        choices = Atividades, selected = "")
      updateSelectInput(session, "regras_ativ_responded_existA",
                        choices = Atividades, selected = "")
      updateSelectInput(session, "regras_ativ_responded_existB",
                        choices = Atividades, selected = "")
      updateSelectInput(session, "regras_ativ_xorA",
                        choices = Atividades, selected = "")
      updateSelectInput(session, "regras_ativ_xorB",
                        choices = Atividades, selected = "")
      }
    })
    # validate(need(nrow(log) > 0, message = NULL))
    
    observeEvent(input$regras_x_occur_n_plus_bt, {
      regras <- tab_regras()
      Ordem <- nrow(regras) + 1
      Regra <- "Ocorre n ou mais"
      Detalhe <- paste0(input$regras_ativ_occur_n_plus,
                                 " | ", input$regras_x_occur_n_plus)
      regras <- rbind.data.frame(regras,
                                 data.frame(Ordem=Ordem,Regra=Regra, Detalhe=Detalhe))
      tab_regras(regras)
    })
    observeEvent(input$regras_x_occur_n_bt, {
      regras <- tab_regras()
      Ordem <- nrow(regras) + 1
      Regra <- "Ocorre exatamente n"
      Detalhe <- paste0(input$regras_ativ_occur_n,
                          " | ", input$regras_x_occur_n)
      regras <- rbind.data.frame(regras,
                                 data.frame(Ordem=Ordem,Regra=Regra, Detalhe=Detalhe))
      tab_regras(regras)
    })
    observeEvent(input$regras_x_not_occur_n1_bt, {
      regras <- tab_regras()
      Ordem <- nrow(regras) + 1
      Regra <- "Não ocorre mais do que (n - 1)"
      Detalhe <- paste0(input$regras_ativ_not_occur_n1,
                        " | ", input$regras_x_not_occur_n1)
      regras <- rbind.data.frame(regras,
                                 data.frame(Ordem=Ordem,Regra=Regra, Detalhe=Detalhe))
      tab_regras(regras)
    })
    observeEvent(input$regras_x_contains_between_bt, {
      regras <- tab_regras()
      Ordem <- nrow(regras) + 1
      Regra <- "Ocorre entre n e k"
      Detalhe <- paste0(input$regras_ativ_contains_between,
                        " | ", input$regras_x_contains_between_min,
                        " | ", input$regras_x_contains_between_max)
      regras <- rbind.data.frame(regras,
                                 data.frame(Ordem=Ordem,Regra=Regra, Detalhe=Detalhe))
      tab_regras(regras)
    })
    observeEvent(input$regras_ativ_starts_bt, {
      regras <- tab_regras()
      Ordem <- nrow(regras) + 1
      Regra <- "Começa com"
      Detalhe <- input$regras_ativ_starts
      regras <- rbind.data.frame(regras,
                                 data.frame(Ordem=Ordem,Regra=Regra, Detalhe=Detalhe))
      tab_regras(regras)
    })
    observeEvent(input$regras_ativ_ends_bt, {
      regras <- tab_regras()
      Ordem <- nrow(regras) + 1
      Regra <- "Termina com"
      Detalhe <- input$regras_ativ_ends
      regras <- rbind.data.frame(regras,
                                 data.frame(Ordem=Ordem,Regra=Regra, Detalhe=Detalhe))
      tab_regras(regras)
    })
    observeEvent(input$regras_x_exist_bt, {
      regras <- tab_regras()
      Ordem <- nrow(regras) + 1
      Regra <- "Sempre existem juntas"
      Detalhe <- paste0(input$regras_ativ_existA,
                        " | ", input$regras_ativ_existB)
      regras <- rbind.data.frame(regras,
                                 data.frame(Ordem=Ordem,Regra=Regra, Detalhe=Detalhe))
      tab_regras(regras)
    })
    observeEvent(input$regras_x_succession_bt, {
      regras <- tab_regras()
      Ordem <- nrow(regras) + 1
      Regra <- "Se A ocorre, B ocorre depois. Se B ocorreu, A deve ter ocorrido"
      Detalhe <- paste0(input$regras_ativ_successionA,
                        " | ", input$regras_ativ_successionB)
      regras <- rbind.data.frame(regras,
                                 data.frame(Ordem=Ordem,Regra=Regra, Detalhe=Detalhe))
      tab_regras(regras)
    })
    observeEvent(input$regras_x_response_bt, {
      regras <- tab_regras()
      Ordem <- nrow(regras) + 1
      Regra <- "Se A ocorre, B ocorre depois"
      Detalhe <- paste0(input$regras_ativ_responseA,
                        " | ", input$regras_ativ_responseB)
      regras <- rbind.data.frame(regras,
                                 data.frame(Ordem=Ordem,Regra=Regra, Detalhe=Detalhe))
      tab_regras(regras)
    })
    observeEvent(input$regras_x_precedence_bt, {
      regras <- tab_regras()
      Ordem <- nrow(regras) + 1
      Regra <- "Se B ocorre, A ocorreu antes"
      Detalhe <- paste0(input$regras_ativ_precedenceA,
                        " | ", input$regras_ativ_precedenceB)
      regras <- rbind.data.frame(regras,
                                 data.frame(Ordem=Ordem,Regra=Regra, Detalhe=Detalhe))
      tab_regras(regras)
    })
    observeEvent(input$regras_x_responded_exist_bt, {
      regras <- tab_regras()
      Ordem <- nrow(regras) + 1
      Regra <- "Se A ocorre, B ocorreu antes ou depois"
      Detalhe <- paste0(input$regras_ativ_responded_existA,
                        " | ", input$regras_ativ_responded_existB)
      regras <- rbind.data.frame(regras,
                                 data.frame(Ordem=Ordem,Regra=Regra, Detalhe=Detalhe))
      tab_regras(regras)
    })
    observeEvent(input$regras_x_xor_bt, {
      regras <- tab_regras()
      Ordem <- nrow(regras) + 1
      Regra <- "A e B nunca existem juntos"
      Detalhe <- paste0(input$regras_ativ_xorA,
                        " | ", input$regras_ativ_xorB)
      regras <- rbind.data.frame(regras,
                                 data.frame(Ordem=Ordem,Regra=Regra, Detalhe=Detalhe))
      tab_regras(regras)
    })
    output$regras_tabela <- renderFormattable({
      tab <- tab_regras()
      shiny::validate(need(nrow(tab) > 0, message = "Sem regras especificadas"))
      formattable(tab, align = c("l","l"))
    })
    observeEvent(input$regras_limpar,{
      regras <- tab_regras()
      regras <- regras[0,]
      tab_regras(regras)
    })
    observeEvent(input$regras_avaliar,{
      tab <- tab_regras()
      shiny::validate(need(nrow(tab) > 0, message = "Sem regras especificadas"))
      codigo <- "log %>% \n"
      grupos <- paste0("group_by(", paste("Regra_", 1:nrow(tab), collapse = ",", sep = ""), ") %>% \n")
      for(i in 1:nrow(tab)){
        if(tab$Regra[i] == "Ocorre n ou mais"){
          especs <- trimws(unlist(strsplit(tab$Detalhe[i], split = "\\|")))
          codigo <- paste0(codigo, "check_rule(processcheckR::contains('", 
                           especs[1],"', n = ", especs[2],"), label = 'Regra_",i,"')")
          if(i != nrow(tab)){
            codigo <- paste0(codigo, " %>% \n")
          }else{
            codigo <- paste0(codigo, " %>% \n ", grupos," n_cases()")
          }
        }
        if(tab$Regra[i] == "Ocorre exatamente n"){
          especs <- trimws(unlist(strsplit(tab$Detalhe[i], split = "\\|")))
          codigo <- paste0(codigo, "check_rule(contains_exactly('", 
                           especs[1],"', n = ", especs[2],"), label = 'Regra_",i,"')")
          if(i != nrow(tab)){
            codigo <- paste0(codigo, " %>% \n")
          }else{
            codigo <- paste0(codigo, " %>% \n ", grupos," n_cases()")
            }
        }
        if(tab$Regra[i] == "Não ocorre mais do que (n - 1)"){
          especs <- trimws(unlist(strsplit(tab$Detalhe[i], split = "\\|")))
          codigo <- paste0(codigo, "check_rule(absent('", 
                           especs[1],"', n = ", especs[2],"), label = 'Regra_",i,"')")
          if(i != nrow(tab)){
            codigo <- paste0(codigo, " %>% \n")
          }else{
            codigo <- paste0(codigo, " %>% \n ", grupos," n_cases()")
          }
        }
        if(tab$Regra[i] == "Ocorre entre n e k"){
          especs <- trimws(unlist(strsplit(tab$Detalhe[i], split = "\\|")))
          codigo <- paste0(codigo, "check_rule(contains_between('", 
                           especs[1],"', min = ", especs[2],", max = ", especs[3],
                           "), label = 'Regra_",i,"')")
          if(i != nrow(tab)){
            codigo <- paste0(codigo, " %>% \n")
          }else{
            codigo <- paste0(codigo, " %>% \n ", grupos," n_cases()")
          }
        }
        if(tab$Regra[i] == "Começa com"){ 
          especs <- tab$Detalhe[i]
          codigo <- paste0(codigo, "check_rule(starts('", especs,"'), label = 'Regra_",i,"')")
          if(i != nrow(tab)){
            codigo <- paste0(codigo, " %>% \n")
          }else{
            codigo <- paste0(codigo, " %>% \n ", grupos," n_cases()")
          }
        }
        if(tab$Regra[i] == "Termina com"){ 
          especs <- tab$Detalhe[i]
          codigo <- paste0(codigo, "check_rule(ends('", especs,"'), label = 'Regra_",i,"')")
          if(i != nrow(tab)){
            codigo <- paste0(codigo, " %>% \n")
          }else{
            codigo <- paste0(codigo, " %>% \n ", grupos," n_cases()")
          }
        }
        if(tab$Regra[i] == "Sempre existem juntas"){
          especs <- trimws(unlist(strsplit(tab$Detalhe[i], split = "\\|")))
          codigo <- paste0(codigo, "check_rule(and('", 
                           especs[1],"', activity_b = '", especs[2],"'), label = 'Regra_",i,"')")
          if(i != nrow(tab)){
            codigo <- paste0(codigo, " %>% \n")
          }else{
            codigo <- paste0(codigo, " %>% \n ", grupos," n_cases()")
          }
        }
        if(tab$Regra[i] == "Se A ocorre, B ocorre depois. Se B ocorreu, A deve ter ocorrido"){
          especs <- trimws(unlist(strsplit(tab$Detalhe[i], split = "\\|")))
          codigo <- paste0(codigo, "check_rule(succession('", 
                           especs[1],"', activity_b = '", especs[2],"'), label = 'Regra_",i,"')")
          if(i != nrow(tab)){
            codigo <- paste0(codigo, " %>% \n")
          }else{
            codigo <- paste0(codigo, " %>% \n ", grupos," n_cases()")
          }
        }
        if(tab$Regra[i] == "Se A ocorre, B ocorre depois"){
          especs <- trimws(unlist(strsplit(tab$Detalhe[i], split = "\\|")))
          codigo <- paste0(codigo, "check_rule(response('", 
                           especs[1],"', activity_b = '", especs[2],"'), label = 'Regra_",i,"')")
          if(i != nrow(tab)){
            codigo <- paste0(codigo, " %>% \n")
          }else{
            codigo <- paste0(codigo, " %>% \n ", grupos," n_cases()")
          }
        }
        if(tab$Regra[i] == "Se B ocorre, A ocorreu antes"){
          especs <- trimws(unlist(strsplit(tab$Detalhe[i], split = "\\|")))
          codigo <- paste0(codigo, "check_rule(precedence('", 
                           especs[1],"', activity_b = '", especs[2],"'), label = 'Regra_",i,"')")
          if(i != nrow(tab)){
            codigo <- paste0(codigo, " %>% \n")
          }else{
            codigo <- paste0(codigo, " %>% \n ", grupos," n_cases()")
          }
        }
        if(tab$Regra[i] == "Se A ocorre, B ocorreu antes ou depois"){
          especs <- trimws(unlist(strsplit(tab$Detalhe[i], split = "\\|")))
          codigo <- paste0(codigo, "check_rule(responded_existence('", 
                           especs[1],"', activity_b = '", especs[2],"'), label = 'Regra_",i,"')")
          if(i != nrow(tab)){
            codigo <- paste0(codigo, " %>% \n")
          }else{
            codigo <- paste0(codigo, " %>% \n ", grupos," n_cases()")
          }
        }
        if(tab$Regra[i] == "A e B nunca existem juntos"){
          especs <- trimws(unlist(strsplit(tab$Detalhe[i], split = "\\|")))
          codigo <- paste0(codigo, "check_rule(xor('", 
                           especs[1],"', activity_b = '", especs[2],"'), label = 'Regra_",i,"')")
          if(i != nrow(tab)){
            codigo <- paste0(codigo, " %>% \n")
          }else{
            codigo <- paste0(codigo, " %>% \n ", grupos," n_cases()")
          }
        }
      }
      res <- eval(parse(text = codigo))
      tab_res_regras(res)
    })
    output$regras_tab_verdade <- function(){
      result <- tab_res_regras()
      shiny::validate(need(nrow(result) > 0, message = "Sem regras avaliadas"))
      knitr::kable(result) %>%
        kable_styling("striped", full_width = F)
    }
}

shinyApp(ui, server)