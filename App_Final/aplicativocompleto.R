library(plyr)
library(tidyverse)
library(arulesSequences)
library(plotly)
library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(shinyWidgets)
library(dashboardthemes)
library(shinydashboardPlus)
library(dplyr)
library(DT)
library(waiter)
library(bupaR)
library(edeaR)
library(processmapR)
library(DiagrammeR)
library(processanimateR)
library(shinyjs)
library(ggplot2)
library(daqapo)
library(processcheckR)
library(formattable)
library(lubridate)
library(kableExtra)
library(TraMineR)
library(flashClust)
library(dendextend)
library(igraph)
library(visNetwork)
library(writexl)


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

elapsed_months <- function(end_date, start_date) {
  ed <- as.POSIXlt(end_date)
  sd <- as.POSIXlt(start_date)
  12 * (ed$year - sd$year) + (ed$mon - sd$mon)
}

cria_act_log <- function(x, case, timestamp, resource, activity){
  x <- x %>% group_by(.data[[case]]) %>%
    mutate(start = .data[[timestamp]],
           complete = dplyr::lead(.data[[timestamp]], 1)) %>%
    filter(!is.na(complete)) %>% data.frame()
  act_log <- activitylog(x, case, activity, resource, c("start","complete"))
  return(act_log)
}

cria_grafo <- function(log, type = c("activity", "resource")){
  if(type == "activity"){
    graph <- log %>% process_map(render = F)
  }else if(type == "resource"){
    graph <- log %>% resource_map(render = F)
  }
  model <- add_global_graph_attrs(graph, attr = "rankdir", 
                                  value = "LR",
                                  # value = input$orientation, 
                                  attr_type = "graph")
  model$nodes_df$label <- sub("\\n.*", "", model$nodes_df$label)
  model$edges_df$from <- model$nodes_df$label[model$edges_df$from]
  model$edges_df$to <- model$nodes_df$label[model$edges_df$to]
  
  net <- graph.data.frame(d = model$edges_df[,2:3], vertices = model$nodes_df$label)
  return(net)
}

letterwrap <- function(n, depth = 1) {
  args <- lapply(1:depth, FUN = function(x) return(LETTERS))
  x <- do.call(expand.grid, args = list(args, stringsAsFactors = F))
  x <- x[, rev(names(x)), drop = F]
  x <- do.call(paste0, x)
  if (n <= length(x)) return(x[1:n])
  return(c(x, letterwrap(n - length(x), depth = depth + 1)))
}


transn_seq <- function(x){
  x = unlist(strsplit(x, split = "-"))
  n = length(x) + 1
  return(paste(unlist(mapply(function(a,b) setdiff(a,b) ,x[-n],c(x[-1],NA))), collapse = "-"))
}

n.inertia.clus <- function(x,min=2,max){ ### O x é uma matriz de distâncias
  hc <- flashClust::hclust(x)
  inert.gain <- rev(hc$height)
  intra <- rev(cumsum(rev(inert.gain)))
  quot <- intra[min:(max)]/intra[(min - 1):(max - 1)] 
  nb.clust <- which.min(quot) + min -1
  return(nb.clust)
}


dados <- read.csv2("D:/unb/6º Semestre/labest/dados.txt")
names(dados)[c(4,12,17)] <- c("Caso","Tempo","Recurso")
dados$Tempo <- as.POSIXct(dados$Tempo, tryFormats = "%d/%m/%Y %H:%M")
act_log <- cria_act_log(dados, "Caso", "Tempo", "Recurso", "Atividade")
act_log$Atividade <- gsub(" ", "_", act_log$Atividade)
dados$Atividade2 <- mapvalues(dados$Atividade,
                            from = unique(dados$Atividade),
                            to = letterwrap(length(unique(dados$Atividade))))

sequencias <- dados %>% group_by(Caso) %>%
  dplyr::summarise(seq = paste(Atividade2, collapse = "-")) %>% data.frame()
sequencias$dss <- unlist(lapply(sequencias[,2], function(x) transn_seq(x)))
seqs <- seqdef(sequencias$dss)

costs <- seqcost(seqs, method = "INDELSLOG")
om <- seqdist(seqs, method = "OM",
              indel = costs$indel, sm = costs$sm)

n.clusters <- n.inertia.clus(as.dist(om), max = 15)

clusters <- cutree(flashClust::hclust(as.dist(om)), k = n.clusters)
clusters.p <- cutree(flashClust::hclust(as.dist(om)), k = 5)

### clusters <- cutree(flashClust::hclust(as.dist(om)), k = 5)
sequencias$clusters <- clusters
sequencias$clusters.p <- clusters.p

dados <- merge(dados, sequencias[,c(1,4)], by = "Caso")

log <- cria_log_evento(dados)

# The tool I'd suggest using is dendextend + ggplot + plotly.

dend <- log %>%
  dist() %>%
  hclust(method = "ave") %>%
  as.dendrogram()
dend2 <- color_branches(dend, 5)
p <- ggplot(dend2, horiz = T, offset_labels = -3)
p

###########################################################################
#medidas de centralidade Atividade#
degree.activity.in <- degree(cria_grafo(log, "activity"), mode = "in")
degree.activity.out <- degree(cria_grafo(log, "activity"), mode = "out")
degree.activity.all <- degree(cria_grafo(log, "activity"), mode = "all")
closeness.activity.in <- closeness(cria_grafo(log,"activity"), mode = "in", weights=NA)
closeness.activity.out <- closeness(cria_grafo(log,"activity"), mode = "out", weights=NA)
closeness.activity.all <- closeness(cria_grafo(log,"activity"), mode = "all", weights=NA)
betweenness.activity <- betweenness(cria_grafo(log,"activity"), directed=T, weights=NA)

#medidas de centralidade Recursos#
degree.resource.in <- degree(cria_grafo(log, "resource"), mode = "in")
degree.resource.out <- degree(cria_grafo(log, "resource"), mode = "out")
degree.resource.all <- degree(cria_grafo(log, "resource"), mode = "all")
closeness.resource.in <- closeness(cria_grafo(log,"resource"), mode = "in", weights=NA)
closeness.resource.out <- closeness(cria_grafo(log,"resource"), mode = "out", weights=NA)
closeness.resource.all <- closeness(cria_grafo(log,"resource"), mode = "all", weights=NA)
betweenness.resource <- betweenness(cria_grafo(log,"resource"), directed=T, weights=NA)

######################## visnetwork ##############################
##### ATIVIDADE
## grafos  
graph <- log %>% process_map(render = F)
model <- add_global_graph_attrs(graph, attr = "rankdir", 
                                value = "LR",
                                # value = input$orientation, 
                                attr_type = "graph")
model$nodes_df$label <- sub("\\n.*", "", model$nodes_df$label)
model$edges_df$from <- model$nodes_df$label[model$edges_df$from]
model$edges_df$to <- model$nodes_df$label[model$edges_df$to]

net <- graph.data.frame(d = model$edges_df[,2:3], vertices = model$nodes_df$label)

as.data.frame(model$nodes_df$label)
net
edges.atividade <- model$edges_df[,2:3]

## DETECÇÃO DE COMUNIDADES

#Community detection based on edge betweenness (Newman-Girvan)
ceb <- cluster_edge_betweenness(cria_grafo(log, "activity")) 

length(ceb)

tabela.ceb <- data.frame(Vértices = ceb$names,Comunidade = ceb$membership)

#Community detection based on based on propagating labels

clp <- cluster_label_prop(cria_grafo(log, "activity"))

length(clp)

tabela.clp <- data.frame(Vértices = clp$names,Comunidade = clp$membership)

#Community detection based on greedy optimization of modularity

cfg <- cluster_fast_greedy(as.undirected(cria_grafo(log, "activity")))

length(cfg)

tabela.cfg <- data.frame(Vértices = cfg$names,Comunidade = cfg$membership)

##### RECURSOS
## grafos  
graph2 <- log %>% resource_map(render = F)
model2 <- add_global_graph_attrs(graph2, attr = "rankdir", 
                                 value = "LR",
                                 # value = input$orientation, 
                                 attr_type = "graph")
model2$nodes_df$label <- sub("\\n.*", "", model2$nodes_df$label)
model2$edges_df$from <- model2$nodes_df$label[model2$edges_df$from]
model2$edges_df$to <- model2$nodes_df$label[model2$edges_df$to]

net2 <- graph.data.frame(d = model2$edges_df[,2:3], vertices = model2$nodes_df$label)

as.data.frame(model2$nodes_df$label)
net2
edges.recursos <- model2$edges_df[,2:3]

## DETECÇÃO DE COMUNIDADES

#Community detection based on edge betweenness (Newman-Girvan)
ceb.resource <- cluster_edge_betweenness(cria_grafo(log, "resource")) 

length(ceb.resource)

tabela.ceb.resource <- data.frame(Vértices = ceb.resource$names,Comunidade = ceb.resource$membership)

#Community detection based on based on propagating labels

clp.resource <- cluster_label_prop(cria_grafo(log, "resource"))

length(clp.resource)

tabela.clp.resource <- data.frame(Vértices = clp.resource$names,Comunidade = clp.resource$membership)

#Community detection based on greedy optimization of modularity

cfg.resource <- cluster_fast_greedy(as.undirected(cria_grafo(log, "resource")))

length(cfg.resource)

tabela.cfg.resource <- data.frame(Vértices = cfg.resource$names,Comunidade = cfg.resource$membership)


####################################################################################

header <- dashboardHeader(title = "AuditFlow", titleWidth = 300)
sidebar <- dashboardSidebar(width = 300,
                            sidebarMenu(
                              menuItem("Análise de Redes Sociais", tabName = "vitor", 
                                       icon = icon("bezier-curve"),
                                       menuSubItem("Atividades", tabName = "subitem1"),
                                       menuSubItem("Recursos", tabName = "subitem2")),
                              menuItem("Detecção de Padrões de Sequências", tabName = "padseqmenu", 
                                       icon = icon("filter", lib = "glyphicon")),
                              menuItem("Detecção de Outliers", tabName = "rafinha", 
                                       icon = icon("screenshot", lib = "glyphicon")),
                              menuItem("Checagem de Processos", tabName = "datum", 
                                       icon = icon("money")),
                              menuItem("Clusterização de Processos", tabName = "lucas", 
                                       icon = icon("money"))
                            )
)

body <- dashboardBody(useShinyjs(), use_waiter(), use_hostess(),
                      tabItems(
                        tabItem(tabName = "subitem1",
                                tabBox(
                                  title = " ", width = 12, height = "400px",
                                  tabPanel("Grafo",
                                           selectInput("vertices1","Selecione o tamanho dos vértices",
                                                       choices = c("Padrão","Degree","Closeness","Betweenness")),
                                           hidden(
                                             selectInput("modo1","Selecione o modo de cálculo",
                                                         choices = c("in","out","all"))),
                                           visNetworkOutput("grafo1",width = "100%", height = "790px")),
                                  tabPanel("Detecção de Comunidades",
                                           downloadButton("download1", "Tabela com as Comunidades"),
                                           valueBoxOutput("comunidades1", width = 4),
                                           selectInput("algoritmo1","Selecione o Algoritmo", 
                                                       choices = c("Edge Betweenness","Propagating Labels",
                                                                   "Greedy Optimization of Modularity")),
                                           visNetworkOutput("grafo2",width = "100%", height = "668px"),
                                  )
                                )),
                        tabItem(tabName = "subitem2",
                                tabBox(
                                  title = " ", width = 12, height = "400px",
                                  tabPanel("Grafo",
                                           selectInput("vertices2","Selecione o tamanho dos vértices",
                                                       choices = c("Padrão","Degree","Closeness","Betweenness")),
                                           hidden(
                                             selectInput("modo2","Selecione o modo de cálculo",
                                                         choices = c("in","out","all"))),
                                           visNetworkOutput("grafo3",width = "100%", height = "790px")),
                                  tabPanel("Detecção de Comunidades",
                                           downloadButton("download2", "Tabela com as Comunidades"),
                                           valueBoxOutput("comunidades2", width = 4),
                                           selectInput("algoritmo2","Selecione o Algoritmo", 
                                                       choices = c("Edge Betweenness","Propagating Labels",
                                                                   "Greedy Optimization of Modularity")),
                                           visNetworkOutput("grafo4",width = "100%", height = "668px"),)
                                )),
                        tabItem(tabName = "padseqmenu",
                                navbarPage("", tabPanel("Padrões e Sequências",
                                                        selectInput("padseq", "Selecione a variável a ser analisada:",
                                                                    choices = c("Atividade", "Recurso"), selected = "Atividade"),
                                                        sliderInput("support", "Selecione o valor mínimo do suporte:",
                                                                    min = 0.01, max = 1, value = 0.01),
                                                        tabsetPanel(
                                                          tabPanel("Sequências",
                                                                   DTOutput("dt_support") ), 
                                                          tabPanel("Resumo", verbatimTextOutput("resumo_padrao")),
                                                          tabPanel("Padrões",sliderInput("confianca", "Selecione o valor mínimo de confiança:",
                                                                                         min = 0.01, max = 1, value = 0.8),
                                                                   DTOutput("padroes")))),
                                           tabPanel("Explorando as Sequências",
                                                    sidebarLayout(sidebarPanel(numericInput("trace", "Selecione o número de traços", value = 5), 
                                                                               #sliderInput("coverage", "Ou se preferir selecione a cobertura dos traços",
                                                                               #min = 0.01, max = 1, value = .2),
                                                                               radioButtons("type", "Selecione a frequência das variáveis:", choices = c("frequente" = "frequent" ,"menos frequente"= "infrequent" ), selected = "frequent")
                                                    ), mainPanel(plotlyOutput("grafico")))
                                           )
                                           ### Seus códigos de UI aqui nessa parte!                     
                                )),
                        tabItem(tabName = "rafinha",
                                navbarPage("", tabPanel("Detectar outliers de duração",
                                                        selectInput("atividade", "Selecione uma atividade:",
                                                                    choices = c("ATESTO_RECEBIMENTO","BLOQUEADO", "DESPACHO",
                                                                                "DOCUMENTO_EXCLUÍDO", "EMPENHO", "INÍCIO", "OB", 
                                                                                "PCSF", "PRESTAÇÃO_DE_CONTAS")),
                                                               DTOutput("dt_1")), 
                                                      tabPanel("Detectar anomalias de frequência", 
                                                               selectInput("atividade_1", "Selecione a 1º atividade:",
                                                                           choices = c("ATESTO_RECEBIMENTO","BLOQUEADO", "DESPACHO",
                                                                                       "DOCUMENTO_EXCLUÍDO", "EMPENHO", "INÍCIO", "OB", 
                                                                                       "PCSF", "PRESTAÇÃO_DE_CONTAS")), 
                                                               selectInput("atividade_2", "Selecione a 2º atividade:",
                                                                           choices = c("ATESTO_RECEBIMENTO","BLOQUEADO", "DESPACHO",
                                                                                       "DOCUMENTO_EXCLUÍDO", "EMPENHO", "INÍCIO", "OB", 
                                                                                       "PCSF", "PRESTAÇÃO_DE_CONTAS")),           
                                                               DTOutput("dt_2")),
                                                      tabPanel("Detectar duas atividades em paralelo",
                                                               textOutput("mensagem"),
                                                               DTOutput("dt_3")),
                                                      tabPanel("Detecar anomalias nas datas das atividades",
                                                               DTOutput("dt_4")),
                                                      tabPanel("Detectar casos incompletos",
                                                               checkboxInput("id", "Ver todos os id's", value = FALSE, width = NULL),
                                                               DTOutput("dt_5"))
                                           )  
                                ),
                        tabItem(tabName = "datum",
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
                        ),
                        tabItem(tabName = "lucas",
                                sidebarLayout(
                                  
                                  sidebarPanel(
                                    
                                    h1(
                                      
                                      radioButtons("clust","Tipo de Cluster", choices = c( "Método da Inércia" , "Eu escolho" ) ),
                                      
                                      conditionalPanel(
                                        condition = "input.clust == 'Eu escolho'",
                                        numericInput("nclust", "Número de Clusters:", 3 , min = 2,  max = 15)
                                      )
                                    )
                                    
                                  ),
                                  
                                  
                                  mainPanel(
                                    
                                    textOutput("TituloP"),
                                    plotOutput("pizza"),
                                    tableOutput("pizzaT"),
                                    plotlyOutput("dendograma")
                                    
                                    
                                  )
                                )
                                )
                              
))

ui <- dashboardPage(header, sidebar, body) # UI

server <- function(input, output, session){
  dados_sei <- reactiveVal() # Usar essa mesma variÃ¡vel
  dados_carregados <- reactiveVal()
  log_evento <- reactiveVal()
  tab_regras <- reactiveVal()
  tab_res_regras <- reactiveVal()
  df_regras <- data.frame()
  tab_regras(df_regras)
  dados <- read.csv2("D:/unb/6º Semestre/labest/dados.txt")
  names(dados)[c(4,12,17)] <- c("Caso","Tempo","Recurso")
  dados$Tempo <- as.POSIXct(dados$Tempo, tryFormats = "%d/%m/%Y %H:%M")
  act_log <- cria_act_log(dados, "Caso", "Tempo", "Recurso", "Atividade")
  act_log$Atividade <- gsub(" ", "_", act_log$Atividade)
  log <- cria_log_evento(dados)
  vertice.atividade <- reactive({input$vertices1})
  modo.atividade <- reactive({input$modo1})
  output$grafo1 <- renderVisNetwork({
    if (vertice.atividade()== "Padrão") {
      nodes.atividade.padrao <- data.frame(id = model$nodes_df$label, label = model$nodes_df$label)
      visNetwork(nodes.atividade.padrao,edges.atividade)
    } else if (vertice.atividade()=="Degree" && modo.atividade() == "in") {
      nodes.atividade.degree.in <- data.frame(id = model$nodes_df$label, label = model$nodes_df$label, value = degree.activity.in)
      visNetwork(nodes.atividade.degree.in,edges.atividade)
    } else if (vertice.atividade()=="Degree" && modo.atividade() == "out") {
      nodes.atividade.degree.out <- data.frame(id = model$nodes_df$label, label = model$nodes_df$label, value = degree.activity.out)
      visNetwork(nodes.atividade.degree.out,edges.atividade)
    } else if (vertice.atividade()=="Degree" && modo.atividade() == "all") {
      nodes.atividade.degree.all <- data.frame(id = model$nodes_df$label, label = model$nodes_df$label, value = degree.activity.all)
      visNetwork(nodes.atividade.degree.all,edges.atividade)
    } else if (vertice.atividade()=="Closeness" && modo.atividade() == "in") {
      nodes.atividade.closeness.in <- data.frame(id = model$nodes_df$label, label = model$nodes_df$label, value = closeness.activity.in)
      visNetwork(nodes.atividade.closeness.in,edges.atividade)
    } else if (vertice.atividade()=="Closeness" && modo.atividade() == "out") {
      nodes.atividade.closeness.out <- data.frame(id = model$nodes_df$label, label = model$nodes_df$label, value = closeness.activity.out)
      visNetwork(nodes.atividade.closeness.out,edges.atividade)
    } else if (vertice.atividade()=="Closeness" && modo.atividade() == "all") {
      nodes.atividade.closeness.all <- data.frame(id = model$nodes_df$label, label = model$nodes_df$label, value = closeness.activity.all)
      visNetwork(nodes.atividade.closeness.all,edges.atividade)
    } else if (vertice.atividade()=="Betweenness") {
      nodes.atividade.betweenness <- data.frame(id = model$nodes_df$label, label = model$nodes_df$label, value = betweenness.activity)
      visNetwork(nodes.atividade.betweenness,edges.atividade)
    }
  })
  comunidades.atividade <- reactive({input$algoritmo1})
  output$grafo2 <- renderVisNetwork({
    if (comunidades.atividade()=="Edge Betweenness") {
      nodes.atividade.ceb <- data.frame(id = model$nodes_df$label, label = model$nodes_df$label, group = ceb$membership)
      visNetwork(nodes.atividade.ceb,edges.atividade) 
    } else if (comunidades.atividade()=="Propagating Labels") {
      nodes.atividade.clp <- data.frame(id = model$nodes_df$label, label = model$nodes_df$label, group = clp$membership)
      visNetwork(nodes.atividade.clp,edges.atividade)
    } else if (comunidades.atividade()=="Greedy Optimization of Modularity") {
      nodes.atividade.cfg <- data.frame(id = model$nodes_df$label, label = model$nodes_df$label, group = cfg$membership)
      visNetwork(nodes.atividade.cfg,edges.atividade)
    }
  })
  output$comunidades1 <- renderValueBox({
    if (comunidades.atividade()=="Edge Betweenness") {
      valueBox("Número de comunidades",length(ceb), icon = NULL, color = "light-blue") 
    } else if (comunidades.atividade()=="Propagating Labels") {
      valueBox("Número de comunidades",length(clp), icon = NULL, color = "light-blue") 
    } else if (comunidades.atividade()=="Greedy Optimization of Modularity") {
      valueBox("Número de comunidades",length(cfg), icon = NULL, color = "light-blue") 
    }
  })
  modo.recursos <- reactive({input$modo2})
  vertice.recursos <- reactive({input$vertices2})
  output$grafo3 <- renderVisNetwork({
    if (vertice.recursos()== "Padrão") {
      nodes.recursos.padrao <- data.frame(id = model2$nodes_df$label, label = model2$nodes_df$label)
      visNetwork(nodes.recursos.padrao,edges.recursos)
    } else if (vertice.recursos()=="Degree" && modo.recursos() == "in") {
      nodes.recursos.degree.in <- data.frame(id = model2$nodes_df$label, label = model2$nodes_df$label, value = degree.resource.in)
      visNetwork(nodes.recursos.degree.in,edges.recursos)
    } else if (vertice.recursos()=="Degree" && modo.recursos() == "out") {
      nodes.recursos.degree.out <- data.frame(id = model2$nodes_df$label, label = model2$nodes_df$label, value = degree.resource.out)
      visNetwork(nodes.recursos.degree.out,edges.recursos)
    } else if (vertice.recursos()=="Degree" && modo.recursos() == "all") {
      nodes.recursos.degree.all <- data.frame(id = model2$nodes_df$label, label = model2$nodes_df$label, value = degree.resource.all)
      visNetwork(nodes.recursos.degree.all,edges.recursos)
    } else if (vertice.recursos()=="Closeness" && modo.recursos() == "in") {
      nodes.recursos.closeness.in <- data.frame(id = model2$nodes_df$label, label = model2$nodes_df$label, value = closeness.resource.in)
      visNetwork(nodes.recursos.closeness.in,edges.recursos)
    } else if (vertice.recursos()=="Closeness" && modo.recursos() == "out") {
      nodes.recursos.closeness.out <- data.frame(id = model2$nodes_df$label, label = model2$nodes_df$label, value = closeness.resource.out)
      visNetwork(nodes.recursos.closeness.out,edges.recursos)
    } else if (vertice.recursos()=="Closeness" && modo.recursos() == "all") {
      nodes.recursos.closeness.all <- data.frame(id = model2$nodes_df$label, label = model2$nodes_df$label, value = closeness.resource.all)
      visNetwork(nodes.recursos.closeness.all,edges.recursos)
    } else if (vertice.recursos()=="Betweenness") {
      nodes.recursos.betweenness <- data.frame(id = model2$nodes_df$label, label = model2$nodes_df$label, value = betweenness.resource)
      visNetwork(nodes.recursos.betweenness,edges.recursos)
    }
  })
  comunidades.recurso <- reactive({input$algoritmo2})
  output$grafo4 <- renderVisNetwork({
    if (comunidades.recurso()=="Edge Betweenness") {
      nodes.recursos.ceb <- data.frame(id = model2$nodes_df$label, label = model2$nodes_df$label, group = ceb.resource$membership)
      visNetwork(nodes.recursos.ceb,edges.recursos) 
    } else if (comunidades.recurso()=="Propagating Labels") {
      nodes.recursos.clp <- data.frame(id = model2$nodes_df$label, label = model2$nodes_df$label, group = clp.resource$membership)
      visNetwork(nodes.recursos.clp,edges.recursos) 
    } else if (comunidades.recurso()=="Greedy Optimization of Modularity") {
      nodes.recursos.cfg <- data.frame(id = model2$nodes_df$label, label = model2$nodes_df$label, group = cfg.resource$membership)
      visNetwork(nodes.recursos.cfg,edges.recursos)
    }
  })
  output$comunidades2 <- renderValueBox({
    if (comunidades.recurso()=="Edge Betweenness") {
      valueBox("Número de comunidades",length(ceb.resource), icon = NULL, color = "light-blue") 
    } else if (comunidades.recurso()=="Propagating Labels") {
      valueBox("Número de comunidades",length(clp.resource), icon = NULL, color = "light-blue") 
    } else if (comunidades.recurso()=="Greedy Optimization of Modularity") {
      valueBox("Número de comunidades",length(cfg.resource), icon = NULL, color = "light-blue") 
    }
  })
  observeEvent(input$vertices1, {
    if (vertice.atividade()== "Padrão") {
      hide("modo1")
    } else if (vertice.atividade()== "Degree") {
      show("modo1")
    } else if (vertice.atividade()== "Closeness") {
      show("modo1")
    } else if (vertice.atividade()== "Betweenness") {
      hide("modo1")
    }})
  observeEvent(input$vertices2, {
    if (vertice.recursos()== "Padrão") {
      hide("modo2")
    } else if (vertice.recursos()== "Degree") {
      show("modo2")
    } else if (vertice.recursos()== "Closeness") {
      show("modo2")
    } else if (vertice.recursos()== "Betweenness") {
      hide("modo2")
    }})
  output$download1 <- downloadHandler(
    filename = function() { 
      paste("Atividades ",comunidades.atividade(),".xlsx",sep = "")
    },
    content = function(file) {
      if (comunidades.atividade() == "Edge Betweenness") {
        writexl::write_xlsx(tabela.ceb, file)
      } else if (comunidades.atividade() == "Propagating Labels"){
        writexl::write_xlsx(tabela.clp, file)
      } else if (comunidades.atividade() == "Greedy Optimization of Modularity") {
        writexl::write_xlsx(tabela.cfg, file)
      }
    })
  output$download2 <- downloadHandler(
    filename = function() { 
      paste("Recursos ",comunidades.recurso(),".xlsx",sep = "")
    },
    content = function(file) {
      if (comunidades.recurso() == "Edge Betweenness") {
        writexl::write_xlsx(tabela.ceb.resource, file)
      } else if (comunidades.recurso() == "Propagating Labels"){
        writexl::write_xlsx(tabela.clp.resource, file)
      } else if (comunidades.recurso() == "Greedy Optimization of Modularity") {
        writexl::write_xlsx(tabela.cfg.resource, file)
      }
    })
  ##########################################################
  #Import standard transaction data
  transactions = log
  
  # Start time of data to be considered
  start_month <- "2016-07-21"
  
  # Create list of Azure services by customer ID and CleanMonth (formatted dates)
  log2 <- reactive({if(input$padseq == "Atividade"){
    
    trans_sequence <- transactions %>%
      group_by(IdProcedimento, DataAutuacao) %>%
      summarize(
        SIZE = n(),
        ServiceLevel = paste(as.character(Atividade), collapse = ';')
      )
  } else {trans_sequence <- transactions %>%
    group_by(IdProcedimento, DataAutuacao) %>%
    summarize(
      SIZE = n(),
      ServiceLevel = paste(as.character(Recurso), collapse = ';')
    )}
    
    
    
    
    trans_sequence$DataAutuacao <-  as.Date(trans_sequence$DataAutuacao, "%d/%m/%Y")
    
    
    trans_sequence$eventID <- elapsed_months(trans_sequence$DataAutuacao, start_month)
    trans_sequence = trans_sequence[,c(1,5,3,4)]
    names(trans_sequence) = c("sequenceID", "eventID", "SIZE", "items")
    
    trans_sequence <- data.frame(lapply(trans_sequence, as.factor))
    trans_sequence <- trans_sequence[order(trans_sequence$sequenceID, trans_sequence$eventID),]
    
    # Convert to transaction matrix data type
    write.table(trans_sequence, "mytxtout.txt", sep=";", row.names = FALSE, col.names = FALSE, quote = FALSE)
    trans_matrix <- read_baskets("mytxtout.txt", sep = ";", info = c("sequenceID","eventID","SIZE"))
  })
  padroes <- reactive({cspade(log2(), parameter = list(support = input$support),
                              control = list(verbose = TRUE))})
  output$dt_support = renderDT({ 
    
    s1 <-  padroes()
    s1.df <- as(s1, "data.frame")
    colnames(s1.df) <- c("sequência", "suporte")
    s1.df[order(-s1.df$suporte),]
  })
  output$resumo_padrao = renderPrint({summary(padroes())})
  
  output$padroes = renderDT({
    rules <- eclat(log2(), parameter = list(supp = input$support), control = list(verbose = TRUE))
    
    rules.df <- as(ruleInduction(rules, confidence = input$confianca), "data.frame")
    colnames(rules.df) <- c("regras", "suporte", "confiança","aumento")
    rules.df})
  
  output$grafico <- renderPlotly({p <- ggplotly(trace_explorer(log, n_traces = input$trace, type = input$type, coverage_labels = c( "relative", "absolute" )))
  p %>% 
    layout(plot_bgcolor='gray') %>% 
    layout(paper_bgcolor='gray') %>%
    layout(legend = list(orientation = "h", y = -.2, x = 1)) })
#######################################################################
  output$dt_1 <- renderDT ({outlier <- eval(parse(text = paste0("detect_duration_outliers(activitylog = act_log, ", 
                                                                input$atividade, " = duration_within(bound_sd = 1))")))
  })
  
  output$dt_2 = renderDT({
    eval(parse(text = paste0("detect_activity_frequency_violations( activitylog = act_log, ", 
                             input$atividade_1 , " = 1,", input$atividade_2, "= 1)")))
  })
  output$dt_4 = renderDT({detect_time_anomalies( activitylog = act_log)
  })
  output$dt_5 = renderDT({
    if (input$id == TRUE) {
      detect_incomplete_cases( activitylog = act_log, activities = c("ATESTO_RECEBIMENTO","BLOQUEADO", "DESPACHO",
                                                                     "DOCUMENTO_EXCLUÍDO", "EMPENHO", "INÍCIO", "OB", 
                                                                     "PCSF", "PRESTAÇÃO_DE_CONTAS"))
    }
    else {
      data.frame(detect_incomplete_cases( activitylog = act_log, activities = c("ATESTO_RECEBIMENTO","BLOQUEADO", "DESPACHO",
                                                                                "DOCUMENTO_EXCLUÍDO", "EMPENHO", "INÍCIO", "OB", 
                                                                                "PCSF", "PRESTAÇÃO_DE_CONTAS")))[,c(1,2)]}
    
  })
  output$dt_3 = renderDT({detect_overlaps( activitylog = act_log) 
  })
  output$mensagem = renderText({"As atividades não estão se sobrepondo!"})
###########################################################################
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
######################################################################################  
  plot_pie <- reactive ({
    
    if( input$clust == "Eu escolho" ){
      
      
      clusters.p <- cutree(flashClust::hclust(as.dist(om)), k = input$nclust)
      
      x <- pie(prop.table(table(clusters.p)))
      
      
      x 
      
    } 
    
    else 
      
      
      if ( input$clust == "Método da Inércia" ) {
        
        
        n.clusters <- n.inertia.clus(as.dist(om), max = 10)
        clusters <- cutree(flashClust::hclust(as.dist(om)), k = n.clusters)
        
        pie(prop.table(table(clusters)))
        
        
      }
    
    
  })
  
  
  #Título do Gráfico com as proporções
  
  output$TituloP <- renderText({
    
    print( " Gráfico de pizza com proporções por Cluster"
           
    )
    
  })
  
  #Gráfico com as proporções 
  
  
  output$pizza <- renderPlot({
    
    plot_pie()
    
    
  })
  
  #Tabela com frequências
  
  table_pizza <- reactive ({
    
    if( input$clust == "Eu escolho" ){
      
      clusters.p <- cutree(flashClust::hclust(as.dist(om)), k = input$nclust)
      
      x <- prop.table(table(clusters.p))*100
      
      x
      
      
    } 
    
    else 
      
      
      if ( input$clust == "Método da Inércia" ) {
        
        n.clusters <- n.inertia.clus(as.dist(om), max = 10)
        clusters <- cutree(flashClust::hclust(as.dist(om)), k = n.clusters)
        
        x <- prop.table(table(clusters))*100
        
        x 
        
        
      }
    
    
  })
  
  
  
  
  
  
  output$pizzaT <- renderTable({
    
    table_pizza()
    
    
  })
  
  
  #DENDOGRAMA 
  
  
  dendo <- reactive ({
    
    if(input$clust == "Eu escolho"){
      
      dend <- log %>%
        dist() %>%
        hclust(method = "ave")%>%
        as.dendrogram()
      dend2 <- color_branches(dend, input$nclust )
      
      p <- ggplot(dend2, horiz = T, offset_labels = -3)
      ggplotly(p)
      
    }
    
    else
      
      
      if(input$clust == "Método da Inércia" ){
        
        dend <- log %>%
          dist() %>%
          hclust(method = "ave") %>%
          as.dendrogram()
        dend2 <- color_branches(dend, n.clusters )
        
        p <- ggplot(dend2, horiz = T, offset_labels = -3)
        ggplotly(p)
        
      }    
    
  })
  
  
  
  
  
  output$dendograma <- renderPlotly({
    
    dendo()  
    
    
  })
}

shinyApp(ui,server)







