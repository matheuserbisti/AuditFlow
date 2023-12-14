#Conditional Panel

conditionalPanel("input.regras_select == 'Não ocorre mais do que (n - 1) vezes'",
                 selectInput("regras_ativ_not_occur_n1","Atividade",
                             choices = NULL),
                 numericInput("regras_x_not_occur_n1", "Nº vezes", value = 1, min = 1),
                 actionButton("regras_x_not_occur_n1_bt", "Adicionar regra")
),

#Update Select Input

updateSelectInput(session, "regras_ativ_not_occur_n1",
                  choices = Atividades, selected = "")

# Observe Event da regra escolhida

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

# Condicional do loop for das regras

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