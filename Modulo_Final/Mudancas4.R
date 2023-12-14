#Conditional Panel

conditionalPanel("input.regras_select == 'Não ocorre menos do que n, e não mais do que k vezes'",
                 selectInput("regras_ativ_contains_between","Atividade",
                             choices = NULL),
                 numericInput("regras_x_contains_between_min", "Nº mínimo", value = 1, min = 1),
                 numericInput("regras_x_contains_between_max", "Nº máximo", value = 1, min = 1),
                 actionButton("regras_x_contains_between_bt", "Adicionar regra")
),

#Update Select Input

updateSelectInput(session, "regras_ativ_contains_between",
                  choices = Atividades, selected = "")

# Observe Event da regra escolhida

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

# Condicional do loop for das regras

if(tab$Regra[i] == "Ocorre entre n e k"){
  especs <- trimws(unlist(strsplit(tab$Detalhe[i], split = "\\|")))
  codigo <- paste0(codigo, "check_rule(contains_between('", 
                   especs[1],"', min = ", especs[2],"', max = ", especs[3],
                   "), label = 'Regra_",i,"')")
  if(i != nrow(tab)){
    codigo <- paste0(codigo, " %>% \n")
  }else{
    codigo <- paste0(codigo, " %>% \n ", grupos," n_cases()")
  }
}