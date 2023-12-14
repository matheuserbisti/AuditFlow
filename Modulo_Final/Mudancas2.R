#Conditional Panel

conditionalPanel("input.regras_select == 'Termina com'",
                 selectInput("regras_ativ_ends","Atividade",
                             choices = NULL),
                 actionButton("regras_ativ_ends_bt", "Adicionar regra")
),

#Update Select Input

updateSelectInput(session, "regras_ativ_ends",
                  choices = Atividades, selected = "")

# Observe Event da regra escolhida

observeEvent(input$regras_ativ_ends_bt, {
  regras <- tab_regras()
  Ordem <- nrow(regras) + 1
  Regra <- "Termina com"
  Detalhe <- input$regras_ativ_ends
  regras <- rbind.data.frame(regras,
                             data.frame(Ordem=Ordem,Regra=Regra, Detalhe=Detalhe))
  tab_regras(regras)
})

# Condicional do loop for das regras

if(tab$Regra[i] == "Termina com"){ 
  especs <- tab$Detalhe[i]
  codigo <- paste0(codigo, "check_rule(ends('", especs,"'), label = 'Regra_",i,"')")
  if(i != nrow(tab)){
    codigo <- paste0(codigo, " %>% \n")
  }else{
    codigo <- paste0(codigo, " %>% \n ", grupos," n_cases()")
  }
}