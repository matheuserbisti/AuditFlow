#Conditional Panel

conditionalPanel("input.regras_select == 'Se a Atividade A existir, B não deve existir; se a Atividade B existe, A não deve existir'",
                 selectInput("regras_ativ_xorA","Atividade A",
                             choices = NULL),
                 selectInput("regras_ativ_xorB","Atividade B",
                             choices = NULL),
                 actionButton("regras_x_xor_bt", "Adicionar regra")
),

#Update Select Input

updateSelectInput(session, "regras_ativ_xorA",
                  choices = Atividades, selected = "")
updateSelectInput(session, "regras_ativ_xorB",
                  choices = Atividades, selected = "")

# Observe Event da regra escolhida

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

# Condicional do loop for das regras

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