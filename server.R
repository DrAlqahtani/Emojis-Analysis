#SERVER.R
# Server is a function used to render the objects created in the User Interface function of the shiny Application
# It takes the input and output as an argument
server=function(input,output){
  
  dInput <- reactive({
    (input$d)
  })
  
  powerInput <- reactive({
    (input$power)
  })
  
  kInput <- reactive({
    (input$k)
  })
 
 output$sample_size<- renderTable({  
 D=reactive({dInput()})
 P=reactive({ powerInput()})
 K=reactive({ kInput()})
 #sampleSize=pwr.t.test( d =as.numeric(D()) , power = as.numeric(P()))
 
 
 sampleSize=pwr.anova.test(  k =as.numeric(K()),f =as.numeric(D()) , sig.level = as.numeric(input$sig_level) , power = as.numeric(P()))
 
 data.frame(
   lNames = rep(names(sampleSize), lapply(sampleSize, length)),
   lVal = unlist(sampleSize))
 
 })
}
  

#shinyApp(ui=ui,server=server)
