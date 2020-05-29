# Functions for creating fluid page layouts in shiny Application.
# A fluid page layout consists of rows which in turn include columns
################################################################################
ui=fluidPage(
  titlePanel(" one-way ANOVA sample calculator by Khaled Alqahtani @alqahtani_khald"),
  fluidRow(
    column(3),
    column(5,
           textInput("d", "the effect size",value = 0.3),
           textInput("k", "is the number of groups",value = 3),
           textInput('power', 'Power of the test ( between 0 and 1, suggested 0.8)',value = 0.8),
           selectInput("sig_level", "sig.level",c(0.05,0.01))
           )
    
    ),
  
  fluidRow(
    column(6,offset=3,
           tableOutput("sample_size")))
)