library("shiny")
library("ggplot2")
library("DT")
library("reshape2")
library("dplyr")
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Positive predictive value and its association with prevalence"),
  
  sidebarLayout(position='left',
    sidebarPanel(width=3,
      
      sliderInput("prevalence",
                  "Prevalence of the hypothetical disease in population",
                  min = 0,
                  max = 25,
                  value = 3, step = 0.1),
      
      
      sliderInput("population", "Size of the hypothetical population",
                    min = 100, max=10000, value=1000, step=100),
      
    
      sliderInput("sens",
                  "Sensitivity of the hypothetical diagnostic/screening test",
                  min = 1,
                  max = 100,
                  value =80, step = 1),

      sliderInput("spec",
                  "Specificity of the hypothetical diagnostic/screening test",
                  min = 1,
                  max = 100,
                  value = 90, step = 1)

    
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      
      h5(textOutput("text1")),
      h5(textOutput("text2")),
      
      h4(textOutput("sen")),
      h4(textOutput("spc")),
      
      h4(textOutput("ppv")),
      h4(textOutput("npv")),
      
      
      # splitLayout(dataTableOutput("contigencyTable", width=200), 
      #              plotOutput("pre-post-plot", width = 1000))
      # 
      dataTableOutput("contigencyTable", width=400),
      
      plotOutput("pre-post-plot", width = 1000),
      
      h5(textOutput("note"))
      
                            
                   )
      

    )
  )
)
