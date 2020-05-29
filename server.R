library(shiny)
library(ggplot2)
library(DT)
# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {


  
  epiTable <- function(sens, spec, population, prevalence){
    sensitivity <- round(sens/100,2)
    specificity <- round(spec/100,2)
    popsize <- population
    prev <- round(prevalence/100,4)
    
    tp_fn <- round(popsize*prev,0)
    tp <- round(tp_fn*sensitivity,0)
    fn <- tp_fn - tp
    fp_tn <- popsize - tp_fn
    tn <- round(fp_tn * specificity,0)
    fp <- popsize - (tp+tn+fn)
    
    return(matrix(c(tp,fp,fn,tn), ncol = 2, dimnames = list(c("Disease Positive", "Disease Negative"),
                                                            c("Test Positive", "Test Negative"))))
  }
  

  
  generate_contigency_table <- reactive({
    sensitivity <- round(input$sens/100,2)
    specificity <- round(input$spec/100,2)
    popsize <- input$population
    prev <- round(input$prevalence/100,4)
    
    tp_fn <- round(popsize*prev,0)
    tp <- round(tp_fn*sensitivity,0)
    fn <- tp_fn - tp
    fp_tn <- popsize - tp_fn
    tn <- round(fp_tn * specificity,0)
    fp <- popsize - (tp+tn+fn)
    
    return(matrix(c(tp,fp,fn,tn), ncol = 2, dimnames = list(c("Disease Positive", "Disease Negative"),
                                                                      c("Test Positive", "Test Negative"))))
  })
  
  
  pre_post_test_probs <- reactive({
    
    sensitivity <- round(input$sens/100,2)
    specificity <- round(input$spec/100,2)
    popsize <- input$population
    
    vec <- seq(0,100,0.1) #vector for pre-test probabilities (prevalence)
    #initiate empty vectors for storing computed post-test probs
    postProbs1 <- as.numeric()
    postProbs2 <- as.numeric()
    
    for (i in 1:length(vec)){
      prev <- round(vec[i]/100,4)
      tp_fn <- round(popsize*prev,0)
      tp <- round(tp_fn*sensitivity,0)
      fn <- tp_fn - tp
      fp_tn <- popsize - tp_fn
      tn <- round(fp_tn * specificity,0)
      fp <- popsize - (tp+tn+fn)    
    
      t <-  matrix(c(tp,fp,fn,tn), ncol = 2) #contigency table for the current sensitivity, specificity, prevalence and population size
    
    
      postProbs1[i] <- (t[1,1]/ (t[1,1] + t[2,1])) *100
      postProbs2[i] <- 100 - ((t[2,2]/ (t[2,2] + t[1,2])) *100)
      
    }
    return(list("postProbs1"= postProbs1, "postProbs2" = postProbs2, "preProbs" = vec))
    
    
  })
  
  
  output$text1 <- renderText("The sensitivity and specificity of a diagnostic/screening test are the most commonly used metrics to 
                             evaluate the accuracy of the test. Both these metrics do not depend on the distribution or prevalence of
                             the disease in population. However, in actual clinical practice, it is imperative that you are cognizant of
                             the positive predictive value (PPV) of the test. This is because it is PPV which tells you the probability of
                             having the disease if at all you are tested positive. PPV depends not just on the sensitivity and specificity 
                             of the test, but also on the prevalence of the disease. This is a remarkable property that leads to the phenomenon
                             of having low PPV in scenarios where the disease of interest is having a low prevalence. In case of very 
                             low-prevalent diseases, the PPV would still be low even if the tests are near-perfect")
  
  output$text2 <- renderText("This RShiny dashboard demonstrates the effect of prevalence of the disease on the PPV of the test. You can
                             choose a hypothetical test with pre-specified sensitivity and specificity values using the last two sliders.
                             You can also choose a hypothetical population with a pre-specified prevalence for a hypothetical disease. The table
                             below shows the classic contigency table in epidemiology and the PPV and the NPV(negative predictive value) of the tests are 
                             computed from the contigency table and are displayed below. You can adjust the prevalence, sensitivity and specificity
                             using the sliders and see how PPV (and NPV, for that matter) varies. Notably, you can see that PPV values decrease as you
                             decrease the prevalence of the disease for a given sensitivity and specificity. Note that the numbers in the contigency table
                             are rounded so that all numbers in the table are in integer form without decimals and hence in extreme cases there might be
                             some discrepancies in the actual values. But this doesn't matter and it doesn't take anything away from the interpretation.")
  
  
  output$ppv <- renderText({
    mat <- generate_contigency_table()
    text <- paste0("Positive Predictive Value: ", round( (mat[1,1])/(mat[2,1]+mat[1,1]),2)*100, "%")
    print(text)
  })
  
  output$npv <- renderText({
    mat <- generate_contigency_table()
    text <- paste0("Negative Predictive Value: ", round( (mat[2,2])/(mat[2,2]+mat[1,2]),2)*100, "%")
    print(text)
  })
  
  
  output$contigencyTable <- renderDataTable({
    DT::datatable(generate_contigency_table(), options = list(dom='t',ordering=F))
  })
  
  output$`pre-post-plot`<- renderPlot({
    l <- pre_post_test_probs()
    df <- data.frame("preProbs"=l$preProbs, "postProbs1" = l$postProbs1, "postProbs2" = l$postProbs2)
    
    df <- melt(df, id.vars = c("preProbs"), variable.name = "postProbsType", value.name = "value")
    
    subs <- paste0("Shows the association of pre-test probabilities (prevalence) post-test probabilities of a positive and negative test", "\n",
                   "The plot below is generated for a hypothetical test with ", input$sens, "% sensitivity and ",
                   input$spec, "% specificity", "\n", "The dotted lines within the plot depicts the post-test probability for a positive test (PPV) for the current selected pre-test probability (prevalence)")
    
    #to mark pret-test and post-test probs for the current user-defined prevalence
    x <- round(input$prevalence, 1)
    y <- df[df$preProbs==x & df$postProbsType=="postProbs1",]$value
    
    if(length(y)==0){
      x <- round(x)
      y <- df[df$preProbs==x & df$postProbsType=="postProbs1",]$value
    }
    

    p <- (ggplot(df, aes(x=preProbs, y= value, group=postProbsType, color=postProbsType)) + geom_line(lwd=1) + 
            geom_abline(slope = 1, lty=2) + scale_x_continuous(limits=c(0,100), breaks = seq(0,100,5), expand=c(0,0)) +
            scale_y_continuous(limits=c(0,100), breaks=seq(0,100,5), expand=c(0,0)) +
            ggtitle(label = "Post-Test Probability vs Pre-Test Probability (Prevalence)",
                    subtitle = subs) + xlab("Pre-test probability (Prevalence) %") + ylab("Post-test probability (%)")
          +labs(color='Post-test probability for:') + 
            scale_color_manual(values=c("dodgerblue4", "hotpink"), labels=c("Positive Test", "Negative Test"))+
            
            geom_polygon(aes(x=preProbs, y=value), data=df[df$postProbsType=="postProbs1",], fill="dodgerblue4", alpha=0.3) +
            geom_polygon(aes(x=preProbs, y=value), data=df[df$postProbsType=="postProbs2",], fill="hotpink", alpha=0.3))
    
    if(length(y!=0)){
      q <- p + geom_segment(aes(x = 0 , y = y, xend = x, yend = y), lwd=1, lty=2, col="black") +
        geom_segment(aes(x = x , y = 0, xend = x, yend = y), lwd=1, lty=2, col="black")
    }else {
      q <- p
    }
            
      r <- q +      guides(colour = guide_legend(override.aes = list(shape = 20))) +
            theme(axis.text.y = element_text(face="bold", color="#000000",
                                             size=10, angle=0),
                  axis.text.x = element_text(face="bold", color="#000000",
                                             size=10, angle=0),
                  axis.title.x = element_text(size = 11, face="bold"),
                  axis.title.y = element_text(size = 11, face="bold"),
                  plot.title = element_text(face="bold", size = 15, hjust = 0.5),
                  plot.subtitle = element_text(size = 12),
                  plot.background = element_rect(fill = 'ivory1', colour = 'black'),
                  panel.background = element_rect(fill = "white",
                                                  colour = "gray45",
                                                  size = 0.1, linetype = "solid"),
                  panel.grid.major = element_line(size = 0.1, linetype = 'dotted',
                                                  colour = "grey"), 
                  panel.grid.minor = element_line(size = 0.1, linetype = 'dotted',
                                                  colour = "grey")
            )
    r

    
  })
  
  output$note <- renderText("It is important to note that the pre-test probability (probability of having a disease prior to testing) is
                            equal to prevalence of the disease at a population level. The pre-test probability can also be affected by
                            clinical judgement prior to testing. For example, if the prevalence of coronary artery disease (CAD) is
                            3%, then the pre-test probability of CAD for a random patient from population is 3% without any clinical judgement.
                            However, pre-test probability can still vary from patient to patient. For example, the pre-test probability of CAD
                            for a 60 year old chronic smoker with metabolic syndrome is definitely higher than 3%. This is because the clinical
                            judgement has raised the pre-test probability from 3% to a higher level. Accordingly, the post-test probability of
                            CAD after a diagnostic test will also be higher for this patient as compared to general population")
  
  
})