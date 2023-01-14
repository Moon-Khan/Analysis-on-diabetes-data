#
library(tidyverse)
library(elo)
library(shiny)
library(DT)
library(shinydashboard)
library(shinythemes)
library(shiny)
library(dplyr)
library(tidyr)
library(shinyWidgets)
library(ggplot2)


shinyServer(function(input,output,session){
  
  
  output$homepic <- renderImage({
    outfile <- tempfile(fileext='.png')


    # Generate a png
    png(outfile, width=400, height=400)

    list(src = "home.png",
         contentType = 'image/png',
         width = 400,
         height = 400,
         alt = "This is alternate text")
  })

  ## diabetes dataset
  output$mydatatable <- renderDataTable({
    diabetes
    
  })
  
  
  output$summary <- renderPrint({
    diabetes %>%
      summary()
  })

 
  
  output$P_plot <- renderPlot({
     ggplot(diabetes,aes(x=Pregnancies))+
      geom_histogram()
      })
  
  output$G_plot <- renderPlot({
    ggplot(diabetes,aes(x=Glucose))+
      geom_histogram() 
    })
  
  output$BP_plot <- renderPlot({
    ggplot(diabetes,aes(x=BloodPressure))+
      geom_histogram()  
    })
  
  output$Sk_plot <- renderPlot({
    ggplot(diabetes,aes(x=SkinThickness))+
      geom_histogram()  
    })
  
  output$insulin_plot <- renderPlot({
    ggplot(diabetes,aes(x=Insulin ))+
      geom_histogram()  
    })
  
  output$bmi_plot <- renderPlot({
    ggplot(diabetes,aes(x=BMI))+
      geom_histogram()
    })
  
  output$dpf_plot <- renderPlot({
    ggplot(diabetes,aes(x=DiabetesPedigreeFunction))+
      geom_histogram()
    })
  
  output$age_plot <- renderPlot({
    ggplot(diabetes,aes(x=Age))+
      geom_histogram()
    })
  
  
  #BOXPLOTS
  output$P_Bplot <- renderPlot({

      ggplot(diabetes,aes(x=Pregnancies)) +
      geom_boxplot() 

  })
  
  output$G_Bplot <- renderPlot({
    
    ggplot(diabetes,aes(x=Glucose)) +
      geom_boxplot() 
    
  })
  
  output$BP_Bplot <- renderPlot({
    
    ggplot(diabetes,aes(x=BloodPressure)) +
      geom_boxplot() 
    
  })
  
  output$Sk_Bplot <- renderPlot({
    ggplot(diabetes,aes(x=SkinThickness))+
      geom_boxplot()  
  })
  
  output$insulin_Bplot <- renderPlot({
    
    ggplot(diabetes,aes(x=Insulin)) +
      geom_boxplot() 
    
  })
  
  output$bmi_Bplot <- renderPlot({
    
    ggplot(diabetes,aes(x=BMI)) +
      geom_boxplot() 
    
  })
  
  output$dpf_Bplot <- renderPlot({
    
    ggplot(diabetes,aes(x=DiabetesPedigreeFunction)) +
      geom_boxplot() 
    
  })
  
  output$age_Bplot <- renderPlot({
    
    ggplot(diabetes,aes(x=Age)) +
      geom_boxplot() 
    
  })
  

  
  
  
  #STATICAL ANALYSIS OF PREGNANCIES
  
 
  
  output$Median_P <- renderInfoBox({
    infoBox(title = tags$b("Median_P"),
            value = median(diabetes$Pregnancies),
            subtitle = "Medain of Pregnancies",fill = T,color = "blue"
    )
  })
  
  output$SD_P <- renderInfoBox({
    infoBox(title = tags$b("SD_P"),
            value = median(diabetes$Pregnancies),
            subtitle = "Standard Devitaion of Pregnancies",fill = T,color = "blue"
    )
  })
  
  output$V_P <- renderInfoBox({
    infoBox(title = tags$b("V_P"),
            value = median(diabetes$Pregnancies),
            subtitle = "Variance of Pregnancies",fill = T,color = "blue"
    )
  })
  
  #STATICAL ANALYSIS OF GLUCOSE
  
  output$Mean_G <- renderInfoBox({
    infoBox(title = tags$b("Mean_G"),
            value = mean(diabetes$Glucose),
            subtitle = "Mean of Glucose",fill = T,color = "blue"
    )
  })
  

  
  output$SD_G <- renderInfoBox({
    infoBox(title = tags$b("SD_G"),
            value = median(diabetes$Glucose),
            subtitle = "Standard Devitaion of Glucose",fill = T,color = "blue"
    )
  })
  
  output$V_G <- renderInfoBox({
    infoBox(title = tags$b("V_G"),
            value = median(diabetes$Glucose),
            subtitle = "Variance of Glucose",fill = T,color = "blue"
    )
  })
  
  #STATICAL ANALYSIS OF Blood Pressure
  
  output$Mean_BP <- renderInfoBox({
    infoBox(title = tags$b("Mean_BP"),
            value = mean(diabetes$BloodPressure),
            subtitle = "Mean of Blood Pressure",fill = T,color = "blue"
    )
  })
  

  
  output$SD_BP <- renderInfoBox({
    infoBox(title = tags$b("SD_BP"),
            value = median(diabetes$BloodPressure),
            subtitle = "Standard Devitaion of Blood Pressure",fill = T,color = "blue"
    )
  })
  
  output$V_BP <- renderInfoBox({
    infoBox(title = tags$b("V_BP"),
            value = median(diabetes$BloodPressure),
            subtitle = "Variance of Blood Pressure",fill = T,color = "blue"
    )
  })
  
  
  #STATICAL ANALYSIS OF Skin Thickness

  
  output$Median_SK <- renderInfoBox({
    infoBox(title = tags$b("Median_ST"),
            value = median(diabetes$SkinThickness),
            subtitle = "Medain of Skin Thickness",fill = T,color = "blue"
    )
  })
  
  output$SD_SK <- renderInfoBox({
    infoBox(title = tags$b("SD_ST"),
            value = median(diabetes$SkinThickness),
            subtitle = "Standard Devitaion of Skin Thickness",fill = T,color = "blue"
    )
  })
  
  output$V_SK <- renderInfoBox({
    infoBox(title = tags$b("V_BP"),
            value = median(diabetes$SkinThickness),
            subtitle = "Variance of Skin Thickness",fill = T,color = "blue"
    )
  })
  
  
  #STATICAL ANALYSIS OF Insulin
  
 
  
  output$Median_I <- renderInfoBox({
    infoBox(title = tags$b("Median_I"),
            value = median(diabetes$Insulin),
            subtitle = "Medain of Insulin",fill = T,color = "blue"
    )
  })
  
  output$SD_I <- renderInfoBox({
    infoBox(title = tags$b("SD_I"),
            value = median(diabetes$Insulin),
            subtitle = "Standard Devitaion of Insulin",fill = T,color = "blue"
    )
  })
  
  output$V_I<- renderInfoBox({
    infoBox(title = tags$b("V_I"),
            value = median(diabetes$Insulin),
            subtitle = "Variance of Insulin",fill = T,color = "blue"
    )
  })
  
  
  #STATICAL ANALYSIS OF BMI
  
  output$Mean_bmi <- renderInfoBox({
    infoBox(title = tags$b("Mean_bmi"),
            value = mean(diabetes$BMI),
            subtitle = "Mean of BMI",fill = T,color = "blue"
    )
  })
  

  
  output$SD_bmi <- renderInfoBox({
    infoBox(title = tags$b("SD_bmi"),
            value = median(diabetes$BMI),
            subtitle = "Standard Devitaion of BMI",fill = T,color = "blue"
    )
  })
  
  output$V_bmi <- renderInfoBox({
    infoBox(title = tags$b("V_bmi"),
            value = median(diabetes$BMI),
            subtitle = "Variance of BMI",fill = T,color = "blue"
    )
  })
  
  #STATICAL ANALYSIS OF Diabetes

  
  output$Median_dpf <- renderInfoBox({
    infoBox(title = tags$b("Median_D"),
            value = median(diabetes$DiabetesPedigreeFunction),
            subtitle = "Medain of Skin Thickness",fill = T,color = "blue"
    )
  })
  
  output$SD_dpf <- renderInfoBox({
    infoBox(title = tags$b("SD_D"),
            value = median(diabetes$DiabetesPedigreeFunction),
            subtitle = "Standard Devitaion of Diabetes Pedigree Functio",fill = T,color = "blue"
    )
  })
  
  output$V_dpf <- renderInfoBox({
    infoBox(title = tags$b("V_D"),
            value = median(diabetes$DiabetesPedigreeFunction),
            subtitle = "Variance of Diabetes Pedigree Functio",fill = T,color = "blue"
    )
  })
  
  #STATICAL ANALYSIS OF Age
  
  
  output$Median_A <- renderInfoBox({
    infoBox(title = tags$b("Median_A"),
            value = median(diabetes$Age),
            subtitle = "Medain of Age",fill = T,color = "blue"
    )
  })
  
  output$SD_A <- renderInfoBox({
    infoBox(title = tags$b("SD_A"),
            value = median(diabetes$Age),
            subtitle = "Standard Devitaion of Age",fill = T,color = "blue"
    )
  })
  
  output$V_A <- renderInfoBox({
    infoBox(title = tags$b("V_A"),
            value = median(diabetes$Age),
            subtitle = "Variance of Age",fill = T,color = "blue"
    )
  })
  
  #poisson
  output$PoissonPlot1 <- renderPlot({
    
    # generate lambda based on input$lambda from ui.R
    l = median(diabetes$Pregnancies)
    x <- 0:17
    
    # generate trials based on lambda value
    muCalculation <- function(x, lambda) {dpois(x, l=lambda)}
    probability_at_lambda <- sapply(input$x, muCalculation, seq(1, 17, 0.01))
    # draw the probability
    plot(x, dpois(x, l), type='h',lwd=3)
    title(main="Poisson Distribution")
  })
  
  output$PoissonPlot2 <- renderPlot({
    
    # generate lambda based on input$lambda from ui.R
    l = mean(diabetes$Glucose)
    x <- 0:199
    
    # generate trials based on lambda value
    muCalculation <- function(x, lambda) {dpois(x, l=lambda)}
    probability_at_lambda <- sapply(input$x, muCalculation, seq(1, 199, 0.01))
    # draw the probability
    plot(x, dpois(x, l), type='h',lwd=3)
    title(main="Poisson Distribution")
  })
  
  
  output$PoissonPlot3 <- renderPlot({
    
    # generate lambda based on input$lambda from ui.R
    l = mean(diabetes$BloodPressure)
    x <- 0:122
    
    # generate trials based on lambda value
    muCalculation <- function(x, lambda) {dpois(x, l=lambda)}
    probability_at_lambda <- sapply(input$x, muCalculation, seq(1, 199, 0.01))
    # draw the probability
    plot(x, dpois(x, l), type='h',lwd=3)
    title(main="Poisson Distribution")
  })
  
  
  
  
  #PICS
  output$pic1 <- renderImage({
    
    outfile <- tempfile(fileext='.png')
    
    
    # Generate a png
    png(outfile, width=400, height=400)
    
    list(src = "P1.png",
         contentType = 'image/png',
         width = 400,
         height = 300,
         alt = "This is alternate text")
    
  }
  )
  
  
  output$pic2 <- renderImage({
    
    outfile <- tempfile(fileext='.png')

    png(outfile, width=400, height=400)
    
    list(src = "P2.png",
         contentType = 'image/png',
         width = 400,
         height = 300,
         alt = "This is alternate text")
    
  }
  )
  
  
  output$pic3 <- renderImage({
    
    outfile <- tempfile(fileext='.png')

    png(outfile, width=400, height=400)
    
    list(src = "P3.png",
         contentType = 'image/png',
         width = 400,
         height = 300,
         alt = "This is alternate text")
    
  }
  )
  
  
})