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

shinyUI(
  dashboardPage(
    dashboardHeader(title = "Probability Project",titleWidth = 200),
    dashboardSidebar(
      sidebarMenu(
        
        menuItem("Home",tabName = "home",icon = icon("home")),
        

                 
        menuItem("Diabetes Data",tabName = "data",icon = icon("database")
                # menuSubItem("Summary",tabName="summary")
                 ),
        
        menuItem("Graphical Representation",tabName = "box",
                  menuSubItem("Histograms",tabName = "histogram"),
               
                 #BOX PLOTS
                 menuSubItem("Box Plots",tabName="Boxplot")
                 
                 
        ),
        menuItem("Statistical Analysis ",tabName = "Mean",
                 #Pregnancies
                 menuSubItem(" Pregnancies",tabName = "Preg"),
                 menuSubItem(" Glucose",tabName = "glucose"),
                 menuSubItem(" Blood Pressure",tabName = "BP"),
                 menuSubItem(" Skin Thickness",tabName = "SK"),
                 menuSubItem(" Insulin",tabName = "Ins"),
                 menuSubItem(" BMI",tabName = "bmi"),
                 menuSubItem(" Diabetes Pedigree Function",tabName = "DPF"),
                 menuSubItem(" Age",tabName = "A")
               
        ),
 
        #Poisson
        menuItem("Poisson Distribuition",tabName = "Poisson",
                 menuSubItem(" Pregnancies",tabName = "Poisson_P"),
                 menuSubItem("Glucose",tabName = "Poisson_G"),
                 menuSubItem("Blood Pressure",tabName = "Poisson_BP")
                 ),
        
        #PICS
        menuItem("Regression",tabName = "pics",
                 menuSubItem(" BMI and Age",tabName = "B_A"),
                 menuSubItem("Pregnancies and Age",tabName = "P_A"),
                 menuSubItem("Pregnancies and BMI",tabName = "P_B")
        )
      )
   
    ),
    dashboardBody(

      tabItems(
        tabItem(tabName="home", width = 12, 
               fluidRow(
                 column(width = 11, imageOutput("homepic"), #tags$img(src="home.png", width =300 , height = 300),
                        tags$br() ,
                        tags$a("EVENS STATS"), align = "center"),
                 column(width = 11, tags$br() ,
                        tags$p("After detailed research THE EVENS have selected women diabetes data set from KAGGLE. It contains the details information about Gravida [a number to indicate the number of pregnancies a woman has had], Hypertension [blood pressure], Blood sugar [glucose], BMI [Body Mass Index] and insulin.

We applied different statistical methods, graphical representations, and probability methods using these attributes.
", align = "center")
                 )
               )
        ),
        
        tabItem(tabName="data",fluidPage(
          titlePanel("Data"),dataTableOutput("mydatatable"),  
          titlePanel("Summary"), verbatimTextOutput("summary"))
        ),
          
        #tabItem(tabName="summary", verbatimTextOutput("summary")),  
        
        tabItem(tabName = "histogram",fluidPage(
                     titlePanel("Pregnancies Histogram"),plotOutput("P_plot"),
                     titlePanel("Glucose Histogram"), plotOutput("G_plot"),
                     titlePanel("Blood Pressure Historgam") ,plotOutput("BP_plot"),
                     titlePanel("SkinThickness Historgam") ,plotOutput("Sk_plot"),
                     titlePanel("Insulin Historgam") ,plotOutput("insulin_plot"),
                     titlePanel("BMI Historgam") ,plotOutput("bmi_plot"),
                     titlePanel("Diabetes Pedigree Function Historgam") ,plotOutput("dpf_plot"),
                     titlePanel("Age Historgam") ,plotOutput("age_plot")
                     
                     
                     )
        ),
        
        tabItem(tabName = "Boxplot",fluidPage(
                    titlePanel("Pregnancies Boxplot"),plotOutput("P_Bplot"),
                    titlePanel("Glucose Boxplot"), plotOutput("G_Bplot"),
                    titlePanel("Blood Pressure Boxplot") ,plotOutput("BP_Bplot"),
                    titlePanel("SkinThickness Boxplot") ,plotOutput("Sk_Bplot"),
                    
                    titlePanel("Insulin Boxplot") ,plotOutput("insulin_Bplot"),
                    titlePanel("BMI Boxplot") ,plotOutput("bmi_Bplot"),
                    titlePanel("Diabetes Pedigree Function Boxplot") ,plotOutput("dpf_Bplot"),
                    titlePanel("Age Boxplot") ,plotOutput("age_Bplot")
                    
          
        )
        ),
                
                
      
        
        #STATICAL ANALYSIS OF PREGNANCIES
        
        tabItem(tabName = "Preg",fluidPage(
          
          titlePanel("Median"),infoBoxOutput("Median_P",width = 4),
          titlePanel("Standard Deviation"),infoBoxOutput("SD_P",width = 4),
          titlePanel("Variance"),infoBoxOutput("V_P",width = 4)

         )
         ),
        
        
            #STATICAL ANALYSIS OF GLUCOSE
        
          tabItem(tabName = "glucose",fluidPage(
          
          titlePanel("Mean"),infoBoxOutput("Mean_G",width = 4),
          titlePanel("Standard Deviation"),infoBoxOutput("SD_G",width = 4),
          titlePanel("Variance"),infoBoxOutput("V_G",width = 4)
          
        )
        ),
        
        #STATICAL ANALYSIS OF BloodPressure
        
        tabItem(tabName = "BP",fluidPage(
          
          titlePanel("Mean"),infoBoxOutput("Mean_BP",width = 4),
          titlePanel("Standard Deviation"),infoBoxOutput("SD_BP",width = 4),
          titlePanel("Variance"),infoBoxOutput("V_BP",width = 4)
          
        )
        ),
        
        
        #STATICAL ANALYSIS OF Skin Thickness
        
        tabItem(tabName = "SK",fluidPage(
          
          titlePanel("Median"),infoBoxOutput("Median_SK",width = 4),
          titlePanel("Standard Deviation"),infoBoxOutput("SD_SK",width = 4),
          titlePanel("Variance"),infoBoxOutput("V_SK",width = 4)
          
        )
        ),
        
        #STATICAL ANALYSIS OF Insulin
        
        tabItem(tabName = "Ins",fluidPage(
          
          titlePanel("Median"),infoBoxOutput("Median_I",width = 4),
          titlePanel("Standard Deviation"),infoBoxOutput("SD_I",width = 4),
          titlePanel("Variance"),infoBoxOutput("V_I",width = 4)
          
        )
        ),
        
        #STATICAL ANALYSIS OF BMI
        
        tabItem(tabName = "bmi",fluidPage(
          
          titlePanel("Mean"),infoBoxOutput("Mean_bmi",width = 4),
          titlePanel("Standard Deviation"),infoBoxOutput("SD_bmi",width = 4),
          titlePanel("Variance"),infoBoxOutput("V_bmi",width = 4)
          
        )
        ),
        
        
        #STATICAL ANALYSIS OF Diabetes
        
        tabItem(tabName = "DPF",fluidPage(
          
          titlePanel("Median"),infoBoxOutput("Median_dpf",width = 4),
          titlePanel("Standard Deviation"),infoBoxOutput("SD_dpf",width = 4),
          titlePanel("Variance"),infoBoxOutput("V_dpf",width = 4)
          
        )
        ),
        
        #STATICAL ANALYSIS OF Age
        
        
        tabItem(tabName = "A",fluidPage(
          
          titlePanel("Median"),infoBoxOutput("Median_A",width = 4),
          titlePanel("Standard Deviation"),infoBoxOutput("SD_A",width = 4),
          titlePanel("Variance"),infoBoxOutput("V_A",width = 4)
          
        )
        ),
        
        
        # Poisson
        tabItem(tabName = "Poisson_P",  plotOutput("PoissonPlot1")),
        tabItem(tabName = "Poisson_G",  plotOutput("PoissonPlot2")),
        tabItem(tabName = "Poisson_BP",  plotOutput("PoissonPlot3")),
        
        #Regression
        tabItem(tabName = "B_A",imageOutput("pic1")),
        tabItem(tabName = "P_A",imageOutput("pic2")),
        tabItem(tabName = "P_B",imageOutput("pic3"))

      )
    )
  )
)
