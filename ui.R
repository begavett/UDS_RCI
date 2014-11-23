# The MIT License (MIT)
# 
# Copyright (c) 2014 Brandon Gavett
# 
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#   
#   The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
# 
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

library(shiny)

shinyUI(fluidPage(
  # Application Title
  title= "Regression-Based Calculator for Reliable Change in the Uniform Data Set Neuropsychological Battery",
  fluidRow(
column(8,
h2("Regression-Based Calculator for Reliable Change in the Uniform Data Set Neuropsychological Battery")),
column(4,
img(src = "http://www.uccs.edu/Images/brand/uccs-logo.png", width=400, height=58))),
tags$hr(),
  fluidRow(
    column(4,
           selectInput("Test","Test:",
                       choices=list("Mini-Mental State Examination" = "MMSE",
                                    "WAIS-R Digit Span Forward" = "DSF",
                                    "WAIS-R Digit Span Backward" = "DSB",
                                    "WAIS-R Digit Symbol Coding" = "DSC",
                                    "Trail Making Test part A" = "TMTA",
                                    "Trail Making Test part B" = "TMTB",
                                    "WMS-R Logical Memory (Story A) Immediate Recall" = "LMI",
                                    "WMS-R Logical Memory (Story A) Delayed Recall" = "LMD",
                                    "Animal Fluency" = "Animals",
                                    "Vegetable Fluency" = "Vegetables",
                                    "Boston Naming Test (30 odd items)" = "BNT"),multiple=FALSE),
           numericInput("T1Score", "Baseline Test Score:",29,min=0,max=300, step = 1),
           numericInput("T2Score", "Follow-up Test Score:", 25 , min = 0, max = 300, step = 1),
           numericInput("BAge", "Age at Baseline (years):",73, min = 50, max = 100, step = .01),
           numericInput("Edu", "Education (years):",16, min = 3, max = 25, step = 1),
           numericInput("Interval", "Interval from Baseline to Follow-up (months):", 13, 4.8, 18, step = .01),
           selectInput("Race", "Race:",
                       choices = list("Caucasian" = "Caucasian",
                                      "Non-Caucasuan" = "NonCaucasian"), multiple = FALSE),
           selectInput("Sex", "Sex:",
                       choices = list("Male" = "Male",
                                      "Female" = "Female"), multiple = FALSE),
           numericInput("PI", "Prediction Interval (%):", 95, min = 50, max = 99, step = 1),
           submitButton("Submit")),
    
    column(8,
           h4("Predicted Score at Follow-Up"),
           h5('Follow-up test scores outside the prediction interval indicate "reliable" change.'),
           br(),
           textOutput("PT2S"),
           textOutput("LowerCI"),
           textOutput("UpperCI"),
           textOutput("BaseRate"),
           br(),
           fluidRow(plotOutput("plot")))
    
    )
))
