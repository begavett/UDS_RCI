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

# Define server logic required to calculate various reliable change indices
shinyServer(function(input, output) {
  Yprime <- reactive({
    if (input$Test == "MMSE") {
      new.MMSE <- data.frame(MMSE.1 = input$T1Score, AGE.1 = input$BAge, EDUC = input$Edu, Interval1 = input$Interval, CAUC = factor(input$Race), SEX = factor(input$Sex))
      MMSE.lm <- readRDS("MMSE.RDS")
      predMMSE <- round(predict(MMSE.lm, new.MMSE),2)
      return(predMMSE)
    }
    if (input$Test == "DSF") {
      new.DIGIF <- data.frame(DIGIF.1 = input$T1Score, AGE.1 = input$BAge, EDUC = input$Edu, Interval1 = input$Interval, CAUC = factor(input$Race), SEX = factor(input$Sex))
      DIGIF.lm <- readRDS("DIGIF.RDS")
      predDIGIF <- round(predict(DIGIF.lm, new.DIGIF),2)
      return(predDIGIF)
    }
    if (input$Test == "DSB") {
      new.DIGIB <- data.frame(DIGIB.1 = input$T1Score, AGE.1 = input$BAge, EDUC = input$Edu, Interval1 = input$Interval, CAUC = factor(input$Race), SEX = factor(input$Sex))
      DIGIB.lm <- readRDS("DIGIB.RDS")
      predDIGIB <- round(predict(DIGIB.lm, new.DIGIB),2)
      return(predDIGIB)
    }
    if (input$Test == "DSC") {
      new.WAIS <- data.frame(WAIS.1 = input$T1Score, AGE.1 = input$BAge, EDUC = input$Edu, Interval1 = input$Interval, CAUC = factor(input$Race), SEX = factor(input$Sex))
      WAIS.lm <- readRDS("WAIS.RDS")
      predWAIS <- round(predict(WAIS.lm, new.WAIS),2)
      return(predWAIS)
    }
    if (input$Test == "TMTA") {
      new.TRAILA <- data.frame(TRAILA.1 = input$T1Score, AGE.1 = input$BAge, EDUC = input$Edu, Interval1 = input$Interval, CAUC = factor(input$Race), SEX = factor(input$Sex))
      TRAILA.lm <- readRDS("TRAILA.RDS")
      predTRAILA <- round(predict(TRAILA.lm, new.TRAILA),2)
      return(predTRAILA)
    }
    if (input$Test == "TMTB") {
      new.TRAILB <- data.frame(TRAILB.1 = input$T1Score, AGE.1 = input$BAge, EDUC = input$Edu, Interval1 = input$Interval, CAUC = factor(input$Race), SEX = factor(input$Sex))
      TRAILB.lm <- readRDS("TRAILB.RDS")
      predTRAILB <- round(predict(TRAILB.lm, new.TRAILB),2)
      return(predTRAILB)
    }
    if (input$Test == "LMI") {
      new.LOGIMEM <- data.frame(LOGIMEM.1 = input$T1Score, AGE.1 = input$BAge, EDUC = input$Edu, Interval1 = input$Interval, CAUC = factor(input$Race), SEX = factor(input$Sex))
      LOGIMEM.lm <- readRDS("LOGIMEM.RDS")
      predLOGIMEM <- round(predict(LOGIMEM.lm, new.LOGIMEM),2)
      return(predLOGIMEM)
    }
    if (input$Test == "LMD") {
      new.MEMUNITS <- data.frame(MEMUNITS.1 = input$T1Score, AGE.1 = input$BAge, EDUC = input$Edu, Interval1 = input$Interval, CAUC = factor(input$Race), SEX = factor(input$Sex))
      MEMUNITS.lm <- readRDS("MEMUNITS.RDS")
      predMEMUNITS <- round(predict(MEMUNITS.lm, new.MEMUNITS),2)
      return(predMEMUNITS)
    }
    if (input$Test == "Animals") {
      new.ANIMALS <- data.frame(ANIMALS.1 = input$T1Score, AGE.1 = input$BAge, EDUC = input$Edu, Interval1 = input$Interval, CAUC = factor(input$Race), SEX = factor(input$Sex))
      ANIMALS.lm <- readRDS("ANIMALS.RDS")
      predANIMALS <- round(predict(ANIMALS.lm, new.ANIMALS),2)
      return(predANIMALS)
    }
    if (input$Test == "Vegetables") {
      new.VEG <- data.frame(VEG.1 = input$T1Score, AGE.1 = input$BAge, EDUC = input$Edu, Interval1 = input$Interval, CAUC = factor(input$Race), SEX = factor(input$Sex))
      VEG.lm <- readRDS("VEG.RDS")
      predVEG <- round(predict(VEG.lm, new.VEG),2)
      return(predVEG)
    }
    if (input$Test == "BNT") {
      new.BOSTON <- data.frame(BOSTON.1 = input$T1Score, AGE.1 = input$BAge, EDUC = input$Edu, Interval1 = input$Interval, CAUC = factor(input$Race), SEX = factor(input$Sex))
      BOSTON.lm <- readRDS("BOSTON.RDS")
      predBOSTON <- round(predict(BOSTON.lm, new.BOSTON),2)
      return(predBOSTON)
    }
  })
  
  
  LowerCI <- reactive({
    if (input$Test == "MMSE") {
      new.MMSE <- data.frame(MMSE.1 = input$T1Score, AGE.1 = input$BAge, EDUC = input$Edu, Interval1 = input$Interval, CAUC = factor(input$Race), SEX = factor(input$Sex))
      MMSE.lm <- readRDS("MMSE.RDS")
      predMMSE <- round(predict(MMSE.lm, new.MMSE, interval = "prediction", level = input$PI/100),2)
      return(predMMSE[2])
    }
    if (input$Test == "DSF") {
      new.DIGIF <- data.frame(DIGIF.1 = input$T1Score, AGE.1 = input$BAge, EDUC = input$Edu, Interval1 = input$Interval, CAUC = factor(input$Race), SEX = factor(input$Sex))
      DIGIF.lm <- readRDS("DIGIF.RDS")
      predDIGIF <- round(predict(DIGIF.lm, new.DIGIF, interval = "prediction", level = input$PI/100),2)
      return(predDIGIF[2])
    }
    if (input$Test == "DSB") {
      new.DIGIB <- data.frame(DIGIB.1 = input$T1Score, AGE.1 = input$BAge, EDUC = input$Edu, Interval1 = input$Interval, CAUC = factor(input$Race), SEX = factor(input$Sex))
      DIGIB.lm <- readRDS("DIGIB.RDS")
      predDIGIB <- round(predict(DIGIB.lm, new.DIGIB, interval = "prediction", level = input$PI/100),2)
      return(predDIGIB[2])
    }
    if (input$Test == "DSC") {
      new.WAIS <- data.frame(WAIS.1 = input$T1Score, AGE.1 = input$BAge, EDUC = input$Edu, Interval1 = input$Interval, CAUC = factor(input$Race), SEX = factor(input$Sex))
      WAIS.lm <- readRDS("WAIS.RDS")
      predWAIS <- round(predict(WAIS.lm, new.WAIS, interval = "prediction", level = input$PI/100),2)
      return(predWAIS[2])
    }
    if (input$Test == "TMTA") {
      new.TRAILA <- data.frame(TRAILA.1 = input$T1Score, AGE.1 = input$BAge, EDUC = input$Edu, Interval1 = input$Interval, CAUC = factor(input$Race), SEX = factor(input$Sex))
      TRAILA.lm <- readRDS("TRAILA.RDS")
      predTRAILA <- round(predict(TRAILA.lm, new.TRAILA, interval = "prediction", level = input$PI/100),2)
      return(predTRAILA[2])
    }
    if (input$Test == "TMTB") {
      new.TRAILB <- data.frame(TRAILB.1 = input$T1Score, AGE.1 = input$BAge, EDUC = input$Edu, Interval1 = input$Interval, CAUC = factor(input$Race), SEX = factor(input$Sex))
      TRAILB.lm <- readRDS("TRAILB.RDS")
      predTRAILB <- round(predict(TRAILB.lm, new.TRAILB, interval = "prediction", level = input$PI/100),2)
      return(predTRAILB[2])
    }
    if (input$Test == "LMI") {
      new.LOGIMEM <- data.frame(LOGIMEM.1 = input$T1Score, AGE.1 = input$BAge, EDUC = input$Edu, Interval1 = input$Interval, CAUC = factor(input$Race), SEX = factor(input$Sex))
      LOGIMEM.lm <- readRDS("LOGIMEM.RDS")
      predLOGIMEM <- round(predict(LOGIMEM.lm, new.LOGIMEM, interval = "prediction", level = input$PI/100),2)
      return(predLOGIMEM[2])
    }
    if (input$Test == "LMD") {
      new.MEMUNITS <- data.frame(MEMUNITS.1 = input$T1Score, AGE.1 = input$BAge, EDUC = input$Edu, Interval1 = input$Interval, CAUC = factor(input$Race), SEX = factor(input$Sex))
      MEMUNITS.lm <- readRDS("MEMUNITS.RDS")
      predMEMUNITS <- round(predict(MEMUNITS.lm, new.MEMUNITS, interval = "prediction", level = input$PI/100),2)
      return(predMEMUNITS[2])
    }
    if (input$Test == "Animals") {
      new.ANIMALS <- data.frame(ANIMALS.1 = input$T1Score, AGE.1 = input$BAge, EDUC = input$Edu, Interval1 = input$Interval, CAUC = factor(input$Race), SEX = factor(input$Sex))
      ANIMALS.lm <- readRDS("ANIMALS.RDS")
      predANIMALS <- round(predict(ANIMALS.lm, new.ANIMALS, interval = "prediction", level = input$PI/100),2)
      return(predANIMALS[2])
    }
    if (input$Test == "Vegetables") {
      new.VEG <- data.frame(VEG.1 = input$T1Score, AGE.1 = input$BAge, EDUC = input$Edu, Interval1 = input$Interval, CAUC = factor(input$Race), SEX = factor(input$Sex))
      VEG.lm <- readRDS("VEG.RDS")
      predVEG <- round(predict(VEG.lm, new.VEG, interval = "prediction", level = input$PI/100),2)
      return(predVEG[2])
    }
    if (input$Test == "BNT") {
      new.BOSTON <- data.frame(BOSTON.1 = input$T1Score, AGE.1 = input$BAge, EDUC = input$Edu, Interval1 = input$Interval, CAUC = factor(input$Race), SEX = factor(input$Sex))
      BOSTON.lm <- readRDS("BOSTON.RDS")
      predBOSTON <- round(predict(BOSTON.lm, new.BOSTON, interval = "prediction", level = input$PI/100),2)
      return(predBOSTON[2])
    }
  })
  
  UpperCI <- reactive({
    if (input$Test == "MMSE") {
      new.MMSE <- data.frame(MMSE.1 = input$T1Score, AGE.1 = input$BAge, EDUC = input$Edu, Interval1 = input$Interval, CAUC = factor(input$Race), SEX = factor(input$Sex))
      MMSE.lm <- readRDS("MMSE.RDS")
      predMMSE <- round(predict(MMSE.lm, new.MMSE, interval = "prediction", level = input$PI/100),2)
      return(predMMSE[3])
    }
    if (input$Test == "DSF") {
      new.DIGIF <- data.frame(DIGIF.1 = input$T1Score, AGE.1 = input$BAge, EDUC = input$Edu, Interval1 = input$Interval, CAUC = factor(input$Race), SEX = factor(input$Sex))
      DIGIF.lm <- readRDS("DIGIF.RDS")
      predDIGIF <- round(predict(DIGIF.lm, new.DIGIF, interval = "prediction", level = input$PI/100),2)
      return(predDIGIF[3])
    }
    if (input$Test == "DSB") {
      new.DIGIB <- data.frame(DIGIB.1 = input$T1Score, AGE.1 = input$BAge, EDUC = input$Edu, Interval1 = input$Interval, CAUC = factor(input$Race), SEX = factor(input$Sex))
      DIGIB.lm <- readRDS("DIGIB.RDS")
      predDIGIB <- round(predict(DIGIB.lm, new.DIGIB, interval = "prediction", level = input$PI/100),2)
      return(predDIGIB[3])
    }
    if (input$Test == "DSC") {
      new.WAIS <- data.frame(WAIS.1 = input$T1Score, AGE.1 = input$BAge, EDUC = input$Edu, Interval1 = input$Interval, CAUC = factor(input$Race), SEX = factor(input$Sex))
      WAIS.lm <- readRDS("WAIS.RDS")
      predWAIS <- round(predict(WAIS.lm, new.WAIS, interval = "prediction", level = input$PI/100),2)
      return(predWAIS[3])
    }
    if (input$Test == "TMTA") {
      new.TRAILA <- data.frame(TRAILA.1 = input$T1Score, AGE.1 = input$BAge, EDUC = input$Edu, Interval1 = input$Interval, CAUC = factor(input$Race), SEX = factor(input$Sex))
      TRAILA.lm <- readRDS("TRAILA.RDS")
      predTRAILA <- round(predict(TRAILA.lm, new.TRAILA, interval = "prediction", level = input$PI/100),2)
      return(predTRAILA[3])
    }
    if (input$Test == "TMTB") {
      new.TRAILB <- data.frame(TRAILB.1 = input$T1Score, AGE.1 = input$BAge, EDUC = input$Edu, Interval1 = input$Interval, CAUC = factor(input$Race), SEX = factor(input$Sex))
      TRAILB.lm <- readRDS("TRAILB.RDS")
      predTRAILB <- round(predict(TRAILB.lm, new.TRAILB, interval = "prediction", level = input$PI/100),2)
      return(predTRAILB[3])
    }
    if (input$Test == "LMI") {
      new.LOGIMEM <- data.frame(LOGIMEM.1 = input$T1Score, AGE.1 = input$BAge, EDUC = input$Edu, Interval1 = input$Interval, CAUC = factor(input$Race), SEX = factor(input$Sex))
      LOGIMEM.lm <- readRDS("LOGIMEM.RDS")
      predLOGIMEM <- round(predict(LOGIMEM.lm, new.LOGIMEM, interval = "prediction", level = input$PI/100),2)
      return(predLOGIMEM[3])
    }
    if (input$Test == "LMD") {
      new.MEMUNITS <- data.frame(MEMUNITS.1 = input$T1Score, AGE.1 = input$BAge, EDUC = input$Edu, Interval1 = input$Interval, CAUC = factor(input$Race), SEX = factor(input$Sex))
      MEMUNITS.lm <- readRDS("MEMUNITS.RDS")
      predMEMUNITS <- round(predict(MEMUNITS.lm, new.MEMUNITS, interval = "prediction", level = input$PI/100),2)
      return(predMEMUNITS[3])
    }
    if (input$Test == "Animals") {
      new.ANIMALS <- data.frame(ANIMALS.1 = input$T1Score, AGE.1 = input$BAge, EDUC = input$Edu, Interval1 = input$Interval, CAUC = factor(input$Race), SEX = factor(input$Sex))
      ANIMALS.lm <- readRDS("ANIMALS.RDS")
      predANIMALS <- round(predict(ANIMALS.lm, new.ANIMALS, interval = "prediction", level = input$PI/100),2)
      return(predANIMALS[3])
    }
    if (input$Test == "Vegetables") {
      new.VEG <- data.frame(VEG.1 = input$T1Score, AGE.1 = input$BAge, EDUC = input$Edu, Interval1 = input$Interval, CAUC = factor(input$Race), SEX = factor(input$Sex))
      VEG.lm <- readRDS("VEG.RDS")
      predVEG <- round(predict(VEG.lm, new.VEG, interval = "prediction", level = input$PI/100),2)
      return(predVEG[3])
    }
    if (input$Test == "BNT") {
      new.BOSTON <- data.frame(BOSTON.1 = input$T1Score, AGE.1 = input$BAge, EDUC = input$Edu, Interval1 = input$Interval, CAUC = factor(input$Race), SEX = factor(input$Sex))
      BOSTON.lm <- readRDS("BOSTON.RDS")
      predBOSTON <- round(predict(BOSTON.lm, new.BOSTON, interval = "prediction", level = input$PI/100),2)
      return(predBOSTON[3])
    }
  })
  
  BaseRate <- reactive({
    change <- readRDS("change.RDS")
    if (input$Test == "MMSE") {
      chg <- change$MMSE[order(change$MMSE)]
      chg <- cumsum(table(chg))
      chg <- data.frame(change = as.numeric(names(chg)), freq = chg)
      chg <- rbind(chg, data.frame(change = c(min(chg$change):max(chg$change))[!(min(chg$change):max(chg$change) %in% chg$change)], 
                                   freq = rep(0, length(c(min(chg$change):max(chg$change))[!(min(chg$change):max(chg$change) %in% chg$change)]))))
      chg <- chg[order(chg$change),]
      chg$freq <- cummax(chg$freq)
      chg$br <- chg$freq/max(chg$freq)
      return(chg$br[chg$change == (input$T2Score - input$T1Score)])
    }
    if (input$Test == "DSF") {
      chg <- change$DIGIF[order(change$DIGIF)]
      chg <- cumsum(table(chg))
      chg <- data.frame(change = as.numeric(names(chg)), freq = chg)
      chg <- rbind(chg, data.frame(change = c(min(chg$change):max(chg$change))[!(min(chg$change):max(chg$change) %in% chg$change)], 
                                   freq = rep(0, length(c(min(chg$change):max(chg$change))[!(min(chg$change):max(chg$change) %in% chg$change)]))))
      chg <- chg[order(chg$change),]
      chg$freq <- cummax(chg$freq)
      chg$br <- chg$freq/max(chg$freq)
      return(chg$br[chg$change == (input$T2Score - input$T1Score)])
    }
    if (input$Test == "DSB") {
      chg <- change$DIGIB[order(change$DIGIB)]
      chg <- cumsum(table(chg))
      chg <- data.frame(change = as.numeric(names(chg)), freq = chg)
      chg <- rbind(chg, data.frame(change = c(min(chg$change):max(chg$change))[!(min(chg$change):max(chg$change) %in% chg$change)], 
                                   freq = rep(0, length(c(min(chg$change):max(chg$change))[!(min(chg$change):max(chg$change) %in% chg$change)]))))
      chg <- chg[order(chg$change),]
      chg$freq <- cummax(chg$freq)
      chg$br <- chg$freq/max(chg$freq)
      return(chg$br[chg$change == (input$T2Score - input$T1Score)])
      
    }
    if (input$Test == "DSC") {
      chg <- change$DSym[order(change$DSym)]
      chg <- cumsum(table(chg))
      chg <- data.frame(change = as.numeric(names(chg)), freq = chg)
      chg <- rbind(chg, data.frame(change = c(min(chg$change):max(chg$change))[!(min(chg$change):max(chg$change) %in% chg$change)], 
                                   freq = rep(0, length(c(min(chg$change):max(chg$change))[!(min(chg$change):max(chg$change) %in% chg$change)]))))
      chg <- chg[order(chg$change),]
      chg$freq <- cummax(chg$freq)
      chg$br <- chg$freq/max(chg$freq)
      return(chg$br[chg$change == (input$T2Score - input$T1Score)])
      
    }
    if (input$Test == "TMTA") {
      chg <- change$TrailsA[order(change$TrailsA)]
      chg <- -chg
      chg <- cumsum(table(chg))
      chg <- data.frame(change = as.numeric(names(chg)), freq = chg)
      chg <- rbind(chg, data.frame(change = c(min(chg$change):max(chg$change))[!(min(chg$change):max(chg$change) %in% chg$change)], 
                                   freq = rep(0, length(c(min(chg$change):max(chg$change))[!(min(chg$change):max(chg$change) %in% chg$change)]))))
      chg <- chg[order(chg$change),]
      chg$freq <- cummax(chg$freq)
      chg$br <- chg$freq/max(chg$freq)
      chg$change <- -chg$change
      return(chg$br[chg$change == (input$T2Score - input$T1Score)])
      
    }
    if (input$Test == "TMTB") {
      chg <- change$TrailsB[order(change$TrailsB)]
      chg <- -chg
      chg <- cumsum(table(chg))
      chg <- data.frame(change = as.numeric(names(chg)), freq = chg)
      chg <- rbind(chg, data.frame(change = c(min(chg$change):max(chg$change))[!(min(chg$change):max(chg$change) %in% chg$change)], 
                                   freq = rep(0, length(c(min(chg$change):max(chg$change))[!(min(chg$change):max(chg$change) %in% chg$change)]))))
      chg <- chg[order(chg$change),]
      chg$freq <- cummax(chg$freq)
      chg$br <- chg$freq/max(chg$freq)
      chg$change <- -chg$change
      return(chg$br[chg$change == (input$T2Score - input$T1Score)])
      
    }
    if (input$Test == "LMI") {
      chg <- change$LMI[order(change$LMI)]
      chg <- cumsum(table(chg))
      chg <- data.frame(change = as.numeric(names(chg)), freq = chg)
      chg <- rbind(chg, data.frame(change = c(min(chg$change):max(chg$change))[!(min(chg$change):max(chg$change) %in% chg$change)], 
                                   freq = rep(0, length(c(min(chg$change):max(chg$change))[!(min(chg$change):max(chg$change) %in% chg$change)]))))
      chg <- chg[order(chg$change),]
      chg$freq <- cummax(chg$freq)
      chg$br <- chg$freq/max(chg$freq)
      return(chg$br[chg$change == (input$T2Score - input$T1Score)])
      
    }
    if (input$Test == "LMD") {
      chg <- change$LMD[order(change$LMD)]
      chg <- cumsum(table(chg))
      chg <- data.frame(change = as.numeric(names(chg)), freq = chg)
      chg <- rbind(chg, data.frame(change = c(min(chg$change):max(chg$change))[!(min(chg$change):max(chg$change) %in% chg$change)], 
                                   freq = rep(0, length(c(min(chg$change):max(chg$change))[!(min(chg$change):max(chg$change) %in% chg$change)]))))
      chg <- chg[order(chg$change),]
      chg$freq <- cummax(chg$freq)
      chg$br <- chg$freq/max(chg$freq)
      return(chg$br[chg$change == (input$T2Score - input$T1Score)])
      
    }
    if (input$Test == "Animals") {
      chg <- change$ANIMALS[order(change$ANIMALS)]
      chg <- cumsum(table(chg))
      chg <- data.frame(change = as.numeric(names(chg)), freq = chg)
      chg <- rbind(chg, data.frame(change = c(min(chg$change):max(chg$change))[!(min(chg$change):max(chg$change) %in% chg$change)], 
                                   freq = rep(0, length(c(min(chg$change):max(chg$change))[!(min(chg$change):max(chg$change) %in% chg$change)]))))
      chg <- chg[order(chg$change),]
      chg$freq <- cummax(chg$freq)
      chg$br <- chg$freq/max(chg$freq)
      return(chg$br[chg$change == (input$T2Score - input$T1Score)])
      
    }
    if (input$Test == "Vegetables") {
      chg <- change$VEG[order(change$VEG)]
      chg <- cumsum(table(chg))
      chg <- data.frame(change = as.numeric(names(chg)), freq = chg)
      chg <- rbind(chg, data.frame(change = c(min(chg$change):max(chg$change))[!(min(chg$change):max(chg$change) %in% chg$change)], 
                                   freq = rep(0, length(c(min(chg$change):max(chg$change))[!(min(chg$change):max(chg$change) %in% chg$change)]))))
      chg <- chg[order(chg$change),]
      chg$freq <- cummax(chg$freq)
      chg$br <- chg$freq/max(chg$freq)
      return(chg$br[chg$change == (input$T2Score - input$T1Score)])
      
    }
    if (input$Test == "BNT") {
      chg <- change$BNT[order(change$BNT)]
      chg <- cumsum(table(chg))
      chg <- data.frame(change = as.numeric(names(chg)), freq = chg)
      chg <- rbind(chg, data.frame(change = c(min(chg$change):max(chg$change))[!(min(chg$change):max(chg$change) %in% chg$change)], 
                                   freq = rep(0, length(c(min(chg$change):max(chg$change))[!(min(chg$change):max(chg$change) %in% chg$change)]))))
      chg <- chg[order(chg$change),]
      chg$freq <- cummax(chg$freq)
      chg$br <- chg$freq/max(chg$freq)
      return(chg$br[chg$change == (input$T2Score - input$T1Score)])
      
    }
  })
  
  FreqPlot <- reactive({
    change <- readRDS("change.RDS")
    if (input$Test == "MMSE") {
      chg <- change$MMSE[order(change$MMSE)]
      chg <- cumsum(table(chg))
      chg <- data.frame(change = as.numeric(names(chg)), freq = chg)
      chg <- rbind(chg, data.frame(change = c(min(chg$change):max(chg$change))[!(min(chg$change):max(chg$change) %in% chg$change)], 
                                   freq = rep(0, length(c(min(chg$change):max(chg$change))[!(min(chg$change):max(chg$change) %in% chg$change)]))))
      chg <- chg[order(chg$change),]
      chg$freq <- cummax(chg$freq)
      chg$br <- chg$freq/max(chg$freq)
      return(chg)
    }
    if (input$Test == "DSF") {
      chg <- change$DIGIF[order(change$DIGIF)]
      chg <- cumsum(table(chg))
      chg <- data.frame(change = as.numeric(names(chg)), freq = chg)
      chg <- rbind(chg, data.frame(change = c(min(chg$change):max(chg$change))[!(min(chg$change):max(chg$change) %in% chg$change)], 
                                   freq = rep(0, length(c(min(chg$change):max(chg$change))[!(min(chg$change):max(chg$change) %in% chg$change)]))))
      chg <- chg[order(chg$change),]
      chg$freq <- cummax(chg$freq)
      chg$br <- chg$freq/max(chg$freq)
      return(chg)
    }
    if (input$Test == "DSB") {
      chg <- change$DIGIB[order(change$DIGIB)]
      chg <- cumsum(table(chg))
      chg <- data.frame(change = as.numeric(names(chg)), freq = chg)
      chg <- rbind(chg, data.frame(change = c(min(chg$change):max(chg$change))[!(min(chg$change):max(chg$change) %in% chg$change)], 
                                   freq = rep(0, length(c(min(chg$change):max(chg$change))[!(min(chg$change):max(chg$change) %in% chg$change)]))))
      chg <- chg[order(chg$change),]
      chg$freq <- cummax(chg$freq)
      chg$br <- chg$freq/max(chg$freq)
      return(chg)
    }
    if (input$Test == "DSC") {
      chg <- change$DSym[order(change$DSym)]
      chg <- cumsum(table(chg))
      chg <- data.frame(change = as.numeric(names(chg)), freq = chg)
      chg <- rbind(chg, data.frame(change = c(min(chg$change):max(chg$change))[!(min(chg$change):max(chg$change) %in% chg$change)], 
                                   freq = rep(0, length(c(min(chg$change):max(chg$change))[!(min(chg$change):max(chg$change) %in% chg$change)]))))
      chg <- chg[order(chg$change),]
      chg$freq <- cummax(chg$freq)
      chg$br <- chg$freq/max(chg$freq)
      return(chg)
    }
    if (input$Test == "TMTA") {
      chg <- change$TrailsA[order(change$TrailsA)]
      chg <- -chg
      chg <- cumsum(table(chg))
      chg <- data.frame(change = as.numeric(names(chg)), freq = chg)
      chg <- rbind(chg, data.frame(change = c(min(chg$change):max(chg$change))[!(min(chg$change):max(chg$change) %in% chg$change)], 
                                   freq = rep(0, length(c(min(chg$change):max(chg$change))[!(min(chg$change):max(chg$change) %in% chg$change)]))))
      chg <- chg[order(chg$change),]
      chg$freq <- cummax(chg$freq)
      chg$br <- chg$freq/max(chg$freq)
      chg$change <- -chg$change
      return(chg)
    }
    if (input$Test == "TMTB") {
      chg <- change$TrailsB[order(change$TrailsB)]
      chg <- -chg
      chg <- cumsum(table(chg))
      chg <- data.frame(change = as.numeric(names(chg)), freq = chg)
      chg <- rbind(chg, data.frame(change = c(min(chg$change):max(chg$change))[!(min(chg$change):max(chg$change) %in% chg$change)], 
                                   freq = rep(0, length(c(min(chg$change):max(chg$change))[!(min(chg$change):max(chg$change) %in% chg$change)]))))
      chg <- chg[order(chg$change),]
      chg$freq <- cummax(chg$freq)
      chg$br <- chg$freq/max(chg$freq)
      chg$change <- -chg$change
      return(chg)
    }
    if (input$Test == "LMI") {
      chg <- change$LMI[order(change$LMI)]
      chg <- cumsum(table(chg))
      chg <- data.frame(change = as.numeric(names(chg)), freq = chg)
      chg <- rbind(chg, data.frame(change = c(min(chg$change):max(chg$change))[!(min(chg$change):max(chg$change) %in% chg$change)], 
                                   freq = rep(0, length(c(min(chg$change):max(chg$change))[!(min(chg$change):max(chg$change) %in% chg$change)]))))
      chg <- chg[order(chg$change),]
      chg$freq <- cummax(chg$freq)
      chg$br <- chg$freq/max(chg$freq)
      return(chg)
    }
    if (input$Test == "LMD") {
      chg <- change$LMD[order(change$LMD)]
      chg <- cumsum(table(chg))
      chg <- data.frame(change = as.numeric(names(chg)), freq = chg)
      chg <- rbind(chg, data.frame(change = c(min(chg$change):max(chg$change))[!(min(chg$change):max(chg$change) %in% chg$change)], 
                                   freq = rep(0, length(c(min(chg$change):max(chg$change))[!(min(chg$change):max(chg$change) %in% chg$change)]))))
      chg <- chg[order(chg$change),]
      chg$freq <- cummax(chg$freq)
      chg$br <- chg$freq/max(chg$freq)
      return(chg)
    }
    if (input$Test == "Animals") {
      chg <- change$ANIMALS[order(change$ANIMALS)]
      chg <- cumsum(table(chg))
      chg <- data.frame(change = as.numeric(names(chg)), freq = chg)
      chg <- rbind(chg, data.frame(change = c(min(chg$change):max(chg$change))[!(min(chg$change):max(chg$change) %in% chg$change)], 
                                   freq = rep(0, length(c(min(chg$change):max(chg$change))[!(min(chg$change):max(chg$change) %in% chg$change)]))))
      chg <- chg[order(chg$change),]
      chg$freq <- cummax(chg$freq)
      chg$br <- chg$freq/max(chg$freq)
      return(chg)
    }
    if (input$Test == "Vegetables") {
      chg <- change$VEG[order(change$VEG)]
      chg <- cumsum(table(chg))
      chg <- data.frame(change = as.numeric(names(chg)), freq = chg)
      chg <- rbind(chg, data.frame(change = c(min(chg$change):max(chg$change))[!(min(chg$change):max(chg$change) %in% chg$change)], 
                                   freq = rep(0, length(c(min(chg$change):max(chg$change))[!(min(chg$change):max(chg$change) %in% chg$change)]))))
      chg <- chg[order(chg$change),]
      chg$freq <- cummax(chg$freq)
      chg$br <- chg$freq/max(chg$freq)
      return(chg)
    }
    if (input$Test == "BNT") {
      chg <- change$BNT[order(change$BNT)]
      chg <- cumsum(table(chg))
      chg <- data.frame(change = as.numeric(names(chg)), freq = chg)
      chg <- rbind(chg, data.frame(change = c(min(chg$change):max(chg$change))[!(min(chg$change):max(chg$change) %in% chg$change)], 
                                   freq = rep(0, length(c(min(chg$change):max(chg$change))[!(min(chg$change):max(chg$change) %in% chg$change)]))))
      chg <- chg[order(chg$change),]
      chg$freq <- cummax(chg$freq)
      chg$br <- chg$freq/max(chg$freq)
      return(chg)
    }
  })
  
  
  output$PT2S <- renderText(paste0("Predicted Time 2 Score: ", Yprime()))
  output$LowerCI <- renderText({paste0("Lower Limit of ", as.numeric(input$PI), "% Prediction Interval: ", LowerCI())})
  output$UpperCI <- renderText({paste0("Upper Limit of ", as.numeric(input$PI), "% Prediction Interval: ", UpperCI())})
  x <- reactive({
      if((input$T2Score - input$T1Score) < min(FreqPlot()$change)) min(FreqPlot()$change)
      if((input$T2Score - input$T1Score) > max(FreqPlot()$change)) max(FreqPlot()$change) else (input$T2Score - input$T1Score)
    })
  
  output$BaseRate <- renderText({
    if (input$Test == "TMTA" | input$Test == "TMTB") {
    paste0("The base rate of a score change of \u2265 ", x(), " points is ", round(100*BaseRate(),2), "%.")
    } else     paste0("The base rate of a score change of \u2264 ", x(), " points is ", round(100*BaseRate(),2), "%.")  
  })
    
  output$plot <- renderPlot({
    par(mfrow = c(1,2))
    plot(input$T2Score, 1, type = "p", xlab = "Score", axes = FALSE, ylab = "", 
         pch = 16, xlim = c(min(input$T2Score, Yprime(), LowerCI()), max(input$T2Score, Yprime(), UpperCI())),
         ylim=c(.5, 1.5))
    axis(1)
    points(Yprime(), 1, pch = 17, col = "red")
    arrows(Yprime(), 1, LowerCI(), 1, col = "red", angle = 90)
    arrows(Yprime(), 1, UpperCI(), 1, col = "red", angle = 90)
    legend("top", pch = c(16, 17), col = c("black","red"), 
           c("Observed Time 2 Score", "Predicted Time 2 Score"))
    if (input$Test == "TMTA" | input$Test == "TMTB") {
    plot(FreqPlot()$change, FreqPlot()$br, type = "l", xlab = "Change", ylab = "Cumulative Percentage", xlim = rev(range(FreqPlot()$change)))
    points(x(), BaseRate(), col = "black", pch = 16)
    } else {
      plot(FreqPlot()$change, FreqPlot()$br, type = "l", xlab = "Change", ylab = "Cumulative Percentage")
      points(x(), BaseRate(), col = "black", pch = 16)
    }
  })
})

