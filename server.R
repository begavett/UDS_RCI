# The MIT License (MIT)
# 
# Copyright (c) 2015 Brandon Gavett
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
library(lme4)

# Define server logic required to calculate various reliable change indices
shinyServer(function(input, output) {
  Yprime <- reactive({
    if (input$Test == "MMSE") {
      newdat <- data.frame(MMSE.bl = input$T1Score, AGE.bl = input$BAge, 
                           EDUC = input$Edu, vnumber = 2, CAUC = factor(input$Race, levels = c("Caucasian", "NonCaucasian")), 
                           SEX = factor(input$Sex, levels = c("Male", "Female")), MMSE = input$T2Score)
      model <- readRDS("MMSE.Rds")
      mm <- model.matrix(terms(model), newdat)
      newdat$test.fu <- y_prime <- predict(model, newdat, re.form = NA)
      return(y_prime)
    }
    if (input$Test == "DSF") {
      newdat <- data.frame(DIGIF.bl = input$T1Score, AGE.bl = input$BAge, 
                           EDUC = input$Edu, vnumber = 2, CAUC = factor(input$Race, levels = c("Caucasian", "NonCaucasian")), 
                           SEX = factor(input$Sex, levels = c("Male", "Female")), DIGIF = input$T2Score)
      model <- readRDS("DIGIF.Rds")
      mm <- model.matrix(terms(model), newdat)
      newdat$test.fu <- y_prime <- predict(model, newdat, re.form = NA)
      return(y_prime)
    }
    if (input$Test == "DSB") {
      newdat <- data.frame(DIGIB.bl = input$T1Score, AGE.bl = input$BAge, 
                           EDUC = input$Edu, vnumber = 2, CAUC = factor(input$Race, levels = c("Caucasian", "NonCaucasian")), 
                           SEX = factor(input$Sex, levels = c("Male", "Female")), DIGIB = input$T2Score)
      model <- readRDS("DIGIB.Rds")
      mm <- model.matrix(terms(model), newdat)
      newdat$test.fu <- y_prime <- predict(model, newdat, re.form = NA)
      return(y_prime)
    }
    if (input$Test == "DSC") {
      newdat <- data.frame(WAIS.bl = input$T1Score, AGE.bl = input$BAge, 
                           EDUC = input$Edu, vnumber = 2, CAUC = factor(input$Race, levels = c("Caucasian", "NonCaucasian")), 
                           SEX = factor(input$Sex, levels = c("Male", "Female")), WAIS = input$T2Score)
      model <- readRDS("WAIS.Rds")
      mm <- model.matrix(terms(model), newdat)
      newdat$test.fu <- y_prime <- predict(model, newdat, re.form = NA)
      return(y_prime)
    }
    if (input$Test == "TMTA") {
      newdat <- data.frame(TRAILA.bl = input$T1Score, AGE.bl = input$BAge, 
                           EDUC = input$Edu, vnumber = 2, CAUC = factor(input$Race, levels = c("Caucasian", "NonCaucasian")), 
                           SEX = factor(input$Sex, levels = c("Male", "Female")), TRAILA = input$T2Score)
      model <- readRDS("TRAILA.Rds")
      mm <- model.matrix(terms(model), newdat)
      newdat$test.fu <- y_prime <- predict(model, newdat, re.form = NA)
      return(y_prime)
    }
    if (input$Test == "TMTB") {
      newdat <- data.frame(TRAILB.bl = input$T1Score, AGE.bl = input$BAge, 
                           EDUC = input$Edu, vnumber = 2, CAUC = factor(input$Race, levels = c("Caucasian", "NonCaucasian")), 
                           SEX = factor(input$Sex, levels = c("Male", "Female")), TRAILB = input$T2Score)
      model <- readRDS("TRAILB.Rds")
      mm <- model.matrix(terms(model), newdat)
      newdat$test.fu <- y_prime <- predict(model, newdat, re.form = NA)
      return(y_prime)
    }
    if (input$Test == "LMI") {
      newdat <- data.frame(LOGIMEM.bl = input$T1Score, AGE.bl = input$BAge, 
                           EDUC = input$Edu, vnumber = 2, CAUC = factor(input$Race, levels = c("Caucasian", "NonCaucasian")), 
                           SEX = factor(input$Sex, levels = c("Male", "Female")), LOGIMEM = input$T2Score)
      model <- readRDS("LOGIMEM.Rds")
      mm <- model.matrix(terms(model), newdat)
      newdat$test.fu <- y_prime <- predict(model, newdat, re.form = NA)
      return(y_prime)
    }
    if (input$Test == "LMD") {
      newdat <- data.frame(MEMUNITS.bl = input$T1Score, AGE.bl = input$BAge, 
                           EDUC = input$Edu, vnumber = 2, CAUC = factor(input$Race, levels = c("Caucasian", "NonCaucasian")), 
                           SEX = factor(input$Sex, levels = c("Male", "Female")), MEMUNITS = input$T2Score)
      model <- readRDS("MEMUNITS.Rds")
      mm <- model.matrix(terms(model), newdat)
      newdat$test.fu <- y_prime <- predict(model, newdat, re.form = NA)
      return(y_prime)
    }
    if (input$Test == "Animals") {
      newdat <- data.frame(ANIMALS.bl = input$T1Score, AGE.bl = input$BAge, 
                           EDUC = input$Edu, vnumber = 2, CAUC = factor(input$Race, levels = c("Caucasian", "NonCaucasian")), 
                           SEX = factor(input$Sex, levels = c("Male", "Female")), ANIMALS = input$T2Score)
      model <- readRDS("ANIMALS.Rds")
      mm <- model.matrix(terms(model), newdat)
      newdat$test.fu <- y_prime <- predict(model, newdat, re.form = NA)
      return(y_prime)
    }
    if (input$Test == "Vegetables") {
      newdat <- data.frame(VEG.bl = input$T1Score, AGE.bl = input$BAge, 
                           EDUC = input$Edu, vnumber = 2, CAUC = factor(input$Race, levels = c("Caucasian", "NonCaucasian")), 
                           SEX = factor(input$Sex, levels = c("Male", "Female")), VEG = input$T2Score)
      model <- readRDS("VEG.Rds")
      mm <- model.matrix(terms(model), newdat)
      newdat$test.fu <- y_prime <- predict(model, newdat, re.form = NA)
      return(y_prime)
    }
    if (input$Test == "BNT") {
      newdat <- data.frame(BOSTON.bl = input$T1Score, AGE.bl = input$BAge, 
                           EDUC = input$Edu, vnumber = 2, CAUC = factor(input$Race, levels = c("Caucasian", "NonCaucasian")), 
                           SEX = factor(input$Sex, levels = c("Male", "Female")), BOSTON = input$T2Score)
      model <- readRDS("BOSTON.Rds")
      mm <- model.matrix(terms(model), newdat)
      newdat$test.fu <- y_prime <- predict(model, newdat, re.form = NA)
      return(y_prime)
    }
  })
  
  
  SEP <- reactive({
    if (input$Test == "MMSE") {
      newdat <- data.frame(MMSE.bl = input$T1Score, AGE.bl = input$BAge, 
                           EDUC = input$Edu, vnumber = 2, CAUC = factor(input$Race, levels = c("Caucasian", "NonCaucasian")), 
                           SEX = factor(input$Sex, levels = c("Male", "Female")), MMSE = input$T2Score)
      model <- readRDS("MMSE.Rds")
      mm <- model.matrix(terms(model), newdat)
      #newdat$test.fu <- predict(model, newdat, re.form = NA)
      pvar1 <- diag(mm %*% tcrossprod(vcov(model),mm))
      tvar1 <- pvar1+summary(model)$sigma^2
      newdat$se <- se <- sqrt(tvar1)
      return(se)
    }
    if (input$Test == "DSF") {
      newdat <- data.frame(DIGIF.bl = input$T1Score, AGE.bl = input$BAge, 
                           EDUC = input$Edu, vnumber = 2, CAUC = factor(input$Race, levels = c("Caucasian", "NonCaucasian")), 
                           SEX = factor(input$Sex, levels = c("Male", "Female")), DIGIF = input$T2Score)
      model <- readRDS("DIGIF.Rds")
      mm <- model.matrix(terms(model), newdat)
      #newdat$test.fu <- predict(model, newdat, re.form = NA)
      pvar1 <- diag(mm %*% tcrossprod(vcov(model),mm))
      tvar1 <- pvar1+summary(model)$sigma^2
      newdat$se <- se <- sqrt(tvar1)
      return(se)
    }
    if (input$Test == "DSB") {
      newdat <- data.frame(DIGIB.bl = input$T1Score, AGE.bl = input$BAge, 
                           EDUC = input$Edu, vnumber = 2, CAUC = factor(input$Race, levels = c("Caucasian", "NonCaucasian")), 
                           SEX = factor(input$Sex, levels = c("Male", "Female")), DIGIB = input$T2Score)
      model <- readRDS("DIGIB.Rds")
      mm <- model.matrix(terms(model), newdat)
      #newdat$test.fu <- predict(model, newdat, re.form = NA)
      pvar1 <- diag(mm %*% tcrossprod(vcov(model),mm))
      tvar1 <- pvar1+summary(model)$sigma^2
      newdat$se <- se <- sqrt(tvar1)
      return(se)
    }
    if (input$Test == "DSC") {
      newdat <- data.frame(WAIS.bl = input$T1Score, AGE.bl = input$BAge, 
                           EDUC = input$Edu, vnumber = 2, CAUC = factor(input$Race, levels = c("Caucasian", "NonCaucasian")), 
                           SEX = factor(input$Sex, levels = c("Male", "Female")), WAIS = input$T2Score)
      model <- readRDS("WAIS.Rds")
      mm <- model.matrix(terms(model), newdat)
      #newdat$test.fu <- predict(model, newdat, re.form = NA)
      pvar1 <- diag(mm %*% tcrossprod(vcov(model),mm))
      tvar1 <- pvar1+summary(model)$sigma^2
      newdat$se <- se <- sqrt(tvar1)
      return(se)
    }
    if (input$Test == "TMTA") {
      newdat <- data.frame(TRAILA.bl = input$T1Score, AGE.bl = input$BAge, 
                           EDUC = input$Edu, vnumber = 2, CAUC = factor(input$Race, levels = c("Caucasian", "NonCaucasian")), 
                           SEX = factor(input$Sex, levels = c("Male", "Female")), TRAILA = input$T2Score)
      model <- readRDS("TRAILA.Rds")
      mm <- model.matrix(terms(model), newdat)
      #newdat$test.fu <- predict(model, newdat, re.form = NA)
      pvar1 <- diag(mm %*% tcrossprod(vcov(model),mm))
      tvar1 <- pvar1+summary(model)$sigma^2
      newdat$se <- se <- sqrt(tvar1)
      return(se)
    }
    if (input$Test == "TMTB") {
      newdat <- data.frame(TRAILB.bl = input$T1Score, AGE.bl = input$BAge, 
                           EDUC = input$Edu, vnumber = 2, CAUC = factor(input$Race, levels = c("Caucasian", "NonCaucasian")), 
                           SEX = factor(input$Sex, levels = c("Male", "Female")), TRAILB = input$T2Score)
      model <- readRDS("TRAILB.Rds")
      mm <- model.matrix(terms(model), newdat)
      #newdat$test.fu <- predict(model, newdat, re.form = NA)
      pvar1 <- diag(mm %*% tcrossprod(vcov(model),mm))
      tvar1 <- pvar1+summary(model)$sigma^2
      newdat$se <- se <- sqrt(tvar1)
      return(se)
    }
    if (input$Test == "LMI") {
      newdat <- data.frame(LOGIMEM.bl = input$T1Score, AGE.bl = input$BAge, 
                           EDUC = input$Edu, vnumber = 2, CAUC = factor(input$Race, levels = c("Caucasian", "NonCaucasian")), 
                           SEX = factor(input$Sex, levels = c("Male", "Female")), LOGIMEM = input$T2Score)
      model <- readRDS("LOGIMEM.Rds")
      mm <- model.matrix(terms(model), newdat)
      #newdat$test.fu <- predict(model, newdat, re.form = NA)
      pvar1 <- diag(mm %*% tcrossprod(vcov(model),mm))
      tvar1 <- pvar1+summary(model)$sigma^2
      newdat$se <- se <- sqrt(tvar1)
      return(se)
    }
    if (input$Test == "LMD") {
      newdat <- data.frame(MEMUNITS.bl = input$T1Score, AGE.bl = input$BAge, 
                           EDUC = input$Edu, vnumber = 2, CAUC = factor(input$Race, levels = c("Caucasian", "NonCaucasian")), 
                           SEX = factor(input$Sex, levels = c("Male", "Female")), MEMUNITS = input$T2Score)
      model <- readRDS("MEMUNITS.Rds")
      mm <- model.matrix(terms(model), newdat)
      #newdat$test.fu <- predict(model, newdat, re.form = NA)
      pvar1 <- diag(mm %*% tcrossprod(vcov(model),mm))
      tvar1 <- pvar1+summary(model)$sigma^2
      newdat$se <- se <- sqrt(tvar1)
      return(se)
    }
    if (input$Test == "Animals") {
      newdat <- data.frame(ANIMALS.bl = input$T1Score, AGE.bl = input$BAge, 
                           EDUC = input$Edu, vnumber = 2, CAUC = factor(input$Race, levels = c("Caucasian", "NonCaucasian")), 
                           SEX = factor(input$Sex, levels = c("Male", "Female")), ANIMALS = input$T2Score)
      model <- readRDS("ANIMALS.Rds")
      mm <- model.matrix(terms(model), newdat)
      #newdat$test.fu <- predict(model, newdat, re.form = NA)
      pvar1 <- diag(mm %*% tcrossprod(vcov(model),mm))
      tvar1 <- pvar1+summary(model)$sigma^2
      newdat$se <- se <- sqrt(tvar1)
      return(se)
    }
    if (input$Test == "Vegetables") {
      newdat <- data.frame(VEG.bl = input$T1Score, AGE.bl = input$BAge, 
                           EDUC = input$Edu, vnumber = 2, CAUC = factor(input$Race, levels = c("Caucasian", "NonCaucasian")), 
                           SEX = factor(input$Sex, levels = c("Male", "Female")), VEG = input$T2Score)
      model <- readRDS("VEG.Rds")
      mm <- model.matrix(terms(model), newdat)
      #newdat$test.fu <- predict(model, newdat, re.form = NA)
      pvar1 <- diag(mm %*% tcrossprod(vcov(model),mm))
      tvar1 <- pvar1+summary(model)$sigma^2
      newdat$se <- se <- sqrt(tvar1)
      return(se)
    }
    if (input$Test == "BNT") {
      newdat <- data.frame(BOSTON.bl = input$T1Score, AGE.bl = input$BAge, 
                           EDUC = input$Edu, vnumber = 2, CAUC = factor(input$Race, levels = c("Caucasian", "NonCaucasian")), 
                           SEX = factor(input$Sex, levels = c("Male", "Female")), BOSTON = input$T2Score)
      model <- readRDS("BOSTON.Rds")
      mm <- model.matrix(terms(model), newdat)
      #newdat$test.fu <- predict(model, newdat, re.form = NA)
      pvar1 <- diag(mm %*% tcrossprod(vcov(model),mm))
      tvar1 <- pvar1+summary(model)$sigma^2
      newdat$se <- se <- sqrt(tvar1)
      return(se)
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
  
  
  output$PT2S <- renderText(paste0("Predicted Time 2 Score: ", round(Yprime(),2)))
  output$SEP <- renderText(paste0("Standard Error of the Prediction: ", round(SEP(),2)))
  output$LowerCI <- renderText({paste0("Lower Limit of ", as.numeric(input$PI), "% Prediction Interval: ", round(Yprime()-SEP()*qnorm(as.numeric(input$PI)/100),2))})
  output$UpperCI <- renderText({paste0("Upper Limit of ", as.numeric(input$PI), "% Prediction Interval: ", round(Yprime()+SEP()*qnorm(as.numeric(input$PI)/100),2))})
  output$zscore <- renderText(paste0("z-score: ", 
                                     round(((input$T2Score-Yprime())/SEP()),2)
                                     ))
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
         pch = 16, xlim = c(min(input$T2Score, Yprime(), Yprime()-SEP()*qnorm(as.numeric(input$PI)/100)), max(input$T2Score, Yprime(), Yprime()+SEP()*qnorm(as.numeric(input$PI)/100))),
         ylim=c(.5, 1.5))
    axis(1)
    points(Yprime(), 1, pch = 17, col = "red")
    arrows(Yprime(), 1, Yprime()-SEP()*qnorm(as.numeric(input$PI)/100), 1, col = "red", angle = 90)
    arrows(Yprime(), 1, Yprime()+SEP()*qnorm(as.numeric(input$PI)/100), 1, col = "red", angle = 90)
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


