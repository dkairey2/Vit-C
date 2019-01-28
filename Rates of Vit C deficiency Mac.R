## Script to plot a graph of rates of Vit C deficiency the rates as found in literature and stored in excel file
## "Rates of Vit C deficiency.xlsx"


###########  Load required Packages ###########

library(plotrix)
library(readxl)
library(tidyverse)
library(ggplot2)
library(mosaic)


## Set working directorty, debending on system.


switch( Sys.info()[['sysname']], 
        "Darwin" = {setwd("/Users/davidairey/Dropbox/Academia/Research/Vitamin C Deficieny/VitC wd") },     #  wd in Dropbox on a Mac
        "Windows" = {setwd("C:/Users/David Airey/Dropbox/Academia/Research/Vitamin C Deficieny/VitC wd")
          print('Windows')}  # wd in Dropox in Windows
) 


##########  LDR == Literature Deficiency Rate #####
### Read from Excel File : 


LDR <-as.data.frame(read_excel("Rates of Vit C deficiency  Cohorts.xlsx" ))


####### Order by rates of viamine deficiency in column Def Rate (which is column 1 #########

OrderedRates <-LDR[order(LDR[,"Def Rate"]),]


############# Main Plot #################

dev.new(width = 29.7, height=21, units ="cm")
plot(OrderedRates[,1], main = "Ordered plot of Rates of Vitamin C deficiency, Eight sudies 1998-2017" , xlab="", ylab="Pecentage Deficient")




##  place data point labels.

textbox(c(0.6,1.8),10,c(OrderedRates[1,"Lable 1"],OrderedRates[1,"Lable 2"]) )
textbox(c(1.6,2.6),18,c(OrderedRates[2,"Lable 1"],OrderedRates[2,"Lable 2"]) )
textbox(c(2.4,3.6),6,c(OrderedRates[3,"Lable 1"],OrderedRates[3,"Lable 2"]) )
textbox(c(4.1,5.5),8,c(OrderedRates[4,"Lable 1"],OrderedRates[4,"Lable 2"]) )     
textbox(c(3.9,5.5),17,c(OrderedRates[5,"Lable 1"],OrderedRates[5,"Lable 2"]) ) 
textbox(c(3.9,5.5),17,c(OrderedRates[5,"Lable 1"],OrderedRates[5,"Lable 2"]) )    
textbox(c(6.0,7.2),12.8,c(OrderedRates[6,"Lable 1"],OrderedRates[6,"Lable 2"]) )
textbox(c(6.0,7.5),21.5,c(OrderedRates[7,"Lable 1"],OrderedRates[7,"Lable 2"]) )   
textbox(c(7.7,9.0),17.6,c(OrderedRates[8,"Lable 1"],OrderedRates[8,"Lable 2"]) )   
textbox(c(9.1, 10.8),21.4,c(OrderedRates[9,"Lable 1"],OrderedRates[9,"Lable 2"]) )   

