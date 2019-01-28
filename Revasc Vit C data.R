


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


##########  VCdatOrg  == Main dataframe for Vit C #####
##########  VCdata == Cleaned version of VitC
##########  VC Temp  == Temporay data frame


### Read from Excel File

LCdeplete <-11      # Level indicating severe  vitamin C Depleteion 
LCdeficient <-26    # Level indicating some  vitamin C Deficiency
LZdeficient <-10    # LEvel indicating some Zn Deficiency


VCTemp <- VCdatOrg[FALSE,]  ## initialise VCTemp with the headers of VCdatOrg
            #   VCCat <- data.frame(vitCnum = 0, binval =character(), terval=character() )  #initialise VCCat with Headers
#VCnum[1] = 0
VCnum <-c(0)



#VCbinval <-c("normal")
#VCterval <-c("normal")
#VCDeplete <-c(FALSE)    # column  indicating  Severe Vit C depleteion
#VCDeficient <-c(FALSE)  # Colum indicating Vit C deficiency
#ZnDeficiency <-c(FALSE)  #colum indicating  presence of Zn Deficiency


######################             Major loop ###################################################  
##  step thgrought each line, eliminate blank lines, translate vit C value to numeric , accumulat i vector VCnum
##  
## As of 4/12/2018, we assue that all lab estimates are equivalent, even though the normal ranges are different.  


## Reset VCTemp
VCTemp <- VCdatOrg[FALSE,]
VCnum <-c(0)
            
## Reset J, counter 
j <-1L; 

## nrow(VCdatOrg)
for (i in 1:nrow(VCdatOrg) )
  {if (!is.na(VCdatOrg[i,"vit c value"])) {     
      
        VCTemp <- rbind(VCTemp, VCdatOrg[i,])  # Accululate relevant  entries into VCTemp
        VCTemp[j,"vit c value"] <- gsub("<","",VCTemp[j,"vit c value"])  #delete the "<" sign, replac "<5" with "5"
        VCnum[j] <- as.numeric(VCTemp[j,"vit c value"])
      j<-j+1
        
      }
}

###############################  End Major Loop #####################################################

# Bind definitite dataframe

VCdata<-cbind(VCTemp,VCnum)

############################# Sub anatlysis dataframes ############################################


####################### Subgroup of diabetes ######################################################

VCDiab <- filter(VCdata, Diabetes == TRUE)
VCNonDiab <-filter(VCdata, Diabetes == FALSE)
NDiab <- length(VCDiab[,"VCnum"])             # number of diabetics
NNonDiab <-  length(VCNonDiab[,"VCnum"])      # number of non-diabetics
TotDiab = NDiab + NNonDiab

NDepDiab <- sum(VCDiab[,"VCnum"]<12)          # number of diabetics with severe depletion
NDepNonDiab <- sum(VCNonDiab [,"VCnum"]<12)   # number of non-diabetics with severe depletion

NNormDiab <- sum(VCDiab[,"VCnum"]>=12)          # number of diabetics with severe depletion
NNormNonDiab <- sum(VCNonDiab [,"VCnum"] >= 12)   # number of non-diabetics with severe depletion



######### Contingency Tales for   DM vs non DM   #################################
ConTabDiab  <-matrix(c(NDepNonDiab, NDepDiab,NNormNonDiab, NNormDiab), ncol = 2)  # 2x2 Contingency Table
rownames(ConTabDiab) <-c("Non-Diabetic ", "Diabetic")
colnames(ConTabDiab) <-c("Deplete", " Non-Deplete")



####################### Subgroup of smoking  ######################################################

VCSmoke <- filter(VCdata, Smoking == TRUE)
VCNonSmoke <-filter(VCdata, Smoking  == FALSE)
NSmoke  <- length(VCSmoke[,"VCnum"])             # number of current Smokers 
NNonSmoke <-  length(VCNonSmoke[,"VCnum"])      # number of non Smokers

TotSmoke <-NSmoke + NNonSmoke                 # Number of people wiht smoking Hx to analyse

NDepSmoke <- sum(VCSmoke [,"VCnum"]<12)         # number of Smokers  with  severe Vit C depletion
NNormSmoke <- sum(VCSmoke [,"VCnum"]>=12)       # number of smokers   without severe depletion 

NDepNonSmoke <-sum(VCNonSmoke[,"VCnum"]<12)     # number of non-smokers  with severe Vit C depletion
NNormNonSmoke <-sum(VCNonSmoke [,"VCnum"]>=12)  # Number of smokers without Severe Vit C depleteion

ConTabSmoke  <-matrix(c(NDepNonSmoke, NDepSmoke,NNormNonSmoke, NNormSmoke), ncol = 2)
rownames(ConTabSmoke) <-c("Non-Smoker", "Active Smoker")
colnames(ConTabSmoke) <-c("Deplete", " Non-Deplete")


hist(VCNonSmoke[,"VCnum"], breaks = hbreak, freq=FALSE, add = TRUE, col="light green")
hist(VCSmoke[,"VCnum"], breaks = hbreak, freq = FALSE)






# histograms  




#plot histogram of Serum Vit C
hbreak <- seq(0,250, by=5)  ####   set histogram bins at 5 



hist(VCdata[,"VCnum"],breaks=hbreak, freq = TRUE, main = "Distribution of Serum Vitamin C in Chronic wound patients ", 
     xlab = expression(paste("Vit C level (",mu,"mol/l)")) , col='blue'  )

VCsort <-sort(VCnum) 
plot (VCsort, main  =" Ordered plot of Serum Vitamin C levels in chronic wound patients",
                           xlab="", ylab =expression(paste("Vit C level (",mu,"mol/l)")))
abline(h=6)




# plot histogram of age of patients

AgeBreak <-seq(30,100, by=5)
hist(VCTemp[,"Age"], breaks = AgeBreak, main= "Age of Patient", xlab="Age (years)", col='dark green')


# plote histogram of Albumin  ... Watch spelling
Albbreak <- seq(25,55)
hist(VCTemp[,'Albumin'], breaks=Albbreak, col = 'red',freq = TRUE, main = "Distribution of Albumin in Chronic wound patients ", xlab = expression(paste("Albumin (",mu,"mol/l)")))

#Plot Histogram of Zn Values.
ZnBreak <-seq(0:42)
hist(VCTemp[,'Zn Value'], breaks = ZnBreak, col='Red',  main = "Distribution of Zinc levels in Chronic wound patients ",xlab = expression(paste("Serum Zinc levels (",mu,"mol/l)"))) 

################### Comparison of Different pathology providers ########################################

###  DHM laboratory##################
VCdhm <-filter (VCdata, Lab =="DHM")
Ndhm <- length(VCdhm[,"Lab"])     # Count of numbers
hist(VCdhm[,"VCnum"],breaks=hbreak, freq = TRUE, main = "Distribution of Serum Vitamin C , DHM pathology", 
     xlab = expression(paste("Vit C level (",mu,"mol/l)")) , col='blue'  )

### Laverty Pathology#################

VClav <-filter (VCdata, Lab =="LAV")
Nlav <- length(VCdhm[,"Lab"])     # Count of numbers
hist(VClav[,"VCnum"],breaks=hbreak, freq = TRUE, main = "Distribution of Serum Vitamin C, Laverty pathology ", 
     xlab = expression(paste("Vit C level (",mu,"mol/l)")) , col='red'  )

############  Pathology Providers, Combined Histogram of densities ############


VClavDist<-hist(VClav[,"VCnum"],breaks=hbreak, freq = FALSE, main = "Distribution of Serum Vitamin C, Laverty pathology ", 
     xlab = expression(paste("Vit C level (",mu,"mol/l)")) , col=rgb(60,144,144,128, maxColorValue =255)  )

VCdhmDist <-hist(VCdhm[,"VCnum"],breaks=hbreak, freq = FALSE, 
     xlab = expression(paste("Vit C level (",mu,"mol/l)")) , col=rgb(255,10,10,128, maxColorValue =255) ,add =TRUE )



############# Pathology providers, 2 x 2 contingency table for severe deficiency#############
############# requires mosaaic Package  #####################################################
######## odd ration calcuated with orrr(), chi.s

NDepDHM <- sum(VCdhm[,"VCnum"]<12)          # number of diabetics with severe depletion
NDepLAV <- sum(VClav [,"VCnum"]<12)   # number of non-diabetics with severe depletion
NNormDHM <- sum(VCdhm[,"VCnum"]>=12)          # number of diabetics with severe depletion
NNormLAV <- sum(VClav [,"VCnum"] >= 12)   # number of non-diabetics with severe depletion


ConTabLabs  <-matrix(c(NDepDHM, NDepLAV,NNormDHM, NNormLAV), ncol = 2)
rownames(ConTabLabs) <-c("DHM", "Laverty")
colnames(ConTabLabs) <-c("Deplete", " Non-Deplete")

###################################################################################
################### Comparison of Gender   ########################################
###################################################################################


# Create subgroup of males, Histogram of Vit C levels ##################
VCMale <-filter (VCdata, Gender =="M")
NMale <- length(VCMale[,"Lab"])     # Count of numbers
hist(VCMale[,"VCnum"],breaks=hbreak, freq = TRUE, main = "Distribution of Serum Vitamin C , Males" , 
     xlab = expression(paste("Vit C level (",mu,"mol/l)")) , col='blue'  )

# Create subgroup of Females, histogram of Vit C levels #################

VCFemale <-filter (VCdata, Gender =="F")
NFemale <- length(VCFemale[,"Gender"])     # Count of numbers
hist(VCFemale[,"VCnum"],breaks=hbreak, freq = TRUE, main = "Distribution of Serum Vitamin C, Females ", 
     xlab = expression(paste("Vit C level (",mu,"mol/l)")) , col='red'  )

# Combined Histogram of densities ############
VCMaleDist<-hist(VCMale[,"VCnum"],breaks=hbreak, freq = FALSE, main = "Distribution of Serum Vitamin C by  Gender", 
                xlab = expression(paste("Vit C level (",mu,"mol/l)")) , col=rgb(60,144,144,128, maxColorValue =255)  )

VCFemaleDist <-hist(VCFemale[,"VCnum"],breaks=hbreak, freq = FALSE, 
                 xlab = expression(paste("Vit C level (",mu,"mol/l)")) , col=rgb(255,10,10,128, maxColorValue =255) ,add =TRUE )


##   2 x 2 contingency table for severe deficiency vs Gender
##   orrr(), and chisq.test requires mosaaic Package

NDepMALE <- sum(VCMale[,"VCnum"]<12)          # number of diabetics with severe depletion
NDepFEMALE <- sum(VCFemale [,"VCnum"]<12)   # number of non-diabetics with severe depletion
NNormMALE <- sum(VCMale[,"VCnum"]>=12)          # number of diabetics with severe depletion
NNormFEMALE <- sum(VCFemale [,"VCnum"] >= 12)   # number of non-diabetics with severe depletion


ConTabGender  <-matrix(c(NDepMALE, NDepFEMALE,NNormMALE, NNormFEMALE), ncol = 2)
rownames(ConTabGender) <-c("Male", "Female")
colnames(ConTabGender) <-c("Deplete", " Non-Deplete")


#################################################################################################
###################  Heamoglobim,  Anaemia and Vitamin C ########################################
#################################################################################################
HbMean<- mean(filter(VCdata,Hb > 0) [,"Hb"])  ## mean of all valid Hb
HbSD<- sd(filter(VCdata,Hb > 0) [,"Hb"])  ## standard deviation of all valid Hb

HbMeanMale<- mean(filter(VCMale,Hb > 0) [,"Hb"])   ## mean of all valid  Hb for Males
HbSDMale<- sd(filter(VCMale,Hb > 0) [,"Hb"])   ## standard deviation  of all valid  Hb for Males

HbMeanFemale<- mean(filter(VCFemale,Hb > 0) [,"Hb"])   ## mean of all valid Hb for Females
HbSDFemale<- sd(filter(VCFemale,Hb > 0) [,"Hb"])   ## sd of all valid Hb for Females

HbBreak <-seq(65, 175, by=10)  # bins for histograms
hist(VCdata[,"Hb"], breaks = HbBreak, col="dark red", 
     main = "Distribution of Heamoglobin, Full Cohort", xlab = "Hb  (g/dL)")  ### Full Cohort Histogram


VCDeplete <-filter(VCdata, VCnum < 12)
hist(VCDeplete[,"Hb"], breaks = HbBreak)
VCNonDeplete <-filter(VCdata, VCnum >11)
hist(VCNonDeplete[,"Hb"], breaks = HbBreak)

####################### Scatterplot of Vit C vs Hb       ###################
plot( VCdata[,"Hb"], VCdata[,"VCnum"], main = "Vitamin C vs Heamaglobin", 
      xlab = "Heamaglobin  (g/dL)" , ylab = expression(paste("Vit C level (",mu,"mol/l)")), pch=19, col="dark red")

############# Correlation Coeficient  Vit C vs Hb #######################################

cor(HbX,VcY,use="pairwise.complete.obs")
cor(VCdata[,"Hb"],VCdata[,"VCnum"],use="pairwise.complete.obs")
