####Script created by Amanda Cardoso May 2019. 
### 1. Proportional Unweighted DDM measures: counts the number of times a variable occurs in the dataframe
###     and counts the number of times a variant we are looking at occurs. Then calculates
###     proportions for the question

### 2. Question Unweighted Proportional DDM Measure: count the number of times any variable occurs in the 
###     dataframe and count the number of times a variant occurs. Then calculate each questions 
###     overall proportions. 

### 3. Question Weighted Proportional DDM Measure: count the weighted of times any variable occurs in the 
###     dataframe and count the weight of each variable. Then calculate each questions 
###     overall proportions. 


### 4. Cluster Unweighted DDM measures: calculates the points for a 3 second range with one second overlap


##required packages: 
# NOTE install packages are commented out, but if you haven't used the package before 
# make sure to uncomment

#install.packages("ggplot2")
#install.packages("tidyverse")

library(ggplot2)
library(dplyr)
library(magrittr)

#import spreadsheet - change spreadsheet name as required or if using the code at the end 
#                     which converts a textgrid, then this step is not required.

DDM_Prop <- read.csv('~/Dropbox/ABFAB Project Documents/Data & Analysis/DDM/DDMAnaly_140619_ACDS_final.csv')

#coercing spreadsheet into data frame
DDM_Prop1 <- as.data.frame(DDM_Prop)

#turning character columns into factors
colnames(DDM_Prop1)[12] <- "Feature_Weight"

DDM_Prop1$Speaker <- as.factor(DDM_Prop1$Speaker)
DDM_Prop1$Question <- as.factor(DDM_Prop1$Question)
DDM_Prop1$Variable[DDM_Prop$Variable==""] <- NA
DDM_Prop1$Variable <- as.factor(DDM_Prop1$Variable)
DDM_Prop1$Feature[DDM_Prop$Feature==""] <- NA
DDM_Prop1$Feature <- as.factor(DDM_Prop1$Feature)
DDM_Prop1$Variable_Weight[DDM_Prop$Variable_Weight==""] <- NA
DDM_Prop1$Variable_Weight[is.na(DDM_Prop1$Variable_Weight)] <- 0
DDM_Prop1$Variable_Weight <- as.numeric(DDM_Prop1$Variable_Weight)
DDM_Prop1$Feature_Weight[DDM_Prop$Feature_Weight==""] <- NA
DDM_Prop1$Feature_Weight[is.na(DDM_Prop1$Feature_Weight)] <- 0
DDM_Prop1$Feature_Weight <- as.numeric(DDM_Prop1$Feature_Weight)


#1.create spreadsheet and calculate proportions for Unweighted proportional DDM measures by individual feature
DDM_Feat_PropsUnweight <- DDM_Prop1 %>%
  select(Accent, Speaker, Question, Variable, Feature) %>% #select columns to work with
  filter(!is.na(Variable)) %>% #filter out NAs
  group_by(Accent, Speaker, Question, Variable) %>% #set grouping factors 
  mutate(Variable_n = n()) %>% #add in variable counts
  ungroup() %>% #ungroup to update speadsheet with values
  group_by(Accent, Speaker, Question, Variable, Variable_n) %>% #regroup for feature counts
  dplyr::count(Feature) %>% #add in feature counts 
  ungroup() %>% #ungroup to udpate spreadsheet
  mutate(Feature_prop = n / Variable_n) #calculate proportions

DDM_Feat_PropsUnweight1 <- subset(DDM_Feat_PropsUnweight, Feature!="")

#summary graph
ggplot(subset(DDM_Feat_PropsUnweight1, Question=="Q6P" & Variable %in% c("DH", "FACE", "GOAT", "L", "K")), aes(Feature, Feature_prop, col=Speaker)) +
  geom_point() +
  facet_wrap(~Accent)

ggplot(subset(DDM_Feat_Props1, Question=="Q13P" & Variable %in% c("DH", "FACE", "GOAT", "L", "K")), aes(Feature, Feature_prop, col=Speaker)) +
  geom_point() +
  facet_wrap(~Accent)

  
#2.create spreadsheet and calculate proportions for overall proportional unweighted DDM
DDM_Ques_PropsUnWeight <- DDM_Prop1 %>%
  select(Accent, Speaker, Question, Variable, Feature) %>% #select columns to work with
  filter(!is.na(Variable)) %>% #filter out NAs
  group_by(Accent, Speaker, Question) %>% #set grouping factors 
  mutate(Variable_n = n()) %>% #add in variable counts
  ungroup() %>% #ungroup to update speadsheet with values
  filter(!is.na(Feature)) %>% #filter out NAs
  group_by(Accent, Speaker, Question, Variable_n) %>% #regroup for feature counts
  mutate(Feature_n= n()) %>% #add in feature counts 
  ungroup() %>% #ungroup to udpate spreadsheet
  mutate(Question_prop = Feature_n / Variable_n) %>% #calculate proportions
  select(Accent, Speaker, Question, Feature_n, Variable_n, Question_prop) %>% #remove extra columns
  distinct()

write.csv(DDM_Ques_PropsUnWeight, "~/Documents/ABFAB/AudioRecordings/DDM_Ques_PropsUnWeight.csv")

DDM_Ques_PropsUnWeight1 <- read.csv("~/Documents/ABFAB/AudioRecordings/DDM_Ques_PropsUnWeight.csv")

#2. graphing the questions
  ggplot(DDM_Ques_Props, aes(x=interaction(Accent, Speaker), Question_prop, fill=Accent)) +
    geom_col() +
    facet_wrap(~Question)

#3.create spreadsheet and calculate proportions for overall proportional weighted DDM
  DDM_Ques_PropsWeight <- DDM_Prop1 %>%
    select(Accent, Speaker, Question, Variable_Weight, Feature_Weight) %>% #select columns to work with
    group_by(Accent, Speaker, Question) %>% #set grouping factors 
    summarise(Variable_Weight_n = sum(Variable_Weight)) %>%
    ungroup()  #ungroup to update speadsheet with values
    
  DDM_Ques_PropsWeight1 <- DDM_Prop1 %>%
    select(Accent, Speaker, Question, Variable_Weight, Feature_Weight) %>% #select columns to work with
    group_by(Accent, Speaker, Question) %>% #set grouping factors 
    summarise(Feature_Weight_n = sum(Feature_Weight)) %>%
    ungroup() #ungroup to update speadsheet with values
  
  DDM_Ques_PropsWeight2 <- cbind(DDM_Ques_PropsWeight, DDM_Ques_PropsWeight1[4])
      
  DDM_Ques_PropsWeight3 <- DDM_Ques_PropsWeight2 %>%
    select(Accent, Speaker, Question, Variable_Weight_n, Feature_Weight_n) %>% #select columns to work with
    group_by(Accent, Speaker, Question) %>% #set grouping factors 
    mutate(Question_prop = Feature_Weight_n / Variable_Weight_n) %>% #calculate proportions
    select(Accent, Speaker, Question, Feature_Weight_n, Variable_Weight_n, Question_prop) %>% #remove extra columns
    distinct()
  
   
## creating combined spreadsheet
  colnames(DDM_Ques_PropsUnWeight1)[5] <- "Raw_Unweight_Feature_Count"
  colnames(DDM_Ques_PropsUnWeight1)[6] <- "Raw_Unweight_Variable_Count"
  colnames(DDM_Ques_PropsUnWeight1)[7] <- "Unweight_Question_Prop"
  
  colnames(DDM_Ques_PropsWeight3)[4] <- "Raw_Weight_Feature_Count"
  colnames(DDM_Ques_PropsWeight3)[5] <- "Raw_Weight_Variable_Count"
  colnames(DDM_Ques_PropsWeight3)[6] <- "Weighted_Question_Prop"
  
## merge the two spreadsheets
  dropDDM <- "X"
  DDM_Ques_PropsUnWeight1 <- DDM_Ques_PropsUnWeight1[ , !(names(DDM_Ques_PropsUnWeight1) %in% dropDDM)]  

  DDM_Ques_Props <- merge(DDM_Ques_PropsUnWeight1, DDM_Ques_PropsWeight3, by=c("Accent", "Speaker", "Question")) 

  
 write.csv(DDM_Ques_PropsWeight3, "~/Documents/ABFAB/AudioRecordings/DDMs_Weighted.csv")
 write.csv(DDM_Ques_PropsUnWeight1, "~/Documents/ABFAB/AudioRecordings/DDMs_UnWeighted.csv")
 write.csv(DDM_Ques_Props, "~/Documents/ABFAB/AudioRecordings/DDMprop_170619.csv")
 
      
#4.Cluster Unweighted and weighted DDM
  
#calculate the DDM every 3 seconds with overlap of -1 second  
#create a new dataframe
 DDM_Glob_Sum_UnWeight <- data.frame(Speaker="", Question="", Start=0, End=0, Score=0, stringsAsFactors=F)[0]

 DDM_Glob_Sum_Weight<- data.frame(Speaker="", Question="", Start=0, End=0, Score=0, stringsAsFactors=F)[0]


#create a combined variable with speaker and question included
DDM_Prop1$SpeakerQ <- paste(DDM_Prop1$Speaker, DDM_Prop1$Question)

#for loop to get unweighted measures
for (speq in unique(DDM_Prop1$SpeakerQ)) {
  speq_subset <- filter(DDM_Prop1, SpeakerQ==speq)
  timess <- seq(min(speq_subset$BeginTime), max(speq_subset$End.Time_Phone)-3, 1) 
  for (t in timess) {
    chunk <- filter(speq_subset, BeginTime >= t, End.Time_Phone < (t + 3)) #create time chunks
    DDM_Glob_Sum_UnWeight <- rbind(DDM_Glob_Sum_UnWeight, data.frame(Speaker=as.character(speq_subset$Speaker[1]), #get number of points within time chunk & paste into speadsheet
                                          Question=as.character(speq_subset$Question[1]),
                                          Start=t,
                                          End=t+3,
                                          Score=sum(!is.na(chunk$Feature))))
  }
}

View(DDM_Glob_Sum_UnWeight)  


#for loop to get weighted measures
for (speq in unique(DDM_Prop1$SpeakerQ)) {
  speq_subset <- filter(DDM_Prop1, SpeakerQ==speq)
  timess <- seq(min(speq_subset$BeginTime), max(speq_subset$End.Time_Phone)-3, 1) 
  for (t in timess) {
    chunk <- filter(speq_subset, BeginTime >= t, End.Time_Phone < (t + 3)) #create time chunks
    DDM_Glob_Sum_Weight <- rbind(DDM_Glob_Sum_Weight, data.frame(Speaker=as.character(speq_subset$Speaker[1]), #get number of points within time chunk & paste into speadsheet
                                 Question=as.character(speq_subset$Question[1]),
                                 Start=t,
                                 End=t+3,
                                 Score=sum(chunk$Feature_Weight, na.rm=T)))
  }
}

View(DDM_Glob_Sum_Weight)  

DDM_Glob_Sum_Weight$Accent <- ""
DDM_Glob_Sum_Weight$Accent[DDM_Glob_Sum_Weight$Speaker %in% c("AE", "PE")] <- "RP"
DDM_Glob_Sum_Weight$Accent[DDM_Glob_Sum_Weight$Speaker %in% c("AL", "EA")] <- "MLE"
DDM_Glob_Sum_Weight$Accent[DDM_Glob_Sum_Weight$Speaker %in% c("GM", "RC")] <- "UWYE"
DDM_Glob_Sum_Weight$Accent[DDM_Glob_Sum_Weight$Speaker %in% c("JS", "CL")] <- "GNE"
DDM_Glob_Sum_Weight$Accent[DDM_Glob_Sum_Weight$Speaker %in% c("DD", "LB")] <- "EE"
DDM_Glob_Sum_Weight$Accent <- as.factor(DDM_Glob_Sum_Weight$Accent)
DDM_Glob_Sum_Weight <- DDM_Glob_Sum_Weight[,c(6,1,2,3,4,5)]

colnames(DDM_Glob_Sum_Weight)[6] <- "Score_Weight"

DDM_Glob_Sum_UnWeight$Accent <- ""
DDM_Glob_Sum_UnWeight$Accent[DDM_Glob_Sum_UnWeight$Speaker %in% c("AE", "PE")] <- "RP"
DDM_Glob_Sum_UnWeight$Accent[DDM_Glob_Sum_UnWeight$Speaker %in% c("AL", "EA")] <- "MLE"
DDM_Glob_Sum_UnWeight$Accent[DDM_Glob_Sum_UnWeight$Speaker %in% c("GM", "RC")] <- "UWYE"
DDM_Glob_Sum_UnWeight$Accent[DDM_Glob_Sum_UnWeight$Speaker %in% c("JS", "CL")] <- "GNE"
DDM_Glob_Sum_UnWeight$Accent[DDM_Glob_Sum_UnWeight$Speaker %in% c("DD", "LB")] <- "EE"
DDM_Glob_Sum_UnWeight$Accent <- as.factor(DDM_Glob_Sum_UnWeight$Accent)
DDM_Glob_Sum_UnWeight <- DDM_Glob_Sum_UnWeight[,c(6,1,2,3,4,5)]

colnames(DDM_Glob_Sum_UnWeight)[6] <- "Score_UnWeight"

DDM_Glob_Sum <- merge(DDM_Glob_Sum_UnWeight, DDM_Glob_Sum_Weight, by=c("Accent", "Speaker", "Question", "Start", "End")) 

write.csv(DDM_Glob_Sum_Weight, "~/Documents/ABFAB/AudioRecordings/DDMcluster_Weight_190619.csv")
write.csv(DDM_Glob_Sum_UnWeight, "~/Documents/ABFAB/AudioRecordings/DDMcluster_UnWeight_190619.csv")
write.csv(DDM_Glob_Sum, "~/Documents/ABFAB/AudioRecordings/DDMcluster_190619.csv")


### Calculating distances between speakers DDMs
###create averages for distance calculatons
DDM_final_ave <- aggregate( ~ speaker, data=f0_A.LFinal, FUN=mean)
View(f0_ave)

#f0_ave$speaker = paste('div', f0_ave$speaker, sep='')


### calculate Euclidean distance between speakers:
## VPA: using all the descriptions     
rmDDm <- c("Accent")
rm2 <- c("Speaker", "Lax larynx", "Murmur", "Tense larynx", "Whispery")
VPA_Phon.sp <- VPA_Phon[ , !(names(VPA_Phon) %in% rm1)]
VPA_dist <- dist(VPA_Phon.sp)
plot(VPA_dist)

VPA_Phon.sp1 <- VPA_Phon[ , !(names(VPA_Phon) %in% rm2)]
VPA_dist2 <- dist(VPA_Phon.sp1)



##### Unused code
#3.add in column with points
MLE_2 <- c("mono FACE",	"near mono FACE","backed GOAT", "monophthongal GOAT",	"near mono GOAT",
           "mono PRICE", "backed FOOT", "DH-stopping", "K-backing", "dark-L", "strong-H")
MLE_1 <- c("GOOSE fronting", "FOOT fronting", "TRAP", "DH-fronting","DH-gliding", "TH-fronting",
           "T-glottaling", "L-vocalisation", "IN for ING")
MLE_n1 <- c("final T")

EE_2 <- c("shifted FACE", "shifted PRICE", "dark-L", "H-dropping")
EE_1 <- c("GOOSE fronting", "FOOT fronting", "TRAP", "DH-fronting", "TH-fronting",
          "T-glottaling", "L-vocalisation", "IN for ING", "mono PRICE")
EE_n1 <- c("final T")

RP_2 <- c("merge FOOT/STRUT")
RP_1 <- c("GOOSE fronting", "FOOT fronting", "TRAP", "final T-glottaling", "L-vocalisation", 
          "IN for ING", "mono PRICE", "near L-vocalisation", "intrustive-R")
RP_n1 <- c("final T", "lax happY", "posh NEAR")

UWYE_2 <- c("mono FACE", "monophthongal GOAT", "not STRUT", "lax happY", "dark-L", "H-dropping")
UWYE_1 <- c("GOOSE fronting", "diphthongal GOOSE", "merge FOOT/STRUT", "merge BATH/TRAP","DH-fronting",
            "TH-fronting", "T-glottaling", "final T-glottaling", "L-vocalisation", "IN for ING")
UWYE_n1 <- c("final T")

GNE_2 <- c("mono FACE", "monophthongal GOAT", "lax happY")
GNE_1 <- c("near mono FACE", "near mono GOAT", "mono PRICE", "GOOSE fronting", "FOOT fronting", 
           "merge FOOT/STRUT", "merge BATH/TRAP","DH-fronting", "mid BATH/TRAP", "TH-fronting", 
           "T-glottaling", "final T-glottaling", "L-vocalisation", "near L-vocalisation", 
           "IN for ING", "dark-L", "intrustive-R")
GNE_n1 <- c("final T", "shifted GOAT", "split FOOT/STRUT")


DDM_Prop1$Feature_Weight <- NA
DDM_Prop1$Feature_Weight[(DDM_Prop1$Accent=="EE") & (DDM_Prop1$Feature %in% c(EE_2))] <- 2
DDM_Prop1$Feature_Weight[(DDM_Prop1$Accent=="EE") & (DDM_Prop1$Feature %in% c(EE_1))] <- 1
DDM_Prop1$Feature_Weight[(DDM_Prop1$Accent=="EE") & (DDM_Prop1$Feature %in% c(EE_n1))] <- -1
DDM_Prop1$Feature_Weight[(DDM_Prop1$Accent=="MLE") & (DDM_Prop1$Feature %in% c(MLE_2))] <- 2
DDM_Prop1$Feature_Weight[(DDM_Prop1$Accent=="MLE") & (DDM_Prop1$Feature %in% c(MLE_1))] <- 1
DDM_Prop1$Feature_Weight[(DDM_Prop1$Accent=="MLE") & (DDM_Prop1$Feature %in% c(MLE_n1))] <- -1
DDM_Prop1$Feature_Weight[(DDM_Prop1$Accent=="RP") & (DDM_Prop1$Feature %in% c(RP_2))] <- 2
DDM_Prop1$Feature_Weight[(DDM_Prop1$Accent=="RP") & (DDM_Prop1$Feature %in% c(RP_1))] <- 1
DDM_Prop1$Feature_Weight[(DDM_Prop1$Accent=="RP") & (DDM_Prop1$Feature %in% c(RP_n1))] <- -1
DDM_Prop1$Feature_Weight[(DDM_Prop1$Accent=="GNE") & (DDM_Prop1$Feature %in% c(GNE_2))] <- 2
DDM_Prop1$Feature_Weight[(DDM_Prop1$Accent=="GNE") & (DDM_Prop1$Feature %in% c(GNE_1))] <- 1
DDM_Prop1$Feature_Weight[(DDM_Prop1$Accent=="GNE") & (DDM_Prop1$Feature %in% c(GNE_n1))] <- -1
DDM_Prop1$Feature_Weight[(DDM_Prop1$Accent=="UWYE") & (DDM_Prop1$Feature %in% c(UWYE_2))] <- 2
DDM_Prop1$Feature_Weight[(DDM_Prop1$Accent=="UWYE") & (DDM_Prop1$Feature %in% c(UWYE_1))] <- 1
DDM_Prop1$Feature_Weight[(DDM_Prop1$Accent=="UWYE") & (DDM_Prop1$Feature %in% c(UWYE_n1))] <- -1


######future coding work
#install.packages('textgRid')
library(textgRid) # allows access to information in textgrids


##import textGrids 

textgrid_DDEE1 <- TextGrid('~/Dropbox/ABFAB Project Documents/Audio/DDM-prep/Annotated/EE/DD_EE_Q6P_annot.TextGrid')
textgrid_DDEE2 <- TextGrid('~/Dropbox/ABFAB Project Documents/Audio/DDM-prep/Annotated/EE/DD_EE_Q13P_annot.TextGrid')
textgrid_LBEE1 <- TextGrid('~/Dropbox/ABFAB Project Documents/Audio/DDM-prep/Annotated/EE/LB_EE_Q6P_annot.TextGrid')
textgrid_LBEE2 <- TextGrid('~/Dropbox/ABFAB Project Documents/Audio/DDM-prep/Annotated/EE/LB_EE_Q13P_annot.TextGrid')

##convert textGrid to data.frame
mat_TG_DDEE1 <- as.matrix(textgrid_DDEE1)
TG_DDEE1 <- as.data.frame(mat_TG_DDEE1)

findIntervals(textgrid_DDEE1$'Orthographic-Words')
