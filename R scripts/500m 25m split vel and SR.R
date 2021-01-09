#1000m

library(svDialogs)
library(ggplot2)
library(dplyr)
library(stringr)
library(tcltk)
library(data.table)
library(plyr)
library(lubridate)
library(reshape2)
library(formattable)
library(matrixStats)
library(broom)
library(tibble)
library(Rmisc)
library(DT)
library(tidyverse)

graphics.off()

#Select data folder

Sys.sleep(1) #pause just a little for dailogs
data_folder <- tk_choose.dir(getwd(), caption = "Select directory")

#data_folder = "C:/Users/aaron.beach/OneDrive - nswis.com.au/R/Canoe Race Model/data"



#identify data labels

filenames = list.files(data_folder, pattern = "*.csv", full.names = T)
dataname = basename(filenames)
dataname <-  str_remove_all(dataname, ".csv")
testdf <-  strsplit(dataname, "_")
for i = length(testdf) {
if (length(testdf[[i]])==11){
  labels 
  
}

labels <-  t(data.frame(strsplit(dataname, "_")))
fullnames <-  data.frame(dataname, labels)
fullnames <-  fullnames[,c(1,2,3,4,7,8,10)]


fullnames_500 <-  fullnames[fullnames[,6] == 500,]
fullnames_1000 <-  fullnames[fullnames[,6] == 1000,]

#Read data into a list
table1 <- lapply(filenames, fread, skip = 1, header=TRUE, stringsAsFactors=FALSE, fill=TRUE)


#Convert mm:ss.0 time to secs.0

for (i in 1:length(table1)){
  for (j in 1:length(table1[[i]][["Time"]])){
    if (str_detect(replace_na(table1[[i]][["Time"]][j],''), ":") == TRUE){
      table1[[i]][["Time"]][j] <- as.numeric(ms(table1[[i]][["Time"]][j]))
    }else{
      table1[[i]][["Time"]][j] <-  as.numeric(table1[[i]][["Time"]][j])
    }
  }
}

#Convert whole Time variable to numeric

for (i in 1:length(table1)){
  table1[[i]][["Time"]] <-  as.numeric(table1[[i]][["Time"]])
}

#### Create col names ####
col_names_500 <- c(
  "ID", "First_Name",  "Last_Name",  "Competition",  "Class",  "Distance",  "Phase",
  "Time_25m", "Time_50m", "Time_75m", "Time_100m", 
  "Time_125m", "Time_150m", "Time_175m", "Time_200m", 
  "Time_225m", "Time_250m", "Time_275m", "Time_300m", 
  "Time_325m", "Time_350m", "Time_375m", "Time_400m", 
  "Time_425m", "Time_450m", "Time_475m", "Time_500m",
  "Split_25m", "Split_50m", "Split_75m", "Split_100m", 
  "Split_125m", "Split_150m", "Split_175m", "Split_200m", 
  "Split_225m", "Split_250m", "Split_275m", "Split_300m", 
  "Split_325m", "Split_350m", "Split_375m", "Split_400m", 
  "Split_425m", "Split_450m", "Split_475m", "Split_500m", "Split_Avg",
  "Vel_25m", "Vel_50m", "Vel_75m", "Vel_100m", 
  "Vel_125m", "Vel_150m", "Vel_175m", "Vel_200m", 
  "Vel_225m", "Vel_250m", "Vel_275m", "Vel_300m", 
  "Vel_325m", "Vel_350m", "Vel_375m", "Vel_400m", 
  "Vel_425m", "Vel_450m", "Vel_475m", "Vel_500m", "Vel_Avg",
  "SR_25m", "SR_50m", "SR_75m", "SR_100m", 
  "SR_125m", "SR_150m", "SR_175m", "SR_200m", 
  "SR_225m", "SR_250m", "SR_275m", "SR_300m", 
  "SR_325m", "SR_350m", "SR_375m", "SR_400m", 
  "SR_425m", "SR_450m", "SR_475m", "SR_500m", "SR_Avg")

col_names_1000 <- c(
  "ID", "First_Name",  "Last_Name",  "Competition",  "Class",  "Distance",  "Phase",
  "Time_25m", "Time_50m", "Time_75m", "Time_100m", 
  "Time_125m", "Time_150m", "Time_175m", "Time_200m", 
  "Time_225m", "Time_250m", "Time_275m", "Time_300m", 
  "Time_325m", "Time_350m", "Time_375m", "Time_400m", 
  "Time_425m", "Time_450m", "Time_475m", "Time_500m",
  "Time_525m", "Time_550m", "Time_575m", "Time_600m", 
  "Time_625m", "Time_650m", "Time_675m", "Time_700m", 
  "Time_725m", "Time_750m", "Time_775m", "Time_800m", 
  "Time_825m", "Time_850m", "Time_875m", "Time_900m", 
  "Time_925m", "Time_950m", "Time_975m", "Time_1000m",
  
  
  "Split_25m", "Split_50m", "Split_75m", "Split_100m", 
  "Split_125m", "Split_150m", "Split_175m", "Split_200m", 
  "Split_225m", "Split_250m", "Split_275m", "Split_300m", 
  "Split_325m", "Split_350m", "Split_375m", "Split_400m", 
  "Split_425m", "Split_450m", "Split_475m", "Split_500m", 
  "Split_525m", "Split_550m", "Split_575m", "Split_600m", 
  "Split_625m", "Split_650m", "Split_675m", "Split_700m", 
  "Split_725m", "Split_750m", "Split_775m", "Split_800m", 
  "Split_825m", "Split_850m", "Split_875m", "Split_900m", 
  "Split_925m", "Split_950m", "Split_975m", "Split_1000m",
  "Split_Avg",
  
  "Vel_25m", "Vel_50m", "Vel_75m", "Vel_100m", 
  "Vel_125m", "Vel_150m", "Vel_175m", "Vel_200m", 
  "Vel_225m", "Vel_250m", "Vel_275m", "Vel_300m", 
  "Vel_325m", "Vel_350m", "Vel_375m", "Vel_400m", 
  "Vel_425m", "Vel_450m", "Vel_475m", "Vel_500m", 
  "Vel_525m", "Vel_550m", "Vel_575m", "Vel_600m", 
  "Vel_625m", "Vel_650m", "Vel_675m", "Vel_700m", 
  "Vel_725m", "Vel_750m", "Vel_775m", "Vel_800m", 
  "Vel_825m", "Vel_850m", "Vel_875m", "Vel_900m", 
  "Vel_925m", "Vel_950m", "Vel_975m", "Vel_1000m",
  "Vel_Avg",
  
  "SR_25m", "SR_50m", "SR_75m", "SR_100m", 
  "SR_125m", "SR_150m", "SR_175m", "SR_200m", 
  "SR_225m", "SR_250m", "SR_275m", "SR_300m", 
  "SR_325m", "SR_350m", "SR_375m", "SR_400m", 
  "SR_425m", "SR_450m", "SR_475m", "SR_500m", 
  "SR_525m", "SR_550m", "SR_575m", "SR_600m", 
  "SR_625m", "SR_650m", "SR_675m", "SR_700m", 
  "SR_725m", "SR_750m", "SR_775m", "SR_800m", 
  "SR_825m", "SR_850m", "SR_875m", "SR_900m", 
  "SR_925m", "SR_950m", "SR_975m", "SR_1000m",
  "SR_Avg")

col_namesdist_500 <- c(
  "ID", "First_Name",  "Last_Name",  "Competition",  "Class",  "Distance",  "Phase",
  25, 50, 75, 100, 
  125, 150, 175, 200, 
  225, 250, 275, 300, 
  325, 350, 375, 400, 
  425, 450, 475, 500)

col_namesdist_1000 <- c(
  "ID", "First_Name",  "Last_Name",  "Competition",  "Class",  "Distance",  "Phase",
  25, 50, 75, 100, 
  125, 150, 175, 200, 
  225, 250, 275, 300, 
  325, 350, 375, 400, 
  425, 450, 475, 500,
  525, 550, 575, 600, 
  625, 650, 675, 700, 
  725, 750, 775, 800, 
  825, 850, 875, 900, 
  925, 950, 975, 1000)

col_namesdistavg_500 <- c(
  "ID", "First_Name",  "Last_Name",  "Competition",  "Class",  "Distance",  "Phase",
  25, 50, 75, 100, 
  125, 150, 175, 200, 
  225, 250, 275, 300, 
  325, 350, 375, 400, 
  425, 450, 475, 500, "avg")


col_namesdistavg_1000 <- c(
  "ID", "First_Name",  "Last_Name",  "Competition",  "Class",  "Distance",  "Phase",
  25, 50, 75, 100, 
  125, 150, 175, 200, 
  225, 250, 275, 300, 
  325, 350, 375, 400, 
  425, 450, 475, 500,
  525, 550, 575, 600, 
  625, 650, 675, 700, 
  725, 750, 775, 800, 
  825, 850, 875, 900, 
  925, 950, 975, 1000, "avg")





#### Transpose data to along columns of one row ####

data_transposed_500 = list()
for (i in 1:length(table1)){
  data2 = table1[[i]]
  if (data2[21,1]=="Av"){
    data_transposed_500[[i]] <-  data.frame(t(data.frame(data2[1:20,2])), t(data.frame(data2[,3])), t(data.frame(data2[,4])), t(data.frame(data2[,5])))
  }
}

data_transposed_1000 = list()
for (i in 1:length(table1)){
  data2 = table1[[i]]
  if (data2[21,1]==525){
    data_transposed_1000[[i]] <-  data.frame(t(data.frame(data2[1:40,2])), t(data.frame(data2[,3])), t(data.frame(data2[,4])), t(data.frame(data2[,5])))
  }
}

data_transposed_500 = data_transposed_500 %>% discard(is.null)
data_transposed_1000 = data_transposed_1000 %>% discard(is.null)

#combine all dataframes in list to one dataframe

data_500 <-  rbindlist(data_transposed_500, fill=TRUE)
data_1000 <-  rbindlist(data_transposed_1000, fill=TRUE)



#### combine  labels and data into one dataframe ####


Labelled_data_500 <-  data.frame(fullnames_500, data_500)
Labelled_data_1000 <-  data.frame(fullnames_1000, data_1000)

#define column and row names (data500 labels)

row_numbers_500 <-  1:nrow(fullnames_500)
row_numbers_1000 <-  1:nrow(fullnames_1000)

colnames(Labelled_data_500) <-  col_names_500
colnames(Labelled_data_1000) <-  col_names_1000


rownames(Labelled_data_500) <-  row_numbers_500
rownames(Labelled_data_1000) <-  row_numbers_1000



#### sort data500combined by final time ####
Labelled_data_500 <-  arrange(Labelled_data_500, Time_500m)
Labelled_data_1000 <-  arrange(Labelled_data_1000, Time_1000m)

#### extract just time variable ####

Labelled_data_500_time <-  Labelled_data_500[,1:27]

colnames(Labelled_data_500_time) <-  col_namesdist_500

#1000
Labelled_data_1000_time <-  Labelled_data_1000[,1:47]

colnames(Labelled_data_1000_time) <-  col_namesdist_1000


##### extract just split variable  ####
cols <-c(1:7, 28:48)
Labelled_data_500_split <-  Labelled_data_500[,cols]
colnames(Labelled_data_500_split) <-  col_namesdistavg_500
#1000
cols <-c(1:7, 48:88)
Labelled_data_1000_split <-  Labelled_data_1000[,cols]
colnames(Labelled_data_1000_split) <-  col_namesdistavg_1000


##### extract just Vel variable ####
cols <-c(1:7, 49:69)
Labelled_data_500_vel <-  Labelled_data_500[,cols]
colnames(Labelled_data_500_vel) <-  col_namesdistavg_500
#1000
cols <-c(1:7, 48:88)
Labelled_data_1000_vel <-  Labelled_data_1000[,cols]
colnames(Labelled_data_1000_vel) <-  col_namesdistavg_1000


####melt Time data grouping by ID for ggplot####


Labelled_data_500_time_melted <- melt(Labelled_data_500_time, id = c("ID","First_Name", "Last_Name","Competition","Class", "Distance","Phase"), 
#                                  measure.vars = c("25", "50", "75", "100", 
 #                                                  "125", "150", "175", "200", 
  #                                                 "225", "250", "275", "300", 
   #                                                "325", "350", "375", "400", 
    #                                               "425", "450", "475", "500")
)
Labelled_data_500_time_melted_top10 <- melt(Labelled_data_500_time[1:10,], id = c("ID","First_Name", "Last_Name","Competition","Class", "Distance","Phase"), 
                                        measure.vars = c("25", "50", "75", "100", 
                                                         "125", "150", "175", "200", 
                                                         "225", "250", "275", "300", 
                                                         "325", "350", "375", "400", 
                                                         "425", "450", "475", "500"))

#Make split column ("Variable") as numeric for plotting
Labelled_data_500_time_melted$variable <-  as.numeric(as.character(Labelled_data_500_time_melted$variable))
Labelled_data_500_time_melted_top10$variable <-  as.numeric(as.character(Labelled_data_500_time_melted_top10$variable))

#calculate mean and CI from melted data (group by split)
Labelled_data_500_time_melted_meanCI <- 
  Labelled_data_500_time_melted_top10 %>%
  group_by(variable)%>%
  dplyr::summarise(UpperLimit = CI(value, ci=0.95)[1],
                   Average = mean(value), 
                   LowerLimit = CI(value, ci=0.95)[3]) %>%
  mutate(variable = variable %>% as.factor())

#make sure the split variable is numeric
Labelled_data_500_time_melted_meanCI$variable <-  as.numeric(as.character(Labelled_data_500_time_melted_meanCI$variable))


#round data to 2 DPs
Labelled_data_500_time_melted_meanCI <-  round(Labelled_data_500_time_melted_meanCI, digits = 2)

##1000

Labelled_data_1000_time_melted <- melt(Labelled_data_1000_time, id = c("ID","First_Name", "Last_Name","Competition","Class", "Distance","Phase"), 
                                  )
Labelled_data_1000_time_melted_top10 <- melt(Labelled_data_1000_time[1:10,], id = c("ID","First_Name", "Last_Name","Competition","Class", "Distance","Phase"), 
                                        measure.vars = c("25", "50", "75", "100", "125", "150", "175", "200", "225", "250", "275", "300", 
                                                         "325", "350", "375", "400", "425", "450", "475", "500", "525", "550", "575", "600", 
                                                         "625", "650", "675", "700", "725", "750", "775", "800", "825", "850", "875", "900", 
                                                         "925", "950", "975", "1000"))

#Make split column ("Variable") as numeric for plotting
Labelled_data_1000_time_melted$variable <-  as.numeric(as.character(Labelled_data_1000_time_melted$variable))
Labelled_data_1000_time_melted_top10$variable <-  as.numeric(as.character(Labelled_data_1000_time_melted_top10$variable))

#calculate mean and CI from melted data (group by split)
Labelled_data_1000_time_melted_meanCI <- 
  Labelled_data_1000_time_melted_top10 %>%
  group_by(variable)%>%
  dplyr::summarise(UpperLimit_1000 = CI(value, ci=0.95)[1],
                   Average_1000 = mean(value), 
                   LowerLimit_1000 = CI(value, ci=0.95)[3]) %>%
  mutate(variable = variable %>% as.factor())

#make sure the split variable is numeric
Labelled_data_1000_time_melted_meanCI$variable <-  as.numeric(as.character(Labelled_data_1000_time_melted_meanCI$variable))


#round data to 2 DPs
Labelled_data_1000_time_melted_meanCI <-  round(Labelled_data_1000_time_melted_meanCI, digits = 2)


#melt Split data grouping by ID for ggplot####


Labelled_data_500_split_melted <- melt(Labelled_data_500_split, id = c("ID","First_Name", "Last_Name","Competition","Class", "Distance","Phase"))
                                  # measure.vars = c("25", "50", "75", "100",
                                  #                  "125", "150", "175", "200",
                                  #                  "225", "250", "275", "300",
                                  #                  "325", "350", "375", "400",
                                  #                  "425", "450", "475", "500", "avg"))
Labelled_data_500_split_melted_top10 <- melt(Labelled_data_500_split[1:10,], id = c("ID","First_Name", "Last_Name","Competition","Class", "Distance","Phase"))
                                        # measure.vars = c("25", "50", "75", "100",
                                        #                  "125", "150", "175", "200",
                                        #                  "225", "250", "275", "300",
                                        #                  "325", "350", "375", "400",
                                        #                  "425", "450", "475", "500", "avg"))

#Make split column ("Variable") as numeric for plotting
Labelled_data_500_split_melted$variable <-  as.numeric(as.character(Labelled_data_500_split_melted$variable))
Labelled_data_500_split_melted_top10$variable <-  as.numeric(as.character(Labelled_data_500_split_melted_top10$variable))

#calculate mean and CI from melted data (group by split)
Labelled_data_500_split_melted_meanCI <- 
  Labelled_data_500_split_melted_top10 %>%
  group_by(variable)%>%
  dplyr::summarise(SplitUpperLimit = CI(value, ci=0.95)[1],
                   SplitAverage = mean(value), 
                   SplitLowerLimit = CI(value, ci=0.95)[3]) %>%
  mutate(variable = variable %>% as.factor())

#make sure the split variable is numeric
#Labelled_data_split_melted_meanCI$variable <-  as.numeric(as.character(Labelled_data_split_melted_meanCI$variable))


#round data to 2 DPs
#Labelled_data_split_melted_meanCI <-  round(Labelled_data_split_melted_meanCI, digits = 2)




#melt Vel data grouping by ID for ggplot####


#Labelled_data_vel_melted <- melt(Labelled_data_vel, id = c("ID","First_Name", "Last_Name","Competition","Class", "Distance","Phase"), 
#                                 measure.vars = c("25", "50", "75", "100", 
#                                                  "125", "150", "175", "200", 
#                                                  "225", "250", "275", "300", 
#                                                  "325", "350", "375", "400", 
#                                                  "425", "450", "475", "500", "avg"))
#Labelled_data_vel_melted_top10 <- melt(Labelled_data_vel[1:10,], id = c("ID","First_Name", "Last_Name","Competition","Class", "Distance","Phase"), 
#                                       measure.vars = c("25", "50", "75", "100", 
#                                                        "125", "150", "175", "200", 
#                                                        "225", "250", "275", "300", 
#                                                        "325", "350", "375", "400", 
#                                                        "425", "450", "475", "500", "avg"))

#Make split column ("Variable") as numeric for plotting
#Labelled_data_vel_melted$variable <-  as.numeric(as.character(Labelled_data_vel_melted$variable))
#Labelled_data_vel_melted_top10$variable <-  as.numeric(as.character(Labelled_data_vel_melted_top10$variable))

#calculate mean and CI from melted data (group by split)
#Labelled_data_vel_melted_meanCI <- 
#  Labelled_data_vel_melted_top10 %>%
#  group_by(variable)%>%
#  dplyr::summarise(VelUpperLimit = CI(value, ci=0.95)[1],
#                   VelAverage = mean(value), 
#                   VelLowerLimit = CI(value, ci=0.95)[3]) %>%
#  mutate(variable = variable %>% as.factor())

#make sure the vel variable is numeric
#Labelled_data_vel_melted_meanCI$variable <-  as.numeric(as.character(Labelled_data_vel_melted_meanCI$variable))


#round data to 2 DPs
#Labelled_data_vel_melted_meanCI <-  round(Labelled_data_vel_melted_meanCI, digits = 2)
















#### Filtered data ####

data_500_time_y <-  Labelled_data_500_time_melted %>% filter(First_Name == "Ella") %>% filter(Last_Name == "Beere") %>% filter(Competition == "GrandPrix2") %>% filter(Phase=="F")
#data_500_split_y <-  Labelled_data_500_split_melted %>% filter(First_Name == "Tom") %>% filter(Last_Name == "Green") %>% filter(Competition == "GP22020") %>% filter(Phase=="Final")
#data_500_vel_y <-  Labelled_data_500_vel_melted %>% filter(First_Name == "Tom") %>% filter(Last_Name == "Green") %>% filter(Competition == "GP22020") %>% filter(Phase=="Final")

data_1000_time_y <-  Labelled_data_1000_time_melted %>% filter(First_Name == "Jemma") %>% filter(Last_Name == "Smith") %>% filter(Competition == "NSWStateChampionships") %>% filter(Phase=="F")
#data_1000_split_y <-  Labelled_data_1000_split_melted %>% filter(First_Name == "Tom") %>% filter(Last_Name == "Green") %>% filter(Competition == "GP22020") %>% filter(Phase=="Final")
#data_1000_vel_y <-  Labelled_data_1000_vel_melted %>% filter(First_Name == "Tom") %>% filter(Last_Name == "Green") %>% filter(Competition == "GP22020") %>% filter(Phase=="Final")





#### GGPLOT Time ####
windows()

ggplot(Labelled_data_500_time_melted_meanCI) + geom_line(aes(variable, Average), group=1) +   xlab("Split (m)") + 
  ylab("Time (sec)") + 
  scale_y_continuous(breaks = seq(0,100,len=21)) +
  geom_ribbon(aes(ymin=LowerLimit, ymax=UpperLimit, x=variable), alpha = 0.3) + geom_line(data=data_500_time_y, aes(variable,value),colour = "red")


ggplot(Labelled_data_1000_time_melted_meanCI) + geom_line(aes(variable, Average_1000), group=1) +   xlab("Split (m)") + 
  ylab("Time (sec)") + 
  #scale_y_continuous(limits = c(0,max(data_1000_time_y$value)), breaks = seq(0,max(data_1000_time_y$value), by = 2000),expand = c(0,0)) + 
  geom_ribbon(aes(ymin=LowerLimit_1000, ymax=UpperLimit_1000, x=variable), alpha = 0.3) + geom_line(data=data_1000_time_y, aes(variable,value),colour = "red")




#### GGPLOT Split ####
#windows()
#ggplot(Labelled_data_split_melted_meanCI) + geom_line(aes(variable, SplitAverage), group=1) +   xlab("Split (m)") + 
#  ylab("Time (sec)") + 
#  scale_y_continuous(breaks = seq(0,100,len=21)) +
#  geom_ribbon(aes(ymin=SplitLowerLimit, ymax=SplitUpperLimit, x=variable), alpha = 0.3) + geom_line(data=datasplity, aes(variable,value),colour = "red")




#### GGPLOT Vel ####
#windows()
#ggplot(Labelled_data_vel_melted_meanCI) + geom_line(aes(variable, VelAverage), group=1) +   xlab("Split (m)") + 
#  ylab("Time (sec)") + 
#  scale_y_continuous(breaks = seq(0,100,len=21)) +
#  geom_ribbon(aes(ymin=VelLowerLimit, ymax=VelUpperLimit, x=variable), alpha = 0.3) + geom_line(data=datavely, aes(variable,value),colour = "red")







#### Tables ####
#New xlabels
Xlabels <-  c(  "25m", "50m", "75m", "100m", 
                "125m", "150m", "175m", "200m", 
                "225m", "250m", "275m", "300m", 
                "325m", "350m", "375m", "400m", 
                "425m", "450m", "475m", "500m")


##fancy table

#custom colours and new column labels
customGreen0 = "#DeF7E9"
customGreen = "#71CA97"
customRed = "#ff7f7f"

tablecolumns <-  c("ID", "25m", "50m", "75m", "100m", 
                   "125m", "150m", "175m", "200m", 
                   "225m", "250m", "275m", "300m", 
                   "325m", "350m", "375m", "400m", 
                   "425m", "450m", "475m", "500m")

#table


Tabledata <- column_to_rownames(Labelled_data_500_time_melted_meanCI,'variable')
Tabledata <-  data.frame(t(Tabledata))
Tabledata <-  rownames_to_column(Tabledata)
colnames(Tabledata) <-  tablecolumns

Filtereddata <-  data_500_time_y %>% select(variable, value)
Filtereddata <- column_to_rownames(Filtereddata,'variable')
Filtereddata <-  data.frame(t(Filtereddata))
Filtereddata <-  rownames_to_column(Filtereddata)
colnames(Filtereddata)<-  tablecolumns
Filtereddata[1,1] <- (levels(droplevels(data_500_time_y$ID)))[1]



Tabledata2 <-  bind_rows(Filtereddata, Tabledata)

#Tabledata <- rownames_to_column(Tabledata, var="data")

datatable(Tabledata2, rownames = NULL)
formattable(Tabledata2, align = c("l", "c","c","c","c","c","c","c","c","c","c","c","c","c","c","c","c","c","c","c","c"), 
            list("ID" = formatter("span", style = ~ style(color = "red", font.weight = "bold")), '25m' = color_tile(customRed,"white")))
