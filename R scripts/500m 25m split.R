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
labels <-  t(data.frame(strsplit(dataname, "_")))

#Read data into a list
table1 <- lapply(filenames, fread, skip = 1, header=TRUE, stringsAsFactors=FALSE)


#Convert mm:ss.0 time to secs.0

for (i in 1:length(table1)){
  for (j in 1:length(table1[[i]][["Time"]])){
    if (str_detect(table1[[i]][["Time"]][j], ":") == TRUE){
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

col_names <- c(
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


col_namesdist <- c(
  "ID", "First_Name",  "Last_Name",  "Competition",  "Class",  "Distance",  "Phase",
  25, 50, 75, 100, 
  125, 150, 175, 200, 
  225, 250, 275, 300, 
  325, 350, 375, 400, 
  425, 450, 475, 500)

col_namesdistavg <- c(
  "ID",
  25, 50, 75, 100, 
  125, 150, 175, 200, 
  225, 250, 275, 300, 
  325, 350, 375, 400, 
  425, 450, 475, 500, "avg")


#Transpose data to along columns of one row

data_transposed = list()
for (i in 1:length(table1)){
  data2 = table1[[i]]
  data_transposed[[i]] <-  data.frame(t(data.frame(data2[1:20,2])), t(data.frame(data2[,3])), t(data.frame(data2[,4])), t(data.frame(data2[,5])))
}

#combine all dataframes in list to one dataframe

all <-  rbindlist(data_transposed, fill=TRUE)

#combine all labels into one dataframe


#each label as a different variable
metadt <- data.frame(rbind(labels))
#combined label with _
metadt2 <-  data.frame(t(data.frame(rbind(dataname))))

#combine labels with data into a dataframe
Labelled_data <-  data.frame(metadt2, metadt, all)


#define column and row names (all labels)

row_numbers <-  1:nrow(metadt)

colnames(Labelled_data) <-  col_names
rownames(Labelled_data) <-  row_numbers



####################

#sort allcombined by final time
Labelled_data <-  arrange(Labelled_data, Time_500m)

#extract just time variable and arrange by final time

Labelled_data_time <-  Labelled_data[,1:27]
Labelled_data_time <-  arrange(Labelled_data_time, Time_500m)

colnames(Labelled_data_time) <-  col_namesdist


#melt Time data grouping by ID for ggplot


Labelled_data_time_melted <- melt(Labelled_data_time, id = c("ID","First_Name", "Last_Name","Competition","Class", "Distance","Phase"), 
                                  measure.vars = c("25", "50", "75", "100", 
                                                   "125", "150", "175", "200", 
                                                   "225", "250", "275", "300", 
                                                   "325", "350", "375", "400", 
                                                   "425", "450", "475", "500"))
Labelled_data_time_melted_top10 <- melt(Labelled_data_time[1:10,], id = c("ID","First_Name", "Last_Name","Competition","Class", "Distance","Phase"), 
                                       measure.vars = c("25", "50", "75", "100", 
                                                        "125", "150", "175", "200", 
                                                        "225", "250", "275", "300", 
                                                        "325", "350", "375", "400", 
                                                        "425", "450", "475", "500"))

#Make split column ("Variable") as numeric for plotting
Labelled_data_time_melted$variable <-  as.numeric(as.character(Labelled_data_time_melted$variable))
Labelled_data_time_melted_top10$variable <-  as.numeric(as.character(Labelled_data_time_melted_top10$variable))

#calculate mean and CI from melted data (group by split)
Labelled_data_time_melted_meanCI <- 
  Labelled_data_time_melted_top10 %>%
  group_by(variable)%>%
  dplyr::summarise(UpperLimit = CI(value, ci=0.95)[1],
                   Average = mean(value), 
                   LowerLimit = CI(value, ci=0.95)[3]) %>%
  mutate(variable = variable %>% as.factor())

#make sure the split variable is numeric
Labelled_data_time_melted_meanCI$variable <-  as.numeric(as.character(Labelled_data_time_melted_meanCI$variable))


#round data to 2 DPs
Labelled_data_time_melted_meanCI <-  round(Labelled_data_time_melted_meanCI, digits = 2)


#Filtered data

datay <-  Labelled_data_time_melted %>% filter(First_Name == "Tom") %>% filter(Last_Name == "Green") %>% filter(Competition == "GP22020") %>% filter(Phase=="Final")


windows()
ggplot(Labelled_data_time_melted_meanCI) + geom_line(aes(variable, Average), group=1) +   xlab("Split (m)") + 
  ylab("Time (sec)") + 
  scale_y_continuous(breaks = seq(0,100,len=21)) +
  geom_ribbon(aes(ymin=LowerLimit, ymax=UpperLimit, x=variable), alpha = 0.3) + geom_line(data=datay, aes(variable,value),colour = "red")



##GGPLOT
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


Tabledata <- column_to_rownames(Labelled_data_time_melted_meanCI,'variable')
Tabledata <-  data.frame(t(Tabledata))
Tabledata <-  rownames_to_column(Tabledata)
colnames(Tabledata) <-  tablecolumns

Filtereddata <-  datay %>% select(variable, value)
Filtereddata <- column_to_rownames(Filtereddata,'variable')
Filtereddata <-  data.frame(t(Filtereddata))
Filtereddata <-  rownames_to_column(Filtereddata)
colnames(Filtereddata)<-  tablecolumns
Filtereddata[1,1] <- (levels(droplevels(datay$ID)))[1]



Tabledata2 <-  bind_rows(Filtereddata, Tabledata)

#Tabledata <- rownames_to_column(Tabledata, var="data")

datatable(Tabledata2, rownames = NULL)
formattable(Tabledata2, align = c("l", "c","c","c","c","c","c","c","c","c","c","c","c","c","c","c","c","c","c","c","c"), 
            list("ID" = formatter("span", style = ~ style(color = "red", font.weight = "bold")), '25m' = color_tile(customRed,"white")))
