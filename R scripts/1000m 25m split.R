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

graphics.off()
#Select data folder

Sys.sleep(1) #pause just a little for dailogs
data_folder <- tk_choose.dir(getwd(), caption = "Select directory")

#data_folder = "C:/Users/aaron.beach/OneDrive - nswis.com.au/R/Canoe Race Model/data"

#List filenames and create labels

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

#Set column names

col_names <- c(
  "First_Name",  "Last_Name",  "Competition",  "Class",  "Distance",  "Phase",
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
  "Split_925m", "Split_950m", "Split_975m", "Split_1000m", "Split_Avg",

  "Vel_25m", "Vel_50m", "Vel_75m", "Vel_100m", 
  "Vel_125m", "Vel_150m", "Vel_175m", "Vel_200m", 
  "Vel_225m", "Vel_250m", "Vel_275m", "Vel_300m", 
  "Vel_325m", "Vel_350m", "Vel_375m", "Vel_400m", 
  "Vel_425m", "Vel_450m", "Vel_475m", "Vel_500m",
  "Vel_525m", "Vel_550m", "Vel_575m", "Vel_600m", 
  "Vel_625m", "Vel_650m", "Vel_675m", "Vel_700m", 
  "Vel_725m", "Vel_750m", "Vel_775m", "Vel_800m", 
  "Vel_825m", "Vel_850m", "Vel_875m", "Vel_800m", 
  "Vel_925m", "Vel_950m", "Vel_975m", "Vel_1000m", "Vel_Avg",

  "SR_25m", "SR_50m", "SR_75m", "SR_100m", 
  "SR_125m", "SR_150m", "SR_175m", "SR_200m", 
  "SR_225m", "SR_250m", "SR_275m", "SR_300m", 
  "SR_325m", "SR_350m", "SR_375m", "SR_400m", 
  "SR_425m", "SR_450m", "SR_475m", "SR_500m",
  "SR_525m", "SR_550m", "SR_575m", "SR_600m", 
  "SR_625m", "SR_650m", "SR_675m", "SR_700m", 
  "SR_725m", "SR_750m", "SR_775m", "SR_800m", 
  "SR_825m", "SR_850m", "SR_875m", "SR_900m", 
  "SR_925m", "SR_950m", "SR_975m", "SR_1000m", "SR_Avg")

col_names2 <- c(
  "ID",
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
  "Split_925m", "Split_950m", "Split_975m", "Split_1000m", "Split_Avg",
  
  "Vel_25m", "Vel_50m", "Vel_75m", "Vel_100m", 
  "Vel_125m", "Vel_150m", "Vel_175m", "Vel_200m", 
  "Vel_225m", "Vel_250m", "Vel_275m", "Vel_300m", 
  "Vel_325m", "Vel_350m", "Vel_375m", "Vel_400m", 
  "Vel_425m", "Vel_450m", "Vel_475m", "Vel_500m",
  "Vel_525m", "Vel_550m", "Vel_575m", "Vel_600m", 
  "Vel_625m", "Vel_650m", "Vel_675m", "Vel_700m", 
  "Vel_725m", "Vel_750m", "Vel_775m", "Vel_800m", 
  "Vel_825m", "Vel_850m", "Vel_875m", "Vel_800m", 
  "Vel_925m", "Vel_950m", "Vel_975m", "Vel_1000m", "Vel_Avg",
  
  "SR_25m", "SR_50m", "SR_75m", "SR_100m", 
  "SR_125m", "SR_150m", "SR_175m", "SR_200m", 
  "SR_225m", "SR_250m", "SR_275m", "SR_300m", 
  "SR_325m", "SR_350m", "SR_375m", "SR_400m", 
  "SR_425m", "SR_450m", "SR_475m", "SR_500m",
  "SR_525m", "SR_550m", "SR_575m", "SR_600m", 
  "SR_625m", "SR_650m", "SR_675m", "SR_700m", 
  "SR_725m", "SR_750m", "SR_775m", "SR_800m", 
  "SR_825m", "SR_850m", "SR_875m", "SR_900m", 
  "SR_925m", "SR_950m", "SR_975m", "SR_1000m", "SR_Avg")
#Transpose data to along columns of one row

data_transposed = list()
for (i in 1:length(table1)){
  data2 = table1[[i]]
  data_transposed[[i]] <-  data.frame(t(data.frame(data2[1:40,2])), t(data.frame(data2[,3])), t(data.frame(data2[,4])), t(data.frame(data2[,5])))
}

#combine all dataframes in list to one dataframe

all <-  rbindlist(data_transposed)

#combine all labels into one dataframe

metadt <- data.frame(rbind(labels))
metadt2 <-  data.frame(t(data.frame(rbind(dataname))))

#combine labels with data into a dataframe

allcombined <-  data.frame(metadt2, all)

#define column and row names
row_names <-  1:nrow(metadt2)

colnames(allcombined) <-  col_names2
rownames(allcombined) <-  row_names

####################

#Plots

#sort allcombined by final time
allcombined <-  arrange(allcombined, Time_500m)

#extract just time variable and arrange by final time
allcombinedtime <-  allcombined[,1:41]
allcombinedtime <-  arrange(allcombinedtime, Time_500m)

#melt Time data grouping by ID for ggplot
allcombinedmelttime <- melt(allcombined,  id.vars = "ID", measure.vars = c(   "Time_25m", "Time_50m", "Time_75m", "Time_100m", 
                                                                              "Time_125m", "Time_150m", "Time_175m", "Time_200m", 
                                                                              "Time_225m", "Time_250m", "Time_275m", "Time_300m", 
                                                                              "Time_325m", "Time_350m", "Time_375m", "Time_400m", 
                                                                              "Time_425m", "Time_450m", "Time_475m", "Time_500m",
                                                                              "Time_525m", "Time_550m", "Time_575m", "Time_600m", 
                                                                              "Time_625m", "Time_650m", "Time_675m", "Time_700m", 
                                                                              "Time_725m", "Time_750m", "Time_775m", "Time_800m", 
                                                                              "Time_825m", "Time_850m", "Time_875m", "Time_900m", 
                                                                              "Time_925m", "Time_950m", "Time_975m", "Time_1000m"))
#melt split data grouping by ID for ggplot
allcombinedmeltSplit <- melt(allcombined,  id.vars = "ID", measure.vars = c(     "Split_25m", "Split_50m", "Split_75m", "Split_100m", 
                                                                                 "Split_125m", "Split_150m", "Split_175m", "Split_200m", 
                                                                                 "Split_225m", "Split_250m", "Split_275m", "Split_300m", 
                                                                                 "Split_325m", "Split_350m", "Split_375m", "Split_400m", 
                                                                                 "Split_425m", "Split_450m", "Split_475m", "Split_500m",
                                                                                 "Split_525m", "Split_550m", "Split_575m", "Split_600m", 
                                                                                 "Split_625m", "Split_650m", "Split_675m", "Split_700m", 
                                                                                 "Split_725m", "Split_750m", "Split_775m", "Split_800m", 
                                                                                 "Split_825m", "Split_850m", "Split_875m", "Split_900m", 
                                                                                 "Split_925m", "Split_950m", "Split_975m", "Split_1000m", "Split_Avg"))

##GGPLOT
#New xlabels
Xlabels <-  c(  "25m", "50m", "75m", "100m", 
                "125m", "150m", "175m", "200m", 
                "225m", "250m", "275m", "300m", 
                "325m", "350m", "375m", "400m", 
                "425m", "450m", "475m", "500m",
                "525m", "550m", "575m", "600m", 
                "625m", "650m", "675m", "700m", 
                "725m", "750m", "775m", "800m", 
                "825m", "850m", "875m", "900m", 
                "925m", "950m", "975m", "1000m")

#GGplot
windows()
ggplot(allcombinedmelttime, aes(x=variable, y=value, group=factor(ID))) + 
  geom_line(aes(color=factor(ID))) + 
  geom_point() + 
  scale_x_discrete(labels=Xlabels) + 
  xlab("Split") + 
  ylab("Time (sec)") + 
  scale_y_continuous(breaks = seq(0,100,len=21))


##fancy table

#custom colours and new column labels
customGreen0 = "#DeF7E9"
customGreen = "#71CA97"
customRed = "#ff7f7f"

tablecolumns <-  c("ID", "25m", "50m", "75m", "100m", 
                   "125m", "150m", "175m", "200m", 
                   "225m", "250m", "275m", "300m", 
                   "325m", "350m", "375m", "400m", 
                   "425m", "450m", "475m", "500m",
                   "525m", "550m", "575m", "600m", 
                   "625m", "650m", "675m", "700m", 
                   "725m", "750m", "775m", "800m", 
                   "825m", "850m", "875m", "900m", 
                   "925m", "950m", "975m", "1000m")

#table

formattable(allcombinedtime, col.names = tablecolumns, align = c("l", "c","c","c","c","c","c","c","c","c","c","c","c","c","c","c","c","c","c","c","c","c","c","c","c","c","c","c","c","c","c","c","c","c","c","c","c","c","c","c","c"), 
            list('ID' = formatter("span", style = ~ style(color = "red", font.weight = "bold")), 'Time_25m' = color_tile(customRed,"white")))

