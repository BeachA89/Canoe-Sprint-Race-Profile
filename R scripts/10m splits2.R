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


#Select data folder

Sys.sleep(1) #pause just a little for dailogs
data_folder <- tk_choose.dir(getwd(), caption = "Select directory")

#data_folder = "C:/Users/

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
  "Time_10m", "Time_20m", "Time_30m", "Time_40m", "Time_50m", "Time_60m", "Time_70m", "Time_80m", "Time_90m", "Time_100m", 
  "Time_110m", "Time_120m", "Time_130m", "Time_140m", "Time_150m", "Time_160m", "Time_170m", "Time_180m", "Time_190m", "Time_200m",  
  "Time_210m", "Time_220m", "Time_230m", "Time_240m", "Time_250m", "Time_260m", "Time_270m", "Time_280m", "Time_290m", "Time_300m", 
  "Time_310m", "Time_320m", "Time_330m", "Time_340m", "Time_350m", "Time_360m", "Time_370m", "Time_380m", "Time_390m", "Time_400m", 
  "Time_410m", "Time_420m", "Time_430m", "Time_440m", "Time_450m", "Time_460m", "Time_470m", "Time_480m", "Time_490m", "Time_500m",
  
  "Split_10m", "Split_20m", "Split_30m", "Split_40m", "Split_50m", "Split_60m", "Split_70m", "Split_80m", "Split_90m", "Split_100m", 
  "Split_110m", "Split_120m", "Split_130m", "Split_140m", "Split_150m", "Split_160m", "Split_170m", "Split_180m", "Split_190m", "Split_200m",  
  "Split_210m", "Split_220m", "Split_230m", "Split_240m", "Split_250m", "Split_260m", "Split_270m", "Split_280m", "Split_290m", "Split_300m", 
  "Split_310m", "Split_320m", "Split_330m", "Split_340m", "Split_350m", "Split_360m", "Split_370m", "Split_380m", "Split_390m", "Split_400m", 
  "Split_410m", "Split_420m", "Split_430m", "Split_440m", "Split_450m", "Split_460m", "Split_470m", "Split_480m", "Split_490m", "Split_500m", "Split_Avg",
  
  "Vel_10m", "Vel_20m", "Vel_30m", "Vel_40m", "Vel_50m", "Vel_60m", "Vel_70m", "Vel_80m", "Vel_90m", "Vel_100m", 
  "Vel_110m", "Vel_120m", "Vel_130m", "Vel_140m", "Vel_150m", "Vel_160m", "Vel_170m", "Vel_180m", "Vel_190m", "Vel_200m",  
  "Vel_210m", "Vel_220m", "Vel_230m", "Vel_240m", "Vel_250m", "Vel_260m", "Vel_270m", "Vel_280m", "Vel_290m", "Vel_300m", 
  "Vel_310m", "Vel_320m", "Vel_330m", "Vel_340m", "Vel_350m", "Vel_360m", "Vel_370m", "Vel_380m", "Vel_390m", "Vel_400m", 
  "Vel_410m", "Vel_420m", "Vel_430m", "Vel_440m", "Vel_450m", "Vel_460m", "Vel_470m", "Vel_480m", "Vel_490m", "Vel_500m","Vel_Avg",
  
  "SR_10m", "SR_20m", "SR_30m", "SR_40m", "SR_50m", "SR_60m", "SR_70m", "SR_80m", "SR_90m", "SR_100m", 
  "SR_110m", "SR_120m", "SR_130m", "SR_140m", "SR_150m", "SR_160m", "SR_170m", "SR_180m", "SR_190m", "SR_200m",  
  "SR_210m", "SR_220m", "SR_230m", "SR_240m", "SR_250m", "SR_260m", "SR_270m", "SR_280m", "SR_290m", "SR_300m", 
  "SR_310m", "SR_320m", "SR_330m", "SR_340m", "SR_350m", "SR_360m", "SR_370m", "SR_380m", "SR_390m", "SR_400m", 
  "SR_410m", "SR_420m", "SR_430m", "SR_440m", "SR_450m", "SR_460m", "SR_470m", "SR_480m", "SR_490m", "SR_500m", "SR_Avg" )

col_names2 <- c(
  "ID",
  "Time_10m", "Time_20m", "Time_30m", "Time_40m", "Time_50m", "Time_60m", "Time_70m", "Time_80m", "Time_90m", "Time_100m", 
  "Time_110m", "Time_120m", "Time_130m", "Time_140m", "Time_150m", "Time_160m", "Time_170m", "Time_180m", "Time_190m", "Time_200m",  
  "Time_210m", "Time_220m", "Time_230m", "Time_240m", "Time_250m", "Time_260m", "Time_270m", "Time_280m", "Time_290m", "Time_300m", 
  "Time_310m", "Time_320m", "Time_330m", "Time_340m", "Time_350m", "Time_360m", "Time_370m", "Time_380m", "Time_390m", "Time_400m", 
  "Time_410m", "Time_420m", "Time_430m", "Time_440m", "Time_450m", "Time_460m", "Time_470m", "Time_480m", "Time_490m", "Time_500m",
  
  "Split_10m", "Split_20m", "Split_30m", "Split_40m", "Split_50m", "Split_60m", "Split_70m", "Split_80m", "Split_90m", "Split_100m", 
  "Split_110m", "Split_120m", "Split_130m", "Split_140m", "Split_150m", "Split_160m", "Split_170m", "Split_180m", "Split_190m", "Split_200m",  
  "Split_210m", "Split_220m", "Split_230m", "Split_240m", "Split_250m", "Split_260m", "Split_270m", "Split_280m", "Split_290m", "Split_300m", 
  "Split_310m", "Split_320m", "Split_330m", "Split_340m", "Split_350m", "Split_360m", "Split_370m", "Split_380m", "Split_390m", "Split_400m", 
  "Split_410m", "Split_420m", "Split_430m", "Split_440m", "Split_450m", "Split_460m", "Split_470m", "Split_480m", "Split_490m", "Split_500m", "Split_Avg",
  
  "Vel_10m", "Vel_20m", "Vel_30m", "Vel_40m", "Vel_50m", "Vel_60m", "Vel_70m", "Vel_80m", "Vel_90m", "Vel_100m", 
  "Vel_110m", "Vel_120m", "Vel_130m", "Vel_140m", "Vel_150m", "Vel_160m", "Vel_170m", "Vel_180m", "Vel_190m", "Vel_200m",  
  "Vel_210m", "Vel_220m", "Vel_230m", "Vel_240m", "Vel_250m", "Vel_260m", "Vel_270m", "Vel_280m", "Vel_290m", "Vel_300m", 
  "Vel_310m", "Vel_320m", "Vel_330m", "Vel_340m", "Vel_350m", "Vel_360m", "Vel_370m", "Vel_380m", "Vel_390m", "Vel_400m", 
  "Vel_410m", "Vel_420m", "Vel_430m", "Vel_440m", "Vel_450m", "Vel_460m", "Vel_470m", "Vel_480m", "Vel_490m", "Vel_500m","Vel_Avg",
  
  "SR_10m", "SR_20m", "SR_30m", "SR_40m", "SR_50m", "SR_60m", "SR_70m", "SR_80m", "SR_90m", "SR_100m", 
  "SR_110m", "SR_120m", "SR_130m", "SR_140m", "SR_150m", "SR_160m", "SR_170m", "SR_180m", "SR_190m", "SR_200m",  
  "SR_210m", "SR_220m", "SR_230m", "SR_240m", "SR_250m", "SR_260m", "SR_270m", "SR_280m", "SR_290m", "SR_300m", 
  "SR_310m", "SR_320m", "SR_330m", "SR_340m", "SR_350m", "SR_360m", "SR_370m", "SR_380m", "SR_390m", "SR_400m", 
  "SR_410m", "SR_420m", "SR_430m", "SR_440m", "SR_450m", "SR_460m", "SR_470m", "SR_480m", "SR_490m", "SR_500m", "SR_Avg")

#Transpose data to along columns of one row

data_transposed = list()
for (i in 1:length(table1)){
  data2 = table1[[i]]
  data_transposed[[i]] <-  data.frame(t(data.frame(data2[1:50,2])), t(data.frame(data2[,3])), t(data.frame(data2[,4])), t(data.frame(data2[,5])))
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
allcombinedtime <-  allcombined[,1:51]
allcombinedtime <-  arrange(allcombinedtime, Time_500m)

#melt Time data grouping by ID for ggplot
allcombinedmelttime <- melt(allcombined,  id.vars = "ID", measure.vars = c(    "Time_10m", "Time_20m", "Time_30m", "Time_40m", "Time_50m", "Time_60m", "Time_70m", "Time_80m", "Time_90m", "Time_100m", 
                                                                               "Time_110m", "Time_120m", "Time_130m", "Time_140m", "Time_150m", "Time_160m", "Time_170m", "Time_180m", "Time_190m", "Time_200m",  
                                                                               "Time_210m", "Time_220m", "Time_230m", "Time_240m", "Time_250m", "Time_260m", "Time_270m", "Time_280m", "Time_290m", "Time_300m", 
                                                                               "Time_310m", "Time_320m", "Time_330m", "Time_340m", "Time_350m", "Time_360m", "Time_370m", "Time_380m", "Time_390m", "Time_400m", 
                                                                               "Time_410m", "Time_420m", "Time_430m", "Time_440m", "Time_450m", "Time_460m", "Time_470m", "Time_480m", "Time_490m", "Time_500m"))

#melt split data grouping by ID for ggplot
allcombinedmeltSplit <- melt(allcombined,  id.vars = "ID", measure.vars = c(     "Split_10m", "Split_20m", "Split_30m", "Split_40m", "Split_50m", "Split_60m", "Split_70m", "Split_80m", "Split_90m", "Split_100m", 
                                                                                 "Split_110m", "Split_120m", "Split_130m", "Split_140m", "Split_150m", "Split_160m", "Split_170m", "Split_180m", "Split_190m", "Split_200m",  
                                                                                 "Split_210m", "Split_220m", "Split_230m", "Split_240m", "Split_250m", "Split_260m", "Split_270m", "Split_280m", "Split_290m", "Split_300m", 
                                                                                 "Split_310m", "Split_320m", "Split_330m", "Split_340m", "Split_350m", "Split_360m", "Split_370m", "Split_380m", "Split_390m", "Split_400m", 
                                                                                 "Split_410m", "Split_420m", "Split_430m", "Split_440m", "Split_450m", "Split_460m", "Split_470m", "Split_480m", "Split_490m", "Split_500m",
                                                                                 "Split_Avg"))

##GGPLOT
#New xlabels
Xlabels <-  c("10m", "20m", "30m", "40m", "50m", "60m", "70m", "80m", "90m", "100m", 
              "110m", "120m", "130m", "140m", "150m", "160m", "170m", "180m", "190m", "200m",  
              "210m", "220m", "230m", "240m", "250m", "260m", "270m", "280m", "290m", "300m", 
              "310m", "320m", "330m", "340m", "350m", "360m", "370m", "380m", "390m", "400m", 
              "410m", "420m", "430m", "440m", "450m", "460m", "470m", "480m", "490m", "500m")

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

tablecolumns <-  c("ID",  "10m", "20m", "30m", "40m", "50m", "60m", "70m", "80m", "90m", "100m", 
                   "110m", "120m", "130m", "140m", "150m", "160m", "170m", "180m", "190m", "200m",  
                   "210m", "220m", "230m", "240m", "250m", "260m", "270m", "280m", "290m", "300m", 
                   "310m", "320m", "330m", "340m", "350m", "360m", "370m", "380m", "390m", "400m", 
                   "410m", "420m", "430m", "440m", "450m", "460m", "470m", "480m", "490m", "500m")

#table

formattable(allcombinedtime, col.names = tablecolumns, align = c("l", "c","c","c","c","c","c","c","c","c","c","c","c","c","c","c","c","c","c","c","c","c","c","c","c","c","c","c","c","c","c","c","c","c","c","c","c","c","c","c","c","c","c","c","c","c","c","c","c","c","c"), 
            list('ID' = formatter("span", style = ~ style(color = "red", font.weight = "bold")), 'Time_25m' = color_tile(customRed,"white")))

