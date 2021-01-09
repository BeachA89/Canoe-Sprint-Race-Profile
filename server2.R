server <- function(input, output) {
  observeEvent(input$goButton, {
      #### Read data into a list ####
      
      table1 <-  isolate(lapply(input$file1$datapath, fread, skip = 1, header=TRUE, stringsAsFactors=FALSE))
      
      
      #### identify data labels ####
      
      dataname  <- input$file1[['name']]
      dataname <-  str_remove_all(dataname, ".csv")
      labels <-  t(data.frame(strsplit(dataname, "_")))
      fullnames <-  data.frame(dataname, labels)
      fullnames <-  fullnames[,c(1,2,3,4,7,8,10)]
      fullnames_500 <-  fullnames[fullnames[,6] == 500,]
      fullnames_1000 <-  fullnames[fullnames[,6] == 1000,]
      
      
      ##### Convert mm:ss.0 time to secs.0 ####
      
      for (i in 1:length(table1)){
        for (j in 1:length(table1[[i]][["Time"]])){
          if (str_detect(replace_na(table1[[i]][["Time"]][j],''), ":") == TRUE){
            table1[[i]][["Time"]][j] <- as.numeric(ms(table1[[i]][["Time"]][j]))
          }else{
            table1[[i]][["Time"]][j] <-  as.numeric(table1[[i]][["Time"]][j])
          }
        }
      }
      
      
      #### Convert whole Time variable to numeric ####
      
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
      
      
      
      
      #### Transpose Data ####
      #Transpose data to along columns of one row
      
      data_transposed_500 = list()
      for (i in 1:length(table1)){
        data2 = table1[[i]]
        if (is.na(data2[21,2])){
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
      
      
      
      
      #### change type to character ####
      Labelled_data_500_time$Class = as.character(Labelled_data_500_time$Class)
      Labelled_data_500_time$First_Name = as.character(Labelled_data_500_time$First_Name)
      Labelled_data_500_time$Last_Name = as.character(Labelled_data_500_time$Last_Name)
      Labelled_data_500_time$Competition = as.character(Labelled_data_500_time$Competition)
      Labelled_data_500_time$Phase = as.character(Labelled_data_500_time$Phase)
      
      
      #Filter data for plot
      Labelled_data_1000_time$Class = as.character(Labelled_data_1000_time$Class)    
      Labelled_data_1000_time$First_Name = as.character(Labelled_data_1000_time$First_Name)
      Labelled_data_1000_time$Last_Name = as.character(Labelled_data_1000_time$Last_Name)
      Labelled_data_1000_time$Competition = as.character(Labelled_data_1000_time$Competition)
      Labelled_data_1000_time$Phase = as.character(Labelled_data_1000_time$Phase)    
      
      
      
      
      
      
      #New xlabels, back to 'm
      Xlabels <-  c(  "25m", "50m", "75m", "100m", 
                      "125m", "150m", "175m", "200m", 
                      "225m", "250m", "275m", "300m", 
                      "325m", "350m", "375m", "400m", 
                      "425m", "450m", "475m", "500m")
      
      
      
      #### Filtering based on dropdowns ####
      
      
      
      tab <-  reactive({
        
        get(paste0(input$distance,"_time"))%>%
          melt(id = c("ID","First_Name", "Last_Name","Competition","Class", "Distance","Phase"))%>%
          filter(Class == input$Class) %>% 
          filter(First_Name == input$First_Name) %>% 
          filter(Last_Name == input$Last_Name) %>% 
          filter(Competition == input$Competition) %>% 
          filter(Phase == input$Phase) %>%
          mutate(variable = as.numeric(as.character(variable)))
      })  
      
      tab2 <-  reactive({
        get(paste0(input$distance,"_time")) %>% 
          filter(Class == input$Class) %>%
          arrange_at(ncol(.)) %>%
          head(10) %>%
          melt(id = c("ID","First_Name", "Last_Name","Competition","Class", "Distance","Phase")) %>%
          group_by(variable) %>%
          dplyr::summarise(UpperLimit = CI(value, ci=0.95)[1],
                           Average = mean(value), 
                           LowerLimit = CI(value, ci=0.95)[3]) %>%
          mutate(variable = as.numeric(as.character(variable))) %>%
          mutate(Average = round(Average, digits = 2)) %>%
          mutate(UpperLimit = round(UpperLimit, digits = 2))%>%
          mutate(LowerLimit = round(LowerLimit, digits = 2))
        
      })
      

      output$select_Class <-  renderUI({
        
        selectizeInput('Class', 'Select Class', choices = c("select" = "", unique(get(paste0(input$distance,"_time"))$Class)))  
      }) 
      
      output$select_First_Name <-  renderUI({
        inputClass = as.character(input$Class)
        choice_First_Name <- reactive({
          get(paste0(input$distance,"_time")) %>% 
            filter(Class == inputClass) %>% 
            pull(First_Name) %>% 
            as.character()
          
          
        })
        
        
        selectizeInput('First_Name', 'Select First Name', choices = c("select" = "", choice_First_Name()))  
      })         
      
      
      output$select_Last_Name <-  renderUI({
        inputClass = as.character(input$Class)
        inputFirst_Name = as.character(input$First_Name)
        choice_Last_Name <- reactive({
          get(paste0(input$distance,"_time")) %>% 
            filter(Class == inputClass) %>% 
            filter(First_Name == inputFirst_Name) %>% 
            pull(Last_Name) %>% 
            as.character()
          
          
        })
        
        
        selectizeInput('Last_Name', 'Select Last Name', choices = c("select" = "", choice_Last_Name())) 
        
      })
      
      output$select_Competition <-  renderUI({
        inputClass = as.character(input$Class)
        inputFirst_Name = as.character(input$First_Name)
        inputLast_Name = as.character(input$Last_Name)        
        choice_Competition <- reactive({
          get(paste0(input$distance,"_time")) %>% 
            filter(Class == inputClass) %>% 
            filter(First_Name == inputFirst_Name) %>% 
            filter(Last_Name == inputLast_Name) %>%             
            pull(Competition) %>% 
            as.character()
          
          
        })
        
        
        selectizeInput('Competition', 'Select Competition', choices = c("select" = "", choice_Competition()))  
      })
      
      output$select_Phase <-  renderUI({
        inputClass = as.character(input$Class)
        inputFirst_Name = as.character(input$First_Name)
        inputLast_Name = as.character(input$Last_Name)
        inputcompetition = as.character(input$Competition)
        choice_Phase <- reactive({
          get(paste0(input$distance,"_time")) %>% 
            filter(Class == inputClass) %>% 
            filter(First_Name == inputFirst_Name) %>% 
            filter(Last_Name == inputLast_Name) %>% 
            filter(Competition == inputcompetition) %>% 
            pull(Phase) %>% 
            as.character()
          
          
        })
        
        
        selectizeInput('Phase', 'Select Phase', choices = c("select" = "", choice_Phase()))  
      })     
      
      output$table2 <- renderDataTable({ 
        
        tab2()
      })
      
      
      
      
      
      
      
      
      
      
      #datay = Labelled_data_time_melted %>% filter(First_Name == "Tom" & Last_Name == "Green" & Competition == "GP22020" & Phase=="Final")
      #    output$ggplot <-  renderPlot({
      #    ggplot(tab2()) + geom_line(aes(variable, Average), group=1) +   xlab("Split (m)") + 
      #      ylab("Time (sec)") +
      #      geom_ribbon(aes(ymin=LowerLimit, ymax=UpperLimit, x=variable), alpha = 0.3) + geom_line(data=tab(), aes(variable,value),colour = "red")
      #    })
      
      
      #### Output GGplot ####
      output$ggplot <-  renderPlot({
        if (paste0(input$distance, "_time") == "Labelled_data_500_time"){
          ggplot(tab2()) + geom_line(aes(variable, Average), group=1) +   xlab("Split (m)") + 
            ylab("Time (sec)") +
            geom_ribbon(aes(ymin=LowerLimit, ymax=UpperLimit, x=variable), alpha = 0.3) + geom_line(data=tab(), aes(variable,value),colour = "red")
          
          
        }else{
          
          ggplot(tab2()) + geom_line(aes(variable, Average), group=1) +   xlab("Split (m)") + 
            ylab("Time (sec)") + 
            geom_ribbon(aes(ymin=LowerLimit, ymax=UpperLimit, x=variable), alpha = 0.3) + geom_line(data=tab(), aes(variable,value),colour = "red")        
        }
      })
      
      
      
      #### Output table ####
      tablecolumns500 <-  c("ID", "25m", "50m", "75m", "100m", 
                            "125m", "150m", "175m", "200m", 
                            "225m", "250m", "275m", "300m", 
                            "325m", "350m", "375m", "400m", 
                            "425m", "450m", "475m", "500m")
      tablerows <-  c("Upper", "Top 10 Average", "Lower")
      
      tablecolumns1000 <-  c("ID", "25m", "50m", "75m", "100m", 
                             "125m", "150m", "175m", "200m", 
                             "225m", "250m", "275m", "300m", 
                             "325m", "350m", "375m", "400m", 
                             "425m", "450m", "475m", "500m",
                             "525m", "550m", "575m", "600m", 
                             "625m", "650m", "675m", "700m", 
                             "725m", "750m", "775m", "800m", 
                             "825m", "850m", "875m", "900m",
                             "925m", "950m", "975m", "1000m")
      
      
      output$table3 <-  renderDataTable({
        if (paste0(input$distance, "_time") == "Labelled_data_500_time"){
          Tabledata <- column_to_rownames(tab2(),'variable')
          Tabledata <-  data.frame(t(Tabledata))
          Tabledata <-  rownames_to_column(Tabledata)
          colnames(Tabledata) <-  tablecolumns500
        }else{
          Tabledata <- column_to_rownames(tab2(),'variable')
          Tabledata <-  data.frame(t(Tabledata))
          Tabledata <-  rownames_to_column(Tabledata)
          colnames(Tabledata) <-  tablecolumns1000
        }
        
          if (paste0(input$distance, "_time") == "Labelled_data_500_time"){  
          Filtereddata <-  tab() %>% select(variable, value)
          Filtereddata <- column_to_rownames(Filtereddata,'variable')
          Filtereddata <-  data.frame(t(Filtereddata))
          Filtereddata <-  rownames_to_column(Filtereddata)
          Filtereddata[1,1] <- "Race"
          colnames(Filtereddata) <-  tablecolumns500
          
        }else{
          Filtereddata <-  tab() %>% select(variable, value)
          Filtereddata <- column_to_rownames(Filtereddata,'variable')
          Filtereddata <-  data.frame(t(Filtereddata))
          Filtereddata <-  rownames_to_column(Filtereddata)
          Filtereddata[1,1] <- "Race"
          colnames(Filtereddata) <-  tablecolumns1000
        }
        

        Tabledata2 <-  bind_rows(Filtereddata, Tabledata)
        background <- "value == 'Race' ? 'orange' : value == 'Average' ? 'grey' : value != 'else' ? '' : ''"  
        class(background) <- "JS_EVAL"

        ({datatable(Tabledata2, rownames = NULL, options = list(dom='t', ordering=F)) %>%   
            formatStyle(
              'ID',
              target = 'row',
              backgroundColor = background
            )
          
          
        })
      })
      

      
      #### Output headers ####
      output$Summaryhead <- renderText({paste(tab()$First_Name[1], tab()$Last_Name[1], tab()$Competition[1], tab()$Class[1], tab()$Distance[1], tab()$Phase[1], "vs Top 10 average")
      })

      
      output$Summaryhead2 <- renderText({
        paste(" ")
      })
    })
  
}

