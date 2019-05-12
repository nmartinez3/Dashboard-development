###version number: 2.1.4

###run 'data refresh.R' first to prepare data sets used by this app

#loading packages
require(lubridate)
require(readr)
require(dplyr)
require(ggplot2)
require(shiny)
require(shinydashboard)
require(stringr)
require(rlang)
require(DT)

#data creation date (date data refresh.R was ran)
data_creation_date <- read_rds(path = './data/data_creation_date.rds')

#latest data collection datef
data_collection_date <- read_rds(path = './data/data_collection_date.rds')

#loading schoolweeks data
schoolweeks <- read_rds(path = './data/schoolweeks.rds')

#loading dashboard 1 data
d1studentmonth <- read_rds(path = './data/d1studentmonth.rds')
d1studentquarter <- read_rds(path = './data/d1studentquarter.rds')
d1teachermonth <- read_rds(path = './data/d1teachermonth.rds')
d1teacherquarter <- read_rds(path = './data/d1teacherquarter.rds')

#loading dashboard 2 data
d2student <- read_rds(path = './data/d2student.rds')
d2teacher <- read_rds(path = './data/d2teacher.rds')

#loading dashboard 3 data
d3student <- read_rds(path = './data/d3student.rds')
d3teacher <- read_rds(path = './data/d3teacher.rds')

#loading dashboard 4 data
d4student <- read_rds(path = './data/d4student.rds')
d4teacher <- read_rds(path = './data/d4teacher.rds')

#loading dashboard 5 data
d5student <- read_rds(path = './data/d5student.rds')
d5teacher <- read_rds(path = './data/d5teacher.rds')

#loading dashboard 6 data
tanzstudenttotals <- read_rds(path = './data/d6tanzstudent.rds')
philstudenttotals <- read_rds(path = './data/d6philstudent.rds')
tanzteachertotals <- read_rds(path = './data/d6tanzteacher.rds')
philteachertotals <- read_rds(path = './data/d6philteacher.rds')
d6latestdata <- read_rds(path = './data/latestdata.rds')

#loading dashboard 7 data
d7 <- read_rds(path = './data/d7.rds')
d7schoolnames <- read_rds(path = './data/d7schoolnames.rds')

#loading auto-scaling data
plot_height <- read_rds('./data/plot_height.rds')

#server logic for shiny app
shinyServer(function(input, output, session) {
        ############################
        ###Dashboard 1
        
        observe({
                output$d1plot <- renderPlot({
                        if(input$d1timeperiod == 'Quarterly'){
                                res <- 'YearQuarter'
                                if(input$d1user == 'student') graphdata <- d1studentquarter
                                if(input$d1user == 'teacher') graphdata <- d1teacherquarter
                                x_labs <- str_match(levels(graphdata$YearQuarter), 'Q.') %>% as.vector()
                        }
                        if(input$d1timeperiod == 'Monthly'){
                                res <- 'YearMonth'
                                if(input$d1user == 'student') graphdata <- d1studentmonth
                                if(input$d1user == 'teacher') graphdata <- d1teachermonth
                                x_labs <- str_match(levels(graphdata$YearMonth), '[0-9]{1,2}') %>% as.vector()
                        }
                        if(input$d1metric %in% c('HoursWeek', 'UserHoursWeek')) graphdata <- graphdata %>% filter(!is.infinite(HoursWeek) & !is.infinite(UserHoursWeek))
                        y_max <- max(graphdata[, input$d1metric], na.rm = TRUE)
                        print(graphdata %>% filter(Country == input$d1country) %>%
                                ggplot(aes_string(x = res, y = input$d1metric, fill = 'Year')) + 
                                geom_bar(stat = 'identity') +
                                scale_x_discrete(label = x_labs, drop = FALSE) +
                                scale_y_continuous(limits = c(NA, y_max)) +
                                facet_wrap(~SchoolName, ncol = 3, scales = 'free') +
                                labs(title = paste('Lifetime Usage', input$d1country, input$d1user, input$d1metric, input$d1timeperiod),
                                     caption = paste('"data refresh.R" ran on', as.character(data_creation_date), '| latest data collection date:', data_collection_date)) +
                                xlab(label = ifelse(input$d1timeperiod == 'Monthly', 'Month', 'Quarter')))
                }, height = plot_height[[paste(input$d1country, input$d1user, sep = '_')]])
        })
        
        #writing .csv files to /csv directory
        write_csv(d1studentmonth %>% arrange(Country, SchoolName, Year, Month), 
                  path = paste0('./csv/student monthly lifetime ', data_creation_date, '.csv'))
        write_csv(d1studentquarter %>% arrange(Country, SchoolName, Year, Quarter), 
                  path = paste0('./csv/student quarterly lifetime ', data_creation_date, '.csv'))
        write_csv(d1teachermonth %>% arrange(Country, SchoolName, Year, Month), 
                  path = paste0('./csv/teacher monthly lifetime ', data_creation_date, '.csv'))
        write_csv(d1teacherquarter %>% arrange(Country, SchoolName, Year, Quarter), 
                  path = paste0('./csv/teacher quarterly lifetime ', data_creation_date, '.csv'))

                ############################
        ###Dashboard 2
        
        observe({
                d2startdate <- floor_date(as_date(input$d2date[1]), unit = 'month')
                d2enddate <- floor_date(as_date(input$d2date[2]), unit = 'month')
                d2validweeks <- schoolweeks %>%
                        filter(Time >= d2startdate & Time <= d2enddate) %>%
                        group_by(SchoolName) %>%
                        summarize(Weeks = sum(Weeks))
                
                if(input$d2user == 'student'){
                        graphdata <- d2student
                }
                if(input$d2user == 'teacher'){
                        graphdata <- d2teacher
                }
                graphdata <- graphdata %>% 
                        filter(FloorTime >= d2startdate & FloorTime <= d2enddate) %>%
                        group_by(Country, SchoolName, Weekday) %>%
                        summarise(Hours = sum(Hours),
                                  UserHours = sum(UserHours),
                                  AverageUsers = UserHours/Hours) %>%
                        ungroup()
                graphdata <- left_join(graphdata, d2validweeks, by = 'SchoolName')
                graphdata <- graphdata %>%
                        mutate(Weekday = factor(Weekday, levels = c('Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat')),
                               HoursWeek = Hours/Weeks,
                               UserHoursWeek = UserHours/Weeks)
                y_max <- max(graphdata[, input$d2metric], na.rm = TRUE)
                output$d2plot <- renderPlot({
                        print(graphdata %>% filter(Country == input$d2country) %>% 
                                      ggplot(aes_string(x = 'Weekday', y = input$d2metric)) + 
                                      geom_bar(stat = 'identity', fill = 'blue') + 
                                      scale_x_discrete(drop = FALSE) +
                                      scale_y_continuous(limits = c(NA, y_max)) +
                                      facet_wrap(~SchoolName, ncol = 3, scales = 'free') +
                                      labs(title = paste('Weekday Usage', input$d2country, input$d2user, input$d2metric, d2startdate, 'to', d2enddate),
                                           caption = paste('"data refresh.R" ran on', as.character(data_creation_date), '| latest data collection date:', data_collection_date)) +
                                      xlab(label = 'Weekday'))
                        }, height = plot_height[[paste(input$d2country, input$d2user, sep = '_')]])
                observeEvent(input$d2csvbutton, write_csv(graphdata %>% filter(Country == input$d2country), 
                                                          path = paste('./csv/', today(), 'weekday usage', input$d2country, input$d2user, input$d2metric, input$d2date[1], 'to', input$d2date[2], '.csv')))
        })
        
        ############################
        ###Dashboard 3
        
        observe({
                d3startdate <- floor_date(as_date(input$d3date[1]), unit = 'month')
                d3enddate <- floor_date(as_date(input$d3date[2]), unit = 'month')
                d3validweeks <- schoolweeks %>% 
                        filter(Time >= d3startdate & Time <= d3enddate) %>%
                        group_by(SchoolName) %>% 
                        summarise(Weeks = sum(Weeks))
                
                if(input$d3user == 'student'){
                        graphdata <- d3student
                }
                if(input$d3user == 'teacher'){
                        graphdata <- d3teacher
                }
                graphdata <- graphdata %>%
                        filter(FloorTime <= d3enddate & FloorTime >= d3startdate) %>%
                        group_by(Country, SchoolName, Hour) %>%
                        summarise(Hours = sum(Hours),
                                  UserHours = sum(UserHours),
                                  AverageUsers = UserHours/Hours)
                graphdata <- left_join(graphdata, d3validweeks, by = 'SchoolName')
                graphdata <- graphdata %>% 
                        mutate(Hour = factor(Hour, levels = c(0:23)),
                               HoursWeek = Hours/Weeks,
                               UserHoursWeek = UserHours/Weeks)
                y_max <- max(graphdata[, input$d3metric], na.rm = TRUE)
                output$d3plot <- renderPlot({
                        print(graphdata %>% filter(Country == input$d3country) %>% 
                                      ggplot(aes_string(x = 'Hour', y = input$d3metric)) + 
                                      geom_bar(stat = 'identity', fill = 'blue') + 
                                      scale_x_discrete(drop = FALSE) +
                                      scale_y_continuous(limits = c(NA, y_max)) +
                                      facet_wrap(~SchoolName, ncol = 3, scales = 'free') +
                                      labs(title = paste('Hourly Usage', input$d3country, input$d3user, input$d3metric, d3startdate, 'to', d3enddate),
                                           caption = paste('"data refresh.R" ran on', as.character(data_creation_date), '| latest data collection date:', data_collection_date)) +
                                      xlab(label = 'Time of Day in Hours (0 == midnight in local time)'))
                }, height = plot_height[[paste(input$d3country, input$d3user, sep = '_')]])
                observeEvent(input$d3csvbutton, write_csv(graphdata %>% filter(Country == input$d3country), 
                                                          path = paste('./csv/', today(), 'time of day usage', input$d3country, input$d3user, input$d3metric, input$d3date[1], 'to', input$d3date[2], '.csv')))
        })
        
        ############################
        ###Dashboard 4
        
        observe({
                d4startdate <- floor_date(as_date(input$d4date[1]), unit = 'month')
                d4enddate <- floor_date(as_date(input$d4date[2]), unit = 'month')
                if(input$d4user == 'student'){
                        graphdata <- d4student
                }
                if(input$d4user == 'teacher'){
                        graphdata <- d4teacher
                }
                graphdata <- graphdata %>%
                        filter(FloorTime <= d4enddate & FloorTime >= d4startdate) %>%
                        group_by(Country, SchoolName, NumUsers) %>%
                        summarize(Hours = sum(Hours)) %>%
                        ungroup()
                graphdata <- graphdata %>%
                        mutate(PlotHours = ifelse(Hours > input$d4cutoff, input$d4cutoff, Hours), 
                               Flag = ifelse(Hours > input$d4cutoff, round(Hours), NA))
                x_breaks <- seq(1, max(graphdata$NumUsers), 2)
                y_max <- min(1.15 * input$d4cutoff, max(graphdata$Hours, na.rm = TRUE))
                output$d4plot <- renderPlot({
                        suppressWarnings(print(graphdata %>% filter(Country == input$d4country) %>%
                                      ggplot(aes(x = NumUsers, y = PlotHours)) +
                                      geom_bar(stat = 'identity', fill = 'blue') +
                                      geom_label(aes(label = Flag), size = 3, nudge_y = 0.1 * input$d4cutoff) +
                                      scale_x_continuous(breaks = x_breaks, limits = c(NA, max(x_breaks))) +
                                      scale_y_continuous(breaks = seq(0, 1000, ifelse(y_max <= 150, 25, 50)), limits = c(NA, y_max)) +
                                      facet_wrap(~SchoolName, ncol = 3, scales = 'free') +
                                      labs(title = paste('Usage per Number of Users', input$d4country, input$d4user, input$d4metric, d4startdate, 'to', d4enddate),
                                           caption = paste('"data refresh.R" ran on', as.character(data_creation_date), '| latest data collection date:', data_collection_date)) +
                                      xlab(label = 'Number of Users') +
                                      ylab(label = 'Hours')))
                }, height = plot_height[[paste(input$d4country, input$d4user, sep = '_')]])
                observeEvent(input$d4csvbutton, write_csv(graphdata %>% select(-PlotHours, - Flag) %>% filter(Country == input$d4country), 
                                                          path = paste('./csv/', today(), 'number of users', input$d4country, input$d4user, input$d4date[1], 'to', input$d4date[2], '.csv')))
        })
        
        ############################
        ###Dashboard 5
        
        observe({
                d5startdate <- floor_date(as_date(input$d5date[1]), unit = 'month')
                d5enddate <- floor_date(as_date(input$d5date[2]), unit = 'month')
                d5validweeks <- schoolweeks %>% 
                        filter(Time >= d5startdate & Time <= d5enddate) %>%
                        group_by(SchoolName) %>% 
                        summarise(Weeks = sum(Weeks))
                
                if(input$d5user == 'student'){
                        graphdata <- d5student
                }
                if(input$d5user == 'teacher'){
                        graphdata <- d5teacher
                }
                graphdata <- graphdata %>%
                        filter(FloorTime <= d5enddate & FloorTime >= d5startdate) %>%
                        group_by(Country, SchoolName) %>%
                        summarise(Hours = sum(Hours),
                                  UserHours = sum(UserHours),
                                  AverageUsers = UserHours/Hours)
                graphdata <- left_join(graphdata, d5validweeks, by = 'SchoolName')
                graphdata <- graphdata %>%
                        mutate(HoursWeek = Hours/Weeks,
                               UserHoursWeek = UserHours/Weeks) %>%
                        arrange(desc(!! sym(input$d5metric)))
                graphdata$SchoolName <- factor(graphdata$SchoolName, levels = unique(graphdata$SchoolName))
                y_max <- max(graphdata[, input$d5metric], na.rm = TRUE)
                output$d5plot <- renderPlot({
                        print(graphdata %>%
                                      filter(Country == input$d5country) %>%
                                      ggplot(aes_string(x = 'SchoolName', y = input$d5metric)) +
                                      geom_bar(stat = 'identity', fill = 'blue') +
                                      scale_y_continuous(limits = c(NA, y_max)) +
                                      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  
                                      labs(title = paste('School Comparison', input$d5country, input$d5user, input$d5metric, d5startdate, 'to', d5enddate),
                                           caption = paste('"data refresh.R" ran on', as.character(data_creation_date), '| latest data collection date:', data_collection_date)) +
                                      xlab(label = 'School') +
                                      ylab(label = paste0(input$d5metric, ', max = ', y_max)))
                })
                observeEvent(input$d5csvbutton, write_csv(graphdata %>% filter(Country == input$d5country), 
                                                          path = paste('./csv/', today(), 'school comparison', input$d5country, input$d5user, input$d5metric, input$d5date[1], 'to', input$d5date[2], '.csv')))
        })
        
        ############################
        ###Dashboard 6
        
        #TZ student totals table
        output$d6tanzstudent <- DT::renderDataTable({
                tanzstudenttotals
        })
        
        #PI student totals table
        output$d6philstudent <- DT::renderDataTable({
                philstudenttotals
        })
        
        #TZ teacher totals table
        output$d6tanzteacher <- DT::renderDataTable({
                tanzteachertotals
        })
        
        #PI teacher totals table
        output$d6philteacher <- DT::renderDataTable({
                philteachertotals
        })
        
        #latest data collection date table
        output$d6latestdata <- DT::renderDataTable({
                d6latestdata %>% arrange(LatestData)
        })
        
        #writing csv files
        path_tz_student_totals <- paste('./csv/', data_creation_date, 'Tanzania student grand totals', '.csv')
        path_pi_student_totals <- paste('./csv/', data_creation_date, 'Philippines student grand totals', '.csv')
        path_tz_teacher_totals <- paste('./csv/', data_creation_date, 'Tanzania teacher grand totals', '.csv')
        path_pi_teacher_totals <- paste('./csv/', data_creation_date, 'Philippines teacher grand totals', '.csv')
        path_latest_data <- paste('./csv/', data_creation_date, 'Latest Data Collection Date', '.csv')
                
        if(!(path_tz_student_totals %in% list.files(path = './csv'))) write_csv(tanzstudenttotals, path = path_tz_student_totals)
        if(!(path_pi_student_totals %in% list.files(path = './csv'))) write_csv(philstudenttotals, path = path_pi_student_totals)
        if(!(path_tz_teacher_totals %in% list.files(path = './csv'))) write_csv(tanzteachertotals, path = path_tz_teacher_totals)
        if(!(path_pi_teacher_totals %in% list.files(path = './csv'))) write_csv(philteachertotals, path = path_pi_teacher_totals)
        if(!(path_latest_data %in% list.files(path = './csv'))) write_csv(d6latestdata, path = path_latest_data)
        
        ############################
        ###Dashboard 7
        
        observe({
                d7startdate <- floor_date(as_date(input$d7date[1]), unit = 'month')
                d7enddate <- floor_date(as_date(input$d7date[2]), unit = 'month')
                d7validweeks <- schoolweeks %>%
                        filter(Time <= d7enddate & Time >= d7startdate) %>%
                        group_by(SchoolName) %>%
                        summarize(Weeks = sum(Weeks))
                
                schoolnames <- d7schoolnames
                selectedschool <- input$d7schoolname
                if(selectedschool == 'placeholder') selectedschool <- schoolnames[1]
                updateSelectInput(session, 'd7schoolname', 'School Name', choices = schoolnames, selected = selectedschool)
                
                userusage <- d7 %>% 
                        filter(FloorTime >= d7startdate & FloorTime <= d7enddate) %>%
                        group_by(Country, SchoolName, UserType, UserName) %>%
                        summarize(Hours = sum(Hours))
                userusage <- left_join(userusage, d7validweeks, by = 'SchoolName')
                userusage <- userusage %>%
                        mutate(HoursPerWeek = round(Hours/Weeks, digits = 2)) %>%
                        mutate(SysAdmin = factor(ifelse(UserName == 'sysadmin', 'Yes', 'No'), levels = c('Yes', 'No')))
                
                output$d7userusage <- DT::renderDataTable({
                        userusage %>% 
                                filter(SchoolName == selectedschool) %>%
                                select(-SysAdmin)
                })
                
                #header if zero rows of data for selected school in selected time frame
                output$d7zero_rows <- renderUI({
                        if(nrow(userusage %>% filter(SchoolName == selectedschool)) == 0){
                                fluidRow(headerPanel(title = paste('Zero rows of data for', selectedschool, 'from', as.character(d7startdate), 'to', as.character(d7enddate))))
                        }
                })
                
                output$d7plot <- renderPlot({
                        x_limits <- c(0, max(userusage[, input$d7plotmetric], na.rm = TRUE))
                        suppressMessages(print(ggplot(userusage, aes_string(x = input$d7plotmetric, fill = 'SysAdmin')) +
                                                       geom_dotplot() +
                                                       scale_x_continuous(limits = x_limits) +
                                                       scale_y_continuous(name = NULL, labels = NULL) +
                                                       facet_wrap(~SchoolName, ncol = 6, scales = 'free') +
                                                       labs(title = paste('Sysadmin and Teacher Usage', d7startdate, 'to', d7enddate),
                                                            caption = paste('"data refresh.R" ran on', as.character(data_creation_date), '| latest data collection date:', data_collection_date))))
                }, height = plot_height$d7)
                
                output$d7sysadmintable <- DT::renderDataTable({
                        userusage %>%
                                ungroup() %>%
                                filter(SysAdmin == 'Yes') %>%
                                select(-c(UserType, SysAdmin))
                })
                
                observeEvent(input$d7userusagecsvbutton, write_csv(userusage %>% filter(SchoolName == input$d7schoolname) %>% select(-SysAdmin), 
                                                                   path = paste('./csv/', today(), input$d7schoolname, 'user usage data', input$d7date[1], 'to', input$d7date[2], '.csv')))
                observeEvent(input$d7sysadmincsvbutton, write_csv(userusage %>% filter(SysAdmin == 'Yes') %>% select(-c(UserType, SysAdmin)),
                                                                  path = paste('./csv/', today(), 'SysAdmin usage data', input$d7date[1], 'to', input$d7date[2], '.csv')))
        })
})