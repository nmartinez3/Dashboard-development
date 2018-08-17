###version number: v2.1.1

###run 'reneal install packages.R' first to install required packages

###SQL pull, main data set creation, and running data preparation scripts for all 5 dashboards
#clearing workspace
rm(list=ls())

#loading packages (the red text is OK and normal)
require(dplyr)
require(ggplot2)
require(shiny)
require(dbplyr)
require(DBI)
require(RMySQL)
require(rstudioapi)
require(lubridate)
require(readr)
require(stringr)
require(tidyr)
require(shinydashboard)

setwd(paste0('~/Reneal Dashboards/development/v2.1.1'))

#disconnecting existing open MySQL connections (if any)
message('SQL data import')
opencon <- dbListConnections(drv = MySQL())
if(length(opencon) > 0) lapply(opencon, dbDisconnect)

#connecting to "schooluse" database in MySQL server
con<-dbConnect(RMySQL::MySQL(),
                   host = 'localhost',
                   user = 'root',
                   password = askForPassword('Database password'))
dbSendQuery(con, 'USE schooluse')

###transfering tables from MySQL server to dataset inside R. lines that are commented out are not currently needed for dashboards
#clientcomputers <- tbl(con, 'clientcomputers') %>% collect()
#clientresourceuse <- tbl(con, 'clientresourceuse') %>% collect()
#cpuuse <- tbl(con, 'cpuuse') %>% collect()
#memoryuse <- tbl(con, 'memoryuse') %>% collect()
school <- tbl(con, 'school') %>% collect()
summarydata <- tbl(con, 'summarydata') %>% collect()
users <- tbl(con, 'users') %>% collect()
usersloggedin <- tbl(con, 'usersloggedin') %>% collect()

#data build date
data_creation_date <- today()

#formatting datasets
message('Data processing and transformation')
summarydata$SchoolID <- as.integer(summarydata$SchoolID)
summarydata$RecordIndex <- as.integer(summarydata$RecordIndex)
school <- school %>% rename(SchoolID = RecordIndex) %>% mutate(SchoolID = as.integer(SchoolID))

#joining school information to summarydata table; using inner join to exclude rows that do not have a valid SchoolID
main <- inner_join(summarydata, school, by = 'SchoolID')

#formatting time
main <- main %>% 
        mutate(Time = ifelse(Country == 'Tanzania', Time + 3*60*60, ifelse(Country == 'Philippines', Time + 8*60*60, NA)),
                        Time = as_datetime(Time),
                        Time = floor_date(Time, unit = 'minute')) %>%
        filter(Time >= as_datetime('2016-01-01 00:00:00 UTC'))  #filter to remove any entries w/ date/time prior to 2016-01-01 00:00:00 UTC

#creating data collection date
data_collection_date <- as_date(max(main$Time))

#adding quarter variable
main <- main %>% mutate(Quarter = quarter(Time),
                        Quarter = paste0('Q', as.character(Quarter)))

#future school year adjusted quarter
#main <- main %>% mutate(Quarter = ifelse(Country == 'Tanzania', quarter(Time),
#                                         ifelse(Country == 'Philippines', quarter(Time, fiscal_start = 6), NA)),
#                        Quarter = paste0('Q', as.character(Quarter)))

#adding year variable
main <- main %>% mutate(Year = as.character(year(Time)))

#adding month name variable
main <- main %>% mutate(Month = month(Time))

#rounding all times down to the nearest minute

#creating hour variable
main <- main %>% mutate(Hour = hour(Time))

#creating weekday variable
main <- main %>% mutate(Weekday = weekdays(Time, abbreviate = TRUE))

#creating YearMonth variable
main <- main %>% mutate(YearMonth = paste(Month, Year)) %>%
                arrange(Year, Month)
main$YearMonth <- factor(main$YearMonth, levels = unique(main$YearMonth))

#creating YearQuarter variable
main <- main %>% mutate(YearQuarter = paste(Quarter, Year)) %>%
                arrange(Year, Quarter)
main$YearQuarter <- factor(main$YearQuarter, levels = unique(main$YearQuarter))

#creating latest data collection date table
latestdata <- main %>% 
        group_by(Country, SchoolName) %>%
        summarise(LatestData = as_date(max(Time)))

#creating dataframe with # of valid weeks per month for each school
message('Importing "weeks/weeks.csv" and creating weeks dataframe')
schoolweeks <- suppressMessages(read_csv(file = '~/Reneal Dashboards/weeks/weeks.csv'))
schoolweeks <- schoolweeks %>% 
        gather(matches('[0-9]{1,2}\\/[0-9]{1,2}\\/[0-9]{4}'), key = 'Date', value = 'Weeks') %>% 
        select(School, Country, Date, Weeks) %>%
        filter(Country %in% c('Tanzania', 'Philippines')) %>%
        rename(SchoolName = School) %>%
        mutate(Weeks = ifelse(Weeks == '*','0',Weeks),
               Weeks = as.numeric(Weeks),
               Time = as_date(mdy(Date))) %>%
        select(-Date, -Country)
schoolweeks <- left_join(schoolweeks, latestdata, by = 'SchoolName') %>%
        filter(Time <= LatestData) %>%
        select(-LatestData, -Country)

#writing data to file
write_rds(main, path = './set up/main data/main.rds')
write_rds(schoolweeks, path = './deployment/data/schoolweeks.rds')
write_rds(data_creation_date, path = './deployment/data/data_creation_date.rds')
write_rds(data_collection_date, path = './deployment/data/data_collection_date.rds')
write_rds(latestdata, path = './deployment/data/latestdata.rds')


###=========================================================================
###  Version 2.0 architectural changes below this line (most dashboard data 
###  prep now takes place here):
###=========================================================================



###-------------------------------------------------------------------------
###Dashboard 1 data prep
message('dashboard 1 data prep')

###Monthly data sets

#creating data for valid weeks per month to be joined to student and teacher usage data
validweeksmonth <- schoolweeks %>%
        mutate(Month = month(Time),
               Year = as.character(year(Time))) %>%
        select(-Time)

#building active student data set to be used for graphing
studentmonth <- main %>% filter(ActiveStudentCount >= 1) %>%
        group_by(SchoolName, Country, Year, Month, YearMonth, Time) %>% 
        summarise(Minutes = 1,
                  UserMin = mean(ActiveStudentCount))
studentmonth <- studentmonth %>%
        group_by(SchoolName, Country, Year, Month, YearMonth) %>%
        summarise(Hours = sum(Minutes)/60,
                  UserHours = sum(UserMin)/60)

#adding per week metrics
studentmonth <- studentmonth %>% 
        left_join(validweeksmonth, by = c('SchoolName', 'Year', 'Month')) %>%
        mutate(AverageUsers = UserHours/Hours,
               HoursWeek = Hours/Weeks,
               UserHoursWeek = UserHours/Weeks)

#checking for discrepancy in naming convention between SQL database and weeks.csv file
if(studentmonth %>% filter(is.na(Weeks)) %>% nrow() > 0){
        SQLNamesNotInExcel <- studentmonth %>% filter(is.na(Weeks))
        SQLNamesNotInExcel <- paste(unique(SQLNamesNotInExcel$SchoolName), collapse = '   ')
        message(paste('Warning: The following school names appear in the SQL database but do not appear in "weeks.csv":', SQLNamesNotInExcel,
                      "   These schools' per week metrics will be NA", collapse = '  '))
}

#checking for Inf rows in per week metrics
student_inf_rows <- studentmonth %>% 
        filter(HoursWeek == Inf | UserHoursWeek == Inf) %>% 
        group_by(Country, SchoolName, Year, Month) %>%
        summarise(Bad = unique(paste0(SchoolName, ' ', Month, '/', Year)))
if(nrow(student_inf_rows) > 0){
        message(paste('Warning: ', nrow(student_inf_rows), ' rows of data for per week metrics for student usage have "Inf"'))
        for(i in 1:nrow(student_inf_rows)) message(student_inf_rows$Bad[i])
}

#building active teacher data to be used for graphing
teachermonth <- main %>% filter(ActiveTeacherCount >= 1) %>%
        group_by(SchoolName, Country, Year, Month, YearMonth, Time) %>% 
        summarise(Minutes = 1,
                  UserMin = mean(ActiveTeacherCount))
teachermonth <- teachermonth %>%
        group_by(SchoolName, Country, Year, Month, YearMonth) %>%
        summarise(Hours = sum(Minutes)/60,
                  UserHours = sum(UserMin)/60)

#adding per week metrics
teachermonth <- teachermonth %>% 
        left_join(validweeksmonth, by = c('SchoolName', 'Year', 'Month')) %>%
        mutate(AverageUsers = UserHours/Hours,
               HoursWeek = Hours/Weeks,
               UserHoursWeek = UserHours/Weeks)

#checking for discrepancy in naming convention between SQL database and weeks.csv file
if(teachermonth %>% filter(is.na(Weeks)) %>% nrow() > 0){
        SQLNamesNotInExcel <- teachermonth %>% filter(is.na(Weeks))
        SQLNamesNotInExcel <- paste(unique(SQLNamesNotInExcel$SchoolName), collapse = '   ')
        message(paste('Warning: The following school names appear in the SQL database but do not appear in "weeks.csv":', SQLNamesNotInExcel,
                      "   These schools' per week metrics will be NA", collapse = '  '))
}

#checking for Inf rows in per week metrics
teacher_inf_rows <- teachermonth %>% 
        filter(HoursWeek == Inf | UserHoursWeek == Inf) %>% 
        group_by(Country, SchoolName, Year, Month) %>%
        summarise(Bad = unique(paste0(SchoolName, ' ', Month, '/', Year)))
if(nrow(teacher_inf_rows) > 0){
        message(paste('Warning: ', nrow(teacher_inf_rows), ' rows of data for per week metrics for teacher usage have "Inf"'))
        for(i in 1:nrow(teacher_inf_rows)) message(teacher_inf_rows$Bad[i])
}

###Quarterly data sets

#creating data for valid weeks per quarter to be joined to student and teacher usage data
validweeksquarter <- schoolweeks %>%
        mutate(Quarter = quarter(Time),
               Quarter = paste0('Q', as.character(Quarter)),
               Year = as.character(year(Time))) %>%
        group_by(SchoolName, Year, Quarter) %>%
        summarise(Weeks = sum(Weeks))

#building active student data set to be used for graphing
studentquarter <- main %>% filter(ActiveStudentCount >= 1) %>%
        group_by(SchoolName, Country, Year, Quarter, YearQuarter, Time) %>% 
        summarise(Minutes = 1,
                  UserMin = mean(ActiveStudentCount))
studentquarter <- studentquarter %>% 
        group_by(SchoolName, Country, Year, Quarter, YearQuarter) %>%
        summarise(Hours = sum(Minutes)/60,
                  UserHours = sum(UserMin)/60)

#adding per week metrics
studentquarter <- studentquarter %>% left_join(validweeksquarter, by = c('SchoolName', 'Year', 'Quarter')) 
studentquarter <- studentquarter %>% 
        mutate(AverageUsers = UserHours/Hours,
               HoursWeek = Hours/Weeks,
               UserHoursWeek = UserHours/Weeks)

#building active teacher data to be used for graphing
teacherquarter <- main %>% filter(ActiveTeacherCount >= 1) %>%
        group_by(SchoolName, Country, Year, Quarter, YearQuarter, Time) %>% 
        summarise(Minutes = 1,
                  UserMin = mean(ActiveTeacherCount))
teacherquarter <- teacherquarter %>% 
        group_by(SchoolName, Country, Year, Quarter, YearQuarter) %>%
        summarise(Hours = sum(Minutes)/60,
                  UserHours = sum(UserMin)/60)

#adding per week metrics
teacherquarter <- teacherquarter %>% left_join(validweeksquarter, by = c('SchoolName', 'Year', 'Quarter')) 
teacherquarter <- teacherquarter %>% 
        mutate(AverageUsers = UserHours/Hours,
               HoursWeek = Hours/Weeks,
               UserHoursWeek = UserHours/Weeks)

#writing csv files to './csv'. these files are terminal and are not referenced again
studentquarter %>% arrange(Country, SchoolName, Year, Quarter) %>% 
        write_csv(path = paste0('./deployment/csv/student quarterly lifetime ', today(), '.csv'))
teacherquarter %>% arrange(Country, SchoolName, Year, Quarter) %>% 
        write_csv(path = paste0('./deployment/csv/teacher quarterly lifetime ', today(), '.csv'))
studentmonth %>% arrange(Country, SchoolName, Year, Month) %>% 
        write_csv(path = paste0('./deployment/csv/student monthly lifetime ', today(), '.csv'))
teachermonth %>% arrange(Country, SchoolName, Year, Month) %>% 
        write_csv(path = paste0('./deployment/csv/teacher monthly lifetime ', today(), '.csv'))

#writing month-year combinations w/ Inf for per week metrics to './csv' (if any)
if(nrow(student_inf_rows) > 0){
        message('...Writing student_inf_rows csv file to "./csv" folder')
        student_inf_rows %>% arrange(Country, SchoolName, Year, Month) %>% write_csv(path = paste0('./deployment/csv/student_inf_rows ', today(), '.csv'))
}
if(nrow(teacher_inf_rows) > 0){
        message('...Writing teacher_inf_rows csv file to "./csv" folder')
        teacher_inf_rows %>% arrange(Country, SchoolName, Year, Month) %>% write_csv(path = paste0('./deployment/csv/teacher_inf_rows ', today(), '.csv'))
}

#writing rds files to './data'. these files are referenced by the shiny app
write_rds(studentquarter, path = './deployment/data/d1studentquarter.rds')
write_rds(studentmonth, path = './deployment/data/d1studentmonth.rds')
write_rds(teacherquarter, path = './deployment/data/d1teacherquarter.rds')
write_rds(teachermonth, path = './deployment/data/d1teachermonth.rds')

#cleaning workspace
rm(student_inf_rows, teacher_inf_rows)


###-------------------------------------------------------------------------
###Dashboard 2 data prep
message('dashboard 2 data prep')

d2student <- main %>% filter(ActiveStudentCount >= 1) %>%
        group_by(Country, SchoolName, Weekday, Time) %>%
        summarise(Minutes = 1,
                  UserMin = mean(ActiveStudentCount)) %>%
        ungroup()
d2student <- d2student %>% mutate(FloorTime = as_date(floor_date(Time, unit = 'month'))) %>%
        select(-Time)
d2student <- d2student %>%
        group_by(Country, SchoolName, FloorTime, Weekday) %>%
        summarise(Hours = sum(Minutes)/60,
                  UserHours = sum(UserMin)/60) %>%
        ungroup()

d2teacher <- main %>% filter(ActiveTeacherCount >= 1) %>%
        group_by(Country, SchoolName, Weekday, Time) %>%
        summarise(Minutes = 1,
                  UserMin = mean(ActiveTeacherCount)) %>%
        ungroup()
d2teacher <- d2teacher %>% mutate(FloorTime = as_date(floor_date(Time, unit = 'month'))) %>%
        select(-Time)
d2teacher <- d2teacher %>%
        group_by(Country, SchoolName, FloorTime, Weekday) %>%
        summarise(Hours = sum(Minutes)/60,
                  UserHours = sum(UserMin)/60) %>%
        ungroup()

#writing .rds data files
write_rds(d2teacher, path = './deployment/data/d2teacher.rds')
write_rds(d2student, path = './deployment/data/d2student.rds')


###-------------------------------------------------------------------------
###Dashboard 3 data prep
message('dashboard 3 data prep')

d3student <- main %>% 
        filter(ActiveStudentCount >= 1) %>%
        group_by(Country, SchoolName, Hour, Time) %>%
        summarise(Minutes = 1,
                  UserMin = mean(ActiveStudentCount)) %>%
        ungroup()
d3student <- d3student %>% mutate(FloorTime = as_date(floor_date(Time, unit = 'month'))) %>%
        select(-Time)
d3student <- d3student %>% 
        group_by(Country, SchoolName, FloorTime, Hour) %>%
        summarize(Hours = sum(Minutes)/60,
                  UserHours = sum(UserMin)/60) %>%
        ungroup()

d3teacher <- main %>% 
        filter(ActiveTeacherCount >= 1) %>%
        group_by(Country, SchoolName, Hour, Time) %>%
        summarise(Minutes = 1,
                  UserMin = mean(ActiveTeacherCount)) %>%
        ungroup()
d3teacher <- d3teacher %>% mutate(FloorTime = as_date(floor_date(Time, unit = 'month'))) %>%
        select(-Time)
d3teacher <- d3teacher %>% 
        group_by(Country, SchoolName, FloorTime, Hour) %>%
        summarize(Hours = sum(Minutes)/60,
                  UserHours = sum(UserMin)/60) %>%
        ungroup()

#writing .rds data files
write_rds(d3teacher, path = './deployment/data/d3teacher.rds')
write_rds(d3student, path = './deployment/data/d3student.rds')


###-------------------------------------------------------------------------
###Dashboard 4 data prep
message('dashboard 4 data prep')

d4student <- main %>%
        filter(ActiveStudentCount >= 1) %>%
        mutate(FloorTime = floor_date(as_date(Time), unit = 'month')) %>%
        rename(NumUsers = ActiveStudentCount) %>%
        group_by(Country, SchoolName, FloorTime, NumUsers) %>%
        summarize(Hours = length(unique(Time))/60) %>%
        ungroup()

d4teacher <- main %>%
        filter(ActiveTeacherCount >= 1) %>%
        mutate(FloorTime = floor_date(as_date(Time), unit = 'month')) %>%
        rename(NumUsers = ActiveTeacherCount) %>%
        group_by(Country, SchoolName, FloorTime, NumUsers) %>%
        summarize(Hours = length(unique(Time))/60) %>% 
        ungroup()

#writing .rds data files
write_rds(d4teacher, path = './deployment/data/d4teacher.rds')
write_rds(d4student, path = './deployment/data/d4student.rds')


###-------------------------------------------------------------------------
###Dashboard 5 data prep
message('dashboard 5 data prep')

d5student <- main %>% 
        filter(ActiveStudentCount >= 1) %>%
        group_by(Country, SchoolName, Time) %>%
        summarize(Minutes = 1,
                  UserMin = mean(ActiveStudentCount)) %>%
        ungroup()

d5student <- d5student %>% mutate(FloorTime = floor_date(as_date(Time), unit = 'month'))
d5student <- d5student %>% 
        group_by(Country, SchoolName, FloorTime) %>%
        summarize(Hours = sum(Minutes)/60,
                  UserHours = sum(UserMin)/60) %>%
        ungroup()

d5teacher <- main %>% 
        filter(ActiveTeacherCount >= 1) %>%
        group_by(Country, SchoolName, Time) %>%
        summarize(Minutes = 1,
                  UserMin = mean(ActiveTeacherCount)) %>%
        ungroup()
d5teacher <- d5teacher %>% mutate(FloorTime = floor_date(as_date(Time), unit = 'month'))
d5teacher <- d5teacher %>% 
        group_by(Country, SchoolName, FloorTime) %>%
        summarize(Hours = sum(Minutes)/60,
                  UserHours = sum(UserMin)/60) %>%
        ungroup()

#writing .rds data files
write_rds(d5teacher, path = './deployment/data/d5teacher.rds')
write_rds(d5student, path = './deployment/data/d5student.rds')


###-------------------------------------------------------------------------
###Dashboard 6 data prep
message('dashboard 6 data prep')

tanzschools <- unique(main$SchoolName[main$Country == 'Tanzania'])
philschools <- unique(main$SchoolName[main$Country == 'Philippines'])

d6tanzweeks <- schoolweeks %>%
        filter(SchoolName %in% tanzschools) %>%
        mutate(Year = as.character(year(Time))) %>%
        group_by(Year) %>% 
        summarise(Weeks = sum(Weeks))

d6philweeks <- schoolweeks %>% 
        filter(SchoolName %in% philschools) %>%
        mutate(Year = as.character(year(Time))) %>%
        group_by(Year) %>%
        summarize(Weeks = sum(Weeks))

###student grand totals
tanzstudenttotals <- main %>%
        filter(ActiveStudentCount >= 1 & Country == 'Tanzania') %>%
        group_by(Year, Time) %>%
        summarise(Minutes = 1,
                  UserMin = mean(ActiveStudentCount))
tanzstudenttotals <- tanzstudenttotals %>%
        group_by(Year) %>%
        summarise(Hours = round(sum(Minutes)/60, digits = 1),
                  UserHours = round(sum(UserMin)/60, digits = 1)) %>%
        left_join(d6tanzweeks, by = 'Year')
tanzstudentlifetime <- tanzstudenttotals %>%
        mutate(Period = 'Lifetime') %>%
        group_by(Period) %>%
        summarise(Hours = sum(Hours),
                  UserHours = sum(UserHours),
                  Weeks = sum(Weeks)) %>%
        mutate(AverageUsers = round(UserHours/Hours, digits = 1),
               HoursPerWeek = round(Hours/Weeks, digits = 1),
               UserHoursPerWeek = round(UserHours/Weeks, digits = 1))
tanzstudentyearly <- tanzstudenttotals %>%
        mutate(AverageUsers = round(UserHours/Hours, digits = 1),
               HoursPerWeek = round(Hours/Weeks, digits = 1),
               UserHoursPerWeek = round(UserHours/Weeks, digits = 1)) %>%
        rename(Period = Year)
tanzstudenttotals <- bind_rows(tanzstudentlifetime, tanzstudentyearly)

philstudenttotals <- main %>%
        filter(ActiveStudentCount >= 1 & Country == 'Philippines') %>%
        group_by(Year, Time) %>%
        summarise(Minutes = 1,
                  UserMin = mean(ActiveStudentCount))
philstudenttotals <- philstudenttotals %>%
        group_by(Year) %>%
        summarise(Hours = round(sum(Minutes)/60, digits = 1),
                  UserHours = round(sum(UserMin)/60, digits = 1)) %>%
        left_join(d6philweeks, by = 'Year')
philstudentlifetime <- philstudenttotals %>%
        mutate(Period = 'Lifetime') %>%
        group_by(Period) %>%
        summarise(Hours = sum(Hours),
                  UserHours = sum(UserHours),
                  Weeks = sum(Weeks)) %>%
        mutate(AverageUsers = round(UserHours/Hours, digits = 1),
               HoursPerWeek = round(Hours/Weeks, digits = 1),
               UserHoursPerWeek = round(UserHours/Weeks, digits = 1))
philstudentyearly <- philstudenttotals %>%
        mutate(AverageUsers = round(UserHours/Hours, digits = 1),
               HoursPerWeek = round(Hours/Weeks, digits = 1),
               UserHoursPerWeek = round(UserHours/Weeks, digits = 1)) %>%
        rename(Period = Year)
philstudenttotals <- bind_rows(philstudentlifetime, philstudentyearly)

###teacher grand totals
tanzteachertotals <- main %>%
        filter(ActiveTeacherCount >= 1 & Country == 'Tanzania') %>%
        group_by(Year, Time) %>%
        summarise(Minutes = 1,
                  UserMin = mean(ActiveTeacherCount))
tanzteachertotals <- tanzteachertotals %>%
        group_by(Year) %>%
        summarise(Hours = round(sum(Minutes)/60, digits = 1),
                  UserHours = round(sum(UserMin)/60, digits = 1)) %>%
        left_join(d6tanzweeks, by = 'Year')
tanzteacherlifetime <- tanzteachertotals %>%
        mutate(Period = 'Lifetime') %>%
        group_by(Period) %>%
        summarise(Hours = sum(Hours),
                  UserHours = sum(UserHours),
                  Weeks = sum(Weeks)) %>%
        mutate(AverageUsers = round(UserHours/Hours, digits = 1),
               HoursPerWeek = round(Hours/Weeks, digits = 1),
               UserHoursPerWeek = round(UserHours/Weeks, digits = 1))
tanzteacheryearly <- tanzteachertotals %>%
        mutate(AverageUsers = round(UserHours/Hours, digits = 1),
               HoursPerWeek = round(Hours/Weeks, digits = 1),
               UserHoursPerWeek = round(UserHours/Weeks, digits = 1)) %>%
        rename(Period = Year)
tanzteachertotals <- bind_rows(tanzteacherlifetime, tanzteacheryearly)

philteachertotals <- main %>%
        filter(ActiveTeacherCount >= 1 & Country == 'Philippines') %>%
        group_by(Year, Time) %>%
        summarise(Minutes = 1,
                  UserMin = mean(ActiveTeacherCount))
philteachertotals <- philteachertotals %>%
        group_by(Year) %>%
        summarise(Hours = round(sum(Minutes)/60, digits = 1 ),
                  UserHours = round(sum(UserMin)/60, digits = 1)) %>%
        left_join(d6philweeks, by = 'Year')
philteacherlifetime <- philteachertotals %>%
        mutate(Period = 'Lifetime') %>%
        group_by(Period) %>%
        summarise(Hours = sum(Hours),
                  UserHours = sum(UserHours),
                  Weeks = sum(Weeks)) %>%
        mutate(AverageUsers = round(UserHours/Hours, digits = 1),
               HoursPerWeek = round(Hours/Weeks, digits = 1),
               UserHoursPerWeek = round(UserHours/Weeks, digits = 1))
philteacheryearly <- philteachertotals %>%
        mutate(AverageUsers = round(UserHours/Hours, digits = 1),
               HoursPerWeek = round(Hours/Weeks, digits = 1),
               UserHoursPerWeek = round(UserHours/Weeks, digits = 1)) %>%
        rename(Period = Year)
philteachertotals <- bind_rows(philteacherlifetime, philteacheryearly)

#writing .rds files
write_rds(philstudenttotals, path = './deployment/data/d6philstudent.rds')
write_rds(philteachertotals, path = './deployment/data/d6philteacher.rds')
write_rds(tanzstudenttotals, path = './deployment/data/d6tanzstudent.rds')
write_rds(tanzteachertotals, path = './deployment/data/d6tanzteacher.rds')


###-------------------------------------------------------------------------
###Dashboard 7 data prep
message('dashboard 7 data prep')

d7 <- usersloggedin %>% filter(UserActive == 1)
d7 <- inner_join(usersloggedin, users, by = c('UserRecordIndex' = 'RecordIndex')) %>%
        select(-c(RecordIndex, UserRecordIndex, Uid))
d7 <- d7 %>% rename(SchoolID = SchoolID.x) %>%
        select(-SchoolID.y) %>%
        filter(UserType != 'Student')
d7 <- left_join(d7, school, by = 'SchoolID') %>%
        select(-c(SchoolID, UserActive, MbytesMemory, CPU)) %>%
        mutate(Time = as_datetime(Time)) %>% 
        filter(Time >= as_date('2016-01-01'))
d7 <- d7 %>% mutate(FloorTime = as_date(floor_date(Time, unit = 'month'))) %>%
        select(Time, FloorTime, everything())
d7 <- d7 %>% group_by(Country, SchoolName, UserType, UserName, FloorTime) %>% 
        summarize(Hours = round(length(unique(Time))/60, digits = 1)) %>% 
        arrange(FloorTime) %>%
        ungroup()

d7schoolnames <- sort(unique(d7$SchoolName))
d7schoolnamesTZ <- sort(unique(d7$SchoolName[d7$Country == 'Tanzania']))

#writing data files
write_rds(d7, path = './deployment/data/d7.rds')
write_rds(d7schoolnames, path = './deployment/data/d7schoolnames.rds')


###-------------------------------------------------------------------------
###Auto scaling calculations
message('auto scaling calculations')

n_schools_PI_student <- main %>% filter(Country == 'Philippines' & ActiveStudentCount >= 1) %>% select(SchoolName) %>% unique() %>% nrow()
graph_rows_PI_student <- (n_schools_PI_student %/% 3) + ifelse(n_schools_PI_student %% 3 > 0, 1, 0)
n_schools_TZ_student <- main %>% filter(Country == 'Tanzania' & ActiveStudentCount >= 1) %>% select(SchoolName) %>% unique() %>% nrow()
graph_rows_TZ_student <- (n_schools_TZ_student %/% 3) + ifelse(n_schools_TZ_student %% 3 > 0, 1, 0)
n_schools_PI_teacher <- main %>% filter(Country == 'Philippines' & ActiveTeacherCount >= 1) %>% select(SchoolName) %>% unique() %>% nrow()
graph_rows_PI_teacher <- (n_schools_PI_teacher %/% 3) + ifelse(n_schools_PI_teacher %% 3 > 0, 1, 0)
n_schools_TZ_teacher <- main %>% filter(Country == 'Tanzania' & ActiveTeacherCount >= 1) %>% select(SchoolName) %>% unique() %>% nrow()
graph_rows_TZ_teacher <- (n_schools_TZ_teacher %/% 3) + ifelse(n_schools_TZ_teacher %% 3 > 0, 1, 0)

n_schools_d7 <- d7 %>% select(SchoolName) %>% unique() %>% nrow()
graph_rows_d7 <- (n_schools_d7 %/% 6) + ifelse(n_schools_d7 %% 6 > 0, 1, 0)
plot_height_d7 <- 54 + graph_rows_d7 * 130

n_schools_d7_TZ <- d7 %>% filter(Country == 'Tanzania') %>% select(SchoolName) %>% unique() %>% nrow()
graph_rows_d7_TZ <- (n_schools_d7_TZ %/% 6) + ifelse(n_schools_d7_TZ %% 6 > 0, 1, 0)
plot_height_d7_TZ <- 54 + graph_rows_d7_TZ * 130

plot_height <- list(
        'Philippines_student' = 54 + graph_rows_PI_student * 182,
        'Tanzania_student' = 54 + graph_rows_TZ_student * 182,
        'Philippines_teacher' = 54 + graph_rows_PI_teacher * 182,
        'Tanzania_teacher' = 54 + graph_rows_TZ_teacher * 182,
        'd7' = plot_height_d7
)

plot_height_TZ <- list(
        'Tanzania_student' = 54 + graph_rows_TZ_student * 182,
        'Tanzania_teacher' = 54 + graph_rows_TZ_teacher * 182,
        'd7' = plot_height_d7_TZ
)

write_rds(plot_height, path = './deployment/data/plot_height.rds')


###=========================================================================


#done!
message('Done!')
message('Please open either "/dashboard/ui.R" or "/dashboard/server.R" (or both) and click "Run App" on either file to launch the dashboard')