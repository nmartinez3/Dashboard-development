###version number: v2.1.2

###run 'data refresh.R' first to prepare data sets used by this app

#loading packages
require(lubridate)
require(shiny)
require(shinydashboard)

#UI for shiny app
sidebar <- dashboardSidebar(
        sidebarMenu(
                menuItem('D1: Lifetime Usage', tabName = 'dashboard1'),
                menuItem('D2: Usage by Weekday', tabName = 'dashboard2'),
                menuItem('D3: Usage by Time of Day', tabName = 'dashboard3'),
                menuItem('D4: Usage by Number of Users', tabName = 'dashboard4'),
                menuItem('D5: School Comparison', tabName = 'dashboard5'),
                menuItem('D6: Grand Totals', tabName = 'dashboard6'),
                menuItem('D7: User Usage Data', tabName = 'dashboard7')
        )
)

body <- dashboardBody(
        tabItems(
                tabItem(tabName = 'dashboard1',
                        fluidRow(
                                box(title = 'Inputs', width = 3, status = 'primary', solidHeader = TRUE,
                                    selectInput('d1country', 'Country', c('Tanzania',
                                                                          'Philippines')),
                                    selectInput('d1user', 'User', c('Student' = 'student',
                                                                    'Teacher' = 'teacher')),
                                    selectInput('d1timeperiod', 'Time Period', c('Quarterly' = 'Quarterly',
                                                                                 'Monthly' = 'Monthly')),
                                    selectInput('d1metric', 'Metric', c('Hours' = 'Hours',
                                                                        'User Hours' = 'UserHours',
                                                                        'Average Users' = 'AverageUsers',
                                                                        'Hours per Week' = 'HoursWeek',
                                                                        'User Hours per Week' = 'UserHoursWeek'))
                                ),
                                box(title = 'Dashboard 1: Lifetime Usage', width = 9, status = 'primary', solidHeader = TRUE,
                                    plotOutput('d1plot', height = '100%')
                                )
                        )
                ),
                tabItem(tabName = 'dashboard2',
                        fluidRow(
                                box(title = 'Inputs', width = 3, status = 'primary', solidHeader = TRUE,
                                    selectInput('d2country', 'Country', c('Tanzania',
                                                                          'Philippines')),
                                    selectInput('d2user', 'User', c('Student' = 'student',
                                                                    'Teacher' = 'teacher')),
                                    selectInput('d2metric', 'Metric', c('Hours' = 'Hours',
                                                                        'User Hours' = 'UserHours',
                                                                        'Average Users' = 'AverageUsers',
                                                                        'Hours per Week' = 'HoursWeek',
                                                                        'User Hours per Week' = 'UserHoursWeek')),
                                    dateRangeInput('d2date', 'Date Range (YYYY-MM)', format = 'yyyy-mm', start = '2016-01-01', end = today(), min = '2016-01-01', max = today()),
                                    actionButton('d2csvbutton', 'Download .csv')
                                ),
                                box(title = 'Dashboard 2: Usage per Weekday', width = 9, status = 'primary', solidHeader = TRUE,
                                    plotOutput('d2plot', height = '100%')
                                )
                        )
                ),
                tabItem(tabName = 'dashboard3',
                        fluidRow(
                                box(title = 'Inputs', width = 3, status = 'primary', solidHeader = TRUE,
                                    selectInput('d3country', 'Country', c('Tanzania',
                                                                          'Philippines')),
                                    selectInput('d3user', 'User', c('Student' = 'student',
                                                                    'Teacher' = 'teacher')),
                                    selectInput('d3metric', 'Metric', c('Hours' = 'Hours',
                                                                        'User Hours' = 'UserHours',
                                                                        'Average Users' = 'AverageUsers',
                                                                        'Hours per Week' = 'HoursWeek',
                                                                        'User Hours per Week' = 'UserHoursWeek')),
                                    dateRangeInput('d3date', 'Date Range (YYYY-MM)', format = 'yyyy-mm', start = '2016-01-01', end = today(), min = '2016-01-01', max = today()),
                                    actionButton('d3csvbutton', 'Download .csv')
                                ),
                                box(title = 'Dashboard 3: Usage by Time of Day', width = 9, status = 'primary', solidHeader = TRUE,
                                    plotOutput('d3plot', height = '100%')
                                )
                        )
                ),
                tabItem(tabName = 'dashboard4',
                        fluidRow(
                                box(title = 'Inputs', width = 3, status = 'primary', solidHeader = TRUE,
                                    selectInput('d4country', 'Country', c('Tanzania',
                                                                          'Philippines')),
                                    selectInput('d4user', 'User', c('Student' = 'student',
                                                                    'Teacher' = 'teacher')),
                                    dateRangeInput('d4date', 'Date Range (YYYY-MM)', format = 'yyyy-mm', start = '2016-01-01', end = today(), min = '2016-01-01', max = today()),
                                    sliderInput('d4cutoff', 'Y-axis Cutoff', min = 50, max = 450, value = 250, step = 5),
                                    actionButton('d4csvbutton', 'Download .csv')
                                ),
                                box(title = 'Dashboard 4: Usage per Number of Users', width = 9, status = 'primary', solidHeader = TRUE,
                                    plotOutput('d4plot', height = '100%')
                                )
                        )
                ),
                tabItem(tabName = 'dashboard5',
                        fluidRow(
                                box(title = 'Inputs', width = 3, status = 'primary', solidHeader = TRUE,
                                    selectInput('d5country', 'Country', c('Tanzania',
                                                                          'Philippines')),
                                    selectInput('d5user', 'User', c('Student' = 'student',
                                                                    'Teacher' = 'teacher')),
                                    selectInput('d5metric', 'Metric', c('Hours' = 'Hours',
                                                                        'User Hours' = 'UserHours',
                                                                        'Average Users' = 'AverageUsers',
                                                                        'Hours per Week' = 'HoursWeek',
                                                                        'User Hours per Week' = 'UserHoursWeek')),
                                    dateRangeInput('d5date', 'Date Range (YYYY-MM)', format = 'yyyy-mm', start = '2016-01-01', end = today(), min = '2016-01-01', max = today()),
                                    actionButton('d5csvbutton', 'Download .csv')
                                ),
                                box(title = 'Dashboard 5: School Comparison', width = 9, status = 'primary', solidHeader = TRUE,
                                    plotOutput('d5plot', height = '600px')
                                )
                        )
                ),
                tabItem(tabName = 'dashboard6',
                        headerPanel(title = 'Grand Totals'),
                        fluidRow(
                                box(title = 'Dashboard 6: Philippines Student Grand Totals', width = 10, status = 'primary', solidHeader = TRUE,
                                    DT::dataTableOutput('d6philstudent')
                                )
                        ),
                        fluidRow(
                                box(title = 'Dashboard 6: Tanzania Student Grand Totals', width = 10, status = 'warning', solidHeader = TRUE,
                                    DT::dataTableOutput('d6tanzstudent')
                                )
                        ),
                        fluidRow(
                                box(title = 'Dashboard 6: Philippines Teacher Grand Totals', width = 10, status = 'primary', solidHeader = TRUE,
                                    DT::dataTableOutput('d6philteacher')
                                )
                        ),
                        fluidRow(
                                box(title = 'Dashboard 6: Tanzania Teacher Grand Totals', width = 10, status = 'warning', solidHeader = TRUE,
                                    DT::dataTableOutput('d6tanzteacher')
                                )
                        ),
                        headerPanel(title = 'Latest Data'),
                        fluidRow(
                                box(title = 'Dashboard 6: Latest Data Collection Date', width = 10, status = 'primary', solidHeader = TRUE,
                                    DT::dataTableOutput('d6latestdata')
                                )
                        )
                ),
                tabItem(tabName = 'dashboard7',
                        fluidRow(
                                box(title = 'Inputs', width = 3, status = 'primary', solidHeader = TRUE,
                                    selectInput('d7schoolname', 'School Name', 'placeholder'),
                                    dateRangeInput('d7date', 'Date Range (YYYY-MM)', format = 'yyyy-mm', start = '2016-01-01', end = today(), min = '2016-01-01', max = today()),
                                    selectInput('d7plotmetric', 'Plot Metric', c('Hours' = 'Hours',
                                                                                 'Hours per Week' = 'HoursPerWeek')),
                                    actionButton('d7userusagecsvbutton', 'Download User Usage .csv'),
                                    actionButton('d7sysadmincsvbutton', 'Download SysAdmin Usage .csv')
                                ),
                                box(title = 'Dashboard 7: User Usage Data', width = 7, status = 'primary', solidHeader = TRUE,
                                    DT::dataTableOutput('d7userusage')
                                )
                        ),
                        uiOutput('d7zero_rows'),
                        fluidRow(
                                box(title = 'Dashboard 7: SysAdmin Plot', width = 10, status = 'primary', solidHeader = TRUE,
                                    plotOutput('d7plot', height = '100%')
                                )
                        ),
                        fluidRow(
                                box(title = 'Dashboard 7: SysAdmin Usage', width = 7, status = 'primary', solidHeader = TRUE,
                                    DT::dataTableOutput('d7sysadmintable'))
                        )
                )
        )
)

dashboardPage(dashboardHeader(title = 'Reneal Dashboards 2.1.2', titleWidth = '260px'), sidebar, body)