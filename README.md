# Read me!
Hey guys! In this repository is a dashboard I created for a non-profit that sources old computers from the US destined for the junk bin and takes them to schools in Tanzania and the Philippines to be set up as computer labs for students there.

### The dashboard (run it yourself!):
The dashboard can be found in its entirety in the v2.X.X/deployment/ folder. To run the dashboard yourself (the most recent version with the full data set!), do the following:
1) Make sure both R and R Studio are installed on your machine
2) Download the entire "deployment" folder as is (it can be placed anywhere on your machine; does not need to be downloaded to a certain location)
3) Open up either 'ui.R' or 'server.R' in R Studio (either one works fine)
4) Click the 'Run App' button in the top-right corner of the script editing area
5) Play around and have fun!

Below I'll talk about the business needs and the project in a little more detail (as well as including screenshots of the dashboard).

### The business needs and the dashboard
So basically, the non-profit had a ton of SQL data that was being collected on each active computer for every second that each computer was in use (~470 computers total currently), and they wanted to look at all of this usage data and use it to understand which schools were taking greater advantage of their computer labs relative to others. However, the non-profit was trying to do this analysis with excel files at the time, and the process was messy, took a lot of time out of the non-profit owners' schedules, and did not offer the full dimensionality and filtering options that they wanted to be able to visualize the data thoroughly and with different views over time. To solve this problem, I worked with the non-profit to define an initial series of 5 dashboards (all contained within the same Shiny app; eventually grew to 7 dashboards in total, again in the same Shiny app) and created them in R with Shiny.

#### Data flow
To create the app, I took a dump (terrible, I know) of the non-profit's SQL data onto my own machine and into my own instance of MySQL (to replicate the work setup they would be operating with) and then built an R data prep script to connect to the SQL server, pull in all of the relevant data, process it and prepare it into the right format for the dashboards, and then write the data for the dashboard as .rds files into the directory where the Shiny app was to be located. The Shiny app was created in conjunction with the data prep script. Feel free to look at the code for 'ui.R' and 'server.R' for the app/dashboard code and 'data refresh.R' for the data prep script's code.

#### Quick Overview of Each Dashboard
Coming soon!
