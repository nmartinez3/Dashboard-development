# Run the dashboard yourself!!!

Run the dashboard: https://nmartinez3.shinyapps.io/deployment/

Yeah!!!

## What is this?

This is a dashboard that I made for a non-profit that takes old computers from the US and installs them in computer labs in the Philippines and Tanzania.

# Business motivation

The non-profit had tons of computer usage data for all of their schools in an SQL database they were maintaining, and they wanted to look at the data to understand which schools were taking greater advantage of the system, which were taking less, which were abusing the system (extremely high teacher use with low student use, meaning teacher is hanging out on the computers all the time but students aren't taking advantage of the computer lab), and how their different usage metrics were changing over time. To do this, the non-profit was using excel spreadsheets to generate graphs of the data coming out of the SQL database, but it was super clunky, took lots of time to tweak and maintain each time the graphs were generated, couldn't visualize all of the views they wanted of the data, and didn't make it possible to have all of their visualizations in one place, all with the same 5 metrics they were interested, and all with the same user input filters to control the output of the visualizations (hmmm that sounds like some of the specs for my dashboard!). Basically, their existing system took a lot of time and didn't give them the desired level of insight/multi-dimensionality they wanted for viewing their data.

# The dashboard

So, to solve this issue, I sat down with the non-profit cofounders at the beginning of this year (Dec 2017/Jan 2018) to understand the issues they were having and what they really wanted if they could have a dream visualization suite with all of the views and metrics they desired. We settled on an initial 5 dashboards (all to be contained in the same Shiny app - this expanded to 7 in total over time), each covering a different view and including different user-selectable filters. I'll give a quick rundown of each dashboard below (including all 7 that are currently in the dashboard) as well as the data processing that was involved to take the data from SQL and prepare it in R for use in the Shiny app. At the end of the write-up, I'll give a description of some of the challenges I faced with this project, what I learned from them, and other code highlights for this project. I'm currently looking for a job as a data analyst in a for-profit company, so if this readme file is unfinished when you come to visit it, I apologize about that and you have my word that I will resume working on it when I find the time. Thanks for reading!

