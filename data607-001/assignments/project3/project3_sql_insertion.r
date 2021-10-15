# project3 frame insertion into SQL piece


# Assumptions:
# ------------
# * Windows Machine
# * MySQL Workbench 8.0
#   - there exists a user: 'root' &  server: 'localhost' with password=''. No password required.
# * RStudio installed

# Instructions:
# -------------
# 0.  git clone *.git or retrieve raw files from https://github.com/quaere1verum/sps...
# 1.  Start MySQL Workbench
#     a.) If your MySQL service does not start automatically when Windows starts, press Windows key and search for "Services", press enter.
#         Search for MySQL80 and start the service.
#     b.) Open the file project3.sql and run it. 
# 2.  Start RStudio
#     a.) Open and run the project3_code.r script
#################################################################################



# install required packages and load libraries
#install.packages("DBI")
library(DBI)

#install.packages("RMySQL")
library(RMySQL)

#install.packages("tidyverse")
library(tidyverse)


# create connection to DB 
con <- dbConnect(MySQL(), user='root', dbname='project3', host='localhost')

# set global local_infile=true;
dbSendQuery(con, "set global local_infile=true")

# for some reason if I pass 3 to company id it casts to double and tibble dtype and schemas neeed to match
x <- tibble('companyid'= '3', #as.integer(3),
            'company_name'='Facebook',
            'industry'='technology')

# trying tmp tables
dbWriteTable(con,"myTempTable", x)
dbExecute(con,"insert into companies(companyid, company_name, industry) select companyid, company_name, industry from myTempTable")
dbExecute(con,"drop table if exists myTempTable")
dbExecute(con,"commit;")


# need to populate tables before the skill_rankings table
# dbWriteTable(conn = con, 
#             name = "companies", 
#             overwrite=TRUE,
#             value = x)  ## x is any data frame

companies <- dbSendQuery(con, "select * from companies")
data <- fetch(companies, n=-1)
data
companies <- dbSendQuery(con, "describe companies")
data <- fetch(companies, n=-1)
data



y <- tibble('skill_id'=55,
            'skill_name'='python',
            'description'='python language proficiency')

# trying tmp tables
dbWriteTable(con,"myTempTable", y)
dbExecute(con,"insert into skill_types(skill_id, skill_name, description) select skill_id, skill_name, description from myTempTable")
dbExecute(con,"drop table if exists myTempTable")
dbExecute(con,"commit;")


#dbWriteTable(conn = con, 
#             name = "skill_types", 
#             overwrite=TRUE,
#              value = y)  ## x is any data frame
skill_types <- dbSendQuery(con, "select * from skill_types")
data <- fetch(skill_types, n=-1)
data




skill_rankings_create_sql <- 'CREATE TABLE `skill_rankings`
(   `skill_frequency` int NOT NULL,
    `companyid` varchar(100) NOT NULL,
    `skill_id` varchar(100) NOT NULL,
     primary key( companyid, skill_id),
     foreign key(companyid) references companies(companyid),
	  foreign key (skill_id)  references skill_types(skill_id) )'
dbSendQuery(con, skill_rankings_create_sql)

