

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
#     a.) Open and run the project3.r script

--
# issues, loos like pushing frame from r changes the schema
#################################################################################

drop schema if exists project3;
create schema project3;
use project3;

DROP TABLE IF EXISTS skill_rankings;
DROP TABLE IF EXISTS skill_types;
DROP TABLE IF EXISTS companies;

-- equiv to movie types
-- select * from companies;
CREATE TABLE `skill_types`
(   
	`skill_id` varchar(100) NOT NULL, 
    `skill_name` varchar(100) NOT NULL,
    PRIMARY KEY (skill_id)
);

select * from skill_types;

-- equiv to names
CREATE TABLE `companies`
(   
    `companyid`  varchar(100) NOT NULL, 
    `company_name` varchar(100) NOT NULL,    
    primary key (companyid)
);
select * from companies;
show full columns from companies;
describe companies;

-- define skill rankings
CREATE TABLE `skill_rankings`
(  
	`skill_frequency` int NOT NULL, -- how often the skills comes up in the job description
    `companyid` varchar(100) NOT NULL,
    `skill_id` varchar(100) NOT NULL,
     primary key ( companyid, skill_id),
     foreign key (companyid)
		references companies(companyid),
	 foreign key (skill_id)
		references skill_types(skill_id) );

select * from skill_rankings;
   

