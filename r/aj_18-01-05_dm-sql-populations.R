####################
"DEMOGRAPHICS:"
"PULL PROJECTION DATA FROM SQL"
####################

"NEEDS UPDATING IN LINE WITH NEW SQL PROX.. QUERY"

library(DBI)
library(odbc)
library(dbplyr)
library(stringr)
library(tidyverse)
library(here)


# 1. Establish Connection --------------------------------------------

# Create a connection object using DBI::dbconnect and odbc:
connection <- dbConnect(odbc::odbc(),
                        driver = "SQL Server",
                        server = "CSU-SQL-03",
                        database = "HESData", 
                        port = 1433 # always 1433
                        )

# 2. Queries -------------------------------------------------------

query_pop_estimates <- "
/* 
National Population Figures by Year, Age, Gender 

Note: year = 2006, is fyear 2006/07 
Should be able to update this now for 2016 estimates
*/


SELECT
  [year]
  , age_group
  , gender
  , sum([population]) [population]
  
FROM (
      SELECT

        [year] as [year]
        ,Population as [population]
        , AgeGroup as [age_group]
        , Gender [gender]
        --, CCGCode 

      FROM StrategicReference.dbo.tbCCGPopEstimates_UPDATED_2017_03
        WHERE 
        -- Year = 2014 AND 
        Year > 2004 AND
        AgeGroup != 'AllAges'
        AND Gender   != 'P'
        AND LEN(agegroup) >2
        AND CCGCode is not null -- for years pre 2014 (no CCG)
        AND AgeGroup != '85plus' -- for years pre 2014 (there is an 85plus AND 90plus group)
  
       ) cte

Group by 
year
, age_group
, gender
--, sum(population)

order by 
year
, gender
, age_group

"


query_pop_projections <- "/****** Population Projections  ******/
SELECT [AgeGroup]        as [age_group]
      ,[Gender]          as [gender]
      ,[Year]            as [year]
      ,SUM([Population]) as [population]

FROM

  (
    SELECT 
      [CCGCode]
      ---,[ONSCode]
      ---,[CCGDescription]
      ,[AgeGroup]
      ,[Gender]
      ,[Year]
      ,[Population]
    FROM [StrategicReference].[dbo].[tbCCGPopProjections0516]
  
  Where year >= 2017 AND
  AgeGroup != 'Allages' AND
  Gender != 'P' AND
  LEN(AgeGroup) >2
  
  ) cte
  
GROUP BY 
  [AgeGroup]
  ,[Gender]
  ,[Year]
  
ORDER BY
  Year 
  ,Gender
  ,AgeGroup
"

# 3. Run Queries -------------------------------------------------------


db_pop_estimates   <- dbGetQuery(connection, query_pop_estimates)
db_pop_projections <- dbGetQuery(connection, query_pop_projections)          

# ip_data_2 <- ip_data %>% reduce(bind_rows) 
saveRDS(db_pop_estimates, here("data", "AJ_2018-01-05_pop_est.RDS"))
saveRDS(db_pop_projections, here("data", "AJ_2018-01-05_pop_proj.RDS"))

# getwd()

# testing <- read_rds(here("data", "ip_data.RDS"))
