######################################
"DEMOGRAPHICS:"
"EXPLORING DEATHS"
######################################

library(DBI)
library(odbc)
library(dbplyr)
library(stringr)
library(tidyverse)
library(here)
library(readxl)
library(janitor)

"
I was unable to find a breakdown of historical deaths by age and gender for England.
This lead me to investigate the potential of the mortality tables on CSU-SQL-03 (notably, the 
'derived age at death' field) to provide these figures, and to verify SW's assessment. 

There seemed to be some discrepancies between the ONS reported figures and the 
figures derived from the mortality tables - especially for < 1s. 

Presume this is due to infant deaths not having hospital records from which to derive
other data

LOOKING AT THE YEAR 2014:

"

# 1. Establish Connection --------------------------------------------

# Create a connection object using DBI::dbconnect and odbc:
connection <- dbConnect(odbc::odbc(),
                        driver = "SQL Server",
                        server = "CSU-SQL-03",
                        database = "ONS", 
                        port = 1433 # always 1433
)


# 2. Query -----------------------------------------------------------

query_deaths <- "
-- 
--. 

SELECT SEX [sex]
  , DerivedAge [derived_age]
  , COUNT(encrypted_hesid) as [deaths]

FROM ONS.HESONS.tbMortalityto1516

WHERE DOR >= '2014-01-01'
  AND DOR  < '2015-01-01'

GROUP BY SEX, DerivedAge

ORDER BY SEX, DerivedAge
"

# 3. Execute query ---------------------------------------------------

deaths_derived <-  dbGetQuery(connection, query_deaths)

saveRDS(deaths_derived, here("data", "deaths_derived.RDS"))
read_rds(here("data", "deaths_derived.RDS"))

deaths_derived <- deaths_derived %>% 
  mutate(age = ifelse(derived_age > 104, 105, derived_age)) %>% 
  group_by(sex, age) %>% 
  summarise(deaths = sum(deaths))


# 4. Official deaths -------------------------------------------------

"Includes deaths in WALES"

deaths_m_official  <- read_excel("data/deathsyoauk2016tablesfinal (1).xls",
                                         sheet = "Table 3", skip = 3) %>% 
  clean_names() %>% 
  select(age, x_2014) %>% 
  slice(1:106) %>% 
  mutate(age = str_replace(.$age, "\\+", ""),
         sex = 1) 
  

deaths_f_official  <- read_excel("data/deathsyoauk2016tablesfinal (1).xls",
                                 sheet = "Table 4", skip = 3) %>% 
  clean_names() %>% 
  select(age, x_2014) %>% 
  slice(1:106) %>% 
  mutate(age = str_replace(deaths_f_official$age, "\\+", ""),
         sex = 2) 

deaths_official <- bind_rows(deaths_m_official, deaths_f_official) %>% 
  mutate_all(funs(as.numeric))


# 5. Compare and analyse ---------------------------------------------

compare <- left_join(test, deaths_official, by = c("age", "sex")) %>% 
  mutate(prop = deaths/x_2014)

partial_sum <- partial(sum, na.rm = T)                          

compare %>% summarise_all(funs(partial_sum)) %>% 
  # mutate(prop = deaths/x_2014)

compare %>% filter(is.na(age))
            
ggplot(compare, aes(prop))+
  geom_histogram()

# Use Table 11 in 'tables14and614_tcm...xls' to see Wales deaths.

# male deaths derived    = 231.5k               
# male deaths wales      = 15k
# male deaths (age = NA) = 2k

# male official deaths = 245k 

# NOTE: RARE FOR THOSE WITH WELSH LSOA TO BE IN MORTALITY DB (FROM
# SW derived mortality with LSOA)
