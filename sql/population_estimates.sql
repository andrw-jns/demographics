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
 
 FROM
 ( SELECT 
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
  