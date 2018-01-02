SELECT 
  [fyear]
  , [age_group]
  , cte.[gender]
  --, [ccg_code]
  --, pop.[population]
  --,  SUM(pop.[population]) as [population]
  ,  COUNT([fyear]) as [admissions]
FROM (

SELECT --- top 10000
'201415' as fyear 
, CASE
WHEN (startage between 7001 and 7007) 
THEN N'00to04'
WHEN startage > 89
THEN '90plus' 
WHEN startage is null
THEN NULL
ELSE RIGHT('00' + ISNULL(CAST(FLOOR(startage / 5) * 5 as nvarchar(2)), ''), 2) + 
'to' + RIGHT('00' + ISNULL(CAST((FLOOR(startage / 5) * 5) + 4 as nvarchar(2)), ''), 2) 
END as [age_group] 
, CASE ip.sex
WHEN '1' THEN 'M'
WHEN '2' THEN 'F'
END  as [gender]
, CCGCODE as [ccg_code]

FROM
[HESData].dbo.tbInpatients1415 ip
left outer join [StrategicReference].dbo.vwGPPracticeToCCGAndPCT ccg 
on ip.gpprac = ccg.GPPractice 

WHERE 
1 = 1 
AND epiorder = 1
AND admimeth LIKE '2%' -- EMERGENCY ADMISSIONS​
) cte

--LEFT OUTER JOIN 
-- (
--  SELECT 
--    Population as [population]
--, AgeGroup
--, Gender
--, CCGCode
--  FROM StrategicReference.dbo.tbCCGPopEstimates_UPDATED_2017_03
--  WHERE Year = 2014 
--  ) pop 
--    ON cte.age_group  = pop.AgeGroup COLLATE database_default
--      AND cte.gender   = pop.Gender   COLLATE database_default
--      AND cte.ccg_code = pop.CCGCode  COLLATE database_default

GROUP BY 
  [fyear]
  , [age_group]
  , cte.[gender]
  ---, [ccg_code]
  ---, pop.[population]
  ---, SUM(pop.[population])

  ORDER BY
    cte.gender
   , age_group
    ​

​