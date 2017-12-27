--select TOP 10 * 
--from [StrategicReference].dbo.vwGPPracticeToCCGAndPCT

SELECT *, 
  COUNT([fyear]) as [admissions]
FROM (

SELECT top 10
'201415' as fyear 
, CASE
WHEN startage between 7001 and 7007 
THEN N'00to04'
WHEN startage > 89
THEN '90plus' 
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
-- left outer join [HESData].dbo.tbMFF mff 
-- on mff.FinancialYear = 201415
-- and left(ip.procode,3) = mff.ProviderCode 
WHERE 
1 = 1 

AND epiorder = 1
AND admimeth LIKE '2%' -- EMERGENCY ADMISSIONS​
) cte

LEFT OUTER JOIN StrategicReference.dbo.tbCCGPopEstimates_UPDATED_2017_03 pop
  ON cte.fyear = pop.(ISNULL(CAST(RIGHT(Year,2) as nvarchar(2)), '') + ISNULL(CAST((RIGHT(Year,2) +1) as nvarchar(2)), ''))

GROUP BY 
  [fyear]
  , [age_group]
  , [gender]
  , [ccg_code]

  select top 10 ISNULL(CAST(RIGHT(pop.Year,2) as nvarchar(2)), '') + ISNULL(CAST((RIGHT(pop.Year,2) +1) as nvarchar(2)), '')
    from StrategicReference.dbo.tbCCGPopEstimates_UPDATED_2017_03 pop

--ISNULL(CAST(FLOOR(startage / 5) * 5 as nvarchar(2))