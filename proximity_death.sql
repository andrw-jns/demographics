﻿/* 
Notes:
1. Zero months is less likely than one month as we're using the name of the month of death, rather than the 30 days to death. Eg. Death could occur on the 3rd of the month making multiple admissions in that "zero" month unlikely.
2. This script differs from others by the fact that it includes deaths that occur in the following year. This seems logical to me.
3. Population here is unhelpful?
 */

SELECT 
  [fyear]
  , [age_group]
  , cte.[gender]
  ---, [ccg_code]
  
  ---,  SUM(pop.[population]) as [population]
  
  , proximity_death
  , lunney_group
  --, pop.[population]
  ,  COUNT([fyear]) as [admissions]
FROM
  (


SELECT 
  '201415' as [fyear]
  --, ip.encrypted_hesid
  , CASE
WHEN (startage between 7001 and 7007) 
THEN N'00to04'
WHEN startage > 89
THEN '90plus' 
WHEN startage is null
THEN 'NULL'
ELSE RIGHT('00' + ISNULL(CAST(FLOOR(startage / 5) * 5 as nvarchar(2)), ''), 2) + 
'to' + RIGHT('00' + ISNULL(CAST((FLOOR(startage / 5) * 5) + 4 as nvarchar(2)), ''), 2) 
END as [age_group] 
  
  , CASE ip.sex
      WHEN '1' THEN 'M'
      WHEN '2' THEN 'F'
      END  as [gender]
  , CCGCODE as [ccg_code]
  , d.DerivedAge as [age_at_death]
  
  -- PROXIMITY TO DEATH
  
  , CASE
      WHEN d.Encrypted_HESid is null 
        then NULL
      WHEN DATEDIFF(mm, ip.admidate, d.DOD) between 0 and 24 
        then CAST(DATEDIFF(mm, ip.admidate, d.DOD) as nvarchar(8))
      WHEN DATEDIFF(mm, ip.admidate, d.DOD) > 24
        then NULL
      WHEN DATEDIFF(mm, ip.admidate, d.DOD) < 0
        then 'Error'
      ELSE 'Error' 
      END [proximity_death]
	  
	-- LUNNEY GROUP --
	-- Note: understand these lines that randomly assign 'frailty'. Is this ED only?
	,CASE 
		 --WHEN arrivalage between 65 and 74 and RAND(CAST(NEWID() AS varbinary)) > 0.9 then 'Frailty' -- Randomly assigns 10% of arrivals in age range to Frailty deaths
		 --WHEN DerivedAge between 75 and 84 and RAND(CAST(NEWID() AS varbinary)) > 0.7 then 'Frailty'    -- Randomly assigns 30% of arrivals in age range to Frailty deaths
		 --WHEN DerivedAge between 85 and 120 and RAND(CAST(NEWID() AS varbinary)) > 0.2 then 'Frailty'   -- Randomly assigns 80% of arrivals in age range to Frailty deaths
		WHEN CAUSE_OF_DEATH like 'A0%' then 'SuddenDeath'
		WHEN CAUSE_OF_DEATH like 'A39%' then 'SuddenDeath'
		WHEN CAUSE_OF_DEATH like 'A4[01]%' then 'SuddenDeath'
		WHEN CAUSE_OF_DEATH like 'B2[0-4]%' then 'OtherTerminalIllness'
		WHEN CAUSE_OF_DEATH like 'D[0-3]%' then 'Cancer'
		WHEN CAUSE_OF_DEATH like 'D4[0-8]%' then 'Cancer'
		WHEN CAUSE_OF_DEATH like 'F0[13]%' then 'Frailty'
		WHEN CAUSE_OF_DEATH like 'G0[0-3]%' then 'SuddenDeath'
		WHEN CAUSE_OF_DEATH like 'G30%' then 'Frailty'
		WHEN CAUSE_OF_DEATH like 'H[0-5]%' then 'SuddenDeath'
		WHEN CAUSE_OF_DEATH like 'H[6-9]%' then 'SuddenDeath'
		WHEN CAUSE_OF_DEATH like 'I2[12]%' then 'SuddenDeath'
		WHEN CAUSE_OF_DEATH like 'I63%' then 'SuddenDeath'
		WHEN CAUSE_OF_DEATH like 'I64%' then 'SuddenDeath'
		WHEN CAUSE_OF_DEATH like 'I6[0-2]%' then 'SuddenDeath'
		WHEN CAUSE_OF_DEATH like 'I71%' then 'SuddenDeath'	
		WHEN CAUSE_OF_DEATH like 'J1[2-8]%' then 'SuddenDeath'
		WHEN CAUSE_OF_DEATH like 'K2[5-7]%' then 'SuddenDeath'
		WHEN CAUSE_OF_DEATH like 'K4[0-6]%' then 'SuddenDeath'
		WHEN CAUSE_OF_DEATH like 'K57%' then 'OrganFailure'
		WHEN CAUSE_OF_DEATH like 'K7[0-6]%' then 'OrganFailure'
		WHEN CAUSE_OF_DEATH like 'L%' then 'OrganFailure'
		WHEN CAUSE_OF_DEATH like 'O%' then 'SuddenDeath'
		WHEN CAUSE_OF_DEATH like 'P%' then 'SuddenDeath'
		WHEN CAUSE_OF_DEATH like 'Q[2-8]%' then 'OrganFailure'
		WHEN CAUSE_OF_DEATH like 'R%' then 'SuddenDeath'
		WHEN CAUSE_OF_DEATH like 'R54%' then 'Frailty'
		WHEN CAUSE_OF_DEATH like 'R99%' then 'SuddenDeath'	
		WHEN CAUSE_OF_DEATH like 'U509%' then 'SuddenDeath'
		WHEN CAUSE_OF_DEATH like 'W6[5-9]%' then 'SuddenDeath'
		WHEN CAUSE_OF_DEATH like 'W7[0-4]%' then 'SuddenDeath'
		WHEN CAUSE_OF_DEATH like 'W[01]%' then 'SuddenDeath'
		WHEN CAUSE_OF_DEATH like 'X0%' then 'SuddenDeath'
		WHEN CAUSE_OF_DEATH like 'X41%' then 'SuddenDeath'
		WHEN CAUSE_OF_DEATH like 'X42%' then 'SuddenDeath'
		WHEN CAUSE_OF_DEATH like 'X44%' then 'SuddenDeath'
		WHEN CAUSE_OF_DEATH like 'X59%' then 'SuddenDeath'
		WHEN CAUSE_OF_DEATH like 'X8[0-4]%' then 'SuddenDeath'
		WHEN CAUSE_OF_DEATH like 'X8[5-9]%' then 'SuddenDeath'
		WHEN CAUSE_OF_DEATH like 'X9%' then 'SuddenDeath'
		WHEN CAUSE_OF_DEATH like 'X[67]%' then 'SuddenDeath'
		WHEN CAUSE_OF_DEATH like 'Y0%' then 'SuddenDeath'
		WHEN CAUSE_OF_DEATH like 'Y3[0-4]%' then 'SuddenDeath'
		WHEN CAUSE_OF_DEATH like 'Y[12]%' then 'SuddenDeath'	
		--Catch all
		WHEN CAUSE_OF_DEATH like 'A%' then 'OrganFailure'
		WHEN CAUSE_OF_DEATH like 'B%' then 'OrganFailure'
		WHEN CAUSE_OF_DEATH like 'C%' then 'Cancer'
		WHEN CAUSE_OF_DEATH like 'D[5-8]%' then 'OrganFailure'
		WHEN CAUSE_OF_DEATH like 'E%' then 'OtherTerminalIllness'
		WHEN CAUSE_OF_DEATH like 'F%' then 'OtherTerminalIllness'
		WHEN CAUSE_OF_DEATH like 'G%' then 'OtherTerminalIllness'
		WHEN CAUSE_OF_DEATH like 'I%' then 'OrganFailure'
		WHEN CAUSE_OF_DEATH like 'J%' then 'OrganFailure'
		WHEN CAUSE_OF_DEATH like 'K%' then 'OtherTerminalIllness'
		WHEN CAUSE_OF_DEATH like 'M%' then 'Frailty'
		WHEN CAUSE_OF_DEATH like 'N%' then 'OrganFailure'
		WHEN CAUSE_OF_DEATH like 'Q%' then 'OtherTerminalIllness'
		WHEN CAUSE_OF_DEATH like 'V%' then 'OrganFailure'
		WHEN CAUSE_OF_DEATH like 'W%' then 'OrganFailure'
		WHEN CAUSE_OF_DEATH like 'X%' then 'OrganFailure'
		WHEN CAUSE_OF_DEATH like 'Y%' then 'OrganFailure'
		ELSE NULL
		END [lunney_group]

----

FROM [HESData].dbo.tbInpatients1415 ip
  LEFT OUTER JOIN [StrategicReference].dbo.vwGPPracticeToCCGAndPCT ccg 
    ON ip.gpprac = ccg.GPPractice 

  LEFT OUTER JOIN 
    (
    SELECT
       a.[Encrypted_HESID]
       ,a.[DOD]
       ,a.[CAUSE_OF_DEATH]
	   ,a.derivedage
    FROM 
    [ONS].[HESONS].[tbMortalityto1516] a
	WHERE DOD between '2014-04-01' and '2016-03-31'
    ) d
    ON ip.Encrypted_HESID = d.Encrypted_HESID

---

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
  , proximity_death
  , lunney_group
  --, [population]

ORDER BY
  cte.[gender],
  [age_group]