/****** Population Projections  ******/
SELECT [AgeGroup] as [age_group]
      ,[Gender]   as [gender]
      ,[Year]     as [year]
      ,SUM([Population]) as [population]

FROM

(
SELECT -- TOP 2000
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

 Order BY Year , Gender, AgeGroup