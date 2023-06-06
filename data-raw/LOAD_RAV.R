library(iotc.base.common.data)

### ADMIN REFERENCES

RAV =
  query(connection = DB_RAV(),
        query = "
        WITH LAST_UPDATE_DATE AS (
        	SELECT
        		VRVesselKey AS IOTC_NUMBER,
        		MAX(DateUpdated) AS LAST_UPDATE
        	FROM
        		[IOTCVessels].[dbo].V_RAV
        	GROUP BY VRVesselKey
        )
        SELECT DISTINCT
            VRVesselKey AS IOTC_NUMBER,
            LTRIM(RTRIM(VesselName)) AS NAME,
            LTRIM(RTRIM(Flag)) AS FLAG_CODE,
            LTRIM(RTRIM(GearType)) AS GEAR_CODE,
            VesselCurrent AS [CURRENT],
  		      LAST_UPDATE
        FROM [IOTCVessels].[dbo].V_RAV R
        INNER JOIN  LAST_UPDATE_DATE U
        ON
        	R.VRVesselKey = U.IOTC_NUMBER AND
        	R.DateUpdated = U.LAST_UPDATE
        ORDER BY R.VRVesselKey ASC, 3, 2")

usethis::use_data(RAV, overwrite = TRUE)
