library(iotc.base.common.data)

### ADMIN REFERENCES

RAV =
  query(connection = DB_RAV(),
        query = "
        SELECT DISTINCT
          VRVesselKey AS IOTC_NUMBER,
          LTRIM(RTRIM(VesselName)) AS NAME,
          LTRIM(RTRIM(Flag)) AS FLAG_CODE,
          LTRIM(RTRIM(GearType)) AS GEAR_CODE,
          VesselCurrent AS [CURRENT]
        FROM [IOTCVessels].[dbo].V_RAV
        ORDER BY VRVesselKey ASC, VesselCurrent DESC, 3, 2
        ")

usethis::use_data(RAV, overwrite = TRUE)
