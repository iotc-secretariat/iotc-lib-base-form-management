library(iotc.base.common.data)

YEAR  = 2017
FLEET = c("FRA", "FRA-MYT", "FRA-REU")

SF = query(DB_IOTDB(), paste0("SELECT * FROM V_LEGACY_SF WHERE YEAR = ", YEAR, " AND FLEET_CODE IN (", paste0(paste0("'", FLEET, "'"), collapse = ", "), ")"))

SF$FISHERY_CODE_ORIG = SF$FISHERY_CODE
SF[FISHERY_CODE_ORIG %in% c("ELL", "ELLOB"), FISHERY_CODE := "LLSW"]
SF[FISHERY_CODE_ORIG %in% c("PS",  "PSOB"),  FISHERY_CODE := "PS"]

SF_ =
  SF[, .(MONTH = MONTH_START,
         FISHERY_CODE_ORIG,
         FISHERY_CODE        = ifelse(SCHOOL_TYPE_CODE != "UNCL", paste0(FISHERY_CODE, SCHOOL_TYPE_CODE), FISHERY_CODE),
         TARGET_SPECIES_CODE = ifelse(FISHERY_CODE == "LLSW", "SWO", "AG35"),
         GRID_CODE           = str_trim(FISHING_GROUND_CODE),
         SPECIES_CODE,
         SEX_CODE,
         ESTIMATION_CODE     = ifelse(SAMPLE_SIZE == 0, "SS", "AV"),
         DATA_RAISING_CODE   = ifelse(RAISE_CODE == "OS", "NR", "RT"),
         MEASURE_CODE        = ifelse(MEASURE_TYPE_CODE %in% c("FL", "FLUT"), "FL",
                                      MEASURE_TYPE_CODE),
         MEASURING_TOOL_CODE = ifelse(MEASURE_TYPE_CODE == "FLUT", "FT", "CA"),
         SIZE_CLASS_LOW      = CLASS_LOW,
         SIZE_CLASS_HIGH     = CLASS_HIGH,
         NUM_SAMPLES         = SAMPLE_SIZE,
         NUM_FISH            = FISH_COUNT)]

SF_$FATE_TYPE_CODE = "RE"
SF_$FATE_CODE      = "FL"

SF_$MEASUREMENT_TYPE_CODE = "LN"

SF_[, DATA_TYPE_CODE       := ifelse(FISHERY_CODE_ORIG %in% c("ELLOB", "PSOB"), "PR", "FI")]
SF_[, DATA_SOURCE_CODE     := ifelse(FISHERY_CODE_ORIG %in% c("ELLOB", "PSOB"), "OB", "LG")]
SF_[, DATA_PROCESSING_CODE := ifelse(FISHERY_CODE_ORIG %in% c("ELLOB", "PSOB"), "SP",
                                     ifelse(FISHERY_CODE_ORIG %in% c("PS", "PSLS", "PSFS"), "RS", "NO"))]
SF_[, COVERAGE_TYPE_CODE   := ifelse(FISHERY_CODE_ORIG %in% c("ELL", "ELLOB"), "UN", "TR")]
SF_[, COVERAGE             := ifelse(FISHERY_CODE_ORIG %in% c("ELL", "ELLOB"),    0,  0.7)]

SF_ = SF_[, .(MONTH, FISHERY_CODE, TARGET_SPECIES_CODE, GRID_CODE, SPECIES_CODE, SEX_CODE, FATE_TYPE_CODE, FATE_CODE,
              ESTIMATION_CODE, DATA_TYPE_CODE, DATA_SOURCE_CODE, DATA_PROCESSING_CODE, DATA_RAISING_CODE,
              COVERAGE_TYPE_CODE, COVERAGE,
              MEASUREMENT_TYPE_CODE, MEASURE_CODE, MEASURING_TOOL_CODE,
              SIZE_CLASS_LOW, SIZE_CLASS_HIGH,
              NUM_SAMPLES, NUM_FISH)]

write.table(SF_, paste0("./test_produce/SF_", paste0(FLEET, collapse = "_"), "_", YEAR, ".csv"), na = "", sep = ",", row.names = FALSE)
