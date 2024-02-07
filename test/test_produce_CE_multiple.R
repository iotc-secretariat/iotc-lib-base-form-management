YEAR  = 2017
FLEET = c("FRA", "FRA-MYT", "FRA-REU")

EF = query(DB_IOTDB(), paste0("SELECT * FROM V_LEGACY_EF WHERE YEAR = ", YEAR, " AND FLEET_CODE IN (", paste0(paste0("'", FLEET, "'"), collapse = ", "), ")"))
CA = query(DB_IOTDB(), paste0("SELECT * FROM V_LEGACY_CA WHERE YEAR = ", YEAR, " AND FLEET_CODE IN (", paste0(paste0("'", FLEET, "'"), collapse = ", "), ")"))

EF$FISHERY_CODE_ORIG = EF$FISHERY_CODE
EF[FISHERY_CODE_ORIG %in% c("ELL", "ELLOB"), FISHERY_CODE := "LLSW"]
EF[FISHERY_CODE_ORIG %in% c("PS",  "PSOB"),  FISHERY_CODE := "PS"]

CA$FISHERY_CODE_ORIG = CA$FISHERY_CODE
CA[FISHERY_CODE_ORIG %in% c("ELL", "ELLOB"), FISHERY_CODE := "LLSW"]
CA[FISHERY_CODE_ORIG %in% c("PS",  "PSOB"),  FISHERY_CODE := "PS"]

EF =
  EF[, .(MONTH = MONTH_START,
         FISHERY_CODE        = ifelse(SCHOOL_TYPE_CODE != "UNCL", paste0(FISHERY_CODE, SCHOOL_TYPE_CODE), FISHERY_CODE),
         TARGET_SPECIES_CODE = ifelse(FISHERY_CODE == "LLSW", "SWO", "AG35"),
         GRID_CODE           = str_trim(FISHING_GROUND_CODE),
         EFFORT_UNIT_CODE,
         EFFORT)]

EF_F =
  dcast.data.table(EF, value.var = "EFFORT", formula = MONTH + FISHERY_CODE + TARGET_SPECIES_CODE + GRID_CODE ~ EFFORT_UNIT_CODE)

EF_F$PRIMARY_EFFORT_CODE = character()
EF_F[!is.na(FHOURS)]$PRIMARY_EFFORT_CODE = "FHOURS"
EF_F[!is.na(HOOKS)] $PRIMARY_EFFORT_CODE = "HOOKS"
EF_F[!is.na(FHOURS), PRIMARY_EFFORT := FHOURS]
EF_F[!is.na(HOOKS),  PRIMARY_EFFORT := HOOKS]

EF_F[, ESTIMATION_CODE      := ifelse(FISHERY_CODE == "LLSW", "AV", "SS")]
EF_F[, DATA_TYPE_CODE       := ifelse(FISHERY_CODE == "LLSW", "PR", "FI")]
EF_F[, DATA_SOURCE_CODE     := ifelse(FISHERY_CODE == "LLSW", "OB", "RS")]
EF_F[, DATA_PROCESSING_CODE := ifelse(FISHERY_CODE == "LLSW", "TE", "LS")]
EF_F[, DATA_RAISING_CODE    := ifelse(FISHERY_CODE == "LLSW", "NR", "RT")]
EF_F[, COVERAGE_TYPE_CODE   := ifelse(FISHERY_CODE == "LLSW", "UN", "TR")]
EF_F[, COVERAGE             := ifelse(FISHERY_CODE == "LLSW",    0,  0.7)]

EF_F = EF_F[, .(MONTH, FISHERY_CODE, TARGET_SPECIES_CODE, GRID_CODE,
                ESTIMATION_CODE, DATA_TYPE_CODE, DATA_SOURCE_CODE, DATA_PROCESSING_CODE, DATA_RAISING_CODE,
                COVERAGE_TYPE_CODE, COVERAGE,
                PRIMARY_EFFORT_CODE, PRIMARY_EFFORT)]

CA =
  CA[, .(MONTH = MONTH_START,
         FISHERY_CODE        = ifelse(SCHOOL_TYPE_CODE != "UNCL", paste0(FISHERY_CODE, SCHOOL_TYPE_CODE), FISHERY_CODE),
         TARGET_SPECIES_CODE = ifelse(FISHERY_CODE == "LLSW", "SWO", "AG35"),
         GRID_CODE           = str_trim(FISHING_GROUND_CODE),
         SPECIES_CODE,
         CATCH_UNIT_CODE,
         CATCH = round(CATCH, 2))]

CA_F =
  dcast.data.table(CA[!is.na(CATCH)], value.var = "CATCH",
                   formula = MONTH + FISHERY_CODE + TARGET_SPECIES_CODE + GRID_CODE ~ SPECIES_CODE + CATCH_UNIT_CODE)

CE = merge(EF_F, CA_F, by = c("MONTH", "FISHERY_CODE", "TARGET_SPECIES_CODE", "GRID_CODE"), all.x = TRUE)
CE[, PRIMARY_EFFORT := round(PRIMARY_EFFORT, 2)]

write.table(CE, paste0("./test_produce_multiple/CE_", paste0(FLEET, collapse = "_"), "_", YEAR, ".csv"), na = "", sep = ",", row.names = FALSE)
