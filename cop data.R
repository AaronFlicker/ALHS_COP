library(tidyverse)
library(odbc)
library(tidycensus)

sdn <- c(
  "Avondale",
  "East Westwood",
  "Lower Price Hill",
  "East Price Hill",
  "North Fairmount",
  "South Fairmount",
  "English Woods",
  "Roll Hill",
  "Roselawn",
  "Sedamsville",
  "South Cumminsville",
  "Millvale",
  "West End",
  "Winton Hills"
)

## Load data ==================================================================

con <- dbConnect(odbc(), "ClarityProd")

patients <- dbGetQuery(con, "
  SELECT DISTINCT dx.pat_id
                  ,p.pat_mrn_id
                  ,p.death_date
                  ,p.gender
                  ,r.mapped_race AS Race
    FROM hpceclarity.bmi.dx
    INNER JOIN hpceclarity.bmi.patient p
      ON dx.pat_id = p.pat_id
    INNER JOIN hpceclarity.bmi.y_chmc_race_ethnicity_mapping r
        ON dx.pat_id = r.pat_id
    WHERE current_icd10_list LIKE 'J45%'
                       ") |>
  mutate(
    Race = case_when(
      str_detect(Race, "Black") ~ "Black",
      Race == "Unknown" ~ NA,
      TRUE ~ "Other race"
      )
    )

registry <- dbGetQuery(con, "
  SELECT rdm.x_pat_id AS pat_id
        ,CAST(rdm.x_update_date AS DATE) AS LastRegistryUpdateDate
        ,CAST(rdm.first_include_dttm AS DATE) AS first_include_dttm
        ,rdm.record_id
        ,CAST(p.birth_date AS DATE) AS birth_date
        ,r.mapped_race
        ,p.add_line_1
        ,p.add_line_2
        ,p.city
        ,p.state
        ,p.zip
        ,g.countyfp
        ,g.statefp
        ,g.Neighborhood
        ,g.foster
  FROM hpceclarity.bmi.reg_data_membership rdm
    INNER JOIN hpceclarity.bmi.patient p
      ON rdm.x_pat_id = p.pat_id
    LEFT JOIN temptable.dbo.full_list_geocode g
      ON (p.add_line_1 = g.add_line_1 
        OR (p.add_line_1 IS NULL AND g.add_line_1 IS NULL))
      AND (p.add_line_2 = g.add_line_2 
        OR (p.add_line_2 IS NULL AND g.add_line_2 IS NULL))
      AND (p.city = g.city OR (p.city IS NULL AND g.city IS NULL))
      AND (p.state = g.state OR (p.state IS NULL AND g.state IS NULL))
      AND (p.zip = g.zip OR (p.zip IS NULL AND g.zip IS NULL))
    LEFT JOIN hpceclarity.bmi.y_chmc_race_ethnicity_mapping r
        ON p.pat_id = r.pat_id
  WHERE rdm.registry_id = '100005'
    AND rdm.registry_status_c = 1
                       ") |>
  filter(
    as.numeric(today()-birth_date) < 6575,
    statefp == "39",
    countyfp == "061"
    ) |>
  mutate(
    Neighborhood = ifelse(foster, NA, Neighborhood),
    Race = case_when(
      mapped_race == "Black or African American" ~ "Black",
      mapped_race == "Unknown" ~ NA,
      TRUE ~ "Other race"
    ),
    SDN = Neighborhood %in% sdn,
    Avondale = Neighborhood == "Avondale" & !is.na(Neighborhood)
    )

immun <- dbGetQuery(con, "
  SELECT rdm.x_pat_id AS pat_id
        ,i.immune_date
        ,i.status
        ,i.name
    FROM hpceclarity.bmi.reg_data_membership rdm
      INNER JOIN hpceclarity.bmi.immunization i
        ON rdm.x_pat_id = i.pat_id
    WHERE rdm.registry_id = '100005'
      AND rdm.registry_status_c = 1  
      AND (i.name LIKE '%flu%' OR i.name LIKE '%covid%')
      AND i.status = 'Given'
                        ") |>
  mutate(ImmuneType = ifelse(str_detect(name, "COVID"), "Covid", "Flu")) |>
  group_by(pat_id, ImmuneType) |>
  reframe(immune_date = max(immune_date)) |>
  mutate(UTD = immune_date >= today()-365) |>
  pivot_wider(
    id_cols = pat_id,
    names_from = ImmuneType,
    values_from = UTD
  ) |>
  inner_join(registry) 

ster <- dbGetQuery(con, "
  SELECT rdm.x_pat_id AS pat_id
        ,m.ordering_date
        ,m.order_status
        ,m.order_status_c
        ,m.medication_name
    FROM hpceclarity.bmi.reg_data_membership rdm
      INNER JOIN hpceclarity.bmi.order_med m
        ON rdm.x_pat_id = m.pat_id
    WHERE rdm.registry_id = '100005'
      AND rdm.registry_status_c = 1  
      AND medication_id IN (
        '600134', '10573', '77086', '59006', '6492', '600173', '10575', 
        '600174', '77074', '89374', '14909', '6504', '59002', '14993', '96126', 
        '62835', '6497', '6490', '77836', '89373', '14884', '6494', '602976', 
        '2326', '50387', '50688', '58678', '2324', '11119', '112336', '2323', 
        '92649', '4993', '603911', '50695', '600135', '77847', '11118', '6483', 
        '17946', '125711', '14992', '2320', '86798', '62847', '6491', '96321', 
        '2319', '61157', '2328', '19210', '600008', '125656', '112338', '2155', 
        '80717', '62841', '105872', '10576', '600133', '2318', '2327', '29302', 
        '29301', '62864', '143455', '6496', '6498', '62839', '14887', '4992', 
        '92401', '77837', '15853', '11117', '62849', '90035', '50388', '6493', 
        '6499', '6495', '23265', '116740', '58679', '125652', '604209', 
        '62844', '2322', '600175', '4991', '2325'
        )
                        ") |>
  filter(ordering_date >= today()-365) |>
  distinct(pat_id) |>
  mutate(Steroids = 1) |>
  inner_join(registry) |>
  group_by(Race, SDN, Avondale) |>
  reframe(Steroids = sum(Steroids))

exclude <- dbGetQuery(con, "
  SELECT DISTINCT p.pat_id
    FROM hpceclarity.bmi.problem_list pl
      LEFT JOIN hpceclarity.bmi.patient p
        ON pl.pat_id = p.pat_id
      LEFT JOIN hpceclarity.dbo.chmc_clarity_edg AS edg
        ON pl.dx_id = edg.dx_ID
    WHERE (
      edg.current_icd10_list LIKE '%E84.9%'
	   OR edg.current_icd10_list LIKE '%E84.8%'
	   OR edg.current_icd10_list LIKE '%E84.19%'
	   OR edg.current_icd10_list LIKE '%E84.0%'
	   OR edg.current_icd10_list LIKE '%E84.11%'
	   OR edg.current_icd10_list LIKE '%Z93.0%'
	   OR edg.current_icd10_list LIKE '%Z99.11%'
	   OR edg.current_icd10_list LIKE '%Z99.1%'
	   OR edg.current_icd10_list LIKE '%J96.1%'
	   OR edg.current_icd10_list LIKE '%J96.2%'
	   OR edg.current_icd10_list LIKE '%Q20.%'
	   OR edg.current_icd10_list LIKE '%Q24.%'
	   OR edg.current_icd10_list LIKE '%Q25.%'
	   OR edg.current_icd10_list LIKE '%Q89.3%'
	   OR (
	      edg.current_icd10_list LIKE '%D57.%'
	        AND edg.current_icd10_list NOT LIKE '%D57.3%'
	        )
	   )
    AND pl.status = 'Active'
                      ")

admits <- dbGetQuery(con, "
  SELECT peh.pat_id
        ,peh.pat_enc_csn_id
        ,CAST(peh.hosp_admsn_time AS DATE) AS admission_date
        ,CAST(peh.hosp_disch_time AS DATE) AS discharge_date
        ,dx.current_icd10_list
        ,dx.dx_name
        ,dx.line
        ,r.mapped_race
        ,g.Neighborhood
        ,g.statefp
        ,g.countyfp
        ,g.foster
    FROM hpceclarity.bmi.pat_enc_hsp peh
      INNER JOIN hpceclarity.bmi.dx dx
        ON peh.pat_enc_csn_id = dx.pat_enc_csn_id
      INNER JOIN hpceclarity.bmi.y_chmc_race_ethnicity_mapping r
        ON peh.pat_id = r.pat_id
      LEFT JOIN hpceclarity.bmi.chmc_adt_addr_hx a
        ON a.pat_id = peh.pat_id
      LEFT JOIN temptable.dbo.full_list_geocode g
        ON (a.addr_hx_line1 = g.add_line_1
            OR a.addr_hx_line1 IS NULL AND g.add_line_1 IS NULL)
          AND (a.addr_hx_line2 = g.add_line_2
            OR a.addr_hx_line2 IS NULL AND g.add_line_2 IS NULL)
          AND (a.city_hx = g.city OR a.city_hx IS NULL AND g.city IS NULL)
          AND (a.state = g.state OR a.state IS NULL AND g.state IS NULL)
          AND (a.zip_hx = g.zip OR a.zip_hx IS NULL AND g.zip IS NULL)
    WHERE peh.adt_patient_stat <> 'Hospital Outpatient Visit'
      AND peh.adt_pat_class IN ('Inpatient', 'Observation')
      AND FLOOR(
      (DATEDIFF(DD, peh.birth_date, peh.hosp_admsn_time)/365.25)*12
      ) >= 24
      AND dx.line IN (1, 2)
      AND (
          dx.current_icd10_list LIKE '%J45.21%'
            OR dx.current_icd10_list LIKE '%J45.22%'
            OR dx.current_icd10_list LIKE '%J45.31%'
            OR dx.current_icd10_list LIKE '%J45.32%'
            OR dx.current_icd10_list LIKE '%J45.41%'
            OR dx.current_icd10_list LIKE '%J45.42%'
            OR dx.current_icd10_list LIKE '%J45.51%'
            OR dx.current_icd10_list LIKE '%J45.52%'
            OR dx.current_icd10_list LIKE '%J45.901%'
            OR dx.current_icd10_list LIKE '%J45.902%'
            OR dx.current_icd10_list LIKE '%J00%'
            OR dx.current_icd10_list LIKE '%J06%'
            OR dx.current_icd10_list LIKE '%J30%'
            OR dx.current_icd10_list LIKE '%J31'
            OR dx.current_icd10_list LIKE '%J96.0%'
            OR dx.current_icd10_list LIKE '%J96.9%'
            OR dx.current_icd10_list LIKE '%R05%'
            OR dx.current_icd10_list LIKE '%R06%'
            OR dx.current_icd10_list LIKE '%R09.02%'
          )
      AND peh.hosp_admsn_time > '1/1/2019'
      AND (a.eff_start_date <= peh.hosp_admsn_time OR a.eff_start_date IS NULL)
      AND (a.eff_end_date > peh.hosp_admsn_time OR a.eff_end_date IS NULL)
      ") |>
  mutate(Neighborhood = ifelse(foster == 1, NA, Neighborhood)) |>
  pivot_wider(
    id_cols = c(pat_id:discharge_date, mapped_race:foster),
    names_from = line,
    values_from = current_icd10_list:dx_name
  ) |>
  filter(
    str_starts(current_icd10_list_1, "J45") |
      str_starts(current_icd10_list_2, "J45")
  ) |>
  anti_join(exclude, join_by(pat_id)) |>
  mutate(
    HamCo = ifelse(statefp == "39" & countyfp == "061" & !is.na(statefp), 1, 0),
    ContactMonth = floor_date(admission_date, "month"),
    Race = case_when(
      mapped_race == "Black or African American" ~ "Black",
      mapped_race == "Unknown" ~ NA,
      TRUE ~ "Other race"
      ),
    SDN = Neighborhood %in% sdn,
    Avondale = Neighborhood == "Avondale" & !is.na(Neighborhood)
    )

ed <- dbGetQuery(con, "
  SELECT pe.pat_id
        ,pe.pat_enc_csn_id
        ,CAST(pe.contact_date AS DATE) AS contact_date
        ,dx.line
        ,dx.current_icd10_list
        ,dx_name
        ,r.mapped_race
        ,g.Neighborhood
        ,g.statefp
        ,g.countyfp
        ,g.foster
    FROM hpceClarity.bmi.pat_enc pe
	    INNER JOIN hpceclarity.bmi.pat_enc_hsp peh
	      ON pe.pat_enc_csn_id = peh.pat_enc_csn_id
	    LEFT JOIN hpceclarity.bmi.dx dx
		    ON pe.pat_enc_csn_id = dx.pat_enc_csn_id
		  INNER JOIN hpceclarity.bmi.y_chmc_race_ethnicity_mapping r
        ON pe.pat_id = r.pat_id
      LEFT JOIN hpceclarity.bmi.chmc_adt_addr_hx a
        ON a.pat_id = peh.pat_id
      LEFT JOIN temptable.dbo.full_list_geocode g
        ON (a.addr_hx_line1 = g.add_line_1
            OR a.addr_hx_line1 IS NULL AND g.add_line_1 IS NULL)
          AND (a.addr_hx_line2 = g.add_line_2
            OR a.addr_hx_line2 IS NULL AND g.add_line_2 IS NULL)
          AND (a.city_hx = g.city OR a.city_hx IS NULL AND g.city IS NULL)
          AND (a.state = g.state OR a.state IS NULL AND g.state IS NULL)
          AND (a.zip_hx = g.zip OR a.zip_hx IS NULL AND g.zip IS NULL)
      LEFT JOIN hpceclarity.dbo.chmc_ed_daily_jcaho d
        ON pe.pat_enc_csn_id = d.enc_id
	  WHERE pe.hosp_admsn_type_c IN ('1', '12')
		  AND peh.adt_pat_class = 'Emergency'
		  AND adt_patient_stat = 'Discharged'
		  AND FLOOR((DATEDIFF(DD, pe.birth_date, pe.contact_date)/365.25)*12) >= 24
		  AND dx.line IN ('1', '2')
		  AND (
		    dx.current_icd10_list LIKE '%J45.21%'
		      OR dx.current_icd10_list LIKE '%J45.22%'
		      OR dx.current_icd10_list LIKE '%J45.31%'
		      OR dx.current_icd10_list LIKE '%J45.32%'
 		      OR dx.current_icd10_list LIKE '%J45.41%'
		      OR dx.current_icd10_list LIKE '%J45.42%'
		      OR dx.current_icd10_list LIKE '%J45.51%'
		      OR dx.current_icd10_list LIKE '%J45.52%'
		      OR dx.current_icd10_list LIKE '%J45.901%'
		      OR dx.current_icd10_list LIKE '%J45.902%'
		      OR dx.current_icd10_list LIKE '%J00%'
		      OR dx.current_icd10_list LIKE '%J06%'
		      OR dx.current_icd10_list LIKE '%J30%'
		      OR dx.current_icd10_list LIKE '%J31'
		      OR dx.current_icd10_list LIKE '%J96.0%'
		      OR dx.current_icd10_list LIKE '%J96.9%'
		      OR dx.current_icd10_list LIKE '%R05%'
		      OR dx.current_icd10_list LIKE '%R06%'
		      OR dx.current_icd10_list LIKE '%R09.02%'
		      )
		AND pe.contact_date BETWEEN '1/1/2019'
		  AND EOMONTH(DATEADD(MONTH, -1, GETDATE()))
		AND (a.eff_start_date <= peh.hosp_admsn_time OR a.eff_start_date IS NULL)
    AND (a.eff_end_date > peh.hosp_admsn_time OR a.eff_end_date IS NULL)
    AND d.dispo not like '%LWBS%'
		AND d.dispo <> 'ED Dismiss - Never Arrived'
		AND d.site not like '%URGENT%'
		AND d.room_name not like '%URG%'
		AND d.room_name not like '%MAS%'
		AND d.room_name not like '%UC%'
		AND d.room_name not like '%AND%'
		AND d.room_name not like '%GRN%'
                   ") |>
  mutate(Neighborhood = ifelse(foster == 1, NA, Neighborhood)) |>
  pivot_wider(
    id_cols = c(pat_id:contact_date, mapped_race:foster),
    names_from = line,
    values_from = current_icd10_list:dx_name
  ) |>
  filter(
    str_starts(current_icd10_list_1, "J45") |
      str_starts(current_icd10_list_2, "J45")
  ) |>
  anti_join(exclude, join_by(pat_id)) |>
  mutate(
    HamCo = ifelse(statefp == "39" & countyfp == "061" & !is.na(statefp), 1, 0),
    ContactMonth = floor_date(contact_date, "month"),
    Race = case_when(
      mapped_race == "Black or African American" ~ "Black",
      mapped_race == "Unknown" ~ NA,
      TRUE ~ "Other race"
    ),
    SDN = Neighborhood %in% sdn,
    Avondale = Neighborhood == "Avondale" & !is.na(Neighborhood)
  ) |>
  mutate(Admitted = pat_enc_csn_id %in% admits$pat_enc_csn_id)

clear <- dbGetQuery(con, "
  SELECT op.pat_id
	    	,CONVERT(DATE, MAX(op.ordering_date)) AS MostRecentClearOrderDate
		    ,op.proc_id
		    ,op.proc_name AS ClearProcedure
		    ,order_status_c_name
	FROM hpceclarity.bmi.order_proc op
	INNER JOIN hpceclarity.bmi.y_chmc_race_ethnicity_mapping r
        ON op.pat_id = r.pat_id
  LEFT JOIN hpceclarity.bmi.chmc_adt_addr_hx a
    ON op.pat_id = a.pat_id
    AND op.ordering_date >= a.eff_start_date
    AND (op.ordering_date < a.eff_end_date OR a.eff_end_date IS NULL)
	WHERE proc_id = '10537336'
		AND order_status_c_name = 'Sent'
	GROUP BY op.pat_id
		    	,op.proc_id
			    ,op.proc_name
			    ,op.order_status_c_name
			    ,r.mapped_race
                    ") |>
  inner_join(patients)

help <- dbGetQuery(con, "
  SELECT op.pat_id
    		,CAST(op.ordering_date AS DATE) AS ContactDate
		    ,op.proc_id
		    ,op.proc_name AS ChildHeLPProcedure
		    ,op.order_status_c_name
		    ,g.Neighborhood
	FROM hpceclarity.bmi.order_proc op
	INNER JOIN hpceclarity.bmi.y_chmc_race_ethnicity_mapping r
    ON op.pat_id = r.pat_id
  LEFT JOIN hpceclarity.bmi.chmc_adt_addr_hx a
    ON op.pat_id = a.pat_id
    AND op.ordering_date >= a.eff_start_date
    AND (op.ordering_date < a.eff_end_date OR a.eff_end_date IS NULL)
  LEFT JOIN temptable.dbo.full_list_geocode g
    ON (a.addr_hx_line1 = g.add_line_1 
      OR (a.addr_hx_line1 IS NULL AND g.add_line_1 IS NULL))
    AND (a.addr_hx_line2 = g.add_line_2 
      OR (a.addr_hx_line2 IS NULL AND g.add_line_2 IS NULL))
    AND (a.city_hx = g.city OR (a.city_hx IS NULL AND g.city IS NULL))
    AND (a.state = g.state OR (a.state IS NULL AND g.state IS NULL))
    AND (a.zip_hx = g.zip OR (a.zip_hx IS NULL AND g.zip IS NULL))
	WHERE proc_id = '10450182'
		AND order_status_c_name = 'Sent'
                   ") |>
  inner_join(patients) |>
  unique() |>
  mutate(
    SDN = Neighborhood %in% sdn,
    Avondale = Neighborhood == "Avondale",
    ContactMonth = floor_date(ContactDate, "month")
    ) |>
  group_by(ContactMonth, Race, SDN, Avondale) |>
  reframe(HelpRefers = n())

munis <- get_decennial(
  geography = "county subdivision",
  variables = c("P1_001N", "P1_004N", "P3_001N", "P3_004N"),
  state = "OH",
  county = "Hamilton",
  sumfile = "pl",
  year = 2020
) |>
  filter(value > 0) |>
  separate_wider_delim(
    NAME,
    delim = ",",
    names = c("Neighborhood", "County", "State")
  ) |>
  mutate(
    Neighborhood = str_remove(Neighborhood, "village"),
    Neighborhood = str_remove(Neighborhood, "city"),
    Neighborhood = str_to_title(Neighborhood),
    Neighborhood = str_remove(Neighborhood, "The Village Of"),
    Neighborhood = str_trim(Neighborhood),
    Age = ifelse(str_detect(variable, "P1"), "All", "Adult"),
    Race = ifelse(str_detect(variable, "001"), "All", "Black")
  ) |>
  pivot_wider(
    id_cols = Neighborhood,
    names_from = Age:Race,
    values_from = value
  ) |>
  mutate(
    Child_All = All_All-Adult_All,
    Child_Black = All_Black-Adult_Black,
    Child_NonBlack = Child_All-Child_Black
  ) |>
  filter(Neighborhood != "Cincinnati") |>
  select(Neighborhood, Child_Black, Child_NonBlack)

allocations <- read_csv(
  "C:/Users/FLI6SH/OneDrive - cchmc/ACT_Neighborhood/Repository/neighborhood-repository/neighborhood bg allocations.csv",
  col_types = "c---cnc"
) |>
  filter(Municipality == "Cincinnati")

pop <- get_decennial(
  geography = "block group",
  variables = c("P1_001N", "P1_004N", "P3_001N", "P3_004N"),
  state = "OH",
  county = "Hamilton",
  sumfile = "pl",
  year = 2020
) |>
  inner_join(allocations) |>
  mutate(
    value = value*Allocation,
    Neighborhood = str_remove(Neighborhood, "village"),
    Neighborhood = str_remove(Neighborhood, "city"),
    Neighborhood = str_to_title(Neighborhood),
    Neighborhood = str_remove(Neighborhood, "The Village Of"),
    Neighborhood = str_trim(Neighborhood),
    Age = ifelse(str_detect(variable, "P1"), "All", "Adult"),
    Race = ifelse(str_detect(variable, "001"), "All", "Black")
  ) |>
  group_by(Neighborhood, Age, Race) |>
  reframe(estimate = sum(value)) |>
  pivot_wider(
    id_cols = Neighborhood,
    names_from = Age:Race,
    values_from = estimate
  ) |>
  mutate(
    Child_All = All_All-Adult_All,
    Child_Black = All_Black-Adult_Black,
    Child_NonBlack = Child_All-Child_Black
  ) |>
  select(Neighborhood, Child_Black, Child_NonBlack) |>
  rbind(munis) |>
  mutate(
    SDN = Neighborhood %in% sdn | Neighborhood == "Lower Price Hill-Queensgate",
    Avondale = Neighborhood == "Avondale"
  ) |>
  group_by(SDN, Avondale) |>
  reframe(across(Child_Black:Child_NonBlack, sum)) |>
  pivot_longer(
    Child_Black:Child_NonBlack,
    names_to = "Race",
    values_to = "Children"
  ) |>
  mutate(
    Race = ifelse(Race == "Child_Black", "Black", "Other race"),
    Children = round(Children)
    )

admit_hamco <- filter(admits, HamCo == 1)

admit_count <- admit_hamco |>
  group_by(ContactMonth, Race, SDN, Avondale) |>
  reframe(Admissions = n())

ed_count <- filter(ed, HamCo == 1) |>
  group_by(ContactMonth, Race, SDN, Avondale) |>
  reframe(EDVisits = n())

readmit <- select(
  admit_hamco, 
  pat_id:pat_enc_csn_id, 
  discharge_date,
  ContactMonth:Avondale
  ) |>
  filter(ContactMonth < today()-120) |>
  inner_join(
    select(admits, pat_id, pat_enc_csn_id, admission_date), 
    join_by(pat_id),
    multiple = "all"
    ) |>
  filter(
    admission_date >= discharge_date,
    pat_enc_csn_id.x != pat_enc_csn_id.y
  ) |>
  group_by(pat_id, discharge_date, ContactMonth, Race, SDN, Avondale) |>
  reframe(admission_date = min(admission_date)) |>
  mutate(
    Readmit30 = as.numeric(admission_date-discharge_date) <= 30,
    Discharge90 = as.numeric(today()-discharge_date) >= 90,
    Readmit90 = as.numeric(admission_date-discharge_date) <= 90,
    Readmit90 = ifelse(Discharge90, Readmit90, FALSE),
    Discharge365 = as.numeric(today()-discharge_date) >= 365,
    Readmit365 = as.numeric(admission_date-discharge_date) <= 365,
    Readmit365 = ifelse(Discharge365, Readmit365, FALSE)
  ) |>
  group_by(ContactMonth, Race, SDN, Avondale) |>
  reframe(across(c(Readmit30, Readmit90, Readmit365), sum))

admit_ed <- select(
  admit_hamco, 
  pat_id:pat_enc_csn_id, 
  discharge_date,
  ContactMonth:Avondale
) |>
  filter(ContactMonth < today()-120) |>
  inner_join(
    select(ed, pat_id, pat_enc_csn_id, contact_date), 
    join_by(pat_id),
    multiple = "all"
  ) |>
  filter(
    contact_date >= discharge_date,
    pat_enc_csn_id.x != pat_enc_csn_id.y
  ) |>
  group_by(pat_id, discharge_date, ContactMonth, Race, SDN, Avondale) |>
  reframe(contact_date = min(contact_date)) |>
  mutate(
    ED30 = as.numeric(contact_date-discharge_date) <= 30,
    Discharge90 = as.numeric(today()-discharge_date) >= 90,
    ED90 = as.numeric(contact_date-discharge_date) <= 90,
    ED90 = ifelse(Discharge90, ED90, FALSE),
    Discharge365 = as.numeric(today()-discharge_date) >= 365,
    ED365 = as.numeric(contact_date-discharge_date) <= 365,
    ED365 = ifelse(Discharge365, ED365, FALSE)
  ) |>
  group_by(ContactMonth, Race, SDN, Avondale) |>
  reframe(across(c(ED30, ED90, ED365), sum))

bd <- admit_hamco |>
  mutate(discharge_date = coalesce(discharge_date, today())) |>
  filter(discharge_date > admission_date)

bed_days <- data.frame(
  pat_id = bd$pat_id[1],
  pat_enc_csn_id = bd$pat_enc_csn_id[1],
  BD = seq.Date(bd$admission_date[1], bd$discharge_date[1]-1, by = "days")
)

for (i in 2:nrow(bd)){
  x <- data.frame(
    pat_id = bd$pat_id[i],
    pat_enc_csn_id = bd$pat_enc_csn_id[i],
    BD = seq.Date(bd$admission_date[i], bd$discharge_date[i]-1, by = "days")
  )
  bed_days <- rbind(bed_days, x)
}

bed_count <- bed_days |>
  mutate(ContactMonth = floor_date(BD, "month")) |>
  inner_join(select(admit_hamco, pat_enc_csn_id, Race, Avondale, SDN)) |>
  group_by(ContactMonth, Race, Avondale, SDN) |>
  reframe(BedDays = n())

dateframe <- data.frame(
  ContactMonth = rep(unique(admits$ContactMonth), each = 9),
  Race = rep(c("Black", "Other race", NA), each = 3),
  SDN = rep(c(TRUE, TRUE, FALSE), 3),
  Avondale = rep(c(TRUE, FALSE, FALSE), 3)
)

df1 <- left_join(dateframe, admit_count) |>
  left_join(ed_count) |>
  left_join(bed_count) |>
  left_join(readmit) |>
  left_join(admit_ed) |>
  left_join(help) |>
  mutate(across(Admissions:HelpRefers, \(x) coalesce(x, 0)))

reg_frame <- registry |>
  group_by(Race, Avondale, SDN) |>
  reframe(Patients = n()) |>
  full_join(ster) |>
  mutate(Steroids = coalesce(Steroids, 0))

write_csv(df1, "cop measures.csv")
write_csv(reg_frame, "registry measures.csv")
write_csv(pop, "Hamco population.csv")
