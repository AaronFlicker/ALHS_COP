library(tidyverse)
library(odbc)
library(plotly)
library(tidycensus)

## Colors =====================================================================

cchmc_light_green <- "#A1CA3C"
cchmc_dark_green <- "#76BC44"
cchmc_light_blue <- "#9BD3DD"
cchmc_medium_blue <- "#71C5E8"
cchmc_dark_blue <- "#00AEC7"
cchmc_light_purple <- "#CA5699"
cchmc_medium_purple <- "#9E4679"
cchmc_dark_purple <- "#83286B"
cchmc_pink <- "#E64479"
cchmc_dark_grey <- "#55575A"

## Load data ==================================================================

con <- dbConnect(odbc(), "ClarityProd")

patients <- dbGetQuery(con, "
  SELECT DISTINCT dx.pat_id
                  ,p.pat_mrn_id
                  ,p.death_date
                  ,p.gender
    FROM hpceclarity.bmi.dx
    INNER JOIN hpceclarity.bmi.patient p
      ON dx.pat_id = p.pat_id
    WHERE current_icd10_list LIKE 'J45%'
                       ")

registry <- dbGetQuery(con, "
  SELECT x_pat_id AS pat_id
        ,CAST(x_update_date AS DATE) AS LastRegistryUpdateDate
        ,CAST(first_include_dttm AS DATE) AS first_include_dttm
        ,record_id
  FROM hpceclarity.bmi.reg_data_membership
  WHERE registry_id = '100005'
    AND registry_status_c = 1
                       ")

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
        ,CAST(peh.hosp_admsn_time AS DATE) AS contact_date
        ,dx.current_icd10_list
        ,dx.dx_name
        ,dx.line
        ,r.mapped_race
        ,g.Neighborhood
        ,g.statefp
        ,g.countyfp
        ,1 AS Admission
        ,0 AS ED
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
      AND peh.hosp_admsn_time BETWEEN 
        '1/1/2019' AND EOMONTH(DATEADD(MONTH, -1 , GETDATE()))
      AND (a.eff_start_date <= peh.hosp_admsn_time OR a.eff_start_date IS NULL)
      AND (a.eff_end_date > peh.hosp_admsn_time OR a.eff_end_date IS NULL)
      ")

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
        ,0 AS Admission
        ,1 AS ED
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
                   ")

hood_kids <- read_csv(
  "C:/Users/FLI6SH/OneDrive - cchmc/ACT_Neighborhood/Repository/neighborhood-repository/neighborhood maps/Neighborhood dataset.csv",
  col_types = "c-c---------------------------n-n-n-n--------------------------------------------------------------------------"
) |>
  filter(
    County == "Hamilton",
    Neighborhood != "Cincinnati"
  ) |>
  mutate(
    Neighborhood = case_when(
      Neighborhood == "North Avondale-Paddock Hills" ~ 
        "North Avondale - Paddock Hills",
      Neighborhood == "Lower Price Hill-Queensgate" ~ "Lower Price Hill",
      TRUE ~ Neighborhood
    )
  ) |>
  pivot_longer(AgeUnder5:Age15to19) |>
  group_by(Neighborhood) |>
  reframe(Children = sum(value))

hamco_race <- get_acs(
  "county",
  variables = c(
    paste0("B01001B_00", 3:7),
    paste0("B01001B_0", 18:22)
  ),
  state = "OH",
  county = "Hamilton",
  year = 2022
) |>
  summarise(Black = sum(estimate)) |>
  mutate(
    Total = sum(hood_kids$Children),
    NonBlack = Total-Black
  )

## Functions =================================================================

movavg <- function(df){
  for(i in 1:nrow(df)){
    df$MovingAverage[i] <- ifelse(
      year(df$ContactMonth[i]) > 2019,
      sum(df$Measure[i:(i-12)])/13,
      NA
    )
  }
  df
}

admit_count <- function(df, denom){
  df |> group_by(ContactMonth) |>
    reframe(
      Measure = sum(Admission)*100000/denom,
      MovingAverage = NA
    ) |>
    right_join(dateframe, by = c("ContactMonth" = "Month")) |>
    mutate(Measure = coalesce(Measure, 0)) |>
    arrange(ContactMonth) |>
    movavg() |>
    pivot_longer(Measure:MovingAverage) |>
    mutate(
      name = ifelse(name == "Measure", name, "13-month\nmoving average"),
      name = factor(name, levels = c("Measure", "13-month\nmoving average"))
    )
}

movavgchart <- function(df, title){
  x <- ggplot(
    df, 
    aes(x = ContactMonth, y = value, color = name, linetype = name)
  ) +
    geom_line(linewidth = 1) +
    labs(
      x = NULL, 
      y = "Encounters per 100,000 children", 
      color = NULL,
      linetype = NULL,
      title = title
    ) +
    scale_y_continuous(limits = c(0, 300), breaks = seq(0, 300, 20)) +
    scale_color_manual(values = c(cchmc_dark_blue, cchmc_pink)) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = .5))
  ggplotly(x) 
}

ed_count <- function(df, denom){
  df |> group_by(ContactMonth) |>
    reframe(
      Measure = sum(ED)*100000/denom,
      MovingAverage = NA
    ) |>
    right_join(dateframe, by = c("ContactMonth" = "Month")) |>
    mutate(Measure = coalesce(Measure, 0)) |>
    arrange(ContactMonth) |>
    movavg() |>
    pivot_longer(Measure:MovingAverage) |>
    mutate(
      name = ifelse(name == "Measure", name, "13-month\nmoving average"),
      name = factor(name, levels = c("Measure", "13-month\nmoving average"))
    )
}

## Data munging ===============================================================

encounters <- rbind(admits, ed) |>
  pivot_wider(
    id_cols = c(pat_id:contact_date, mapped_race:ED),
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
    )
  )

hamco_encounters <- filter(encounters, HamCo == 1) |>
  group_by(Neighborhood, Race, ContactMonth) |>
  reframe(
    Admission = sum(Admission),
    ED = sum(ED),
    Encounters = n()
  )

dateframe <- data.frame(
  Month = seq.Date(as.Date("2019-01-01"), today()-30, "month")
)

dh <- c(
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

admit_total <- admit_count(hamco_encounters, hamco_race$Total)

admit_deprived <- admit_count(
  filter(hamco_encounters, Neighborhood %in% dh),
  sum(hood_kids$Children[hood_kids$Neighborhood %in% dh])
) 

admit_avondale <- admit_count(
  filter(hamco_encounters, Neighborhood == "Avondale"),
  hood_kids$Children[hood_kids$Neighborhood == "Avondale"]
)

admit_race <- hamco_encounters |>
  filter(!is.na(Race)) |>
  group_by(ContactMonth, Race) |>
  reframe(Admissions = sum(Admission)) |>
  arrange(Race, ContactMonth) |>
  group_by(Race) |>
  mutate(
    denom = ifelse(Race == "Black", hamco_race$Black, hamco_race$NonBlack),
    Measure = Admissions*100000/denom,
    MovingAverage = NA
  ) 
for(i in 1:nrow(admit_race)){
  admit_race$MovingAverage[i] <- ifelse(
    year(admit_race$ContactMonth[i]) > 2019,
    sum(admit_race$Measure[i:(i-12)])/13,
    NA
  )
}
admit_race <- select(admit_race, ContactMonth, Race, Measure, MovingAverage) |>
  pivot_longer(Measure:MovingAverage) |>
  mutate(
    name = ifelse(name == "Measure", name, "13-month\nmoving average"),
    name = factor(name, levels = c("13-month\nmoving average", "Measure"))
  )

ed_total <- ed_count(hamco_encounters, hamco_race$Total)

ed_deprived <- ed_count(
  filter(hamco_encounters, Neighborhood %in% dh),
  sum(hood_kids$Children[hood_kids$Neighborhood %in% dh])
) 

ed_avondale <- ed_count(
  filter(hamco_encounters, Neighborhood == "Avondale"),
  hood_kids$Children[hood_kids$Neighborhood == "Avondale"]
)

ed_race <- hamco_encounters |>
  filter(!is.na(Race)) |>
  group_by(ContactMonth, Race) |>
  reframe(ED = sum(ED)) |>
  arrange(Race, ContactMonth) |>
  group_by(Race) |>
  mutate(
    denom = ifelse(Race == "Black", hamco_race$Black, hamco_race$NonBlack),
    Measure = ED*100000/denom,
    MovingAverage = NA
  ) 
for(i in 1:nrow(ed_race)){
  ed_race$MovingAverage[i] <- ifelse(
    year(ed_race$ContactMonth[i]) > 2019,
    sum(ed_race$Measure[i:(i-12)])/13,
    NA
  )
}
ed_race <- select(ed_race, ContactMonth, Race, Measure, MovingAverage) |>
  pivot_longer(Measure:MovingAverage) |>
  mutate(
    name = ifelse(name == "Measure", name, "13-month\nmoving average"),
    name = factor(name, levels = c("13-month\nmoving average", "Measure"))
  )

## Charts =====================================================================

movavgchart(admit_total, "Hamilton County") |>
  layout(legend = list(x = 0.05, y = 0.95))

admit_race_chart <- ggplot(
  admit_race, 
  aes(x = ContactMonth, y = value, color = Race, linetype = name)
) +
  geom_line(linewidth = 1) +
  labs(
    x = NULL,
    y = "Encounters per 100,000 children",
    title = "By race",
    color = NULL,
    linetype = NULL
  ) +
  scale_y_continuous(limits = c(0, 300), breaks = seq(0, 300, 20)) +
  scale_color_manual(values = c(cchmc_dark_purple, cchmc_light_green)) +
  scale_linetype_manual(values = c("dashed", "solid")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = .5)) +
  guides(linetype = "none")
ggplotly(admit_race_chart) |> layout(legend = list(x = .85, y = .95))

movavgchart(admit_deprived, "Structually deprived neighborhoods") |>
  layout(showlegend = FALSE)

movavgchart(admit_avondale, "Avondale") |>
  layout(showlegend = FALSE)

movavgchart(ed_total, "Hamilton County") |>
  layout(showlegend = FALSE)

ed_race_chart <- ggplot(
  ed_race, 
  aes(x = ContactMonth, y = value, color = Race, linetype = name)
) +
  geom_line(linewidth = 1) +
  labs(
    x = NULL,
    y = "Encounters per 100,000 children",
    title = "By race",
    color = NULL,
    linetype = NULL
  ) +
  scale_y_continuous(limits = c(0, 300), breaks = seq(0, 300, 20)) +
  scale_color_manual(values = c(cchmc_dark_purple, cchmc_light_green)) +
  scale_linetype_manual(values = c("dashed", "solid")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = .5)) +
  guides(linetype = "none")
ggplotly(ed_race_chart)

movavgchart(ed_deprived, "Structually deprived neighborhoods") |>
  layout(showlegend = FALSE)

movavgchart(ed_avondale, "Avondale") |>
  layout(showlegend = FALSE)







