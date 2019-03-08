#' Identifies seizures with multiple countries of origin
#'
#' The weight of a seizure with multiple countries of origin
#' is allocated according to the proportion allocated to each country.
#' It also identifies the countries listed at each stage in the trade chain.
#' This is required for trade flows and le ratio accounting.

#' This function interacts with the ETIS database and the seizures
#' stored in df.1 created by df_quantities_RIE_separate.fn
#' to identify all seizures with multiple countries of origin.
#' For each seizure with multiple countries of origin it extracts
#' countries of origin, discovery and transit (including export).

#' The main use of this function is to contribute to the calculation
#' of two new database tables for law enforcement ratio and trade flow accounting.

#____________________________________________________________________________________________________

#' @param year.from Starting year
#' @param year.to Ending year
#' @param statusMin Minimum status of seizure record to include in summary (usually 3)
#' @param df.1  Dataframe with list of seizure records to be considered.
#' Created using df_quantities_RIE_separate.fn
#
#' @return A list with four items.
#' Each item is a dataframe with information about the seizures
#' for which there are multiple countries of origin. The dataframes are:

#'\tabular{ll}{
#' disc \tab Country of discovery \cr
#'  \tab One row for each seizure \cr
#' transit \tab Trade route countries \cr
#'  \tab Multiple rows for each seizure to give all combinations
#'          of export and transit countries (with seizure opportunity) \cr
#' orig_raw \tab Countries of origin for raw ivory \cr
#' \tab One row for each country of origin for each seizure.
#'          Includes the proportion of shipment allocated to each country \cr
#' orig_wkd \tab Countries of origin for worked ivory\cr
#'  \tab One row for each country of origin for each seizure.
#'          Includes the proportion of shipment allocated to each country \cr
#' }

#'
#' @seealso
#' \code{\link{df_quantities_RIE_separate}},
#' \code{\link{inout_tables_LE_TF_prep}},
#' \code{\link{inout_tables_LE_TF_final}}
#'
#' @examples
#' year.from <- 1900
#' year.to <- 2100
#' statusMin <- 3
#'
#' df.1 <- df_quantities_RIE_separate(year.from = year.from, year.to = year.to,
#' statusMin = statusMin, reg.model = 'wt est.Rdata')
#'
#' mult.dat <- mult_ctries(year.from = 1900, year.to = 2100, statusMin = 3, df.1 = df.1)
#_______________________________________________________________________
#
mult_ctries <-
  function(year.from = 1900,
           year.to = 2100,
           statusMin = 3,
           df.1 = df.1)
{
df.wts <- df.1 %>%
  select(seizure_id = id, RIE.raw, RIE.wkd, RIE)
# NB: not same as in sz_inout_tables.R - ct_orig_mod removed ...
SQLstr.2 <- '
WITH
  tran_junc AS (
  SELECT  seizure_id,
    CASE WHEN seizure_opportunity THEN country_id
    ELSE NULL
    END AS ct_id            -- set to NULL if no seizure opportunity
  FROM public.seizure_transit_countries ),

tmp AS (
SELECT    s.id, s.status_id, s.seizure_year, s.discovered_country_id,
          j1r.country_id AS ct_raw_orig_id,
          j1w.country_id AS ct_wkd_orig_id,
          j2.country_id AS ct_expt_id,
          j3.ct_id AS ct_tran_id,
          s.destination_country_id
FROM      public.seizures s
LEFT JOIN      public.seizure_raw_origin_countries j1r ON s.id = j1r.seizure_id
LEFT JOIN      public.seizure_worked_origin_countries j1w ON s.id = j1w.seizure_id
LEFT JOIN      public.seizure_export_countries j2 ON s.id = j2.seizure_id
LEFT JOIN      tran_junc j3 ON s.id = j3.seizure_id
WHERE     s.raw_pieces>0 OR s.raw_weight>0 OR s.worked_pieces>0 OR s.worked_weight>0
ORDER BY  s.id)
--  remove country (set to NULL) if same as country of discovery ...
SELECT    id, status_id, seizure_year,
          NULLIF(ct_raw_orig_id, discovered_country_id) AS ct_raw_orig_mod,
          NULLIF(ct_wkd_orig_id, discovered_country_id) AS ct_wkd_orig_mod,
          NULLIF(ct_expt_id, discovered_country_id) AS ct_expt_mod,
          NULLIF(ct_tran_id, discovered_country_id) AS ct_tran_mod,
          NULLIF(destination_country_id, discovered_country_id) AS ct_dest_mod
FROM      tmp
ORDER BY  id
;'

df.countries <- dbReadTable(con, 'countries')
df.countries <- df.countries %>%
  select(id, code)

df.1.lj <- left_join(df.1, df.countries, by = c('discovered_country_id' = 'id')) %>%
  mutate(disc_code = code) %>%
  select(-code)

df.2 <- dbGetQuery(con, SQLstr.2)  # has multiple rows for szs where multiple countries occur
y0 <- max(min(df.1.lj$seizure_year), year.from)
y1 <- min(max(df.1.lj$seizure_year), year.to)
df.2 <- df.2 %>%
  filter(seizure_year >= y0 & seizure_year <= y1) %>%
  filter(status_id >= statusMin) %>%
  select(-status_id)

df.2 <- left_join(df.2, df.countries, by = c('ct_raw_orig_mod' = 'id')) %>%
  mutate(raw_orig_code = code) %>% select(-code)
df.2 <- left_join(df.2, df.countries, by = c('ct_wkd_orig_mod' = 'id')) %>%
  mutate(wkd_orig_code = code) %>% select(-code)
df.2 <- left_join(df.2, df.countries, by = c('ct_expt_mod' = 'id')) %>%
  mutate(expt_code = code) %>% select(-code)
df.2 <- left_join(df.2, df.countries, by = c('ct_tran_mod' = 'id')) %>%
  mutate(tran_code = code) %>% select(-code)
df.2 <- left_join(df.2, df.countries, by = c('ct_dest_mod' = 'id')) %>%
  mutate(dest_code = code) %>% select(-code)


# find szs with multiple ctries of origin:

# raw
SQLstr.r <- '
SELECT   j.id, j.seizure_id, j.country_id AS ct_orig_raw_id,
         j.proportion, s.status_id, s.seizure_year
FROM     seizure_raw_origin_countries j
JOIN     seizures s ON j.seizure_id = s.id
WHERE    j.proportion < 100
ORDER BY j.seizure_id'

df.raw <- dbGetQuery(con, SQLstr.r)
df.raw <- df.raw %>%
  filter(status_id >=  statusMin) %>%
  filter(seizure_year >=  year.from, seizure_year <=  year.to) %>%
  select( -status_id)

df.raw <- inner_join(df.raw, df.wts, by  = 'seizure_id')
df.raw <- df.raw %>%
  mutate(prop_raw_wt = RIE.raw*proportion/100)

df.raw <- left_join(df.raw, df.countries, by = c('ct_orig_raw_id' = 'id')) %>%
  mutate(raw_orig_code = code) %>% select(-code)

# wkd
SQLstr.w <- '
SELECT   j.id, j.seizure_id, j.country_id AS ct_orig_wkd_id,
         j.proportion, s.status_id, s.seizure_year
FROM     seizure_worked_origin_countries j
JOIN     seizures s ON j.seizure_id = s.id
WHERE    j.proportion < 100
ORDER BY j.seizure_id'

df.wkd <- dbGetQuery(con, SQLstr.w)
df.wkd <- df.wkd %>%
  filter(status_id >=  statusMin) %>%
  filter(seizure_year >=  year.from, seizure_year <=  year.to) %>%
  select( -status_id)

df.wkd <- inner_join(df.wkd, df.wts, by  = 'seizure_id')
df.wkd <- df.wkd %>%
  mutate(prop_wkd_wt = RIE.wkd*proportion/100)

df.wkd <- left_join(df.wkd, df.countries, by = c('ct_orig_wkd_id' = 'id')) %>%
  mutate(wkd_orig_code = code) %>% select(-code)

# all seizures with mult cts of orig:
sz.r <- sort(unique(df.raw$seizure_id))
sz.w <- sort(unique(df.wkd$seizure_id))
sz.rw <- sort(unique(c(sz.r, sz.w)))

# szs with mult cts of orig:
df.1m <- df.1.lj %>%
  filter(id %in% sz.rw)
df.2m <- df.2 %>%
  filter(id %in% sz.rw)

return(list(disc = df.1m, transit = df.2m, orig_raw = df.raw, orig_wkd = df.wkd))
}
#_______________________________________________________________________

# Original code written by RW Burn
# Code modified by FM Underwood
