#' Identifies seizures with different countries of origin for raw and worked ivory
#'
#' This is needed when calculating weights out.
#' The countries of origin for seizures containing both raw and worked ivory may
#' be different. For example the raw ivory may come from country A and worked ivory from country B.
#' Hence the raw RIE weight needs to be removed for country B's total weight out
#' and the worked RIE weight needs to be removed from country A's total weight out.
#' This function interacts with the ETIS database and the seizures
#' stored in df.1 created by df_quantities_RIE_separate.fn
#' to identify all seizures with a countrt of origin.
#' It then identifies which seizures have different countries of origin for raw and worked ivory
#'
#' The main use of this function is to contribute to the calculation
#' of two new database tables for law enforcement ratio and trade flow accounting.
#____________________________________________________________________________________________________

#' @param year.from Starting year
#' @param year.to Ending year
#' @param statusMin Minimum status of seizure record to include in summary (usually 3)
#' @param df.1  Dataframe with list of seizure records to be considered.
#' Created using df_quantities_RIE_separate.fn
#
#' @return A list with two items.
#' Each item is a dataframe with information about the seizures
#' for which there are different countries of raw and worked origin.
#' double.raw a dataframe for countries that were not a country of raw origin but were a country of worked origin
#' double.raw a dataframe for countries that were not a country of worked origin but were a country of raw origin
#' Each dataframe has two columns
#' (a) An identifying column of two parts separated by a colon giving seizure id:country code (eg 12345:999)
#' (b) worked or raw weight that should not be attributed to that country for that seizure
#'
#' For example for double.wkd the dataframe might look like the following:
#' \tabular{rr}{
#'  seizure_wkd \tab raw \cr
#'  54321:999 \tab 123.45
#' }
#' For seizure 54321 country 999 was a country of worked origin but was not a country of raw origin.
#' Therefore 123.45 kg (the weight of the raw ivory for that seizure) must be removed from the total weight out
#' attributed to this country
#'
#'
#'
#' @seealso
#' \code{\link{df_quantities_RIE_separate}},
#' \code{\link{mult_ctries}},
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
#' double.recs <- double_count(year.from = year.from, year.to = year.to,
#' statusMin = statusMin, df.RIE = df.1)
#' #_______________________________________________________________________

double_count <- function(year.from = year.from,
                               year.to = year.to,
                               statusMin = statusMin,
                               df.RIE = df.1)
{
  SQLstr.r <- '
  SELECT   j.id, j.seizure_id, j.country_id AS ct_orig_raw_id,
  j.proportion, s.status_id, s.seizure_year
  FROM     seizure_raw_origin_countries j
  JOIN     seizures s ON j.seizure_id = s.id
  ORDER BY j.seizure_id'

  df.raw.all <- dbGetQuery(con, SQLstr.r)
  df.raw <- df.raw.all %>%
       filter(status_id >= statusMin
              & seizure_year >= year.from
              & seizure_year <= year.to)

  SQLstr.w <- '
  SELECT   j.id, j.seizure_id, j.country_id AS ct_orig_wkd_id,
  j.proportion, s.status_id, s.seizure_year
  FROM     seizure_worked_origin_countries j
  JOIN     seizures s ON j.seizure_id = s.id
  ORDER BY j.seizure_id'

  df.wkd.all <- dbGetQuery(con, SQLstr.w)
  df.wkd <- df.wkd.all %>%
    filter(status_id >= statusMin
           & seizure_year >= year.from
           & seizure_year <= year.to)

  df.raw.id <- df.raw$seizure_id
  df.wkd.id <- df.wkd$seizure_id

  df.both.id <- df.raw.id[is.element(df.raw.id, df.wkd.id)]

  df.raw.both <- filter(df.raw, is.element(seizure_id, df.both.id))
  df.wkd.both <- filter(df.wkd, is.element(seizure_id, df.both.id))

  # Are countries of raw origin different to countries of worked origin
  df.raw.both$in.wkd <- TRUE
  for (i in 1:nrow(df.raw.both)){
    df.raw.both.id <- df.raw.both$seizure_id[i]
    df.wkd.ctry <- filter(df.wkd.both, seizure_id == df.raw.both.id)
    df.raw.both$in.wkd[i] <- ifelse(is.element(df.raw.both$ct_orig_raw_id[i], df.wkd.ctry$ct_orig_wkd), TRUE, FALSE)
  }

  df.wkd.both$in.raw <- TRUE
  for (i in 1:nrow(df.wkd.both)){
    df.wkd.both.id <- df.wkd.both$seizure_id[i]
    df.raw.ctry <- filter(df.raw.both, seizure_id == df.wkd.both.id)
    df.wkd.both$in.raw[i] <- ifelse(is.element(df.wkd.both$ct_orig_wkd_id[i], df.raw.ctry$ct_orig_raw), TRUE, FALSE)
  }

  # So in this case need to go to the seizure and remove the values where it happens
  # Extra reduction required


    df.raw.both.red <- filter(df.raw.both, !in.wkd) %>%
      mutate(seizure_raw = paste(seizure_id, ct_orig_raw_id, sep = ':'))

   df.wkd.both.red <- filter(df.wkd.both, !in.raw) %>%
      mutate(seizure_wkd = paste(seizure_id, ct_orig_wkd_id, sep = ':'))



    # Get the RIE that it is over by

    RIE.select <- df.RIE %>%
      select(id, RIE.raw, RIE.wkd)

    df.raw.both.red <- df.raw.both.red %>%
      left_join(RIE.select, by = c("seizure_id" = "id")) %>%
      select(seizure_raw, wkd = RIE.wkd )

    df.wkd.both.red <- df.wkd.both.red %>%
      left_join(RIE.select, by = c("seizure_id" = "id")) %>%
      select(seizure_wkd, raw = RIE.raw )


    return(list(double.raw = df.raw.both.red, double.wkd = df.wkd.both.red))
}

#_______________________________________________________________________

# Code written by FM Underwood




