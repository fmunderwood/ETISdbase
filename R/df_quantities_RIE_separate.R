#' Calculates or estimates RIE for raw and worked ivory seizures
#'
#' Produces a list of all seizures and their RIE
#' for raw and worked ivory. For seizures where only the number of pieces
#' is provided the weight is estimated.
#'
#' This function interacts with the ETIS database, to extract seizure information.
#' It also requires the wt est.Rdata file that was created by the last COP analysis to estimate the
#' weight of seizures where only the number of pieces if provided. Note that since previous ETIS analyses,
#' the model estimates weights from log(pieces + 1).
#'
#' The main use of this function is to contribute to the calculation
#' of two new database tables for law enforcement ratio and trade flow accounting.
#'
#' This function is also used in the ETIS database to produce various summary tables
#____________________________________________________________________________________________________

#' @param year.from Starting year
#' @param year.to Ending year
#' @param statusMin Minimum status of seizure record to include in summary (usually 3)
#' @param reg.model .Rdata file that contains parameters for estimating weights from pieces

#' @return A dataframe which lists for each seizure their:
#'   id, country of discovery, year & ivory quantities (weight and pieces)
#'   including RIE separately for raw and worked. \cr Note that seizures which are
#'   all non-ivory or are ivory seizuress with no information about quantities are excluded.

#' @seealso
#' The dataframe is used in \code{\link{mult_ctries}}, and
#' \code{\link{inout_tables_LE_TF_prep}}. These are then processed using
#' \code{\link{inout_tables_LE_TF_final}} to give the final set of outputs.
#'

#' @examples
#' year.from <- 1900
#' year.to <- 2100
#' statusMin <- 3
#'
#' df.1 <- df_quantities_RIE_separate(year.from = year.from, year.to = year.to,
#' statusMin = statusMin, reg.model = 'wt est.Rdata')
#________________________________________________________________________

df_quantities_RIE_separate <-
  function(year.from = 1900,
           year.to = 2100,
           statusMin = 3,
           reg.model = 'wt est.Rdata')
{

SQLstr.1 <- '
SELECT    id, status_id, seizure_year, discovered_country_id,
          raw_pieces, raw_weight, worked_pieces, worked_weight
FROM      public.seizures
WHERE     (raw_pieces > 0 OR raw_weight > 0 OR worked_pieces > 0 OR worked_weight > 0)
ORDER BY id;'

df.1 <- dbGetQuery(con, SQLstr.1)

y0 <- max(min(df.1$seizure_year), year.from)
y1 <- min(max(df.1$seizure_year), year.to)
df.1 <- df.1[(df.1$seizure_year  >=  y0) & (df.1$seizure_year <= y1), ]
df.1 <- df.1[df.1$status_id  >=  statusMin, ]
df.1 <- df.1[, -2]  # finished with status field, so remove it

# compute predicted weights & RIEs:
load(reg.model) #get regression models
jj <- !is.na(df.1$raw_pieces)&is.na(df.1$raw_weight)
df.new <- data.frame(x = log(df.1$raw_pieces[jj] + 1), sz.yr = df.1$seizure_year[jj])
df.1$raw_weight[jj] <- predict(lm.r, newdata = df.new)^(1/lambda.r)

kk <- !is.na(df.1$worked_pieces)&is.na(df.1$worked_weight)
df.new <- data.frame(x = log(df.1$worked_pieces[kk] + 1), sz.yr = df.1$seizure_year[kk])
df.1$worked_weight[kk] <- predict(lm.w, newdata = df.new)^(1/lambda.w)

df.1$raw_weight[is.na(df.1$raw_weight)] <- 0
df.1$worked_weight[is.na(df.1$worked_weight)] <- 0
df.1$RIE.raw <- df.1$raw_weight
df.1$RIE.wkd <- df.1$worked_weight/0.7
df.1$RIE <- df.1$RIE.raw + df.1$RIE.wkd

df.1$raw_weight[df.1$raw_weight == 0] <- NA
df.1$worked_weight[df.1$worked_weight == 0] <- NA

return(df.1)
}
#_______________________________________________________________________

# Original code written by RW Burn
# Code modified by FM Underwood
