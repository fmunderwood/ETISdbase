#' Prepares data for  szs and wgts in and out
#'
#' The output from this function is a list of seizures and weights in and out
#' that has not been adjusted for multiple countries of origin
#' This function interacts with the ETIS database and the list of relevant seizures
#' stored in df.1 from df_quantities_RIE_separate.fn

#' The main use of this function is to contribute to the calculation
#' of two new database tables for law enforcement ratio and trade flow accounting.
#'
#' This program also deals with seizure 109462.
#' One of the countries of transit that is listed occurs after the country of discovery.
#' For that country, the seizure needs to be included in trade flow accounting and excluded from LE ratio accounting

#' @param year.from Starting year
#' @param year.to Ending year
#' @param statusMin Minimum status of seizure record to include in summary (usually 3)
#' @param ctry_dest_included true if ctry of dest included in computing szs out
#' TRUE for trade flows and FALSE for LE ratio accounting
#' @param df.1  Dataframe with list of seizure records to be considered.
#' Created using df_quantities_RIE_separate.fn

#' @return a list with two items
#' df_inout A dataframe listing seizures and weights in and out for each country in each year
#' ctry_dest_included Flag denoting whether or not country of destination included

#' @seealso \code{\link{df_quantities_RIE_separate}},
#' \code{\link{mult_ctries}},
#' \code{\link{inout_tables_final}}
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
#'
#' ctry_dest_included <- FALSE
#'
#' df.inout <- inout_tables_prep(year.from = year.from, year.to = year.to,
#' statusMin = statusMin, ctry_dest_included = ctry_dest_included,
#' df.1 = df.1)
#--------------------------------------------------------------------------
inout_tables_prep <-
  function(year.from = 1900,
           year.to = 2100,
           statusMin = 3,
           ctry_dest_included,
           df.1 = df.1)
  {
# ids for all countries "in"
ct.id.used.in <- sort(unique(df.1$discovered_country_id))

# df for ctries of orig, expt, tran & dest each distinct from ctry of disc (non-ivory szs removed)
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
      j1.country_id AS ct_orig_id,
      j1r.country_id AS ct_raw_orig_id,
      j1w.country_id AS ct_wkd_orig_id,
      j2.country_id AS ct_expt_id,
      j3.ct_id AS ct_tran_id,
      s.destination_country_id
  FROM      public.seizures s
  LEFT JOIN      public.seizure_origin_countries j1 ON s.id = j1.seizure_id
  LEFT JOIN      public.seizure_raw_origin_countries j1r ON s.id = j1r.seizure_id
  LEFT JOIN      public.seizure_worked_origin_countries j1w ON s.id = j1w.seizure_id
  LEFT JOIN      public.seizure_export_countries j2 ON s.id = j2.seizure_id
  LEFT JOIN      tran_junc j3 ON s.id = j3.seizure_id
  WHERE     s.raw_pieces > 0 OR s.raw_weight > 0 OR s.worked_pieces > 0 OR s.worked_weight > 0
  ORDER BY  s.id)

--  remove country (set to NULL) if same as country of discovery ...
  SELECT    id, status_id, seizure_year,
        NULLIF(ct_orig_id, discovered_country_id) AS ct_orig_mod,
        NULLIF(ct_raw_orig_id, discovered_country_id) AS ct_raw_orig_mod,
        NULLIF(ct_wkd_orig_id, discovered_country_id) AS ct_wkd_orig_mod,
        NULLIF(ct_expt_id, discovered_country_id) AS ct_expt_mod,
        NULLIF(ct_tran_id, discovered_country_id) AS ct_tran_mod,
        NULLIF(destination_country_id, discovered_country_id) AS ct_dest_mod
  FROM      tmp
  ORDER BY  id
;'

df.2 <- dbGetQuery(con, SQLstr.2)  # has multiple rows for szs where multiple countries occur
y0 <- max(min(df.1$seizure_year), year.from)
y1 <- min(max(df.1$seizure_year), year.to)
df.2 <- df.2[(df.2$seizure_year >= y0) & (df.2$seizure_year <= y1), ]
df.2 <- df.2[df.2$status_id >= statusMin, ]
df.2 <- df.2[, -2]  # finished with status field, so remove it

#===================================================================
# Adjustment for seizure 109462
# Travelled from 110 (export) to 188 (transit opportunity to seize)
# But 188 notified of shipment (by 110) and they sent it back to 110
# When ctry_dest_included is FALSE (for LE ratio calculations)
# Need to exclude 188 from seizures out.
#===================================================================
if(!ctry_dest_included){
  df.2$ct_tran_mod[df.2$id==109462] <- NA
}

# create list cts - each component (one for each seizure) is a vector of unique
# countries (ids) recorded outside the country of discovery

ID <- unique(df.2$id)
cts <- list(NULL)
yrs <- numeric(length(ID))

first <- function(x) x[1]

tt <- list(NULL)
for (i in 1:6)
  tt[[i]]<-tapply(df.2[, i + 2], df.2$id, unique, simplify = F)
yr.use <- tapply(df.2[, 2], df.2[, 1], first)

tt.id <- list(NULL)
yr.id <- list(NULL)
for (i in 1:6){
  lgth <- unlist(lapply(tt[[i]], length))
  tt.id[[i]] <- rep(names(tt[[i]]), lgth)
  yr.id[[i]] <- rep(yr.use, lgth)
}


if (ctry_dest_included){
  yr.id <- c(yr.id[[1]], yr.id[[2]], yr.id[[3]], yr.id[[4]], yr.id[[5]], yr.id[[6]])
  res <- cbind(as.numeric(c(tt.id[[1]], tt.id[[2]], tt.id[[3]], tt.id[[4]], tt.id[[5]], tt.id[[6]])),
               as.numeric(c(unlist(tt[[1]]), unlist(tt[[2]]), unlist(tt[[3]]), unlist(tt[[4]]), unlist(tt[[5]]), unlist(tt[[6]]))),
               yr.id)
} else {
  yr.id <- c(yr.id[[1]], yr.id[[2]], yr.id[[3]], yr.id[[4]], yr.id[[5]])
  res <- cbind(as.numeric(c(tt.id[[1]], tt.id[[2]], tt.id[[3]], tt.id[[4]], tt.id[[5]])),
               as.numeric(c(unlist(tt[[1]]), unlist(tt[[2]]), unlist(tt[[3]]), unlist(tt[[4]]), unlist(tt[[5]]))),
               yr.id)
}

ii <- is.na(res[, 2])
res <- res[!ii, ]
cts <-tapply(res[, 2], res[, 1], unique)

yr <- tapply(res[, 3], res[, 1], first)

#_______________________________________________________________________
# create years & RIE vectors to match (unlisted) countries in cts
len.cts <- unlist(lapply(cts, length))
yr.rep <- rep(yr, len.cts)
yr.val <- sort(unique(df.2$seizure_year))        # years that occur in DB
yr.out.fac <- factor(yr.rep, levels = yr.val)      # years as factor
df.RIE <- df.1[is.element(df.1$id, as.numeric(names(cts))), ]
df.RIE <- df.RIE[order(df.RIE$id), ]
RIE.out <- rep(df.RIE$RIE, len.cts)


# ids for all ctries "in" & "out"
ct.id.used.out <- unique(unlist(df.2[, 3:8]))
ct.id.used.all <- sort(union(ct.id.used.in, ct.id.used.out))


# create countries factor for table of sz out
SQLstr.c <- 'SELECT id, code FROM public.countries ORDER BY id;'
df.ctry <- dbGetQuery(con, SQLstr.c, stringsAsFactors = F)
df.used.all <- subset(df.ctry, id %in% ct.id.used.all)
code.used.all <- df.used.all$code                # country codes used ("in" and "out")
code.used.all[is.na(code.used.all)] <- "NA"      # put Namibia (NA) back in!
cts.vec <- unlist(cts)
ct.out.fac <- factor(as.numeric(cts.vec), levels = ct.id.used.all, labels = code.used.all)


# seizures & wgts "out" tables
sz.out.tbl <- table(ct.out.fac, yr.out.fac)
wt.out.tbl <- tapply(RIE.out, list(ct.out.fac, yr.out.fac), sum, na.rm = T)
wt.out.tbl[is.na(wt.out.tbl)] <- 0

# dataframes for analysis
df.sz.out <- as.data.frame(sz.out.tbl)
df.wt.out <- as.data.frame(as.table(wt.out.tbl))

# seizures & wgts "in" tables
# first make country & year factors with same levels & labels as in the "out" table
ct.in.fac <- factor(df.1$discovered_country_id, levels = ct.id.used.all, labels = code.used.all)
yr.in.fac <- factor(df.1$seizure_year, levels = yr.val)
sz.in.tbl <- table(ct.in.fac, yr.in.fac)
wt.in.tbl <- tapply(df.1$RIE, list(ct.in.fac, yr.in.fac), sum, na.rm = T)
wt.in.tbl[is.na(wt.in.tbl)] <- 0

# dataframes for analysis
df.sz.in <- as.data.frame(sz.in.tbl)
df.wt.in <- as.data.frame(as.table(wt.in.tbl))

# LE ratio table
LE.rat.tbl <- sz.in.tbl/(sz.in.tbl + sz.out.tbl)

# combined df for analysis
df.inout <- data.frame(ctry = df.sz.in[, 1], year = df.sz.in[, 2],
                       sz.in = df.sz.in[, 3], sz.out = df.sz.out[, 3],
                       wt.in = df.wt.in[, 3], wt.out = df.wt.out[, 3])
df.inout$id <- 1:dim(df.inout)[1]
df.inout$year <- as.numeric(as.character(df.inout$year))
df.inout <- df.inout[, c(7, 1:6)]

# OUTPUT
return(list(df.inout = df.inout, ctry_dest_included = ctry_dest_included))  # Make sure  next steps make sense
}
#_______________________________________________________________________

# Original code written by RW Burn
# Code modified by FM Underwood
