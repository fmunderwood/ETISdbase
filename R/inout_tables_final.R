#' Adjusts seizures and weights out given multiple countries of origin.
#'
#' Computes corrected number of seizures and weights in and out after adjusting for
#' multiple countries of origin. Outputs can be saved as a csv file, or into the
#' database or as an Excel Workbook.
#'
#' The output from this function is a list of seizures and weights in and out.
#' The user can choose which outputs are required.
#'
#' This function interacts with the ETIS database and the list of relevant seizures
#' stored in df.1 from df_quantities_RIE_separate.fn.
#'
#' The main use of this function is to contribute to the calculation
#' of two new database tables for law enforcement ratio and trade flow accounting.
#'
#' This function makes an adjustment for seizure 109462. For this seizure there is
#' a country of transit listed that falls after the country of discovery. For that country,
#' the seizure needs to be included in trade flow but not LE ratio accounting.
#'
#' @param year.from Starting year
#' @param year.to Ending year
#' @param statusMin Minimum status of seizure record to include in summary (usually 3)
#' @param ctry_dest_included true if ctry of dest included in computing szs out
#' TRUE for trade flows and FALSE for LE ratio accounting. If this is different to
#' the options for df.inout then the function aborts
#' @param size.min This is by default set to zero but is useful when wanting list of seizures
#' used in cluster analysis when a size cutoff is used
#' @param which.out which outputs to include. Denotes which of the outputs to be
#' produced. If it includes 2 then year.from = 1900, year.to = 2100 and size.min = 0.
#' @param mult List created using mult_ctries() with countries of
#' discovery, transit and destination for seizures with multiple countries of origin
#' @param df.inout  List created using inout_tables_LE_TF_prep with  list
#' of seizures and weights in and out uncorrected for multiple countries of origin
#'@param double.recs  List created using double_count of seizures in which countries of raw
#' and worked origin are not (all) the same
#'
#' @return This writes files to three different places. Each gives
#' seizures and weights in and out for each country and year specified:
#' \enumerate{
#' \item csv file: NAME.csv
#' \item database table: szwt_NAME - requires size.min = 0, year.from = 1900, year.to = 2100
#' \item Excel workbook: NAME.xlsx with separate sheets for:
#'     sz in, sz out, wt in, wt out, sz flow, wt flow
#'    }
#' The file names depend on the accounting method that is required and the years included.
#' So file name might be LE_1900_2100.csv or TF_1900_2100.csv.
#' The start of the file name relates to:
#' \tabular{rl}{
#' LE \tab for LE ratio accounting - the country of destination excluded \cr
#' TF \tab if for trade flow accounting - the country of destination included \cr
#' }
#' The years are set as this large range when writing to the database#
#' @seealso \code{\link{df_quantities_RIE_separate}},
#' \code{\link{mult_ctries}},
#' \code{\link{inout_tables_prep}}
#' \code{\link{double_count}}
#'
#' @examples
#' year.from <- 1900
#' year.to <- 2100
#' statusMin <- 3
#' which.out <- 1:3
#'
#' df.1 <- df_quantities_RIE_separate(year.from = year.from, year.to = year.to,
#' statusMin = statusMin, reg.model = 'wt est.Rdata')
#'
#' mult.dat <- mult_ctries(year.from = year.from, year.to = year.to, statusMin = statusMin, df.1 = df.1)
#'
#' double.recs <- double_count(year.from = year.from, year.to = year.to, statusMin = statusMin, df.RIE = df.1)
#'
#' ctry_dest_included <- FALSE
#'
#' df.inout <- inout_tables_prep(year.from = year.from,year.to = year.to,
#'statusMin = statusMin,ctry_dest_included = ctry_dest_included, df.1 = df.1)
#'
#' inout_tables_final(year.from = year.from, year.to = year.to,
#' statusMin = statusMin, ctry_dest_included = ctry_dest_included,
#' size.min = size.min, which.out = which.out, mult.data = mult.dat,
#' inout.data = df.inout, double.recs = double.recs )
#_______________________________________________________________________
inout_tables_final <-
  function(year.from = 1900,
           year.to = 2100,
           statusMin = 3,
           ctry_dest_included,
           size.min = 0,
           which.out = 1:3,
           mult.data,
           inout.data,
           double.recs
           )
{
year.from.all <- year.from
year.to.all <- year.to

year.from.use <- year.from
year.to.use <- year.to

# Check that country of destination is treated in the same way
if (ctry_dest_included != inout.data$ctry_dest_included){
  stop('Stop process: Country of destination flags are not equal')
}

transit <- mult.data$transit
disc <- mult.data$disc

orig_raw <- mult.data$orig_raw
orig_wkd <- mult.data$orig_wkd

df.inout <- inout.data$df.inout

# ======================================================================
# Adjust output for seizures 109462 when ctry_dest_included = FALSE
# Remove ct_tran_mod = 188 and tran_code = SG from transit data frame
# Due to special circmustances for this seizure - see function details
if (!ctry_dest_included){
  transit$ct_tran_mod[transit$id == 109462] <- NA
  transit$tran_code[transit$id == 109462] <- NA
}
# ======================================================================

# Years to use
yr.use <- year.from.use:year.to.use

# Tidying up all dataframes so that
# Only relevant years are used
# Country code is in lower case - and deal with NA issue

# Original number and weight in and out
# Produce key for year and country

df.inout_tidy <- df.inout %>%
  filter(is.element(year, yr.use)) %>%
  mutate(ctry = tolower(ctry)) %>%
  mutate(ctry = replace_na(ctry, "na")) %>%
  mutate(year.ctry = paste(ctry, year, sep = ':'))

# Seizure and reporting rate adjustment

# Country of origin
orig_raw_tidy <- orig_raw %>%
  filter(is.element(seizure_year, yr.use))  %>%
  mutate(raw_orig_code = tolower(raw_orig_code)) %>%
  mutate(raw_orig_code = replace_na(raw_orig_code, "na"))

orig_wkd_tidy <- orig_wkd %>%
  filter(is.element(seizure_year, yr.use))  %>%
  mutate(wkd_orig_code = tolower(wkd_orig_code)) %>%
  mutate(wkd_orig_code = replace_na(wkd_orig_code, "na"))

# Country of discovery
# Only one country of discovery for each seizure
disc_tidy <- disc %>%
  filter(!is.na(discovered_country_id)) %>%
  select(id, discovered_country_id, disc_code) %>%
  mutate(disc_code = tolower(disc_code)) %>%
  mutate(disc_code = replace_na(disc_code, "na"))

# Select only rows with export countries listed
export_tidy <- transit %>%
  filter(!is.na(ct_expt_mod)) %>%
  select(id, ct_expt_mod, expt_code) %>%
  distinct() %>%
  mutate(expt_code = tolower(expt_code)) %>%
  mutate(expt_code = replace_na(expt_code, "na"))

# Select only rows with transit countries listed
transit_tidy <- transit %>%
  filter(!is.na(ct_tran_mod)) %>%
  select(id, ct_tran_mod, tran_code) %>%
  distinct() %>%
  mutate(tran_code = tolower(tran_code)) %>%
  mutate(tran_code = replace_na(tran_code, "na"))

# Obtain country of destination for each seizure
# Only necessary if dest TRUE
if (ctry_dest_included){
  dest_tidy <- transit %>%
    filter(!is.na(ct_dest_mod)) %>%
    select(id, ct_dest_mod, dest_code) %>%
    distinct() %>%
    mutate(dest_code = tolower(dest_code)) %>%
    mutate(dest_code = replace_na(dest_code, "na"))
}

# Now only select seizures where either raw or worked RIE is more than size constraint
orig_raw_tidy <- orig_raw_tidy %>%
  filter(RIE.raw >= size.min)

orig_wkd_tidy <- orig_wkd_tidy %>%
  filter(RIE.wkd >= size.min)

# Select seizures with these values only
disc_tidy <- disc_tidy %>%
  filter(is.element(id, c(orig_raw_tidy$seizure_id, orig_wkd_tidy$seizure_id)))

#==================================================================
# Indicators to mark 1 if country is exclusively an origin country
# Or zero if also a country of discovery, export or transit
# And calculate what reduction is necessary
#==================================================================

# RAW SEIZURES

# Combine discovery information with origin information
# And create a year x country of discovery key
mutual_raw <- orig_raw_tidy %>%
  left_join(disc_tidy, by = c("seizure_id" = "id"))  %>%
  mutate(disc = ifelse(ct_orig_raw_id == discovered_country_id, 0, 1)) %>%
  mutate(year.ctry.disc = paste(disc_code, seizure_year, sep = ':'))

# Combine export information with origin information
# And indicate if country of export is also a country of origin

mutual_raw <- mutual_raw %>%
  left_join(export_tidy, by = c("seizure_id" = "id")) %>%
  mutate(export = ifelse(ct_orig_raw_id == ct_expt_mod, 0, 1)) %>%
  mutate(export = replace_na(export, 1))

# Combine transit information with origin information
# And indicate if country of transit is also a country of origin

mutual_raw <- mutual_raw %>%
  left_join(transit_tidy, by = c("seizure_id" = "id")) %>%
  mutate(transit = ifelse(ct_orig_raw_id == ct_tran_mod, 0, 1)) %>%
  mutate(transit = replace_na(transit, 1))

# Combine destination information with origin information
# And indicate if country ofdestination is also a country of origin

  if (ctry_dest_included){
    mutual_raw <- mutual_raw %>%
      left_join(dest_tidy, by = c("seizure_id" = "id")) %>%
#    mutate(dest = ifelse(ctry_dest_included, ifelse(ct_orig_raw_id == ct_dest_mod, 0, 1), 1)) %>%
      mutate(dest = ifelse(ct_orig_raw_id == ct_dest_mod, 0, 1)) %>%
      mutate(dest = replace_na(dest, 1))
  } else {
     mutual_raw <- mutual_raw %>%
      mutate(dest = 1)
  }


# Calculate how much of RIE should be removed from initial calculation
# This only occurs for seizures where country is only listed
# as a country of origin
# Calculate amount to reduce estimate by

mutual_raw_reduce <- mutual_raw %>%
  mutate(reduce = disc * export * transit * dest) %>%
  mutate(orig_id = paste(raw_orig_code, seizure_id, sep=":"))


# If it is not the right size then a complete reduction is required
mutual_raw_reduce <- mutual_raw_reduce %>%
  mutate(complete_red = prop_raw_wt < size.min)

raw_product <- mutual_raw_reduce %>%
  group_by(seizure_id, raw_orig_code, year.ctry.disc) %>%
  summarise(red = prod(reduce)) %>%
  filter(red == 1) %>%
  mutate(orig_id = paste(raw_orig_code, seizure_id, sep=":"))

mutual_raw_use <- mutual_raw_reduce %>%
  filter(is.element(orig_id, raw_product$orig_id)) %>%
  mutate(amount.reduce = ifelse(complete_red,
                                RIE.raw,
                                reduce * (100 - proportion) * RIE.raw  / 100),
          sz.reduce = ifelse(complete_red, 1, 0)
      )
# Extra column added here seizure_raw for use in double counting
mutual_raw_sum <- mutual_raw_use %>%
  mutate(seizure_raw = paste(seizure_id, ct_orig_raw_id, sep = ':')) %>%
  select(seizure_id, seizure_year, raw_orig_code, ct_orig_raw_id, amount.reduce, sz.reduce, seizure_raw) %>%
  distinct()


mutual_raw_sum_chk <- mutual_raw_use %>%
  group_by(seizure_id, seizure_year, raw_orig_code) %>%
  summarise(am.reduce = first(amount.reduce),
            sz.reduce = first(sz.reduce))



# WORKED SEIZURES

# Combine discovery information with origin information
mutual_wkd <- orig_wkd_tidy %>%
  left_join(disc_tidy, by = c("seizure_id" = "id"))  %>%
  mutate(disc = ifelse(ct_orig_wkd_id == discovered_country_id, 0, 1)) %>%
  mutate(year.ctry.disc = paste(disc_code, seizure_year, sep = ':'))


# Combine export information with origin information
# And indicate if country of export is also a country of origin
mutual_wkd <- mutual_wkd %>%
  left_join(export_tidy, by = c("seizure_id" = "id")) %>%
  mutate(export = ifelse(ct_orig_wkd_id == ct_expt_mod, 0, 1)) %>%
  mutate(export = replace_na(export, 1))

# Combine transit information with origin information
# And indicate if country of transit is also a country of origin
mutual_wkd <- mutual_wkd %>%
  left_join(transit_tidy, by = c("seizure_id" = "id")) %>%
  mutate(transit = ifelse(ct_orig_wkd_id == ct_tran_mod, 0, 1)) %>%
  mutate(transit = replace_na(transit, 1))

# Combine destination information with origin information
# And indicate if country of destination is also a country of origin
if (ctry_dest_included){
  mutual_wkd <- mutual_wkd %>%
    left_join(dest_tidy, by = c("seizure_id" = "id")) %>%
#    mutate(dest = ifelse(ctry_dest_included, ifelse(ct_orig_wkd_id == ct_dest_mod, 0, 1), 1)) %>%
    mutate(dest = ifelse(ct_orig_wkd_id == ct_dest_mod, 0, 1)) %>%
    mutate(dest = replace_na(dest, 1))
  } else {
    mutual_wkd <- mutual_wkd %>%
      mutate(dest = 1)
  }


# Calculate how much of RIE should be removed from initial calculation
# If country is only one of several countries of origin then
# only count proportion of ivory that came from the country
mutual_wkd_reduce <- mutual_wkd %>%
  mutate(reduce = disc * export * transit * dest) %>%
  mutate(orig_id = paste(wkd_orig_code, seizure_id, sep=":"))

# If it is not the right size then a complete reduction is required
mutual_wkd_reduce <- mutual_wkd_reduce %>%
  mutate(complete_red = prop_wkd_wt < size.min)


wkd_product <- mutual_wkd_reduce %>%
  group_by(seizure_id, wkd_orig_code, year.ctry.disc) %>%
  summarise(red = prod(reduce)) %>%
  filter(red == 1) %>%
  mutate(orig_id = paste(wkd_orig_code, seizure_id, sep=":"))

mutual_wkd_use <- mutual_wkd_reduce %>%
  filter(is.element(orig_id, wkd_product$orig_id)) %>%
  mutate(amount.reduce = ifelse(complete_red,
                                RIE.wkd,
                                reduce * (100 - proportion) * RIE.wkd  / 100),
         sz.reduce = ifelse(complete_red, 1, 0)
  )
# Extra column added here seizure_wkd for use in double counting
mutual_wkd_sum <- mutual_wkd_use %>%
  mutate(seizure_wkd = paste(seizure_id, ct_orig_wkd_id, sep = ':')) %>%
  select(seizure_id, seizure_year, wkd_orig_code, ct_orig_wkd_id, amount.reduce, sz.reduce, seizure_wkd) %>%
  distinct()


mutual_wkd_sum_chk <- mutual_wkd_use %>%
  group_by(seizure_id, seizure_year, wkd_orig_code) %>%
  summarise(am.reduce = first(amount.reduce),
            sz.reduce = first(sz.reduce))


# Combine raw and worked summaries
# With original information

# Summarise amount to reduce for  each year and country
# And create a common key
# RAW

#### NEW CODE ADDED HERE for double counting
mutual_raw_sum_wkd <- mutual_raw_sum %>%
  left_join(double.recs$double.raw, by =  "seizure_raw") %>%
  mutate(wkd = replace_na(wkd, 0))

# Adapt this as is the case for the worked below
raw_summ <- mutual_raw_sum_wkd %>%
  group_by(raw_orig_code, seizure_year) %>%
  summarise(raw_reduce = sum(amount.reduce, na.rm = T),
            raw_sz_reduce = sum(sz.reduce, na.rm = T)) %>%
  mutate(year.ctry = paste(raw_orig_code, seizure_year, sep = ':'))


raw_summ_wkd <- mutual_raw_sum_wkd %>%
  group_by(raw_orig_code, seizure_year) %>%
  summarise(wkd_double = sum(wkd, na.rm = T)) %>%
  mutate(year.ctry = paste(raw_orig_code, seizure_year, sep = ':'))


# WORKED
#### NEW CODE ADDED HERE for double counting
mutual_wkd_sum_raw <- mutual_wkd_sum %>%
  left_join(double.recs$double.wkd, by =  "seizure_wkd") %>%
  mutate(raw = replace_na(raw, 0))

wkd_summ <- mutual_wkd_sum_raw %>%
  group_by(wkd_orig_code, seizure_year) %>%
  summarise(wkd_reduce = sum(amount.reduce, na.rm = T),
            wkd_sz_reduce = sum(sz.reduce, na.rm= T)) %>%
  mutate(year.ctry = paste(wkd_orig_code, seizure_year, sep = ':'))


wkd_summ_raw <- mutual_wkd_sum_raw %>%
  group_by(wkd_orig_code, seizure_year) %>%
  summarise(raw_double = sum(raw, na.rm = T)) %>%
  mutate(year.ctry = paste(wkd_orig_code, seizure_year, sep = ':'))


# Combine df.inout_tidy with parts to reduce
# Join with RAW
df.inout_raw <- df.inout_tidy %>%
  left_join(raw_summ, by = "year.ctry" ) %>%
  mutate(raw_reduce = replace_na(raw_reduce, 0),
         raw_sz_reduce = replace_na(raw_sz_reduce, 0))

# Join with WORKED
df.inout_raw_wkd <- df.inout_raw %>%
  left_join(wkd_summ, by = "year.ctry" ) %>%
  mutate(wkd_reduce = replace_na(wkd_reduce, 0),
         wkd_sz_reduce = replace_na(wkd_sz_reduce, 0))

## NEW CODE ADDED HERE
# Join with double count worked (from raw side)
df.inout_raw_wkd <- df.inout_raw_wkd %>%
  left_join(raw_summ_wkd, by = "year.ctry") %>%
  mutate(wkd_double = replace_na(wkd_double, 0))

# Join with double count raw (from worked side)
df.inout_raw_wkd <- df.inout_raw_wkd %>%
  left_join(wkd_summ_raw, by = "year.ctry") %>%
  mutate(raw_double = replace_na(raw_double, 0))


# Calculate overall reduction

df.inout_reduce <- df.inout_raw_wkd %>%
  mutate(total_reduce = raw_reduce + wkd_reduce  + raw_double + wkd_double,
         total_sz_reduce = raw_sz_reduce + wkd_sz_reduce)

df.inout_reduce <- df.inout_reduce %>%
  mutate(wt.out.adj = wt.out - total_reduce,
         sz.out.adj = sz.out - total_sz_reduce) %>%
  select(id:wt.out, raw_reduce, wkd_reduce:sz.out.adj)


# Create a file to use for checking
# Basically everything that is available

df.chk <- df.inout_reduce

# Create set of variables to produce summaries
  df.inout_use <- df.inout_reduce %>%
    mutate(sz.out = sz.out.adj) %>%
    mutate(wt.out = wt.out.adj) %>%
    select(id, ctry, year, sz.in, sz.out, wt.in, wt.out)

df.inout_use <- df.inout_use %>%
  mutate(sz.flow = sz.in/(sz.in + sz.out)) %>%
  mutate(wt.flow = wt.in/(wt.in + wt.out))

# Change NANs to zero
df.inout_use <- df.inout_use %>%
  mutate(sz.flow = replace_na(sz.flow, 0),
         wt.flow = replace_na(wt.flow, 0))

# Create tables for workbook of either trade flows or implicated flows
sz.in.tb <- df.inout_use %>%
  select(ctry, year, sz.in) %>%
  spread(key = year, value = sz.in)

wt.in.tb <- df.inout_use %>%
  select(ctry, year, wt.in) %>%
  spread(key = year, value = wt.in)

sz.out.tb <- df.inout_use %>%
  select(ctry, year, sz.out) %>%
  spread(key = year, value = sz.out)

wt.out.tb <- df.inout_use %>%
  select(ctry, year, wt.out) %>%
  spread(key = year, value = wt.out)

sz.flow.tb <- df.inout_use %>%
  select(ctry, year, sz.flow) %>%
  spread(key = year, value = sz.flow)

wt.flow.tb <- df.inout_use %>%
  select(ctry, year, wt.flow) %>%
  spread(key = year, value = wt.flow)

# File names
  begin.name <- ifelse(ctry_dest_included, "Trade_", "LE_")
  end.name <- paste(year.from.use, '_', year.to.use, sep = '')
  if(size.min > 0)
    end.name <-  paste(end.name, '_min_', size.min, sep = '')

# Write checking file to csv file
check.name <- paste(begin.name,  end.name,'CHK' , '.csv')
write.csv(df.chk, file = check.name)
# OUTPUT OPTIONS

# (1) save df as csv file
if(is.element(1,which.out)){
  csv.name <- paste(begin.name, end.name, '.csv', sep = '')
  write.csv(df.inout_use, file = csv.name, row.names = F)
}

# (2) save df as database table
# Note may need to include public component
#tablename <- paste('public.', begin.name, end.name, sep = '')
if(is.element(2, which.out) & year.from ==1900 & year.to == 2100 & size.min == 0){
  tablename <- paste('szwt_',tolower(begin.name),'inout', sep = '')
# Replace '.' with '_' in column names
  names(df.inout_use) <- sub('.','_', names(df.inout_use), fixed = TRUE)
#SQLstr <- paste('DROP TABLE IF EXISTS public.',tablename,';', sep = '')
  dbWriteTable(con, tablename, df.inout_use, row.names = FALSE, overwrite = TRUE)
  SQLstr <- paste('ALTER TABLE ',tablename,' ADD PRIMARY KEY (id)', sep = '')
  dbSendQuery(con, SQLstr)
}

# (3) save individual tables in Excel Workbook (uses XLConnect)
# Separate sheets for
# Seizures In
# Weights In
# Seizures Out
# Weights Out
# Seizures flow sz.in/(sz.in + sz.out)
# Weights flow  wt.in/(wt.in + wt.out)
if (is.element(3, which.out)){
  xlsx.name <- paste(begin.name, end.name, '_TABLE.xls', sep = '')
  if(file.exists(xlsx.name)) file.remove(xlsx.name)
  wb <- loadWorkbook(xlsx.name, create=T)
  setMissingValue(wb, value = "")
  createSheet(wb, name='sz in')
  createSheet(wb, name='wt in')
  createSheet(wb, name='sz out')
  createSheet(wb, name='wt out')
  createSheet(wb, name='sz flow')
  createSheet(wb, name='wt flow')
  writeWorksheet(wb, sz.in.tb, 'sz in')
  writeWorksheet(wb, wt.in.tb, 'wt in')
  writeWorksheet(wb, sz.out.tb, 'sz out')
  writeWorksheet(wb, wt.out.tb, 'wt out')
  writeWorksheet(wb, sz.flow.tb, 'sz flow')
  writeWorksheet(wb, wt.flow.tb, 'wt flow')

  saveWorkbook(wb)
}
#____________________________________________________________
}

#_______________________________________________________________________

# Original code written by RW Burn and FM Underwood

