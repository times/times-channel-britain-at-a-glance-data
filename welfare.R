pacman::p_load('tidyverse', 'lubridate', 'zoo', 'rvest', 'readxl', 'readODS',
               'httr', 'jsonlite')

# Welfare at a glance — sparklines for the Times Welfare Inquiry page.
# Writes welfare_page.json in the same shape as sparklines-page.json, but with ~10 years of history.
# DWP series come from the Stat-Xplore API: put STATXPLORE_API_KEY=... in .Renviron (gitignored).

options(timeout = 120)
if (file.exists('.Renviron')) readRenviron('.Renviron')
dir.create('downloads', showWarnings = FALSE)

start_date <- floor_date(Sys.Date() - years(10), 'year')

safe_download <- function(url, destfile, retries = 3, pause = 10, ...) {
  Sys.sleep(3)  # breathing gap to avoid ONS rate-limiting
  for (i in seq_len(retries)) {
    err <- tryCatch({ download.file(url, destfile, quiet = TRUE, mode = 'wb', ...); NULL }, error = function(e) e)
    if (is.null(err)) return(invisible())
    if (i < retries) { message("  download error on attempt ", i, ", retrying in ", pause * i, "s..."); Sys.sleep(pause * i) }
  }
  stop("Download failed after ", retries, " attempts: ", url)
}

safe_read_html <- function(url, retries = 3, pause = 10, ...) {
  Sys.sleep(3)
  for (i in seq_len(retries)) {
    result <- tryCatch(rvest::read_html(url, ...), error = function(e) e)
    if (!inherits(result, 'error')) return(result)
    if (i < retries) { message("  read_html error on attempt ", i, ", retrying in ", pause * i, "s..."); Sys.sleep(pause * i) }
  }
  stop("read_html failed after ", retries, " attempts: ", url)
}

safe_read_csv <- function(url, retries = 3, pause = 10, ...) {
  Sys.sleep(3)
  for (i in seq_len(retries)) {
    result <- tryCatch(readr::read_csv(url, ...), error = function(e) e)
    if (!inherits(result, 'error')) return(result)
    if (i < retries) { message("  read_csv error on attempt ", i, ", retrying in ", pause * i, "s..."); Sys.sleep(pause * i) }
  }
  stop("read_csv failed after ", retries, " attempts: ", url)
}

# All links on a page, made absolute (GOV.UK mixes relative and absolute hrefs)
page_links <- function(url) {
  safe_read_html(url) %>% html_nodes('a') %>% html_attr('href') %>% na.omit() %>% xml2::url_absolute(url)
}

# Stat-Xplore table query -> tidy tibble with one column per dimension (d1, d2, ...) plus value
sx_table <- function(database, measure, dims, retries = 3) {
  if (Sys.getenv('STATXPLORE_API_KEY') == '') stop('STATXPLORE_API_KEY not set — add it to .Renviron')
  body <- list(database = database, measures = list(measure), dimensions = lapply(dims, list))
  for (i in seq_len(retries)) {
    res <- tryCatch(POST('https://stat-xplore.dwp.gov.uk/webapi/rest/v1/table',
                         add_headers(APIKey = Sys.getenv('STATXPLORE_API_KEY')),
                         content_type_json(), body = toJSON(body, auto_unbox = TRUE), timeout(300)),
                    error = function(e) e)
    if (!inherits(res, 'error') && status_code(res) == 200) break
    if (i == retries) stop('Stat-Xplore query failed: ', database)
    message('  Stat-Xplore error on attempt ', i, ', retrying...'); Sys.sleep(20 * i)
  }
  out <- fromJSON(content(res, 'text', encoding = 'UTF-8'), simplifyVector = FALSE)
  labs <- map(out$fields, ~ map_chr(.x$items, ~ .x$labels[[1]]))
  names(labs) <- paste0('d', seq_along(labs))
  # cube values are nested with the first dimension outermost; expand.grid varies its first column fastest
  grid <- expand.grid(rev(labs), stringsAsFactors = FALSE)[, rev(seq_along(labs)), drop = FALSE]
  as_tibble(grid) %>% mutate(value = unlist(out$cubes[[measure]]$values))
}

# Stat-Xplore month labels come as 'June 2013', 'May 2013' or 'May-13'
sx_month <- function(x) lubridate::my(str_extract(x, '[A-Za-z]{3,9}[- ][0-9]{2,4}'))

# Financial-year label '2024/25' -> end of that financial year
fy_end <- function(x) as.Date(paste0(as.integer(substr(x, 1, 4)) + 1, '-03-31'))

# Same "build file" logic as compute_file() in script.R, but keeping data from start_date
compute_file <- function(master_in) {
  master_in <- master_in %>% filter(date >= start_date)

  master.adj <- master_in %>%
    bind_rows(master_in %>%
                group_by(label) %>%
                filter(date == max(date)) %>%
                mutate(date = date - years(1),
                       total = NA),
              master_in %>%
                group_by(label) %>%
                filter(date == max(date)) %>%
                mutate(date = date - years(2),
                       total = NA),
              master_in %>%
                group_by(label) %>%
                filter(date == max(date)) %>%
                mutate(date = date - years(5),
                       total = NA)) %>%
    group_by(label) %>%
    arrange(date) %>%
    mutate(total = zoo::na.locf(total)) %>%
    unique()

  master.adj %>%
    left_join(master.adj %>%
                group_by(label) %>%
                filter(date == max(date)) %>%
                select(label, 'now' = total) %>%
                left_join(master.adj %>%
                            group_by(label) %>%
                            filter(date == max(date) - years(1)) %>%
                            select(label, y1 = total)) %>%
                left_join(master.adj %>%
                            group_by(label) %>%
                            filter(date == max(date) - years(2)) %>%
                            select(label, y2 = total)) %>%
                left_join(master.adj %>%
                            group_by(label) %>%
                            filter(date == max(date) - years(5)) %>%
                            select(label, y5 = total)) %>%
                mutate(change1 = now - y1,
                       change2 = now - y2,
                       change5 = now - y5) %>%
                select(-y1, -y2, -y5)) %>%
    arrange(label, date) %>%
    group_by(label, note, parent, now, change1, change2, change5, up, unit) %>%
    nest(data = c(date, total)) %>%
    ungroup() %>%
    mutate(c1 = ifelse(change1 > 0, up, 'reverse'),
           c1 = ifelse(c1 == 'reverse' & up == 'bad', 'good', c1),
           c1 = ifelse(c1 == 'reverse' & up == 'good', 'bad', c1),
           colour1 = ifelse(c1 == 'bad', 'red', ifelse(c1 == 'good', 'green', 'grey'))) %>%
    mutate(c2 = ifelse(change2 > 0, up, 'reverse'),
           c2 = ifelse(c2 == 'reverse' & up == 'bad', 'good', c2),
           c2 = ifelse(c2 == 'reverse' & up == 'good', 'bad', c2),
           colour2 = ifelse(c2 == 'bad', 'red', ifelse(c2 == 'good', 'green', 'grey'))) %>%
    mutate(c5 = ifelse(change5 > 0, up, 'reverse'),
           c5 = ifelse(c5 == 'reverse' & up == 'bad', 'good', c5),
           c5 = ifelse(c5 == 'reverse' & up == 'good', 'bad', c5),
           colour5 = ifelse(c5 == 'bad', 'red', ifelse(c5 == 'good', 'green', 'grey'))) %>%
    select(label, note, parent, 'colour' = colour1, colour2, colour5, now, 'change' = change1, change2, change5, unit, data) %>%
    mutate(data = map(data, ~ {
      df <- .x
      map2(as.character(df$date), df$total, ~ set_names(list(.y), .x))
    }))
}

########## PART ONE: FETCH

#1. PEOPLE ON SICKNESS BENEFITS (ESA + UC limited capability for work, Stat-Xplore)
# UC Health data starts in April 2019, so the combined series starts at the first ESA quarter after that.

esa <- bind_rows(
  sx_table('str:database:ESA_Caseload', 'str:count:ESA_Caseload:V_F_ESA',
           'str:field:ESA_Caseload:F_ESA_QTR:DATE_NAME'),
  sx_table('str:database:ESA_Caseload_new', 'str:count:ESA_Caseload_new:V_F_ESA_NEW',
           'str:field:ESA_Caseload_new:F_ESA_QTR_NEW:DATE_NAME')) %>%
  transmute(date = sx_month(d1), esa = value)

uc_health <- sx_table('str:database:UC_WCA_CASE', 'str:count:UC_WCA_CASE:V_F_UC_WCA_CASE',
                      c('str:field:UC_WCA_CASE:F_UC_WCA_MONTH:DATE_NAME',
                        'str:field:UC_WCA_CASE:V_F_UC_WCA_CASE:STAGE_CODE')) %>%
  filter(grepl('^Limited capability', d2)) %>%
  group_by(date = sx_month(d1)) %>%
  summarise(uc = sum(value))

sickness <- esa %>%
  inner_join(uc_health, by = 'date') %>%
  mutate(total = esa + uc) %>%
  arrange(date)

#2. PIP CLAIMANTS (Stat-Xplore)

pip <- bind_rows(
  sx_table('str:database:PIP_Monthly', 'str:count:PIP_Monthly:V_F_PIP_MONTHLY',
           'str:field:PIP_Monthly:F_PIP_DATE:DATE2'),
  sx_table('str:database:PIP_Monthly_new', 'str:count:PIP_Monthly_new:V_F_PIP_MONTHLY',
           'str:field:PIP_Monthly_new:F_PIP_DATE:DATE2')) %>%
  transmute(date = sx_month(d1), total = value) %>%
  distinct(date, .keep_all = TRUE) %>%
  arrange(date)

#3. UC CLAIMANTS WITH NO WORK REQUIREMENTS (Stat-Xplore)

uc_nwr <- sx_table('str:database:UC_Monthly', 'str:count:UC_Monthly:V_F_UC_CASELOAD_FULL',
                   c('str:field:UC_Monthly:F_UC_DATE:DATE_NAME',
                     'str:field:UC_Monthly:V_F_UC_CASELOAD_FULL:CCCONDITIONALITY_REGIME')) %>%
  filter(d2 == 'No work requirements') %>%
  transmute(date = sx_month(d1), total = value) %>%
  filter(total > 0) %>%
  arrange(date)

#4. BENEFIT EXPENDITURE AND CASELOAD TABLES (DWP) — welfare spending series
# Find the latest year's publication; GOV.UK lists the newest (autumn/spring) file first.

exp_pubs <- page_links('https://www.gov.uk/government/collections/benefit-expenditure-and-caseload-tables') %>%
  .[grepl('benefit-expenditure-and-caseload-tables-[0-9]{4}$', .)] %>%
  unique()
exp_latest <- exp_pubs[which.max(as.integer(str_extract(exp_pubs, '[0-9]{4}$')))]
exp_xlsx <- page_links(exp_latest) %>%
  .[grepl('outturn-and-forecast.*\\.xlsx$', ., ignore.case = TRUE)] %>%
  .[1]
safe_download(exp_xlsx, 'downloads/welfare_spend.xlsx')

# Read a wide DWP sheet; returns the matrix plus the financial-year columns that are outturn (not forecast)
read_exp_sheet <- function(sheet) {
  m <- as.matrix(suppressMessages(read_excel('downloads/welfare_spend.xlsx', sheet, col_names = FALSE)))
  hdr <- which(apply(m, 1, function(r) any(grepl('^[0-9]{4}/[0-9]{2}$', r))))[1]
  cols <- which(grepl('^[0-9]{4}/[0-9]{2}$', m[hdr, ]) & grepl('Outturn', m[hdr + 1, ]))
  list(m = m, years = m[hdr, cols], cols = cols)
}

# Values for the first row labelled `row_label` below the block heading matching `block`
exp_row <- function(s, block, row_label) {
  labs <- apply(s$m[, 1:3], 1, function(r) paste(na.omit(trimws(gsub('\\s+', ' ', r))), collapse = ' '))
  b <- which(grepl(block, labs, ignore.case = TRUE))[1]
  r <- which(labs == row_label & seq_along(labs) > b)[1]
  if (is.na(b) || is.na(r)) stop('Could not find "', row_label, '" under "', block, '"')
  tibble(date = fy_end(s$years), value = as.numeric(s$m[r, s$cols]))
}

uk_welfare <- read_exp_sheet(grep('^UK welfare', excel_sheets('downloads/welfare_spend.xlsx'), value = TRUE)[1])
summary_tab <- read_exp_sheet('Benefit summary table')
table4 <- read_exp_sheet('Table 4')

welfare_real <- exp_row(uk_welfare, 'United Kingdom welfare spending, £ billion, real terms', 'Total') %>%
  mutate(total = value * 1e9)

welfare_gdp <- exp_row(uk_welfare, 'United Kingdom welfare spending, as a share of Gross Domestic Product', 'Total') %>%
  mutate(total = value * 100)

# Working-age welfare = DWP working-age + children's benefits, plus HMRC tax credits and Child Benefit
# (tax credits are included so the migration into Universal Credit doesn't show up as a rise)
gdp_denom <- exp_row(uk_welfare, 'Deflators and Denominators', 'GDP denominator')  # £ million

working_age_gdp <- list(
  exp_row(summary_tab, 'Benefit expenditure, £ billion, nominal terms', 'Working age'),
  exp_row(summary_tab, 'Benefit expenditure, £ billion, nominal terms', 'Children'),
  exp_row(uk_welfare, 'United Kingdom welfare spending, £ billion, nominal terms', 'Personal tax credits'),
  exp_row(uk_welfare, 'United Kingdom welfare spending, £ billion, nominal terms', 'Child Benefit')) %>%
  bind_rows() %>%
  group_by(date) %>%
  summarise(spend = sum(value)) %>%
  inner_join(gdp_denom %>% rename(gdp = value), by = 'date') %>%
  mutate(total = 100 * spend * 1000 / gdp)

health_spend <- exp_row(table4, 'real terms', 'Disability and Incapacity Benefits Total') %>%
  mutate(total = value * 1e6)

#5. LONG-TERM SICK (economically inactive, ONS LF69)

ons_monthly <- function(url) {
  safe_read_csv(url, col_names = c('date', 'value'), skip = 1, show_col_types = FALSE) %>%
    filter(grepl('^[0-9]{4} [A-Z]{3}$', date)) %>%
    mutate(date = ym(date), value = as.numeric(value))
}

inac <- ons_monthly('https://www.ons.gov.uk/generator?format=csv&uri=/employmentandlabourmarket/peoplenotinwork/economicinactivity/timeseries/lf69/lms') %>%
  mutate(total = 1000 * value)

#6. UNEMPLOYMENT RATE (ONS MGSX)

unemp <- ons_monthly('https://www.ons.gov.uk/generator?format=csv&uri=/employmentandlabourmarket/peoplenotinwork/unemployment/timeseries/mgsx/lms') %>%
  rename(total = value)

#7. NEETS AGED 16-24 (ONS)

'https://www.ons.gov.uk/employmentandlabourmarket/peoplenotinwork/unemployment/datasets/youngpeoplenotineducationemploymentortrainingneettable1' %>%
  safe_read_html() %>%
  html_nodes('.btn--thick')  %>%
  html_attr('href') %>%
  paste0('https://www.ons.gov.uk', .)  %>%
  safe_download(., destfile = 'downloads/neets.xlsx')

neets <- read_excel('downloads/neets.xlsx', 2)  %>%
  select('date' = 1, 'total' = 6) %>%
  mutate(date = lubridate::my(gsub('(.*)-', '', date)),
         total = as.numeric(total))  %>%
  filter(!is.na(total), !is.na(date))

#8. HOUSEHOLDS IN TEMPORARY ACCOMMODATION (MHCLG, England, quarterly)

ta_ods <- page_links('https://www.gov.uk/government/statistical-data-sets/live-tables-on-homelessness') %>%
  .[grepl('Statutory_Homelessness_England_Time_Series_[0-9]{6}\\.ods$', .)] %>%
  unique() %>%
  .[which.max(as.integer(str_extract(., '[0-9]{6}(?=\\.ods$)')))]
safe_download(ta_ods, 'downloads/homeless_ts.ods')

ta_raw <- read_ods('downloads/homeless_ts.ods', sheet = 'TA1', col_names = FALSE, .name_repair = 'minimal')
ta_hdr <- which(ta_raw[[1]] == 'Time period')[1]
ta_col <- which(grepl('^Households in TA', unlist(ta_raw[ta_hdr, ])))[1]

ta <- tibble(code = as.character(ta_raw[[2]]), total = suppressWarnings(as.numeric(ta_raw[[ta_col]]))) %>%
  filter(grepl('^[0-9]{6}$', code), !is.na(total)) %>%
  transmute(date = ym(code), total)

#9. ROUGH SLEEPING (MHCLG annual autumn snapshot, England) — same approach as script.R

rough_snaps <- page_links('https://www.gov.uk/government/collections/homelessness-statistics') %>%
  .[grepl('rough-sleeping-snapshot-in-england-autumn-[0-9]{4}$', .)]

rough_latest <- rough_snaps[which.max(as.integer(str_extract(rough_snaps, '[0-9]{4}$')))]

rough_ods <- page_links(rough_latest) %>%
  .[grepl('\\.ods$', ., ignore.case = TRUE)] %>%
  .[1]

safe_download(rough_ods, 'downloads/rough_sleep.ods')

rough_raw <- read_ods('downloads/rough_sleep.ods', sheet = 'Table_1_Total', col_names = FALSE)
rough_hdr <- which(rough_raw[[5]] == '2010')[1]
rough_eng <- which(rough_raw[[4]] == 'England')[1]
rough_hdrvals <- as.character(unlist(rough_raw[rough_hdr, ]))
rough_cols <- which(grepl('^20[0-9]{2}$', rough_hdrvals) & suppressWarnings(as.integer(rough_hdrvals)) >= 2010)

rough_sleep <- tibble(date = as.Date(paste0(as.integer(rough_hdrvals[rough_cols]), '-10-01')),
                      total = as.numeric(unlist(rough_raw[rough_eng, rough_cols]))) %>%
  filter(!is.na(total)) %>%
  arrange(date)

#10. CHILD POVERTY, AFTER HOUSING COSTS (DWP HBAI, relative low income, UK)

hbai_pubs <- page_links('https://www.gov.uk/government/collections/households-below-average-income-hbai--2') %>%
  .[grepl('households-below-average-income-for-financial-years-ending-1995-to-[0-9]{4}$', .)] %>%
  unique()
hbai_latest <- hbai_pubs[which.max(as.integer(str_extract(hbai_pubs, '[0-9]{4}$')))]
hbai_ods <- page_links(hbai_latest) %>%
  .[grepl('summary.*\\.ods$', ., ignore.case = TRUE)] %>%
  .[1]
safe_download(hbai_ods, 'downloads/hbai_summary.ods')

hbai_raw <- as.matrix(read_ods('downloads/hbai_summary.ods', sheet = '1_4a', col_names = FALSE, .name_repair = 'minimal'))
# Merged header cells are offset from the data, so locate the column via the second
# 'Relative low income' heading (first = before housing costs, second = after housing costs)
ahc_col <- which(hbai_raw == 'Relative low income', arr.ind = TRUE) %>%
  as_tibble() %>%
  filter(row == min(row)) %>%
  pull(col) %>%
  sort() %>%
  .[2]

child_pov <- tibble(year = hbai_raw[, 2], total = suppressWarnings(as.numeric(hbai_raw[, ahc_col]))) %>%
  filter(grepl('^[0-9]{4}/[0-9]{2}$', year), !is.na(total)) %>%
  transmute(date = fy_end(year), total)

#11. NATIONAL LIVING WAGE (top adult rate, GOV.UK)
# GOV.UK only lists rates from April 2019; earlier National Living Wage rates are hardcoded.

nlw_hist <- tibble(date = as.Date(c('2016-04-01', '2017-04-01', '2018-04-01')),
                   total = c(7.20, 7.50, 7.83))

nlw <- safe_read_html('https://www.gov.uk/national-minimum-wage-rates') %>%
  html_table() %>%
  map(~ .x %>% select(1, 2) %>% set_names(c('period', 'rate'))) %>%
  bind_rows() %>%
  filter(grepl('^April [0-9]{4}', period)) %>%
  transmute(date = as.Date(paste0(str_extract(period, '[0-9]{4}'), '-04-01')),
            total = as.numeric(gsub('[£,]', '', rate))) %>%
  bind_rows(nlw_hist) %>%
  filter(!is.na(total), date <= Sys.Date()) %>%
  distinct(date, .keep_all = TRUE) %>%
  arrange(date)

#12. FULL NEW STATE PENSION (weekly rate)
# History hardcoded (new state pension began April 2016); current rate scraped from GOV.UK each run.

sp_hist <- tibble(date = as.Date(paste0(2016:2025, '-04-01')),
                  total = c(155.65, 159.55, 164.35, 168.60, 175.20, 179.60, 185.15, 203.85, 221.20, 230.25))

sp_now <- safe_read_html('https://www.gov.uk/new-state-pension/what-youll-get') %>%
  html_text() %>%
  str_extract('full rate of new State Pension is\\s*£[0-9.]+') %>%
  str_extract('[0-9.]+$') %>%
  as.numeric()

fy_start <- ifelse(month(Sys.Date()) >= 4, year(Sys.Date()), year(Sys.Date()) - 1)

state_pension <- sp_hist %>%
  filter(year(date) < fy_start) %>%
  bind_rows(tibble(date = as.Date(paste0(fy_start, '-04-01')), total = sp_now)) %>%
  filter(!is.na(total)) %>%
  arrange(date)

#13. MEDIAN FULL-TIME EARNINGS (ONS ASHE selected estimates, Table 2, UK)

safe_download('https://www.ons.gov.uk/file?uri=/employmentandlabourmarket/peopleinwork/earningsandworkinghours/datasets/ashe1997to2015selectedestimates/current/selectedestimates19972026.xlsx',
              'downloads/ashe_ts.xlsx')

ashe_raw <- as.matrix(suppressMessages(read_excel('downloads/ashe_ts.xlsx', 'Table 2', col_names = FALSE)))
ashe_hdr <- which(apply(ashe_raw, 1, function(r) sum(grepl('^\\s*(19|20)[0-9]{2}', r)) > 10))[1]
ashe_row <- which(grepl('^Median gross annual earnings', ashe_raw[, 1]))[1]

# Where a year appears twice (methodology breaks, e.g. '2011 soc00' / '2011 soc10'), keep the later column
median_pay <- tibble(year = as.integer(str_extract(ashe_raw[ashe_hdr, ], '(19|20)[0-9]{2}')),
                     total = suppressWarnings(as.numeric(ashe_raw[ashe_row, ]))) %>%
  filter(!is.na(year), !is.na(total)) %>%
  group_by(year) %>%
  slice_tail(n = 1) %>%
  ungroup() %>%
  transmute(date = as.Date(paste0(year, '-04-01')), total)

########## STEP TWO

#COMBINE THEM. Note the order is set by the order you arrange them here

master <- bind_rows(list(
  sickness %>%
    mutate(label = 'People on sickness benefits',
           note = "People on Employment and Support Allowance or on Universal Credit with limited capability for work (DWP)",
           parent = 'Health and disability',
           up = 'bad',
           unit = '') %>%
    select(label, note, parent, date, up, unit, total),
  pip %>%
    mutate(label = 'PIP claimants',
           note = "People entitled to Personal Independence Payment (DWP)",
           parent = 'Health and disability',
           up = 'bad',
           unit = '') %>%
    select(label, note, parent, date, up, unit, total),
  health_spend %>%
    mutate(label = 'Health and disability benefit spending',
           note = "Spending on disability and incapacity benefits in Great Britain, financial years, 2026-27 prices (DWP)",
           parent = 'Spending',
           up = 'bad',
           unit = '£') %>%
    select(label, note, parent, date, up, unit, total),
  inac %>%
    mutate(label = 'Long-term sick',
           note = "Number of people aged 16-64 who are economically inactive due to long-term sickness (ONS)",
           parent = 'Health and disability',
           up = 'bad',
           unit = '') %>%
    select(label, note, parent, date, up, unit, total),
  uc_nwr %>%
    mutate(label = 'On UC with no work requirements',
           note = "People on Universal Credit who are not required to look for or prepare for work (DWP)",
           parent = 'Work',
           up = 'bad',
           unit = '') %>%
    select(label, note, parent, date, up, unit, total),
  neets %>%
    mutate(label = 'NEETs aged 16-24',
           note = "Percentage of people aged 16-24 who are not in employment, education or training (ONS)",
           parent = 'Work',
           up = 'bad',
           unit = '%') %>%
    select(label, note, parent, date, up, unit, total),
  unemp %>%
    mutate(label = 'Unemployment',
           note = "Percentage of people not in work but looking for a job (ONS)",
           parent = 'Work',
           up = 'bad',
           unit = '%') %>%
    select(label, note, parent, date, up, unit, total),
  ta %>%
    mutate(label = 'Households in temporary accommodation',
           note = "Households in temporary accommodation arranged by councils (MHCLG, England)",
           parent = 'Housing',
           up = 'bad',
           unit = '') %>%
    select(label, note, parent, date, up, unit, total),
  rough_sleep %>%
    mutate(label = 'Rough sleeping',
           note = "Estimated number of people sleeping rough on a single night in autumn (MHCLG, England)",
           parent = 'Housing',
           up = 'bad',
           unit = '') %>%
    select(label, note, parent, date, up, unit, total),
  welfare_real %>%
    mutate(label = 'Welfare spending',
           note = "Total UK welfare spending, financial years, 2026-27 prices (DWP, HMRC)",
           parent = 'Spending',
           up = 'bad',
           unit = '£') %>%
    select(label, note, parent, date, up, unit, total),
  welfare_gdp %>%
    mutate(label = 'Welfare spending as % of GDP',
           note = "Total UK welfare spending as a share of GDP, financial years (DWP, HMRC)",
           parent = 'Spending',
           up = 'bad',
           unit = '%') %>%
    select(label, note, parent, date, up, unit, total),
  working_age_gdp %>%
    mutate(label = 'Working-age welfare as % of GDP',
           note = "Benefits for working-age adults and children, including tax credits and Child Benefit, as a share of GDP, financial years (DWP, HMRC)",
           parent = 'Spending',
           up = 'bad',
           unit = '%') %>%
    select(label, note, parent, date, up, unit, total),
  child_pov %>%
    mutate(label = 'Child poverty',
           note = "Percentage of children in relative low income after housing costs, financial years (DWP)",
           parent = 'Living standards',
           up = 'bad',
           unit = '%') %>%
    select(label, note, parent, date, up, unit, total),
  nlw %>%
    mutate(label = 'Minimum wage',
           note = "National Living Wage hourly rate for the oldest age band (GOV.UK)",
           parent = 'Living standards',
           up = 'neutral',
           unit = '£') %>%
    select(label, note, parent, date, up, unit, total),
  state_pension %>%
    mutate(label = 'New state pension',
           note = "Full weekly rate of the new state pension (DWP)",
           parent = 'Living standards',
           up = 'neutral',
           unit = '£') %>%
    select(label, note, parent, date, up, unit, total),
  median_pay %>%
    mutate(label = 'Median full-time pay',
           note = "Median gross annual earnings for full-time employees (ONS)",
           parent = 'Living standards',
           up = 'good',
           unit = '£') %>%
    select(label, note, parent, date, up, unit, total)
)) %>%
  filter(date >= start_date)

# POSITION SET BY ORDER OF FIRST APPEARANCE ABOVE
label_order <- master %>%
  select(label) %>%
  unique() %>%
  mutate(position = 1:nrow(.))

file <- compute_file(master) %>%
  left_join(label_order, by = 'label') %>%
  arrange(position) %>%
  select(position, everything())

file %>%
  write_json('welfare_page.json', auto_unbox = T, pretty = T)
