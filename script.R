pacman::p_load('tidyverse', 'lubridate', 'DatawRappr', 
               'zoo', 'rvest', 'readxl', 'readODS',
               'httr', 'jsonlite')


#1. INFLATION

inf <- read_csv('https://www.ons.gov.uk/generator?format=csv&uri=/economy/inflationandpriceindices/timeseries/d7g7/mm23') %>%
  slice(220:nrow(.)) %>%
  select('date' = 1, 'inf' = 2) %>%
  mutate(date = ym(date),
         inf = as.numeric(inf))

#2. REAL GDP PER CAPITA

gdp <- read_csv('https://www.ons.gov.uk/generator?format=csv&uri=/economy/grossdomesticproductgdp/timeseries/ihxw/pn2') %>%
  slice(190:nrow(.)) %>%
  select('date' = 1, 'gdp' = 2) %>%
  mutate(date = yq(date),
         gdp = as.numeric(gdp),
         gdp = rollsum(gdp, 4, fill = NA, align = 'right'))


#3. UNEMPLOYMENT

unemp <- read_csv('https://www.ons.gov.uk/generator?format=csv&uri=/employmentandlabourmarket/peoplenotinwork/unemployment/timeseries/mgsx/lms') %>%
  slice(310:nrow(.)) %>%
  select('date' = 1, 'unem' = 2) %>%
  mutate(date = ym(date),
         unem = as.numeric(unem))


#4. HOUSE PRICES

hp <- read.csv('https://landregistry.data.gov.uk/app/ukhpi/download/new.csv?from=1991-01-01&location=http%3A%2F%2Flandregistry.data.gov.uk%2Fid%2Fregion%2Funited-kingdom&thm%5B%5D=property_type&in%5B%5D=avg') %>%
  filter(`Reporting.period` == 'monthly') %>%
  select('date' = Period, 'price' = `Average.price.All.property.types`) %>%
  mutate(date = ym(date), price = as.numeric(price)) 


#5. NHS WAITING LISTS

wl.url <- 'https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/rtt-data-2025-26/' %>%
  read_html() %>%
  html_nodes('a') %>%
  html_attr('href') 

download.file(wl.url[grepl('Overview-Timeseries', wl.url)],destfile = 'downloads/latest-waiting-list.xlsx')

waits <- read_excel('downloads/latest-waiting-list.xlsx', skip = 10) %>%
  select('date' = 2, 'total' = 21) %>%
  mutate(total = as.numeric(total),
         date = as.Date(as.numeric(date))) %>%
  filter(!is.na(total)) %>%
  mutate(date = zoo::na.approx(date),
         date = as.Date(date, origin = '1899-12-30'))


#6. IMMIGRATION

im.url <- 'https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/internationalmigration/datasets/longterminternationalimmigrationemigrationandnetmigrationflowsprovisional' %>%
  read_html() %>%
  html_nodes('.btn--thick') %>%
  html_attr('href')

download.file(paste0('https://www.ons.gov.uk', im.url[1]) , 'downloads/immigration.xlsx')

imm <- read_excel('downloads/immigration.xlsx', 5, skip = 5)  %>%
  select('flow' = 1, 'date' = 2, 'total' = 3) %>%
  filter(grepl('Net', flow)) %>%
  mutate(date = lubridate::my(date)) %>%
  select(date, total)


#7. HOUSING STARTS
'https://www.ons.gov.uk/peoplepopulationandcommunity/housing/datasets/ukhousebuildingpermanentdwellingsstartedandcompleted' %>%
  read_html() %>%
  html_nodes('.btn--thick') %>%
  html_attr('href') %>%
  paste0('https://www.ons.gov.uk', .) %>%
  download.file(destfile = 'downloads/houses.xlsx')

housing <- read_excel('downloads/houses.xlsx', 6, skip = 5) %>%
  select('date' = 2, 'start' = 3, 'complete' = 7) %>%
  mutate(date = lubridate::my(gsub('(.*) - ','', date )),
         rolling = rollsum(start, 4, align = 'right', 
                         fill = NA))  %>%
  select(date, 'start' = rolling)




#8. CRIME

crime.url <- 'https://www.ons.gov.uk/peoplepopulationandcommunity/crimeandjustice/datasets/crimeinenglandandwalesappendixtables'  %>%
  read_html() %>%
  html_nodes('.btn--thick') %>%
  html_attr('href')

download.file(paste0('https://www.ons.gov.uk', crime.url[1]) , 'downloads/crime.xlsx')

crime <- read_excel('downloads/crime.xlsx', 4, skip = 7) %>%
  select(1:33) %>%
  gather(date, t, 2:ncol(.)) %>%
  rename('offence' = 1, 'date' = 2) %>%
  mutate(date = lubridate::my(gsub('(.*) to ', '', substr(date, 1, 20))),
         t = as.numeric(t),
         offence = gsub(' \\[(.*)', '', offence)) %>%
  filter(grepl('ALL CSEW HEADLINE', offence), !is.na(t)) %>%
  mutate(t = t * 1000) %>%
  spread(offence, t) %>%
  rename('Without fraud' = 2, 'With fraud' = 3)


#9. SMALL BOATS

boat_url = "https://www.gov.uk/government/publications/migrants-detected-crossing-the-english-channel-in-small-boats" %>%
  read_html() %>%
  html_nodes('a') %>%
  html_attr('href') 


download.file(boat_url[grepl('.ods', boat_url)][1],
              'downloads/boats.ods')

boats.daily <- readODS::read_ods('downloads/boats.ods', 3)  %>%
  mutate(date = lubridate::dmy(Date),
         rolling = rollsum(`Migrants arrived`, 365, align = 'right', fill = NA)) %>%
  select(date, 'migrants' = 2, 
         'boats' = 3,  rolling) 

dates.boats <- seq(max(boats.daily$date), min(boats.daily$date), by = '-7 days')

boats <- boats.daily %>% filter(date %in%  dates.boats) 

#10. INACTIVITY BY REASON

inac <- read_csv('https://www.ons.gov.uk/generator?format=csv&uri=/employmentandlabourmarket/peoplenotinwork/economicinactivity/timeseries/lf69/lms') %>%
  slice(300:nrow(.)) %>%
  rename('date' = 1, 'total' = 2) %>%
  mutate(date = lubridate::ym(date), 
         total = 1000 * as.numeric(total))

#11. BASE RATES

bra <- 'https://www.bankofengland.co.uk/boeapps/database/Bank-Rate.asp#' %>%
  read_html() %>%
  html_table() %>%
  nth(1) %>%
  select('date' = 1, 'rate' = 2) %>%
  mutate(date = lubridate::dmy(date))

br <-  bra %>%
  arrange(date) %>%
  mutate(step_date = date - days(1),
         step_rate = lag(rate)) %>%
  filter(!is.na(step_rate)) %>%
  select(date = step_date, rate = step_rate) %>%
  bind_rows(bra,
            bra %>% 
              filter(date == max(date)) %>%
              mutate(date = Sys.Date()),
            tibble(date = Sys.Date() - years(1)))  %>%
  arrange((date))  %>%
  mutate(rate = zoo::na.locf(rate)) 



# 12. Job vacancies

vac <- 'https://www.ons.gov.uk/generator?format=csv&uri=/employmentandlabourmarket/peopleinwork/employmentandemployeetypes/timeseries/ap2y/unem' %>%
  read_csv() %>%
  slice(150:nrow(.)) %>%
  select('date' = 1, 'vacancies' = 2) %>%
  mutate(date = lubridate::ym(date),
         vacancies = 1000 * as.numeric(vacancies))


# 13. real wages (regular pay seasonally adjusted)

wages <- read_csv('https://www.ons.gov.uk/generator?format=csv&uri=/employmentandlabourmarket/peopleinwork/earningsandworkinghours/timeseries/kai7/emp',
         skip = 150) %>%
  select('date' = 1, 'pay' = 2)  %>%
  mutate(date = lubridate::ym(date)) %>%
  left_join(read_csv('https://www.ons.gov.uk/generator?format=csv&uri=/economy/inflationandpriceindices/timeseries/d7bt/mm23')  %>%
              slice(200:nrow(.)) %>%
              select('date' = 1, 'inf' = 2) %>%
              mutate(date = lubridate::ym(date),
                     inf = as.numeric(inf),
                     inf2 = inf / last(inf)))  %>%
  mutate(realpay = pay / inf2) %>%
  select(date, 'wages' = realpay)



# 14. petrol prices (RAC)


petrol <- read_csv('https://static.dwcdn.net/data/D7n0J.csv?v=1774282200000') %>%
  select('date' = 1, 'petrol' = 2) %>%
  mutate(date = lubridate::dmy(date),
         petrol = petrol / 100) 


#15. government approval

'https://api-test.yougov.com/public-data/v5/uk/trackers/government-approval/download/' %>%
  download.file(paste0('downloads/', 'approval.xlsx'))

app <- read_excel('downloads/approval.xlsx') %>%
  gather(date, t, 2:ncol(.)) %>%
  select('q' = 1, date, t) %>%
  filter(grepl('prov', q)) %>%
  spread(q, t) %>%
  mutate(net = 100 * (Approve - Disapprove),
         date = lubridate::ymd(date))  %>%
  select(date, net)


# 16. consumer confidence 

consumer <- read_csv('https://fred.stlouisfed.org/graph/fredgraph.csv?id=CSCICP02GBM460S&cosd=2020-01-01') |>
  select(date = 1, consumer = 2)



# 17.  renewables

rlink <- 'https://www.gov.uk/government/statistics/electricity-section-5-energy-trends' %>%
  read_html() %>%
  html_nodes('a')   %>%
  html_attr('href')

rlink[grepl('5.1_', rlink)] %>% unique() %>%
  download.file('downloads/renewables.xlsx')

r1 <- read_excel('downloads/renewables.xlsx', 'Quarter', skip = 5)  %>%
  slice(40:80) %>%
  filter(`Generator type` == 'All generating companies') %>%
  gather(date, t, 3:ncol(.)) %>%
  mutate(date = gsub("[\r\n]", "", date),
         q = substr(date, 9, 9),
         y = substr(trimws(date), 11,14),
         date = lubridate::yq(paste(y, q)),
         t = as.numeric(t))


ren <- r1 %>%
  filter(grepl('Total all', Fuel)) %>%
  select(date,  'total' = t) %>%
  left_join(r1 %>% 
              filter(grepl('Hydr|Total wind|Solar|Bioener', Fuel)) %>%
              group_by(date) %>%
              summarise('renewable' = sum(t, na.rm = T))) %>%
  mutate(renewablepc = 100 * renewable / total)



# 18. gov borrowing (rates)

gilts.daily <- 'https://www.bankofengland.co.uk/boeapps/database/fromshowcolumns.asp?Travel=NIxIRxSUx&FromSeries=1&ToSeries=50&DAT=ALL&FNY=&CSVF=TT&html.x=54&html.y=45&C=C6S&Filter=N' %>%
  read_html() %>%
  html_table() %>%
  nth(1) %>%
  select('date' = 1, 'yield' = 2) %>%
  mutate(date = lubridate::dmy(date)) 


gilts.dates <- seq(max(gilts.daily$date), min(gilts.daily$date), by = "-7 days")


gilts <- gilts.daily %>% filter(date %in% gilts.dates)


#19  asylum grants

as.u <- 'https://www.gov.uk/government/statistical-data-sets/immigration-system-statistics-data-tables#asylum' %>%
  read_html() %>%
  html_nodes('.gem-c-attachment-link .govuk-link') %>%
  html_attr('href')

download.file(as.u[grepl('/asylum-claims-datasets', as.u)], 'downloads/asylumclaims.xlsx')

asylum <- read_excel('~/Downloads/asylumclaims.xlsx', 11, skip = 1) %>%
  mutate(date = lubridate::yq(Quarter)) %>%
  filter(`Case outcome group` == 'Grant of Protection') %>%
  group_by(date) %>%
  summarise(protec = sum(Decisions)) %>%
  mutate(protec = rollsum(protec, 4, align = 'right', fill = NA)) 



# 20. direct debit failure rate

dd.url <- 'https://www.ons.gov.uk/economy/economicoutputandproductivity/output/datasets/monthlydirectdebitfailurerateandaveragetransactionamount' %>%
  read_html() %>%
  html_nodes('.btn--thick') %>%
  html_attr('href')
download.file(paste0('https://www.ons.gov.uk', dd.url[1]), 'downloads/dd.xlsx')
dd <- read_excel('~/Downloads/dd.xlsx', 4, skip = 4) %>%
  mutate_at(2:ncol(.), function(x) 100 * x) %>%
  select('date' = 1, 'ddfail' = 2)


# 21. A&E 


ae.links <- 'https://www.england.nhs.uk/statistics/statistical-work-areas/ae-waiting-times-and-activity/' %>%
  read_html() %>%
  html_nodes('a') %>%
  html_attr('href')

ae.links[grepl('Monthly-AE', ae.links)] %>%
  download.file(destfile = 'downloads/latest-ae.xls')

ae <- read_excel('downloads/latest-ae.xls', 2) %>%
  select('date' = 1, 'pc4hour' = 10) %>%
  mutate_at(1:2, as.numeric) %>%
  mutate(date = as.Date(date, origin = '1899-12-30'),
         pc4hour = 100 * pc4hour) %>%
  filter(!is.na(date)) 



# 22. private rents

'https://www.ons.gov.uk/economy/inflationandpriceindices/datasets/priceindexofprivaterentsukmonthlypricestatistics' %>%
  read_html() %>%
  html_nodes('.btn--thick') %>%
  html_attr('href') %>%
  nth(1) %>%
  paste0('https://www.ons.gov.uk', .) %>%
  download.file('downloads/rents.xlsx')

rent <- read_excel('downloads/rents.xlsx', 4, skip = 2)  %>%
  filter(`Area name` == "United Kingdom") %>%
  select('date' = 1, 'rent2bed' = 16)  %>%
  mutate(rent2bed = as.numeric(rent2bed)) 

# 23. PRISONS

prison <- read_tsv('https://datawrapper.dwcdn.net/XLaXA/4/dataset.csv') %>%
  mutate(date = lubridate::my(Date),
         prison = Population) %>%
  select(date, prison) %>%
  bind_rows(tibble(date = as.Date(c('2025-10-01', '2025-11-01', '2025-12-01', '2026-01-01', '2026-02-01')),
                   prison = c(87413, 87332, 86596, 87212, 87367)))



# 24. NEETS

neet <- 'https://explore-education-statistics.service.gov.uk/data-tables/permalink/3bf73997-2f44-4a93-e3fa-08de88f3d3c1' %>%
  read_html() %>%
  html_table() %>%
  nth(1) %>%
  select('year' = 1, 'q1' = 6, 'q2' = 7, 'q3' = 8, 'q4' = 9) %>%
  mutate_at(2:5, function(x) as.numeric(gsub('%' ,'', x))) %>%
  filter(!is.na(year)) %>%
  gather(quarter, neet, 2:5) %>%
  mutate(date = lubridate::yq( paste(year, quarter))) %>%
  select(date, neet)


# 25. Crown court open caseload


download.file('https://assets.publishing.service.gov.uk/media/6941934a1ec67214e98f303a/cc_open_tool.xlsx',
              'downloads/cc.xlsx')

cc  <- read_excel('downloads/cc.xlsx', 2, skip = 8) %>%
  mutate(year = zoo::na.locf(year),
         date = lubridate::yq(paste(year, quarter))) %>%
  select(date, 'crowncourt' = 3)



# 26. PINT COST

download.file('https://raw.githubusercontent.com/onsdigital/cpi-items-actions/main/datadownload.xlsx',
              'downloads/pint.xlsx')

pint <- read_excel('downloads/pint.xlsx', 4) %>%
  filter(ITEM_ID == '310110') %>%
  gather(date, total, 2:ncol(.)) %>%
  mutate(date = as.Date(as.numeric(date), origin = '1899-12-30')) %>%
  select(-ITEM_ID)


# 27. Knife crime

crimelink <- 'https://www.gov.uk/government/statistical-data-sets/police-recorded-crime-and-outcomes-open-data-tables' %>%
  read_html() %>%
  html_nodes('.govuk-link') %>%
  html_attr('href')

download.file(crimelink[grepl('knife', crimelink)], 'downloads/knifecrime.ods')

knife <- read_ods( 'downloads/knifecrime.ods',  5) %>%
  mutate(year = as.numeric(paste0('20', substr(`Financial Year`, 6,7))),
         q = as.numeric(gsub('Q', '', `Financial Quarter`)),
         q = ifelse(q == 4, 3, (q*3)+3),
         year = ifelse(q == 3, year , year - 1),
         date = lubridate::ym(paste(year, q))) %>%
  group_by(date) %>%
  summarise(knife = sum(`Force Offences`, na.rm = T)) %>%
  mutate(knife = rollsum(knife, 4, fill = NA, align = 'right')) 


# 28. cup of coffee

coffee <- read_excel('downloads/pint.xlsx', 4) %>%
  filter(ITEM_ID == '220121') %>%
  gather(date, total, 2:ncol(.)) %>%
  mutate(date = as.Date(as.numeric(date), origin = '1899-12-30')) %>%
  select(-ITEM_ID)


# 29. Restaurant main

restaurant <- read_excel('downloads/pint.xlsx', 4) %>%
  filter(ITEM_ID == '220128') %>%
  gather(date, total, 2:ncol(.)) %>%
  mutate(date = as.Date(as.numeric(date), origin = '1899-12-30')) %>%
  select(-ITEM_ID)


# 30. Diesel

diesel <- read_csv('https://static.dwcdn.net/data/YyE8M.csv') %>%
  select('date' = 1, 'diesel' = 2) %>%
  mutate(date = lubridate::dmy(date),
         diesel = diesel / 100) 

# 31. Cancer delays

can.url <- 'https://www.england.nhs.uk/statistics/statistical-work-areas/cancer-waiting-times/'  %>%
  read_html() %>%
  html_nodes('a') %>%
  html_attr('href') 

can.url[grepl('National-Time-Series', can.url)][1] %>%
  download.file(destfile = 'downloads/latest-cancer.xlsx')

cancer <- read_excel('~/Downloads/latest-cancer.xlsx', 2, skip = 3) %>%
  select('date' = 1, 'cancer62' = 19) %>%
  filter(!is.na(cancer62)) %>%
  mutate(cancer62 = 100 * cancer62)


# 32. bed occupancy

bed.url <- 'https://www.england.nhs.uk/statistics/statistical-work-areas/bed-availability-and-occupancy/critical-care-and-general-acute-beds-urgent-and-emergency-care-daily-situation-reports/' %>%
  read_html() %>%
  html_nodes('a') 

download.file(html_attr(bed.url[grepl('.xlsx', bed.url)] , 'href'), 
              'downloads/beds.xlsx')

beds <- read_excel('downloads/beds.xlsx', 2, skip = 12) %>%
  select('date' = Month, 'occupancy' = `G&A occupancy rate` ) %>%
  mutate(date = lubridate::my(date), occupancy = 100 * occupancy) %>%
  filter(!is.na(occupancy))

# 33. national debt

debt <- read_csv('https://www.ons.gov.uk/generator?format=csv&uri=/economy/governmentpublicsectorandtaxes/publicsectorfinance/timeseries/hf6w/pusf') %>%
  slice(300:nrow(.)) %>%
  select('date' = 1, 'debt' = 2) %>%
  mutate(date= lubridate::ym(date),
         debt = 1000000000 * as.numeric(debt)) %>%
  filter(!is.na(debt))


# 34. house price to earnings

house.price.earnings <- read.csv('https://landregistry.data.gov.uk/app/ukhpi/download/new.csv?from=1991-01-01&location=http%3A%2F%2Flandregistry.data.gov.uk%2Fid%2Fregion%2Funited-kingdom&thm%5B%5D=property_type&in%5B%5D=avg') %>%
  filter(`Reporting.period` == 'monthly') %>%
  select('date' = Period, 'price' = 7) %>%
  mutate(date = ym(date), price = as.numeric(price))  %>%
  left_join(read_csv('https://www.ons.gov.uk/generator?format=csv&uri=/employmentandlabourmarket/peopleinwork/earningsandworkinghours/timeseries/kab9/emp') %>%
              slice(140:nrow(.)) %>%
              rename('date' = 1, 'earn' = 2) %>%
              mutate(date = lubridate::ym(date), earn =  as.numeric(earn),
                     earn = 52 * rollmean(earn, 12, fill = NA, align = 'right'))) %>%
  mutate(ratio = price / earn) %>%
  select(date, ratio) %>%
  filter(!is.na(ratio))

# 35.  shoplifting

shop <- read_excel('downloads/crime.xlsx', 13, skip = 8) %>%
  gather(date, crimes, 3:(ncol(.)-1)) %>%
  filter(grepl('Shoplifting', `Offence category`)) %>%
  mutate(date = lubridate::my(trimws(gsub('(.*) to ', '', date))),
         crimes = as.numeric(crimes)) %>%
  select('code' = 1, 'category' = 2, date, crimes) %>%
  filter(!is.na(code))



# 36. theft from the person

theft.person <- read_excel('downloads/crime.xlsx', 13, skip = 8) %>%
  gather(date, crimes, 3:(ncol(.)-1)) %>%
  filter(grepl('Theft from the person', `Offence category`)) %>%
  mutate(date = lubridate::my(trimws(gsub('(.*) to ', '', date))),
         crimes = as.numeric(crimes)) %>%
  select('code' = 1, 'category' = 2, date, crimes) %>%
  filter(!is.na(code))



# 
# download.file(crimelink[grepl('2013-onwards', crimelink)], 'downloads/reportedcrime.ods')
# 
# readODS::read_ods('downloads/reportedcrime.ods', )


#  charge rates


########## STEP TWO

#COMBINE


master <- bind_rows(list(petrol %>%
                           mutate(position = 1,
                                  label = 'Petrol price',
                                  note = "Price of a litre of unleaded petrol (RAC)", 
                                  parent = 'Living standards',
                                  up = 'bad',
                                  unit = '£') %>%
                           select(position, label, note, parent, date, up, unit, 'total' = petrol),
                         gdp %>%
                           mutate(position = 2,
                                  label = 'Real GDP per capita',
                                  up = 'good',
                                  note = "Annualised quarterly GDP per person, adjusted for inflation (ONS)", 
                                  parent = 'Economy',
                                  unit = '£') %>%
                           select(position, label, note, parent, date, up, unit, 'total' = gdp),
                         unemp %>%
                           mutate(position = 3,
                                  label = 'Unemployment',
                                  up = 'bad',
                                  note = "Percentage of people not in work but looking for a job (ONS)", 
                                  parent = 'Economy',
                                  unit = '%') %>%
                           select(position, label, note, parent, date, up, unit, 'total' = unem),
                         gilts %>%
                           mutate(position = 4,
                                  label = 'Government borrowing costs',
                                  note = "10-year gilt yields (Bank of England)", 
                                  parent = 'Government',
                                  up = 'bad',
                                  unit = '%') %>%
                           select(position, label, note, parent, date, up, unit, 'total' = yield),
                         shop %>%
                           mutate(position = 5,
                                  label = 'Shoplifting',
                                  note = 'Shoplifting offences recorded by police',
                                  parent = 'Crime',
                                  up = 'bad',
                                  unit= '') %>%
                           select(position, label, note, parent, date, up, unit, 'total' = crimes),
                         
                         br %>%
                           mutate(position = 6,
                                  label = 'BoE base rate',
                                  up = 'bad',
                                  note = "Base rate that underpins mortgage and savings rates (Bank of England)", 
                                  parent = 'Living standards',
                                  unit = '%') %>%
                           select(position, label, note, parent, date, up, unit, 'total' = rate),
                        hp %>%
                           mutate(position = 7,
                                  label = 'House prices',
                                  up = 'neutral',
                                  note = "Rolling annual average of sold UK house prices (Land Registry)", 
                                  parent = 'Housing',
                                  unit = '£') %>%
                           select(position, label, note, parent, date, up, unit, 'total' = price),
                         waits %>%
                           mutate(position = 8,
                                  label = 'NHS waiting list',
                                  note = "Total size of waiting list (NHS England)", 
                                  parent = 'Health',
                                  up = 'bad',
                                  unit = '') %>%
                           select(position, label, note, parent, date, up, unit, total),
                         inf %>%
                           mutate(position = 9,
                                  label = 'Inflation',
                                  up = 'bad',
                                  note = "Consumer prices index, change on previous 12 months (ONS)",
                                  parent = 'Economy',
                                  unit = '%') %>%
                           select(position, label, note, parent, date, up, unit, 'total' = inf),
                        diesel %>%
                          mutate(position = 10,
                                 label = 'Diesel price',
                                 note = "Price of a litre of diesel (RAC)", 
                                 parent = 'Living standards',
                                 up = 'bad',
                                 unit = '£') %>%
                          select(position, label, note, parent, date, up, unit,  'total' = diesel),
                        
                        housing %>%
                           mutate(position = 11,
                                  label = 'Housing starts',
                                  note = "Number of housing units on which construction has started in England in the past year (MHCLG)", 
                                  parent = 'Housing',
                                  up = 'good',
                                  unit = '') %>%
                           select(position, label, note, parent, date, up, unit, 'total' = start),
                         crime %>%
                           mutate(position = 12,
                                  label = 'Survey-based crime',
                                  note = "Victim-based crime estimate using the Crime Survey of England and Wales (ONS)", 
                                  parent = 'Crime',
                                  up = 'bad',
                                  unit = '') %>%
                           select(position, label, note, parent, date, up, unit, 'total' = `With fraud`),
                         boats %>%
                           mutate(position = 13,
                                  label = 'Small boat crossings',
                                  note = "Number of people who have crossed the Channel in the past year (Home Office)", 
                                  parent = 'Immigration',
                                  up = 'bad',
                                  unit = '') %>%
                           select(position, label, note, parent, date, up, unit, 'total' = rolling),
                         wages %>%
                           mutate(position = 14,
                                  label = 'Real wages',
                                  note = "Average weekly wage, adjusted for inflation (ONS)",
                                  parent = 'Living standards',
                                  up = 'good',
                                  unit = '£') %>%
                           select(position, label, note, parent, date, up, unit, 'total' = wages),
                         imm %>%
                           mutate(position = 15,
                                  label = 'Net migration',
                                  note = "Latest annual estimate of UK immigration, minus emigration (ONS)",
                                  parent = 'Immigration',
                                  up = 'bad',
                                  unit = '') %>%
                           select(position, label, note, parent, date, up, unit, total),
                         app %>%
                           mutate(position = 16,
                                  label = 'Net government approval',
                                  note = "Government approval minus government disapproval  (YouGov)", 
                                  parent = 'Government',
                                  up = 'good',
                                  unit = '%') %>%
                           select(position, label, note, parent, date, up, unit, 'total' = net),
                         theft.person %>%
                           mutate(position = 17,
                                  label = 'Theft from the person',
                                  note = 'Police-recorded theft of items (includes phone snatching)',
                                  parent = 'Crime',
                                  up = 'bad',
                                  unit = '') %>%
                           select(position, label, note, parent, date, up, unit, 'total' = crimes),
                         consumer %>%
                           mutate(position = 18,
                                  label = 'Consumer confidence',
                                  note = "Long-running index of consumers' financial mood (GfK)", 
                                  parent = 'Living standards',
                                  up = 'good',
                                  unit = '%') %>%
                           select(position, label, note, parent, date, up, unit, 'total' = consumer),
                         ren %>%
                           mutate(position = 19,
                                  label = 'Electricity from renewables',
                                  note = "Percentage of UK electricity generated through renewable sources (Department for Energy Security and Net Zero)", 
                                  parent = 'Government',
                                  up = 'good',
                                  unit = '%') %>%
                           select(position, label, note, parent, date, up, unit, 'total' = renewablepc),
                        inac %>%
                          mutate(position = 20,
                                 label = 'Long-term sick',
                                 up = 'bad',
                                 note = "Number of people who are inactive due to long-term sickness", 
                                 parent = 'Health',
                                 unit = '') %>%
                          select(position, label, note, parent, date, up, unit, total),
                        asylum %>%
                           mutate(position = 21,
                                  label = 'Asylum grants',
                                  note = "Number of people granted asylum at initial decision in the past year (Home Office)", 
                                  parent = 'Immigration',
                                  up = 'neutral',
                                  unit = '') %>%
                           select(position, label, note, parent, date, up, unit, 'total' = protec),
                         dd %>%
                           mutate(position = 22,
                                  label = 'Direct debits failing',
                                  note = "Monthly direct debit failure rate (ONS)", 
                                  parent = 'Living standards',
                                  up = 'bad',
                                  unit = '%') %>%
                           select(position, label, note, parent, date, up, unit, 'total' = ddfail),
                         ae %>%
                           mutate(position = 23,
                                  label = 'A&E seen in 4 hours',
                                  note = "Percentage of A&E patients seen within 4 hours (NHS England)", 
                                  parent = 'Health',
                                  up = 'good',
                                  unit = '%') %>%
                           select(position, label, note, parent, date, up, unit, 'total' = pc4hour),
                         rent %>%
                           mutate(position = 24,
                                  label = 'Renting a 2-bed',
                                  note = "Cost of privately renting a 2-bed property (ONS)", 
                                  parent = 'Housing',
                                  up = 'bad',
                                  unit = '£') %>%
                           select(position, label, note, parent, date, up, unit, 'total' = rent2bed),
                         prison %>%
                           mutate(position = 25,
                                  label = 'Prisoners',
                                  note = "Total prison population (Ministry of Justice)", 
                                  parent = 'Crime',
                                  up = 'bad',
                                  unit = '') %>%
                           select(position, label, note, parent, date, up, unit, 'total' = prison),
                         cc %>%
                           mutate(position = 26,
                                  label = 'Crown court caseload',
                                  note = "Number of crown court cases currently open (Ministry of Justice)", 
                                  parent = 'Crime',
                                  up = 'bad',
                                  unit = '') %>%
                           select(position, label, note, parent, date, up, unit, 'total' = crowncourt),
                         neet %>%
                           mutate(position = 27,
                                  label = 'NEETS aged 16-24',
                                  note = "Percentage of people aged 16-24 who are not in employment, education or training (Department for Education)", 
                                  parent = 'Economy',
                                  up = 'bad',
                                  unit = '%') %>%
                           select(position, label, note, parent, date, up, unit, 'total' = neet),
                         pint %>%
                           mutate(position = 28,
                                  label = 'Price of a pint',
                                  note = "Average price of a pint of premium lager (ONS)", 
                                  parent = 'Living standards',
                                  up = 'bad',
                                  unit = '£') %>%
                           select(position, label, note, parent, date, up, unit,  total),
                         knife %>%
                           mutate(position = 29,
                                  label = 'Knife crime',
                                  note = "Offences involving a knife, recorded by policer (Home Office)", 
                                  parent = 'Crime',
                                  up = 'bad',
                                  unit = '') %>%
                           select(position, label, note, parent, date, up, unit,  'total' = knife),
                         coffee %>%
                           mutate(position = 30,
                                  label = 'Price of a coffee',
                                  note = "Average price of a cup of takeaway coffee (ONS)", 
                                  parent = 'Living standards',
                                  up = 'bad',
                                  unit = '£') %>%
                           select(position, label, note, parent, date, up, unit,  'total' = total),
                         restaurant %>%
                           mutate(position = 31,
                                  label = 'Price of a meal',
                                  note = "Average price of a restaurant main course (ONS)", 
                                  parent = 'Living standards',
                                  up = 'bad',
                                  unit = '£') %>%
                           select(position, label, note, parent, date, up, unit,  'total' = total),
                         cancer %>%
                           mutate(position = 32,
                                  label = 'Prompt cancer treatment',
                                  note = "Percentage starting cancer treatment within 62 days of referral (NHS)", 
                                  parent = 'Health',
                                  up = 'good',
                                  unit = '%') %>%
                           select(position, label, note, parent, date, up, unit,  'total' = cancer62),
                         beds %>%
                           mutate(position = 33,
                                  label = 'Bed occupancy',
                                  note = "Percentage of general acute beds occupied (NHS)", 
                                  parent = 'Health',
                                  up = 'bad',
                                  unit = '%') %>%
                           select(position, label, note, parent, date, up, unit,  'total' = occupancy),
                         debt %>%
                           mutate(position = 34,
                                  label = 'National debt',
                                  note = "Size of the national debt (ONS)", 
                                  parent = 'Government',
                                  up = 'bad',
                                  unit = '£') %>%
                           select(position, label, note, parent, date, up, unit,  'total' = debt),
                         house.price.earnings %>%
                           mutate(position = 35,
                                  label = 'House price to earnings',
                                  note = "Ratio of house prices to average annual wages (ONS)", 
                                  parent = 'Housing',
                                  up = 'bad',
                                  unit = '') %>%
                           select(position, label, note, parent, date, up, unit,  'total' = ratio),
                        vac %>%
                          mutate(position = 36,
                                 label = 'Job vacancies',
                                 note = "Total number of job vacancies (ONS)", 
                                 parent = 'Economy',
                                 up = 'good',
                                 unit = '') %>%
                          select(position, label, note, parent, date, up, unit, 'total' = vacancies))) %>%
  filter(date >= as.Date('2020-01-01'))

# ADD A ROW FROM A YEAR AGO 

master.adj <- master %>%
  bind_rows(master %>%
              group_by(label) %>%
              filter(date == max(date)) %>%
              mutate(date = date - years(1) ,
                     total = NA)) %>%
  group_by(label) %>%
  arrange(date) %>%
  mutate(total = zoo::na.locf(total)) %>%
  unique()




# THEN MAKE THE FILE 


file <- master.adj %>% 
  left_join(master.adj %>%
              group_by(label) %>%
              filter(date == max(date)) %>%
              select(label, 'now' = total) %>%
              left_join(master.adj %>% 
                          group_by(label) %>%
                          filter(date == max(date) - years(1)) %>%
                          select(label, y1 = total)) %>%
              mutate(change = now - y1) %>%
              select(-y1)) %>%
  arrange(position, label, date) %>%
  group_by(position, label, note, parent, now, change, up, unit) %>%
  nest(data = c(date, total)) %>%
  ungroup() %>%
  mutate(c = ifelse(change > 0, up, 'reverse'),
         c = ifelse(c == 'reverse' & up == 'bad', 'good',c),
         c = ifelse(c == 'reverse' & up == 'good', 'bad', c),
         colour = ifelse(c == 'bad', 'red', ifelse(c == 'good', 'green', 'grey')))  %>%
  select(position,label, note, parent, colour, now, change, unit, data) %>%
  mutate(data = map(data, ~ {
    df <- .x
    map2(as.character(df$date), df$total, ~ set_names(list(.y), .x))
  })) 

file %>%
  write_json('sparklines-page.json', auto_unbox = T, pretty = T)

file %>%
  write_json('sparklines-slice.json', auto_unbox = T, pretty = T)

