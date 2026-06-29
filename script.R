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
  select('date' = Period, 'price' = 11) %>%
  mutate(date = ym(date), price = as.numeric(price)) 


#5. NHS WAITING LISTS

wl.url <- 'https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/rtt-data-2026-27/' %>%
  read_html() %>%
  html_nodes('a') %>%
  html_attr('href') 

download.file(wl.url[grepl('Overview-Timeseries', wl.url)],destfile = 'downloads/latest-waiting-list.xlsx')

waits <- read_excel('downloads/latest-waiting-list.xlsx', skip = 11) %>%
  select('date' = 2, 'total' = 22, 'average' = 3, 'within18' = 8) %>%
  mutate(total = as.numeric(total),
         within18 = 100 * as.numeric(within18),
         date = as.Date(as.numeric(date)),
         average = as.numeric(average)) %>%
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
  mutate(date = lubridate::my(substr(trimws(gsub('(.*) to', '', date)), 1, 8)),
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



#12. Job vacancies

vac <- 'https://www.ons.gov.uk/generator?format=csv&uri=/employmentandlabourmarket/peopleinwork/employmentandemployeetypes/timeseries/ap2y/unem' %>%
  read_csv() %>%
  slice(150:nrow(.)) %>%
  select('date' = 1, 'vacancies' = 2) %>%
  mutate(date = lubridate::ym(date),
         vacancies = 1000 * as.numeric(vacancies))


#13. real wages (regular pay seasonally adjusted)

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



#14. petrol prices (RAC)


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


#16. consumer confidence 

consumer <- read_csv('https://fred.stlouisfed.org/graph/fredgraph.csv?id=CSCICP02GBM460S&cosd=2020-01-01') |>
  select(date = 1, consumer = 2)



#17.  renewables

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



#18. gov borrowing (rates)

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

asylum <- read_excel('downloads/asylumclaims.xlsx', 11, skip = 1) %>%
  mutate(date = lubridate::yq(Quarter)) %>%
  filter(`Case outcome group` == 'Grant of Protection') %>%
  group_by(date) %>%
  summarise(protec = sum(Decisions)) %>%
  mutate(protec = rollsum(protec, 4, align = 'right', fill = NA)) 



#20. direct debit failure rate

dd.url <- 'https://www.ons.gov.uk/economy/economicoutputandproductivity/output/datasets/monthlydirectdebitfailurerateandaveragetransactionamount' %>%
  read_html() %>%
  html_nodes('.btn--thick') %>%
  html_attr('href')
download.file(paste0('https://www.ons.gov.uk', dd.url[1]), 'downloads/dd.xlsx')
dd <- read_excel('downloads/dd.xlsx', 4, skip = 4) %>%
  mutate_at(2:ncol(.), function(x) 100 * x) %>%
  select('date' = 1, 'ddfail' = 2)


#21. A&E 


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


#22. private rents

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
  bind_rows(tibble(date = as.Date(c('2025-10-01', '2025-11-01', 
                                    '2025-12-01', '2026-01-01', 
                                    '2026-02-01', '2026-03-01',
                                    '2026-04-01')),
                   prison = c(87413, 87332, 
                              86596, 87212, 
                              87367, 87292,
                              85704)))



# 24. NEETS
'https://www.ons.gov.uk/employmentandlabourmarket/peoplenotinwork/unemployment/datasets/youngpeoplenotineducationemploymentortrainingneettable1' %>%
  read_html() %>%
  html_nodes('.btn--thick')  %>%
  html_attr('href') %>%
  paste0('https://www.ons.gov.uk', .)  %>%
  download.file(., destfile = 'downloads/neets.xlsx')

neets <- read_excel('downloads/neets.xlsx', 2)  %>%
  select('date' = 1, 'neet' = 6) %>%
  mutate(date = lubridate::my(gsub('(.*)-', '', date)),
         neet = as.numeric(neet))  %>%
  filter(!is.na(neet), !is.na(date))


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

download.file('https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2024/01/Cancer-Waiting-Times-National-Time-Series-Oct-2009-Sep-2023-with-Revisions.xlsx',
              'downloads/cancer-old.xlsx')



cancer <- read_excel('downloads/latest-cancer.xlsx', 2, skip = 3) %>%
  select('date' = 1, 'cancer62' = 19) %>%
  filter(!is.na(cancer62)) %>%
  mutate(cancer62 = 100 * cancer62) %>%
  bind_rows(read_excel('downloads/cancer-old.xlsx', 2, skip = 3) %>%
              select('date' = 1, 'cancer62' = 47) %>%
              mutate(cancer62 = 100 * cancer62) %>%
              filter(date < as.Date('2022-04-01')))


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

#35.  shoplifting

shop <- read_excel('downloads/crime.xlsx', 13, skip = 8) %>%
  gather(date, crimes, 3:(ncol(.)-1)) %>%
  filter(grepl('Shoplifting', `Offence category`)) %>%
  mutate(date = lubridate::my(trimws(gsub('(.*) to ', '', date))),
         crimes = as.numeric(crimes)) %>%
  select('code' = 1, 'category' = 2, date, crimes) %>%
  filter(!is.na(code))



#36. theft from the person

theft.person <- read_excel('downloads/crime.xlsx', 13, skip = 8) %>%
  gather(date, crimes, 3:(ncol(.)-1)) %>%
  filter(grepl('Theft from the person', `Offence category`)) %>%
  mutate(date = lubridate::my(trimws(gsub('(.*) to ', '', date))),
         crimes = as.numeric(crimes)) %>%
  select('code' = 1, 'category' = 2, date, crimes) %>%
  filter(!is.na(code))

#37 PAYROLL EMPLOYEES

rti_url <- 'https://www.ons.gov.uk/employmentandlabourmarket/peopleinwork/earningsandworkinghours/datasets/realtimeinformationstatisticsreferencetableseasonallyadjusted' %>%
  read_html() %>%
  html_nodes('.btn--thick') %>%
  html_attr('href') %>%
  paste0('https://www.ons.gov.uk', .)

download.file(rti_url[1], 'downloads/rti_sa.xlsx')

# UK total payrolled employees is in table 1 - row label is "United Kingdom"
payrolled <- read_excel('downloads/rti_sa.xlsx',skip = 4, 2) %>%
  mutate(date = lubridate::my(Date)) %>%
  select(date, 'payroll' = 2)


#38. Quarterly GDP 


gdp.growth <- read_csv('https://www.ons.gov.uk/generator?format=csv&uri=/economy/grossdomesticproductgdp/timeseries/ihyq/pn2') %>%
  slice(200:nrow(.)) %>%
  select('date' = 1, 'gdp' = 2) %>%
  mutate(date = lubridate::yq(date),
         gdp = as.numeric(gdp))


#39. Debt as % of GDP

debt.gdp <-  read_csv('https://www.ons.gov.uk/generator?format=csv&uri=/economy/governmentpublicsectorandtaxes/publicsectorfinance/timeseries/hf6x/pusf') %>%
  slice(270:nrow(.)) %>%
  select('date' = 1, 'debt.gdp' = 2) %>%
  mutate(date= lubridate::ym(date),
         debt.gdp = as.numeric(debt.gdp)) %>%
  filter(!is.na(debt.gdp))


#40. potential redundancies

'https://www.ons.gov.uk/economy/economicoutputandproductivity/output/datasets/advancednotificationofpotentialredundancies' %>%
  read_html() %>%
  html_nodes('.btn--thick') %>%
  html_attr('href') %>%
  nth(1) %>%
  paste0('https://ons.gov.uk', .) %>%
  download.file('downloads/redundancies.xlsx')

red <- read_excel('downloads/redundancies.xlsx', 4, skip = 4) %>%
  select('date' = 1, 'redundancies' = 4)  %>%
  mutate(redundancies = as.numeric(redundancies)) %>%
  filter(!is.na(redundancies))


#41. electricity price

'https://www.ons.gov.uk/economy/economicoutputandproductivity/output/datasets/systempriceofelectricity'  %>%
  read_html() %>%
  html_nodes('.btn--thick') %>%
  html_attr('href') %>%
  nth(1) %>%
  paste0('https://ons.gov.uk', .) %>%
  download.file('downloads/electricity.xlsx')

electricity <- read_excel('downloads/electricity.xlsx', 5, skip = 4) %>%
  select('date' = 1, 'electricity' = 2)

#42. debit card spending

'https://www.ons.gov.uk/economy/economicoutputandproductivity/output/datasets/revolutspendingondebitcards' %>%
  read_html() %>%
  html_nodes('.btn--thick') %>%
  html_attr('href') %>%
  nth(1) %>%
  paste0('https://ons.gov.uk', .) %>%
  download.file('downloads/revolut.xlsx')

debitcard <- read_excel('downloads/revolut.xlsx', 9, skip =4) %>%
  select('date' = 1, 'spending' = 2) %>%
  mutate(date = lubridate::my(date),
         spending = as.numeric(spending))


#43. % spent on rent

'https://www.ons.gov.uk/economy/economicoutputandproductivity/output/datasets/renteraffordability' %>%
  read_html() %>%
  html_nodes('.btn--thick') %>%
  html_attr('href') %>%
  nth(1) %>%
  paste0('https://ons.gov.uk', .) %>%
  download.file('downloads/rentspend.xlsx')

rentspend <- read_excel('downloads/rentspend.xlsx', 4, skip = 4) %>%
  select('date' = 1, 'rentspend' = 3) %>%
  mutate(date = lubridate::my(date),
         rentspend = 100 * as.numeric(rentspend))

#44. flights 

'https://www.ons.gov.uk/economy/economicoutputandproductivity/output/datasets/dailyukflights' %>%
  read_html() %>%
  html_nodes('.btn--thick') %>%
  html_attr('href') %>%
  nth(1) %>%
  paste0('https://ons.gov.uk', .) %>%
  download.file('downloads/flights.xlsx')

flights <- read_excel('downloads/flights.xlsx', 5, skip = 4) %>%
  select('date' = 1, 'flights' = 3)


#45. vehicle production

'https://www.ons.gov.uk/economy/economicoutputandproductivity/output/datasets/uknewvehicleregistrationsandproduction' %>%
  read_html() %>%
  html_nodes('.btn--thick') %>%
  html_attr('href') %>%
  nth(1) %>%
  paste0('https://ons.gov.uk', .) %>%
  download.file('downloads/cars.xlsx')

vehicle <- read_excel('downloads/cars.xlsx', 6, skip = 5) %>%
  select('date' = 1, 'vehicles' = 9 )


#46. Real household disposable income

rhdi <- read_csv('https://www.ons.gov.uk/generator?format=csv&uri=/economy/grossdomesticproductgdp/timeseries/crxx/ukea' , skip = 100)  %>%
  select('date' = 1, 'rhdi' = 2) %>%
  mutate(date = lubridate::yq(date),
         rhdi = 4*as.numeric(rhdi))
  




########## STEP TWO

#COMBINE THEM. Note the order is set by the order you arrange them here


master <- bind_rows(list(housing %>%
                           mutate(label = 'Housing starts',
                                  note = "Number of housing units on which construction has started in England in the past year (MHCLG)", 
                                  parent = 'Housing',
                                  up = 'good',
                                  unit = '') %>%
                           select(label, note, parent, date, up, unit, 'total' = start),
                         rhdi %>%
                           mutate(label = 'Real disposable income',
                                  note = "Annualised real household disposable income per person (ONS)", 
                                  parent = 'Living standards',
                                  up = 'good',
                                  unit = '£') %>%
                           select(label, note, parent, date, up, unit, 'total' = rhdi),
                         
                         waits %>%
                           mutate(label = 'NHS waiting list',
                                  note = "Total size of waiting list (NHS England)", 
                                  parent = 'Health',
                                  up = 'bad',
                                  unit = '') %>%
                           select(label, note, parent, date, up, unit, total),
                         ae %>%
                           mutate(label = 'A&E seen in 4 hours',
                                  note = "Percentage of A&E patients seen within 4 hours (NHS England)", 
                                  parent = 'Health',
                                  up = 'good',
                                  unit = '%') %>%
                           select(label, note, parent, date, up, unit, 'total' = pc4hour),
                          red %>%
                           mutate(label = 'Redundancies',
                                  note = "Total potential redundancies from HR1 forms, rolling four-week average (Insolvency Service)",
                                  parent = 'Economy',
                                  up = 'bad',
                                  unit = '') %>%
                           select(label, note, parent, date, up, unit, 'total' = redundancies),
                         inf %>%
                           mutate(label = 'Inflation',
                                  up = 'bad',
                                  note = "Consumer prices index, change on previous 12 months (ONS)",
                                  parent = 'Economy',
                                  unit = '%') %>%
                           select( label, note, parent, date, up, unit, 'total' = inf),
                         unemp %>%
                           mutate(label = 'Unemployment',
                                  up = 'bad',
                                  note = "Percentage of people not in work but looking for a job (ONS)", 
                                  parent = 'Economy',
                                  unit = '%') %>%
                           select(label, note, parent, date, up, unit, 'total' = unem),
                         neets %>%
                           mutate(label = 'NEETS aged 16-24',
                                  note = "Percentage of people aged 16-24 who are not in employment, education or training (Department for Education)", 
                                  parent = 'Economy',
                                  up = 'bad',
                                  unit = '%') %>%
                           select(label, note, parent, date, up, unit, 'total' = neet),
                         imm %>%
                           mutate(label = 'Net migration',
                                  note = "Latest annual estimate of UK immigration, minus emigration (ONS)",
                                  parent = 'Immigration',
                                  up = 'bad',
                                  unit = '') %>%
                           select(label, note, parent, date, up, unit, total),
                         flights %>%
                           mutate(label = 'Flights',
                                  note = 'Number of monthly flights to and from UK airports, seasonally-adjusted (ONS, EUROCONTROL)',
                                  parent = 'Economy',
                                  up = 'good',
                                  unit = '') %>%
                           select(label, note, parent, date, up, unit, 'total' = flights),
                         petrol %>%
                           mutate(label = 'Petrol price',
                                  note = "Price of a litre of unleaded petrol (RAC)", 
                                  parent = 'Living standards',
                                  up = 'bad',
                                  unit = '£') %>%
                           select( label, note, parent, date, up, unit, 'total' = petrol),
                         electricity %>%
                           mutate(label = 'Electricity price',
                                  note = 'System price of electricity per kilowatt-hour, monthly average (ONS, Elexon)',
                                  parent = 'Living standards',
                                  up = 'bad', 
                                  unit = 'p') %>%
                           select(label, note, parent, date, up, unit, 'total' = electricity),
                         gdp %>%
                           mutate(label = 'Real GDP per capita',
                                  up = 'good',
                                  note = "Annualised quarterly GDP per person, adjusted for inflation (ONS)", 
                                  parent = 'Economy',
                                  unit = '£') %>%
                           select(label, note, parent, date, up, unit, 'total' = gdp),
                         
                         payrolled %>%
                           mutate(label = 'Payrolled employees',
                                  up = 'good',
                                  note = "Number of payrolled employees (ONS)", 
                                  parent = 'Economy',
                                  unit = '') %>%
                           select(label, note, parent, date, up, unit, 'total' = payroll),
                         debitcard %>%
                           mutate(label = 'Debit card spend',
                                  up = 'good',
                                  note = "Monthly debit card spending index from Revolut, where 100 = 2023 average (ONS, Revolut)", 
                                  parent = 'Economy',
                                  unit = '') %>%
                           select(label, note, parent, date, up, unit, 'total' = spending),
                        
                         rentspend %>%
                           mutate(label = 'Spend on rent',
                                  up = 'bad',
                                  note = 'Monthly rental spend as a percentage of gross income for new tenancies (ONS, PriceHubble)',
                                  parent = 'Housing',
                                  unit = '%') %>%
                           select(label, note, parent, date, up, unit, 'total' = rentspend),
                          
                         gilts %>%
                           mutate(label = 'Gilt yields',
                                  note = "10-year government borrowing costs(Bank of England)", 
                                  parent = 'Government',
                                  up = 'bad',
                                  unit = '%') %>%
                           select( label, note, parent, date, up, unit, 'total' = yield),
                         debt %>%
                           mutate(label = 'National debt',
                                  note = "Size of the national debt (ONS)", 
                                  parent = 'Government',
                                  up = 'bad',
                                  unit = '£') %>%
                           select(label, note, parent, date, up, unit,  'total' = debt),
                         gdp.growth %>%
                           mutate(label = 'Quarterly GDP growth',
                                  up = 'good',
                                  note = 'Quarter-on-quarter, seasonally-adjusted GDP growth, adjusted for inflation (ONS)',
                                  parent = 'Economy',
                                  unit = '%') %>%
                           select(label, note, parent, date, up, unit, 'total' = gdp),
                         debt.gdp %>%
                           mutate(label = 'Debt-to-GDP ratio',
                                  up = 'bad',
                                  note = 'National debt as a percentage of GDP (ONS)',
                                  parent = 'Economy',
                                  unit = '%') %>%
                           select(label, note, parent, date, up, unit, 'total' = debt.gdp),
                         vehicle %>%
                           mutate(label = 'Vehicle production',
                                  up = 'good',
                                  note = 'Total number of vehicles made in Britain each month, seasonally-adjusted (ONS, SMMT)',
                                  parent = 'Economy',
                                  unit = '') %>%
                           select(label, note, parent, date, up, unit, 'total' = vehicles),
                         inac %>%
                           mutate(label = 'Long-term sick',
                                  up = 'bad',
                                  note = "Number of people who are inactive due to long-term sickness", 
                                  parent = 'Health',
                                  unit = '') %>%
                           select( label, note, parent, date, up, unit, total),
                         wages %>%
                           mutate(label = 'Real wages',
                                  note = "Average weekly wage, adjusted for inflation (ONS)",
                                  parent = 'Living standards',
                                  up = 'good',
                                  unit = '£') %>%
                           select( label, note, parent, date, up, unit, 'total' = wages),
                         shop %>%
                           mutate(label = 'Shoplifting',
                                  note = 'Shoplifting offences recorded by police',
                                  parent = 'Crime',
                                  up = 'bad',
                                  unit= '') %>%
                           select( label, note, parent, date, up, unit, 'total' = crimes),
                         
                         br %>%
                           mutate(label = 'BoE base rate',
                                  up = 'bad',
                                  note = "Base rate that underpins mortgage and savings rates (Bank of England)", 
                                  parent = 'Living standards',
                                  unit = '%') %>%
                           select(label, note, parent, date, up, unit, 'total' = rate),
                        hp %>%
                           mutate(label = 'House prices',
                                  up = 'neutral',
                                  note = "Rolling annual average of sold UK house prices (Land Registry)", 
                                  parent = 'Housing',
                                  unit = '£') %>%
                           select(label, note, parent, date, up, unit, 'total' = price),
                         diesel %>%
                          mutate(label = 'Diesel price',
                                 note = "Price of a litre of diesel (RAC)", 
                                 parent = 'Living standards',
                                 up = 'bad',
                                 unit = '£') %>%
                          select(label, note, parent, date, up, unit,  'total' = diesel),
                        
                        crime %>%
                           mutate(label = 'Survey-based crime',
                                  note = "Victim-based crime estimate using the Crime Survey of England and Wales (ONS)", 
                                  parent = 'Crime',
                                  up = 'bad',
                                  unit = '') %>%
                           select(label, note, parent, date, up, unit, 'total' = `With fraud`),
                         boats %>%
                           mutate(label = 'Small boat crossings',
                                  note = "Number of people who have crossed the Channel in the past year (Home Office)", 
                                  parent = 'Immigration',
                                  up = 'bad',
                                  unit = '') %>%
                           select(label, note, parent, date, up, unit, 'total' = rolling),
                         app %>%
                           mutate(label = 'Net government approval',
                                  note = "Government approval minus government disapproval  (YouGov)", 
                                  parent = 'Government',
                                  up = 'good',
                                  unit = '%') %>%
                           select(label, note, parent, date, up, unit, 'total' = net),
                         theft.person %>%
                           mutate(label = 'Theft from the person',
                                  note = 'Police-recorded theft of items (includes phone snatching)',
                                  parent = 'Crime',
                                  up = 'bad',
                                  unit = '') %>%
                           select(label, note, parent, date, up, unit, 'total' = crimes),
                         consumer %>%
                           mutate(label = 'Consumer confidence',
                                  note = "Long-running index of consumers' financial mood (GfK)", 
                                  parent = 'Living standards',
                                  up = 'good',
                                  unit = '%') %>%
                           select(label, note, parent, date, up, unit, 'total' = consumer),
                         ren %>%
                           mutate(label = 'Electricity from renewables',
                                  note = "Percentage of UK electricity generated through renewable sources (Department for Energy Security and Net Zero)", 
                                  parent = 'Government',
                                  up = 'good',
                                  unit = '%') %>%
                           select(label, note, parent, date, up, unit, 'total' = renewablepc),
                        asylum %>%
                           mutate(label = 'Asylum grants',
                                  note = "Number of people granted asylum at initial decision in the past year (Home Office)", 
                                  parent = 'Immigration',
                                  up = 'neutral',
                                  unit = '') %>%
                           select(label, note, parent, date, up, unit, 'total' = protec),
                         dd %>%
                           mutate(label = 'Direct debits failing',
                                  note = "Monthly direct debit failure rate (ONS)", 
                                  parent = 'Living standards',
                                  up = 'bad',
                                  unit = '%') %>%
                           select(label, note, parent, date, up, unit, 'total' = ddfail),
                        waits %>%
                          mutate(label = 'Average NHS wait',
                                 note = 'Median number of weeks spent waiting for NHS treatment (NHS England)',
                                 parent = 'Health',
                                 up = 'bad',
                                 unit = '')  %>%
                          select(label, note, parent, date, up, unit, 'total' = average),
                        waits %>%
                          mutate(label = 'Seen within 18 weeks',
                                 note = 'Percentage of patients who received hospital treatment within 18 weeks (NHS England)',
                                 parent = 'Health',
                                 up = 'good',
                                 unit = '%')  %>%
                          select(label, note, parent, date, up, unit, 'total' = within18),
                        rent %>%
                           mutate(label = 'Renting a 2-bed',
                                  note = "Cost of privately renting a 2-bed property (ONS)", 
                                  parent = 'Housing',
                                  up = 'bad',
                                  unit = '£') %>%
                           select(label, note, parent, date, up, unit, 'total' = rent2bed),
                         prison %>%
                           mutate(label = 'Prisoners',
                                  note = "Total prison population (Ministry of Justice)", 
                                  parent = 'Crime',
                                  up = 'bad',
                                  unit = '') %>%
                           select(label, note, parent, date, up, unit, 'total' = prison),
                         cc %>%
                           mutate(label = 'Crown court caseload',
                                  note = "Number of crown court cases currently open (Ministry of Justice)", 
                                  parent = 'Crime',
                                  up = 'bad',
                                  unit = '') %>%
                           select(label, note, parent, date, up, unit, 'total' = crowncourt),
                         pint %>%
                           mutate(label = 'Price of a pint',
                                  note = "Average price of a pint of premium lager (ONS)", 
                                  parent = 'Living standards',
                                  up = 'bad',
                                  unit = '£') %>%
                           select(label, note, parent, date, up, unit,  total),
                         knife %>%
                           mutate(label = 'Knife crime',
                                  note = "Offences involving a knife, recorded by policer (Home Office)", 
                                  parent = 'Crime',
                                  up = 'bad',
                                  unit = '') %>%
                           select(label, note, parent, date, up, unit,  'total' = knife),
                         coffee %>%
                           mutate(label = 'Price of a coffee',
                                  note = "Average price of a cup of takeaway coffee (ONS)", 
                                  parent = 'Living standards',
                                  up = 'bad',
                                  unit = '£') %>%
                           select(label, note, parent, date, up, unit,  'total' = total),
                         restaurant %>%
                           mutate(label = 'Price of a meal',
                                  note = "Average price of a restaurant main course (ONS)", 
                                  parent = 'Living standards',
                                  up = 'bad',
                                  unit = '£') %>%
                           select(label, note, parent, date, up, unit,  'total' = total),
                         cancer %>%
                           mutate(label = 'Prompt cancer treatment',
                                  note = "Percentage starting cancer treatment within 62 days of referral (NHS)", 
                                  parent = 'Health',
                                  up = 'good',
                                  unit = '%') %>%
                           select(label, note, parent, date, up, unit,  'total' = cancer62),
                         beds %>%
                           mutate(label = 'Bed occupancy',
                                  note = "Percentage of general acute beds occupied (NHS)", 
                                  parent = 'Health',
                                  up = 'bad',
                                  unit = '%') %>%
                           select(label, note, parent, date, up, unit,  'total' = occupancy),
                         house.price.earnings %>%
                           mutate(label = 'House price to earnings',
                                  note = "Ratio of house prices to average annual wages (ONS)", 
                                  parent = 'Housing',
                                  up = 'bad',
                                  unit = '') %>%
                           select(label, note, parent, date, up, unit,  'total' = ratio),
                        vac %>%
                          mutate(label = 'Job vacancies',
                                 note = "Total number of job vacancies (ONS)", 
                                 parent = 'Economy',
                                 up = 'good',
                                 unit = '') %>%
                          select(label, note, parent, date, up, unit, 'total' = vacancies))) %>%
  filter(date >= as.Date('2020-01-01'))

# POSITION NOW SET BY ORDER

master <- master %>% left_join(master %>%
                      select(label) %>%
                      unique() %>%
                      mutate(position = 1:nrow(.)))


# ADD A ROW FROM 1 AND 2 AND 5 YEARS AGO

master.adj <- master %>%
  bind_rows(master %>%
              group_by(label) %>%
              filter(date == max(date)) %>%
              mutate(date = date - years(1) ,
                     total = NA),
            master %>%
              group_by(label) %>%
              filter(date == max(date)) %>%
              mutate(date = date - years(2),
                     total = NA),
            master %>%
              group_by(label) %>%
              filter(date == max(date)) %>%
              mutate(date = date - years(5),
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
  arrange(position, label, date) %>%
  group_by(position, label, note, parent, now, change1, change2, change5, up, unit) %>%
  nest(data = c(date, total)) %>%
  ungroup() %>%
  mutate(c1 = ifelse(change1 > 0, up, 'reverse'),
         c1 = ifelse(c1 == 'reverse' & up == 'bad', 'good',c1),
         c1 = ifelse(c1 == 'reverse' & up == 'good', 'bad', c1),
         colour1 = ifelse(c1 == 'bad', 'red', ifelse(c1 == 'good', 'green', 'grey')))  %>%
  mutate(c2 = ifelse(change2 > 0, up, 'reverse'),
         c2 = ifelse(c2 == 'reverse' & up == 'bad', 'good',c2),
         c2 = ifelse(c2 == 'reverse' & up == 'good', 'bad', c2),
         colour2 = ifelse(c2 == 'bad', 'red', ifelse(c2 == 'good', 'green', 'grey')))  %>%
  mutate(c5 = ifelse(change5 > 0, up, 'reverse'),
         c5 = ifelse(c5 == 'reverse' & up == 'bad', 'good',c5),
         c5 = ifelse(c5 == 'reverse' & up == 'good', 'bad', c5),
         colour5 = ifelse(c5 == 'bad', 'red', ifelse(c5 == 'good', 'green', 'grey')))  %>%
  
  select(position,label, note, parent, 'colour' = colour1, colour2, colour5, now, 'change' = change1, change2, change5, unit, data) %>%
  mutate(data = map(data, ~ {
    df <- .x
    map2(as.character(df$date), df$total, ~ set_names(list(.y), .x))
  })) 

file %>%
  write_json('sparklines-page.json', auto_unbox = T, pretty = T)

file %>%
  write_json('sparklines-slice.json', auto_unbox = T, pretty = T)

