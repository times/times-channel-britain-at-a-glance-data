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

hp <- read_csv('https://landregistry.data.gov.uk/app/ukhpi/download/new.csv?from=1991-01-01&location=http%3A%2F%2Flandregistry.data.gov.uk%2Fid%2Fregion%2Funited-kingdom&thm%5B%5D=property_type&in%5B%5D=avg') %>%
  filter(`Reporting period` == 'monthly') %>%
  select('date' = Period, 'price' = `Average price All property types`) %>%
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


#7. HOUSING COMPLETIONS
'https://www.ons.gov.uk/peoplepopulationandcommunity/housing/datasets/ukhousebuildingpermanentdwellingsstartedandcompleted' %>%
  read_html() %>%
  html_nodes('.btn--thick') %>%
  html_attr('href') %>%
  paste0('https://www.ons.gov.uk', .) %>%
  download.file(destfile = 'downloads/houses.xlsx')

housing <- read_excel('downloads/houses.xlsx', 6, skip = 5) %>%
  select('date' = 2, 'start' = 3, 'complete' = 7) %>%
  mutate(date = lubridate::my(gsub('(.*) - ','', date )))  %>%
  select(-start)




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

boats <- readODS::read_ods('downloads/boats.ods', 3)  %>%
  mutate(date = lubridate::dmy(Date),
         d2 = as.Date(paste0('2025', substr(date, 5,10))),
         year = substr(date, 1, 4),
         rolling = rollsum(`Migrants arrived`, 365, align = 'right', fill = NA)) %>%
  select(date, 'migrants' = 2, 
         'boats' = 3, 
         d2, year, rolling) 

#10. INACTIVITY BY REASON

inac <- read_csv('https://www.ons.gov.uk/generator?format=csv&uri=/employmentandlabourmarket/peoplenotinwork/economicinactivity/timeseries/lf69/lms') %>%
  slice(300:nrow(.)) %>%
  rename('date' = 1, 'total' = 2) %>%
  mutate(date = lubridate::ym(date), 
         total = 1000 * as.numeric(total))

#COMBINE


master <- bind_rows(list(inf %>%
                           mutate(position = 1,
                                  label = 'Inflation',
                                  up = 'bad',
                                  unit = '%') %>%
                           select(position, label, date, up, unit, 'total' = inf),
                         gdp %>%
                           mutate(position = 2,
                                  label = 'Real GDP per capita',
                                  up = 'good',
                                  unit = '£') %>%
                           select(position, label, date, up, unit, 'total' = gdp),
                         unemp %>%
                           mutate(position = 3,
                                  label = 'Unemployment',
                                  up = 'bad',
                                  unit = '%') %>%
                           select(position, label, date, up, unit, 'total' = unem),
                         inac %>%
                           mutate(position = 4,
                                  label = 'Long-term sick',
                                  up = 'bad',
                                  unit = '') %>%
                           select(position, label, date, up, unit, total),
                         hp %>%
                           mutate(position = 5,
                                  label = 'House prices',
                                  up = 'good',
                                  unit = '£') %>%
                           select(position, label, date, up, unit, 'total' = price),
                         # housing %>%
                         #   mutate(position = 6,
                         #          label = 'Housing completions',
                         #          up = 'good',
                         #          unit = '') %>%
                         #   select(position, label, date, up, unit, 'total' = complete),
                         waits %>%
                           mutate(position = 7,
                                  label = 'NHS waiting list',
                                  up = 'bad',
                                  unit = '') %>%
                           select(position, label, date, up, unit, total),
                         crime %>%
                           mutate(position = 8,
                                  label = 'Crime, including fraud',
                                  up = 'bad',
                                  unit = '') %>%
                           select(position, label, date, up, unit, 'total' = `With fraud`),
                         # boats %>%
                         #   mutate(position = 9,
                         #          label = 'Small boat crossings in past year',
                         #          up = 'bad',
                         #          unit = '') %>%
                         #   select(position, label, date, up, unit, 'total' = rolling),
                         imm %>%
                           mutate(position = 10,
                                  label = 'Net migration',
                                  up = 'bad',
                                  unit = '') %>%
                           select(position, label, date, up, unit, total))) %>%
  filter(date >= as.Date('2020-01-01'))



master %>% filter(grepl('Crime', label)) %>% tail()


file <- master %>% 
  left_join(master %>%
              group_by(label) %>%
              filter(date == max(date)) %>%
              select(label, 'now' = total) %>%
              left_join(master %>% group_by(label) %>%
                          filter(date == max(date) - years(1)) %>%
                          select(label, y1 = total)) %>%
              mutate(change = now - y1) %>%
              select(-y1)) %>%
  arrange(position, label, date) %>%
  group_by(position, label, now, change, up, unit) %>%
  nest(data = c(date, total)) %>%
  ungroup() %>%
  mutate(c = ifelse(change > 0, up, 'reverse'),
         c = ifelse(c == 'reverse' & up == 'bad', 'good',c),
         c = ifelse(c == 'reverse' & up == 'good', 'bad', c),
         colour = ifelse(c == 'bad', 'red', ifelse(c == 'good', 'green', '')))  %>%
  select(position,label, colour, now, change, unit, data) %>%
  mutate(data = map(data, ~ {
    df <- .x
    map2(as.character(df$date), df$total, ~ set_names(list(.y), .x))
  })) 

file %>%
  write_json('master.json',auto_unbox = T, pretty = T)

# jsonfile <- file %>% toJSON(auto_unbox = T, pretty = T)
# 
# t = data.frame(text = jsonfile) %>%
#   mutate(text = as.character(text))
# googlesheets4::write_sheet(t, '1iXjSJWF8eAb4adxn8w8ZO_cIu98ka-Fu65rs15C7uTU', sheet = 'sparklines' )
# 
# 