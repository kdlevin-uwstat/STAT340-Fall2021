pacman::p_load(tidyverse,magrittr,lubridate,glue,httr)

# making use of api documented here:
#   https://earthquake.usgs.gov/fdsnws/event/1/

# # get quakes and save
# quakes = tibble()
# for(y in 2020:1990){
#   try({
#     cat(y,"\n")
#     GET(glue("https://earthquake.usgs.gov/fdsnws/event/1/query?format=\\
#              csv&starttime={y}-01-01&endtime={y}-12-31&minmagnitude=4")) %>% 
#       content(as="raw") %>% 
#       rawConnection %>% 
#       read_csv(col_types="TddddcddddccTccddddccc",progress=F) %>% 
#       map_df(rev) %>% 
#       bind_rows(quakes) -> quakes
#   })
# }
# write_csv(quakes,file="quakes.csv.gz")

# load quakes
quakes = read_csv("quakes.csv.gz",col_types="TddddcddddccTccddddccc")

# filter and bin magnitudes
# fix magtypes to be consistent
quakes %<>% 
  filter(mag >= 4.25) %>% 
  mutate(year = year(time),
         magType = str_remove(str_to_lower(magType),"_20$"),
         mag.binned = mag %>% 
           cut_width(center=.5,width=.5,closed="l") %>% 
           fct_relabel(function(..) sapply(regmatches(..,gregexpr("[0-9.]+",..)),
                                           . %>% as.numeric %>% mean %>% as.character)) %>% 
           as.character %>% as.numeric) %>% 
  relocate(time,year,mag,mag.binned,everything())

# description of sources:
#   https://earthquake.usgs.gov/data/comcat/
# some sources report very few events
quakes %>% count(magSource) %>% arrange(desc(n))

# check distribution of binned magnitudes for top 9 magSource networks
quakes %>% 
  group_by(magSource) %>% 
  mutate(sumSource = n()) %>% 
  filter(sumSource > 500) %>% 
  count(sumSource,magSource,mag.binned) %>% 
  mutate(magSource = str_c(magSource,": ",sumSource)) %>% 
  arrange(desc(sumSource),desc(n)) %>% 
  ggplot(aes(x=mag.binned,y=n)) + geom_col() + 
  facet_wrap(~fct_reorder(magSource,1/sumSource),scales="free_y") + scale_y_log10()

# keep only "us" sources to improve data coherence
quakes %<>% 
  filter(magSource == "us")

# description of magnitude types:
#   https://www.usgs.gov/natural-hazards/earthquake-hazards/science/magnitude-types
# important notes:
#   - mww is best measure, magnitude range: >= 5.0
#   - mwc used if no mww value,      range: >= 5.5
#   - mwb used if no mww or mwc,     range: 5.5-7.0
#   - mwr, mb used otherwise,        range: 4.0-6.0
# these seem to be the most important types

# check distribution of magnitudes for each type
quakes %>% 
  group_by(magType) %>% 
  mutate(sumType=n(),magType=glue("{magType} ({sumType})")) %>% 
  ggplot(aes(fct_reorder(magType,sumType),mag)) + 
  geom_violin(fill="blue",color="blue",scale="area",adjust=3) + 
  scale_fill_continuous(type="viridis",trans='log10') + coord_flip() + 
  labs(y="Magnitude",x="Magnitude type (frequency)")

# also check distribution of magnitudes for each year
quakes %>% 
  group_by(magType) %>% 
  mutate(sumType=n(),magType=glue("{magType} ({sumType})")) %>% 
  count(magType,year,sumType) %>% 
  ggplot(aes(year,fct_reorder(magType,sumType),fill=n)) + geom_tile() + 
  scale_fill_continuous(type="viridis",trans='log10')

# also check distribution of magnitudes for each year
# also check distribution of magnitudes for each year
quakes %>% 
  group_by(year) %>% 
  mutate(total_count = n()) %>% 
  count(magType,year,total_count) %>% 
  arrange(desc(year)) %>% 
  pivot_wider(names_from=magType,values_from=n) %>% 
  relocate(total_count,year,mww,mwc,mwb,mwr,mb,everything()) %>% 
  print(n=Inf)

# we have fairly consistent total_count data and
# good combination of mww/mwc/mwb/mwr down to 2011
# filter to year>=2011
quakes %<>% filter(year>=2011, str_detect(magType,"mww|mwc|mwb|mwr|mb"))

# check plot
quakes %>%
  count(year,mag.binned,name='count') %>%
  group_by(year) %>%
  mutate(N = rev(cumsum(rev(count))))%>% 
  mutate(year = as.factor(year)) %>% 
  ggplot(aes(x=mag.binned,y=N,group=year,color=year,linetype=year)) +
  geom_point() + geom_line() + scale_y_log10() + 
  labs(title="Annual cumulative count of earthquakes v. magnitude",
       x="Magnitude M (rounded to nearest half-integer)",
       y="Cumulative count (# of quakes \u2265M)")

# looks good, save data
write_csv(quakes,file="quakes.csv.gz")
