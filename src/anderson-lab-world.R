library(spData)
library(sf)
library(ggrepel)
library(rvest)
library(tidyverse)
library(xml2)

# Save variable
SAVE <- TRUE

# Scrape the data from andersonlab.info/people to get the links for all people
al.people <- read_html("http://andersonlab.info/people")
al.people.links <- al.people %>%
  html_elements("a") %>% 
  xml_find_all("//a[contains(@class, 'link-block')]")%>%
  html_attr("href") %>% 
  head(-2) # Drop the last two links as they aren't people

# Iterate through all people links and scrape the "name (country)" field
person.header.list <- c()
for (person in al.people.links){
  person <- read_html(paste0("http://andersonlab.info",person))
  
  person.header <- person %>% 
    html_elements("h1") %>% 
    html_text
  person.header.list <- c(person.header.list, person.header)
}

# Collate the data into a dataframe
anderson.lab.people.df <- tibble(people=person.header.list) %>% 
  separate(col = people, into = c('Name',
                                  'Country'),
           sep = '\\((?!.*\\()',) %>% 
  separate(col=Name, into=c('First_name',
                            'Last_name'),
           extra='merge', sep=' ') %>% 
  mutate(Country=str_remove(Country, '\\)')) %>% 
  left_join(tibble(world), by=c("Country"='name_long'))


# Plot the data on the map
ggplot(data=anderson.lab.people.df) + 
  # geom_sf(data=world,aes(geometry = geom), fill='#93cccb') +
  # geom_sf(aes(geometry = geom), fill='#F17925') +
  geom_sf(data=world,aes(geometry = geom), fill='grey') +
  geom_sf(aes(geometry = geom, fill=Country)) +
  geom_text_repel(aes(label=First_name,geometry=geom, colour=Country),
                   stat='sf_coordinates',max.iter = 200000,max.time = 10,
                   force = 4, size = 4,min.segment.length = 0,max.overlaps = 30,
                  bg.color='white', bg.r=0.1) +
  theme_classic() +
  scale_colour_manual(values = rep_len(c("#f17925", "#93cccb", "#ed474d"),
                                    length(unique(anderson.lab.people.df$Country))),guide='none') +
  scale_fill_manual(values = rep_len(c("#f17925", "#93cccb", "#ed474d"),
                                    length(unique(anderson.lab.people.df$Country))),guide='none') +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.margin=grid::unit(c(0,0,0,0), "mm"))

# Save the plot
if (SAVE){
  ggsave('~/Documents/PhD_work/other/group-website/website-scripts/plots/anderson-lab-world.png',width = 14, height=7)
}
