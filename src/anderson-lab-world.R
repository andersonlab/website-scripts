library(spData)
library(sf)
library(ggrepel)
library(rvest)
library(tidyverse)
library(xml2)

# Scrape the data from andersonlab.info/people to get the links for all people
al.people <- read_html("http://andersonlab.info/people")
al.people.links <- al.people %>%
  html_elements("a") %>% 
  xml_find_all("//a[contains(@class, 'link-block')]")%>%
  html_attr("href") %>% 
  head(-2)

person.header.list <- c()
for (person in al.people.links){
  person <- read_html(paste0("http://andersonlab.info",person))
  
  person.header <- person %>% 
    html_elements("h1") %>% 
    html_text
  person.header.list <- c(person.header.list, person.header)
}

anderson.lab.people.df <- tibble(people=person.header.list) %>% 
  separate(col = people, into = c('Name',
                                  'Country'),
           sep = ' \\(') %>% 
  separate(col=Name, into=c('First_name',
                            'Last_name'),
           extra='merge') %>% 
  mutate(Country=str_remove(Country, '\\)'))

anderson.lab.people.df <- anderson.lab.people.df %>% 
  left_join(tibble(world), by=c("Country"='name_long'))

anderson.lab.people.df

# Plot the data on the map
ggplot(data=anderson.lab.people.df) + 
  geom_sf(data=world,aes(geometry = geom), fill='#93cccb') +
  geom_sf(aes(geometry = geom), fill='#F17925') +
  geom_label_repel(aes(label=First_name,geometry=geom),
                   stat='sf_coordinates',max.iter = 200000,max.time = 10,
                   force = 4, size = 5,min.segment.length = 0,max.overlaps = 30) +
  theme_classic() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.margin=grid::unit(c(0,0,0,0), "mm"))

ggsave('./plots/anderson-lab-world.png',width = 14, height=7)

