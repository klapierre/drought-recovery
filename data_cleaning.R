library(tidyverse)

#kim's desktop
setwd('C:\\Users\\la pierrek\\Dropbox (Smithsonian)\\konza projects\\drought recovery')

###Konza - Watersheds

knz_anpp <- read.csv('KNZ_anpp.csv')%>%
  mutate(calendar_year=RECYEAR, site_code='KNZ', community_type=paste(WATERSHED, SOILTYPE, sep='_'), plot_id=paste(TRANSECT, PLOTNUM, sep='_'), grass=ifelse(LVGRASS=='NULL', 0, LVGRASS), forb=ifelse(FORBS=='NULL', 0, FORBS), current_dead=ifelse(CUYRDEAD=='NULL', 0, CUYRDEAD), litter=ifelse(PRYRDEAD=='NULL', 0, PRYRDEAD), woody=ifelse(WOODY=='NULL', 0, WOODY), comments=COMMENTS)%>%
  select(calendar_year, site_code, community_type, plot_id, grass, forb, current_dead, litter, woody, comments)%>%
  gather(key=form, value=biomass, grass:woody)%>%
  mutate(area_sampled=0.1)
# write.csv(knz_anpp, 'KNZ_anpp_formatted.csv')

knz_spp <- read.csv('C:\\Users\\la pierrek\\Dropbox (Smithsonian)\\konza projects\\prairie species lists\\konza_spplist.csv')

knz_comp <- read.csv('KNZ_composition.csv')%>%
  mutate(soil=ifelse(SOILTYPE=='f', 'fl', 'tu'), spnum=SPECODE, trans=ifelse(Transect=='A', 'a', ifelse(Transect=='B', 'b', ifelse(Transect=='C', 'c', 'd'))))%>%
  left_join(knz_spp)%>%
  mutate(calendar_year=RECYEAR, site_code='KNZ', community_type=paste(WATERSHED, soil, sep='_'), plot_id=paste(trans, Plot, sep='_'), species_code=SPECODE, genus_species=paste(genus, spp, sep='_'), abundance=Cover, comments=Comments)%>%
  select(calendar_year, site_code, community_type, plot_id, species_code, genus_species, abundance, comments)
# write.csv(knz_comp, 'KNZ_composition_formatted.csv')


###Saline Experimental Range - NutNet controls
ser_treatments <- read.csv('KNZ_SER_NutNet_plan.csv')%>%
  mutate(treatment=treat_other_name)%>%
  select(site, plot, treatment)

ser_anpp <- read.csv('SER_anpp.csv')%>%
  left_join(ser_treatments)%>%
  mutate(calendar_year=year, site_code=site, community_type='NA', plot_id=plot, comments=note_biomass)%>%
  #note, some samples were lost; marked as 999 in dataset
  filter(mass!=999)%>%
  mutate(type=ifelse(form=='ambr'|form=='forb'|form=='nfix', 'forb', ifelse(form=='andro'|form=='gram'|form=='gram'|form=='schiz'|form=='spor', 'grass', ifelse(form=='pdead'|form=='pyr', 'litter', 'woody'))))%>%
  select(calendar_year, site_code, community_type, plot_id, type, mass, treatment, comments)%>%
  group_by(calendar_year, site_code, community_type, plot_id, treatment, type)%>%
  summarise(biomass=sum(mass))%>%
  ungroup()%>%
  mutate(area_sampled=0.1)
# write.csv(ser_anpp, 'SER_anpp_formatted.csv')

ser_spp <- read.csv('KNZ_SER_NutNet_spp list.csv')%>%
  mutate(sppnum=SppNum)

ser_comp <- read.csv('SER_composition.csv')%>%
  left_join(ser_treatments)%>%
  mutate(calendar_year=year, site_code=site, community_type='NA', plot_id=plot, comments=note_cover)%>%
  filter(sppnum!=999, sppnum!=998, sppnum!=0, sppnum!=888)%>%
  group_by(site_code, calendar_year, community_type, treatment, plot_id, comments, sppnum)%>%
  summarise(abundance=max(cover))%>%
  ungroup()%>%
  left_join(ser_spp)%>%
  mutate(species_code=sppnum, genus_species=taxa)%>%
  select(calendar_year, site_code, community_type, treatment, plot_id, species_code, genus_species, abundance, comments)
# write.csv(ser_comp, 'SER_composition_formatted.csv')

