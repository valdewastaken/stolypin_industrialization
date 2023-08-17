library(readstata13)
library(readxl)
library(writexl)
library(tidyverse)
library(googlesheets4)
library(magrittr)

#############################################

reform1 <- read.dta13("research project/Stol_data.dta")
reform2 <- read_xlsx("research project/For_Igor.xlsx")

provinces <- read_xlsx("research project/provinces_match.xlsx")

names(reform2)[1] <- "province_eng"
reform2 <- plyr::join_all(list(provinces, reform2), by = "province_eng", type = "full")

names(reform2)[3] <- "gub_id"
names(reform1)[3] <- "gub_id"

reform1 <- plyr::join(reform2 %>% select(gub_id, province),
                      reform1,
                      by = "gub_id")
reform1$rural_pop <- reform1$popul*(1-reform1$ushare)

data <- readRDS("research project/stolypin.RDS")

reform <- data %>% 
  select(province,
         exits_sum,
         consolidate_ind,
         consolidate_ind_sing,
         consolidate_ind_coll_sum,
         rural_pop_1907) %>%
  unique() %>% 
  drop_na(rural_pop_1907)

reform$exits <- reform$exits_sum/reform$rural_pop_1907
reform$consolidate <- reform$consolidate_ind/reform$rural_pop_1907
reform$consolidate_sing <- reform$consolidate_ind_sing/reform$rural_pop_1907
reform$consolidate_coll <- reform$consolidate_ind_coll_sum/reform$rural_pop_1907

#reform %<>% select(province,
#                   exits,
#                   consolidate,
#                   consolidate_sing,
#                   consolidate_coll)

reform %<>% select(-rural_pop_1907)

#РЕФОРМА НА УРОВНЕ ДОМОХОЗЯЙСТВ НОРМИРОВАНА НА СЕЛЬСКОЕ НАСЕЛЕНИЕ В 1907


prereform <- data %>% filter(province %in% reform$province) %>% 
                      select(city_id,
                             city_name,
                             uezd_join,
                             province,
                             factories_workers_1904,
                             factories_num_1904,
                             factory_prod_1904,
                             factory_prod_pw_1904,
                             workers_pf1904,
                             prod_pf_1904,
                             city_pop_1904,
                             print_num_1904,
                             rural_pop_1907,
                             rwtarif_1907,
                             rwage_1907,
                             uwage_1907,
                             livestock_1907,
                             yield_1907,
                             rdensity_1907) %>% mutate(period = 0)

postreform <- read_sheet(
  "https://docs.google.com/spreadsheets/d/1yKaZxd4HCTn0DoclhMzRpT6Gs3Umcd2VR7Shok4dLzo/edit#gid=954964940",
  sheet = "cross_fin") %>% 
  filter(province_rus %in% reform$province) %>% 
  select(city_id,
         city_name,
         uezd_join,
         province_rus,
         factories_workers,
         factories_num,
         factory_prod,
         city_pop,
         print_num) %>% 
  mutate(factory_prod_pw = factory_prod/factories_workers,
         workers_pf = factories_workers/factories_num,
         prod_pf = factory_prod/factories_num,
         period = 1)

postreform %<>% rename("province" = "province_rus")

postreform$factory_prod_pw <- na_if(postreform$factory_prod_pw, "NaN")
postreform$factory_prod_pw <- na_if(postreform$factory_prod_pw, "Inf")
postreform$workers_pf <- na_if(postreform$workers_pf, "NaN")
postreform$workers_pf <- na_if(postreform$workers_pf, "Inf")
postreform$prod_pf <- na_if(postreform$prod_pf, "NaN")
postreform$prod_pf <- na_if(postreform$prod_pf, "Inf")

postreform <- plyr::join(postreform,
                         reform1 %>% 
                           filter(year == 1910) %>% 
                           select(province,
                                  rural_pop,
                                  rwtarif,
                                  rwage,
                                  uwage,
                                  livestock,
                                  yield,
                                  rdensity) %>% 
                           unique(),
                         by = "province")


time_invariant <- data %>% filter(province %in% reform$province) %>% 
  select(city_id,
         #city_name,
         #uezd_join,
         province,
         literacy,
         `City administrative status`,
         repartition_province_1907,
         #russian_per,
         malorus_per,
         belorus_per,
         railroad,
         mean_space,
         community_space_share)

names(time_invariant)[4] <- "city_status"

ti4 <- read.dta13("1897_nathov.dta")
ti4 %<>% select(province_rus,
                popul_tot,
                birth_rate,
                velikorus_per,
                popul_m,
                muslims,
                others_non_christ,
                doctors_rural)

ti4 %<>%
  mutate(velikorus = velikorus_per*popul_tot/100,
         muslims = muslims*popul_tot/100,
         others_non_christ = others_non_christ*popul_tot)

ti4 %<>% 
  group_by(province_rus) %>% 
  summarise(birth_rate = sum(birth_rate*popul_tot/1000)*1000/sum(popul_tot),
            velikorus = sum(velikorus),
            muslims = sum(muslims),
            others_non_christ = sum(others_non_christ),
            doctors_rural = sum(doctors_rural*popul_tot/1000)*1000/sum(popul_tot),
            popul_tot = sum(popul_tot)) %>% 
  mutate(velikorus_per = velikorus/popul_tot,
         muslims_per = muslims/popul_tot,
         other_non_christ_per = others_non_christ/popul_tot,
         province_rus = str_to_lower(province_rus)) %>% 
  select(province_rus, birth_rate, doctors_rural, velikorus_per, muslims_per, other_non_christ_per) %>% 
  rename("province" = "province_rus") %>% 
  filter(province %in% reform$province)

conscripts <- read_sheet(
  "https://docs.google.com/spreadsheets/d/19aWS0L0kxw78PF5QdgcXYpm0QUFRc7S0wyuYUPT6Bis/edit#gid=0"
)
conscripts %<>% 
  mutate(pop_1904 = as.numeric(pop_1904)*1000,
         rural_pop_1905 = as.numeric(rural_pop_1905)*1000)

conscripts %<>% 
  select(-province_full) %>% 
  rename("province" = "province_short") %>% 
  filter(province %in% reform$province)

garrisons <- read_sheet(
  "https://docs.google.com/spreadsheets/d/19aWS0L0kxw78PF5QdgcXYpm0QUFRc7S0wyuYUPT6Bis/edit#gid=0",
  sheet = "garrisons"
)
garrisons$garrison <- 1
garrisons %<>% 
  group_by(province) %>% 
  summarise(garrisons = sum(garrison)) %>% 
  drop_na()
garrisons %<>% filter(province %in% reform$province)

time_invariant <- plyr::join(time_invariant, ti4, by = "province")

full_data <- rbind(postreform, prereform %>% 
                     rename("factories_workers" = "factories_workers_1904",
                            "factories_num" = "factories_num_1904",
                            "factory_prod" = "factory_prod_1904",
                            "factory_prod_pw" = "factory_prod_pw_1904",
                            "workers_pf" = "workers_pf1904",
                            "prod_pf" = "prod_pf_1904",
                            "city_pop" = "city_pop_1904",
                            "print_num" = "print_num_1904",
                            "rural_pop" = "rural_pop_1907",
                            "rwtarif" = "rwtarif_1907",
                            "rwage" = "rwage_1907",
                            "uwage" = "uwage_1907",
                            "livestock" = "livestock_1907",
                            "yield" = "yield_1907",
                            "rdensity" = "rdensity_1907"
                            ))


full_data <- plyr::join(full_data, reform, by = "province")
full_data <- plyr::join(full_data, time_invariant, by = c("city_id", "province"))
full_data <- plyr::join_all(list(full_data, conscripts, garrisons),
                            by = "province")


full_data %<>% filter(city_id != 986)

lcs <- full_data %>% select(province, land_captains_1905, land_captains_1910) %>% unique()

full_data %<>% select(-land_captains_1905, -land_captains_1910)

lcs5 <- lcs %>% 
  select(-land_captains_1910) %>% 
  mutate(period = 0) %>% 
  rename("land_captains" = "land_captains_1905")

lcs10 <- lcs %>% 
  select(-land_captains_1905) %>% 
  mutate(period = 1) %>% 
  rename("land_captains" = "land_captains_1910")

lcs <- rbind(lcs5, lcs10)

full_data <- plyr::join(full_data, lcs, by = c("province", "period"))

saveRDS(full_data, "working_dataset.RDS")

fulldata <- readRDS("working_dataset.RDS")

pop05 <- read_sheet(
  "https://docs.google.com/spreadsheets/d/19aWS0L0kxw78PF5QdgcXYpm0QUFRc7S0wyuYUPT6Bis/edit#gid=0"
)

pop05 %<>% 
  select(province_short, pop_1905) %>% 
  mutate(pop_1905 = as.numeric(pop_1905)) %>% 
  rename("province" = "province_short")
migration <- reform1 %>% 
  filter(year <= 1910) %>% 
  select(province, year, migrants) %>% 
  group_by(province) %>% 
  summarise(migrants = sum(migrants))
unrest <- read_xls("Peasant revolts 1895 - 1914 new.xls")  
names(unrest) <- c("gubernia_eng", "year", "peasantrevolt_new", "peasant_revolt_new_reg", "peasant_revolt_add")
unrest <- unrest[-1,]

unrest2 <- read_xls("Peasant revolts 1895 - 1914 new.xls", sheet = 2)
unrest2 %<>% select(gubernia_rus, gubernia_eng)

unrest2$province <- str_to_lower(unrest2$gubernia_rus)
unrest2$province <- gsub(" губерния", "", unrest2$province)
unrest2$province <- gsub(" область", "", unrest2$province)
unrest2 <- plyr::join(unrest2, 
                      fulldata %>% 
                        select(province) %>% 
                        unique() %>% 
                        mutate(check = 1), 
                      by = "province")


unrest <- plyr::join(unrest, unrest2 %>% filter(check == T) %>% select(-check),
                     by = "gubernia_eng")

unrest %<>% filter(!is.na(province)) %>% select(-gubernia_eng, -gubernia_rus)
unrest %<>% filter(year <= 1905)

unrest %<>% 
  mutate(peasantrevolt_new = as.numeric(peasantrevolt_new),
         peasant_revolt_new_reg = as.numeric(peasant_revolt_new_reg),
         peasant_revolt_add = as.numeric(peasant_revolt_add))

unrest_pre <- unrest %>% filter(year <= 1903) %>% 
  group_by(province) %>% 
  summarise(peasantrevolt_new = sum(peasantrevolt_new),
            peasant_revolt_new_reg = sum(peasant_revolt_new_reg, na.rm = T),
            peasant_revolt_add = sum(peasant_revolt_add)) %>% 
  mutate(year = "pre")

unrest <- rbind(unrest %>% filter(year >= 1904), unrest_pre)
unrest %<>% pivot_wider(id_cols = province, 
                        names_from = year, 
                        values_from = c(peasantrevolt_new, peasant_revolt_new_reg, peasant_revolt_add))

full_data <- plyr::join_all(list(fulldata, unrest, migration, pop05),
                            by = "province")  
  
  
saveRDS(full_data, "updated_data.RDS")

unrest <- read_xls("Peasant revolts 1895 - 1914 new.xls")  
names(unrest) <- c("gubernia_eng", "year", "peasantrevolt_new", "peasant_revolt_new_reg", "peasant_revolt_add")
unrest <- unrest[-1,]

unrest2 <- read_xls("Peasant revolts 1895 - 1914 new.xls", sheet = 2)
unrest2 %<>% select(gubernia_rus, gubernia_eng)

unrest2$province <- str_to_lower(unrest2$gubernia_rus)
unrest2$province <- gsub(" губерния", "", unrest2$province)
unrest2$province <- gsub(" область", "", unrest2$province)
unrest2 <- plyr::join(unrest2, 
                      fulldata %>% 
                        select(province) %>% 
                        unique() %>% 
                        mutate(check = 1), 
                      by = "province")


unrest <- plyr::join(unrest, unrest2 %>% filter(check == T) %>% select(-check),
                     by = "gubernia_eng")

unrest %<>% filter(!is.na(province)) %>% select(-gubernia_eng, -gubernia_rus)
unrest %<>% 
  mutate(peasantrevolt_new = as.numeric(peasantrevolt_new),
         peasant_revolt_new_reg = as.numeric(peasant_revolt_new_reg),
         peasant_revolt_add = as.numeric(peasant_revolt_add))


unrest_during <- unrest %>% filter(year >= 1907 & year <= 1910) %>% 
  group_by(province) %>% 
  summarise(peasantrevolt_new = mean(peasantrevolt_new),
            peasant_revolt_new_reg = mean(peasant_revolt_new_reg),
            peasant_revolt_add = mean(peasant_revolt_add)) %>% 
  mutate(year = "during") %>% 
  pivot_wider(id_cols = province, 
              names_from = year, 
              values_from = c(peasantrevolt_new, peasant_revolt_new_reg, peasant_revolt_add))

full_data <- plyr::join(full_data, unrest_during, by = "province") %>%
  mutate(peasantrevolt_new_pre = peasantrevolt_new_pre/9,
         peasant_revolt_new_reg_pre = peasant_revolt_new_reg_pre/9,
         peasant_revolt_add_pre = peasant_revolt_add_pre/9)
  

saveRDS(full_data, "updated_data_1.RDS")

full_data <- readRDS("updated_data_1.RDS")

####spillovers###############################

#db <- available.packages()
#deps <- tools::package_dependencies("spdep", db)$spdep
#install.packages(deps)

library(sf)
#library(spdep)

nc0 <- read_sf(dsn = "research project/1897RussianEmpire.shp")
provinces_map <- readxl::read_xlsx("research project/map_provinces.xlsx")
provinces_map$check <- provinces_map$province %in% full_data$province

provinces_map <- plyr::join(provinces_map, 
                            full_data %>% 
                              select(province, 
                                     exits_sum, 
                                     consolidate_ind,
                                     consolidate_ind_sing,
                                     consolidate_ind_coll_sum) %>% 
                              unique(),
                            by = "province")
provinces_map[is.na(provinces_map$exits_sum),]$exits_sum <- 0
provinces_map[is.na(provinces_map$consolidate_ind),]$consolidate_ind <- 0
provinces_map[is.na(provinces_map$consolidate_ind_sing),]$consolidate_ind_sing <- 0
provinces_map[is.na(provinces_map$consolidate_ind_coll_sum),]$consolidate_ind_coll_sum <- 0


ggplot()+
  geom_sf(data = nc0)+
  theme_bw()

nc0 <- tigris::geo_join(nc0, provinces_map, by = "NameRUS")

ggplot()+
  geom_sf(data = nc0, aes(fill = nc0$check))+
  theme_bw()


nc0 %<>% mutate(c_id = 1:nrow(nc0))        
n = 5                  
grp <- st_intersects(nc0, nc0[n,1], sparse = F ) 

neighborhood <- nc0[grp,]
neighborhood 

plot(neighborhood$geometry)
plot(nc0[n,1], col = 'blue', add = TRUE)

ggplot()+
  geom_sf(data = nc0)+
  geom_sf(data = neighborhood, fill = "lightblue")+
  geom_sf(data = nc0[n,1], fill = "firebrick")+
  coord_sf(xlim = c(20.5, 66.5),
           ylim = c(42, 71),
           expand = FALSE)+
  labs(title = "Example of neighboring provinces",
       subtitle = "Nizhegorodkaya guberniya in red, neighbors in light blue")+
  theme_bw()
  

nc0$exits_near <- 0 
nc0$consolidate_near <- 0

for(i in 1:nrow(nc0)) {
  grp <- st_intersects(nc0, nc0[i,1], sparse = F ) 
  neighborhood <- nc0[grp,]
  nc0[i,]$exits_near <- sum(neighborhood$exits_sum)-nc0[i,]$exits_sum
  nc0[i,]$consolidate_near <- sum(neighborhood$consolidate_ind)-nc0[i,]$consolidate_ind
}

ggplot()+
  geom_sf(data = nc0, aes(fill = exits_near))+
  theme_bw()


ggplot()+
  geom_sf(data = nc0, aes(fill = consolidate_near))+
  theme_bw()


nears <- nc0 %>% 
  filter(check == 1) %>% 
  select(province, exits_near, consolidate_near) %>% 
  as.data.frame() %>% 
  select(-geometry)

full_data <- plyr::join(full_data, nears, by = "province")

saveRDS(full_data, "updated_data_2.RDS")
full_data <- readRDS("updated_data_2.RDS")

full_data <- plyr::join(full_data, 
                        reform1 %>% 
                          select(province, year, popul) %>% 
                          filter(year %in% c(1907, 1910)) %>% 
                          rename("period" = "year") %>% 
                          mutate(period = ifelse(period == 1907, 0 , 1)) %>% 
                          unique(),
                        by = c("province", "period"))
saveRDS(full_data, "updated_data_3.RDS")

#man_nb_queen <- poly2nb(nc0, queen = TRUE)
#plot.nb(man_nb_queen, st_geometry(nc0), lwd = 0.3)











####old#################################

garrisons <- read_sheet(
  "https://docs.google.com/spreadsheets/d/19aWS0L0kxw78PF5QdgcXYpm0QUFRc7S0wyuYUPT6Bis/edit#gid=0",
  sheet = "garrisons"
)

#garrisons_matched <- garrisons %>% filter(!is.na(province)) %>% select(-`...4`)
#garrisons_not_matched <- garrisons %>% filter(is.na(province)) %>% select(-`...4`)
#garrisons_not_matched <- plyr::join(garrisons_not_matched %>% 
#                                      select(city_name_full),
#                                    data %>% 
#                                      select(city_id, city_name, province) %>% 
#                                      rename("city_name_full" = "city_name"),
#                                    by = "city_name_full")
#garrisons <- rbind(garrisons_matched, garrisons_not_matched)

#write_sheet(garrisons,
#  "https://docs.google.com/spreadsheets/d/19aWS0L0kxw78PF5QdgcXYpm0QUFRc7S0wyuYUPT6Bis/edit#gid=0",
#  sheet = "garrisons"
#)

