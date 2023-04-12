### AUTHOR: AHz
### WRITTEN IN: R version 4.2.2
### PURPOSE: help run amanda's code!!


################################################################################
#  LOAD PACKAGES  ####
################################################################################

library(pacman)
p_load(tidyverse, sf, lubridate)

################################################################################
#  SET WD  ####
################################################################################

setwd(here::here())


################################################################################
# LOAD DEPENDENCIES  ####
################################################################################


load("data/processed/steps1-3_for_running_perc_overlay")
load("data/processed/buffering_2023-04-02 18:55:26.RData")

sf_use_s2(FALSE)

n_completed <- 0
batches_to_complete <- floor((length(unique(eji_census_tracts_transformed$geoid))-length(geoids_completed))/1000)
geoids_completed <- names(perc_calc)

start_time <- now()
start_time_batch <- now()
end_time_batch <- ""

for (i in unique(eji_census_tracts_transformed$geoid[which(!eji_census_tracts_transformed$geoid %in% geoids_completed)])) {
  
  tract_select <- eji_census_tracts_transformed %>%
    filter(geoid == i)
  
  buffers[[i]] <- suppressMessages(st_intersection(ppps_buffer_overlap, tract_select))
  
  perc_calc[[i]] <- st_area(buffers[[i]]) / st_area(tract_select)
  
  geoids_completed <- append(geoids_completed, i)
  
  n_completed <- n_completed + 1
  
  if (n_completed %% 1000 == 0) {
    end_time_batch <- now()
    
    print(paste0(
      "batch ",
      n_completed/1000,
      " of ",
      batches_to_complete,
      " complete at ", Sys.time() ," in ", 
      as.duration(interval(start_time_batch, end_time_batch)), 
      " approximately ", 
      as.duration(interval(start_time_batch, end_time_batch))*(batches_to_complete-n_completed/1000),
      " remaining"
    ))
    
    save(geoids_completed, tract_select, buffers, perc_calc,
         file = paste0("data/processed/buffering_batch", n_completed/1000, "_", Sys.time(),".RData"))
    
    beepr::beep(sound = 10)
    start_time_batch <- now()
  
  }
  
}

save(geoids_completed, tract_select, buffers, perc_calc,
     file = paste0("data/processed/buffering_",Sys.time(),".RData"))
