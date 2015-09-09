###################################################################
#gathering and cleaning data#
###################################################################

#reading in libraries##############################################
source("scripts/load_packages.R")
source("scripts/pilot/helper_dospert.R")

#reading in pilot data############################################# 
pilot <- xmlToDataFrame("data/raw_data/Betterment_Pilot_8_10.xml", stringsAsFactors = F) %>% #reading in data
          dplyr::select(unique_id = uid, IPAddress, StartDate:EndDate, finRT_1:dohmen) %>% #dropping some vars
          dplyr::filter(unique_id != "") %>% dplyr::arrange(desc(ymd_hms(StartDate))) %>% #getting rid of empty unique_ids
          dplyr::filter(!(IPAddress %in% c("108.14.252.191", "4.53.135.250", #getting rid of specific users (blank)
                          "143.48.55.55", "199.17.143.15", "24.29.205.187",
                          "45.56.34.87", "171.66.209.4"))) %>% 
          dplyr::filter(month(ymd_hms(StartDate)) >= 7) #geting rid of test data
      yo <- as.data.frame(sapply(pilot %>% dplyr::select(finRT_1:dohmen), as.numeric))
#     yo <- colwise(as.numeric)(pilot %>% dplyr::select(finRT_1:dohmen)) #getting numeric columns from char
  
pilot <- cbind(pilot %>% select(-c(finRT_1:dohmen)), yo) %>%
          mutate(dohmen = ifelse(dohmen == 11, 10,
                          ifelse(dohmen == 10, 9,       
                          ifelse(dohmen == 9, 8,
                          ifelse(dohmen == 8, 7,
                          ifelse(dohmen == 7, 6,
                          ifelse(dohmen == 6, 5,
                          ifelse(dohmen == 5, 4,
                          ifelse(dohmen == 4, 3,
                          ifelse(dohmen == 3, 2,
                          ifelse(dohmen == 2, 1,
                          ifelse(dohmen == 1, 0, NA)))))))))))); remove(yo)

#analyzing pilot data############################################### 

#mean risk attitude
# mra <- dospert::d_clean(pilot, var = "unique_id") %>% #prepping data for scoring
#        dospert::d_score() %>% #scoring
#         rename(risk_attitude = int) #renaming intercept to risk attitude
full_data <- new_d_score(pilot, var = "unique_id"); remove(pilot)      

#subscale scores####################################################

rt_score <- d_sum(full_data, var = "unique_id", domain = "fin", risk_type = "RT", file_type = "xml")
rp_score <- d_sum(full_data, var = "unique_id", domain = "fin", risk_type = "RP", file_type = "xml")
rb_score <- d_sum(full_data, var = "unique_id", domain = "fin", risk_type = "RB", file_type = "xml")

full_data <- left_join(full_data, rt_score, by = "unique_id")
full_data <- left_join(full_data, rp_score, by = "unique_id")
full_data <- left_join(full_data, rb_score, by = "unique_id") %>%
              select(unique_id:finRT_6, finRT_sum, finRP_1:finRP_6, finRP_sum,
                     finRB_1:finRB_6, finRB_sum, dohmen:fin_RP); remove(rt_score, rp_score, rb_score)

#write out data#####################################################
write_csv(full_data, "data/in_process_data/pilot_8_10.csv")

# #hand calculation
# yo <- pilot %>% filter(uid == "HQJQC7AI4FBWL0YQ4KOA2LFIA266P4TH") 
# rt <- melt(yo %>% select(uid, finRT_1:finRT_6), id.vars = "uid") %>% rename(RT = value)
# rb <- melt(yo %>% select(uid, finRB_1:finRB_6), id.vars = "uid") %>% rename(RB = value)
# rp <- melt(yo %>% select(uid, finRP_1:finRP_6), id.vars = "uid") %>% rename(RP = value)
# yo <- cbind(rt,rb,rp) 
# check <- yo[,c(3,6,9)]; remove(rb, rp)
# check <- check %>% mutate(RT = as.numeric(RT),
#                           RP = as.numeric(RP),
#                           RB = as.numeric(RB)) %>% select(-RT) 
# check_t <- as.matrix(check %>% select(RB:RP)) %>%
#             t()
# check <- as.matrix(check)
# rt <- rt %>% select(RT) %>% mutate(RT = as.numeric(RT))
# rt <- as.matrix(rt)
# solve(check_t %*% check) %*% check_t %*% rt
# 
# summary(lm(RT ~ RB + RP, data = check))

