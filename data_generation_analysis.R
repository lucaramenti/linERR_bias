rm(list = ls())
library(readxl)
library(tictoc)
library(polynom)
library(linearERRfit)
library(ggplot2)
library(survival)
library(numDeriv) #for hessian function in no_threshold computation
# It generates a prespecified number of CT cohorts suitable for leukemia or brain tumor analysis using 
# different values of true ERR.
# The specific assumptions are that each subject can have max 5 CTs per
# year and that those CTs can be either head, chest or abdomen CTs.
# The corresponding organ-specific radiation doses were taken from Kim's paper.

# IMPORTANT: Here we assume that all subjects are healthy in order to show how big the bias in estimating true ERR is.

# (0) Study set-up:
organ = 2 # cancer of interest (1-brain tumor, 2-leukemia)
betas = c(0.04) # set of true ERR values to evaluate 
n_betas = length(betas)

no_iterat = 500 # number of cohorts (= no of simulation runs)
N = 150000 # number of subjects
FollowUpYrs = 34 #max duration of follow-up (from 1979 up to and including 2012) 

# Define matrices for storing main output results:
no_events_res = matrix(0, no_iterat, n_betas)
average_person_years_res = matrix(0, no_iterat, n_betas)
beta_est = matrix(0, no_iterat, n_betas)

fit = matrix(0, no_iterat, n_betas)

beta_est = matrix(0, no_iterat, n_betas)
beta_est_f = matrix(0, no_iterat, n_betas)
beta_est_no_thresh = matrix(0, no_iterat, n_betas)
beta_sd = matrix(0, no_iterat, n_betas)
beta_sd_f = matrix(0, no_iterat, n_betas)
convergence_info = matrix(1, no_iterat, n_betas)
convergence_info_corrected = matrix(1, no_iterat, n_betas)
p_value = matrix(0, no_iterat, n_betas)
p_value_score = matrix(0, no_iterat, n_betas)
p_value_f = matrix(0, no_iterat, n_betas)
threshold = matrix(0, no_iterat, n_betas)
thresh_flag = matrix(0, no_iterat, n_betas)
coverage = matrix(0, no_iterat, n_betas)
coverage_score = matrix(0, no_iterat, n_betas)
coverage_f = matrix(0, no_iterat, n_betas)
end_status_matrix = matrix(0, no_iterat, 3*n_betas)
n_headCTs_matrix = matrix(0, no_iterat, n_betas)
n_abdCTs_matrix = matrix(0, no_iterat, n_betas)
n_chestCTs_matrix = matrix(0, no_iterat, n_betas)

# (1) Loading external datasets:
# Load cancer incidence data:
cancer_inc_denom = 100000
age_groups = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39",
               "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79",
               "80-84", "85-89", "90-94", "95-99")
years = as.character(1977:2014)

cancer_inc_male_all_cancer = as.data.frame(read_excel("X:/Department Medizin/Biometrie/Projekte/CT/subprojects/simulation_study_bias/datasets_for_simulations/cancer_inc_male_all_cancer_2015.xlsx", col_names = age_groups))
cancer_inc_male_all_cancer = rbind(do.call("rbind", replicate(12, cancer_inc_male_all_cancer[1,], simplify = F )),
                                   cancer_inc_male_all_cancer)/cancer_inc_denom
row.names(cancer_inc_male_all_cancer) = years

cancer_inc_female_all_cancer = as.data.frame(read_excel("X:/Department Medizin/Biometrie/Projekte/CT/subprojects/simulation_study_bias/datasets_for_simulations/cancer_inc_female_all_cancer_2015.xlsx", col_names = age_groups))
cancer_inc_female_all_cancer = rbind(do.call("rbind", replicate(12, cancer_inc_female_all_cancer[1,], simplify = F )),
                                     cancer_inc_female_all_cancer)/cancer_inc_denom
row.names(cancer_inc_female_all_cancer) = years

cancer_inc_male_leuk = as.data.frame(read_excel("X:/Department Medizin/Biometrie/Projekte/CT/subprojects/simulation_study_bias/datasets_for_simulations/cancer_inc_male_leuk_2015.xlsx", col_names = age_groups))
cancer_inc_male_leuk = rbind(do.call("rbind", replicate(12, cancer_inc_male_leuk[1,], simplify = F )),
                             cancer_inc_male_leuk)/cancer_inc_denom
row.names(cancer_inc_male_leuk) = years

cancer_inc_female_leuk = as.data.frame(read_excel("X:/Department Medizin/Biometrie/Projekte/CT/subprojects/simulation_study_bias/datasets_for_simulations/cancer_inc_female_leuk_2015.xlsx", col_names = age_groups))
cancer_inc_female_leuk = rbind(do.call("rbind", replicate(12, cancer_inc_female_leuk[1,], simplify = F )),
                               cancer_inc_female_leuk)/cancer_inc_denom
row.names(cancer_inc_female_leuk) = years

cancer_inc_male_all_cancer_except_leukemia = cancer_inc_male_all_cancer - cancer_inc_male_leuk
cancer_inc_female_all_cancer_except_leukemia = cancer_inc_female_all_cancer - cancer_inc_female_leuk

# incidence of CNS tumors (include invasive cns tumors only, therefore rates up to 2001 need to be doubled to include benign tumors)
cancer_inc_male_cns = as.data.frame(read_excel("X:/Department Medizin/Biometrie/Projekte/CT/subprojects/simulation_study_bias/datasets_for_simulations/cancer_inc_male_cns_2015.xlsx", col_names = age_groups))
cancer_inc_male_cns = rbind(do.call("rbind", replicate(12, cancer_inc_male_cns[1,], simplify = F )),
                            cancer_inc_male_cns)/cancer_inc_denom
cancer_inc_male_cns[1:25,] = cancer_inc_male_cns[1:25,]*2 # brain tumor rates for years [1977,2001] should be doubled
cancer_inc_male_all_cancer_except_cns = cancer_inc_male_all_cancer - cancer_inc_male_cns

cancer_inc_female_cns = as.data.frame(read_excel("X:/Department Medizin/Biometrie/Projekte/CT/subprojects/simulation_study_bias/datasets_for_simulations/cancer_inc_female_cns_2015.xlsx", col_names = age_groups))
cancer_inc_female_cns = rbind(do.call("rbind", replicate(12, cancer_inc_female_cns[1,], simplify = F )),
                              cancer_inc_female_cns)/cancer_inc_denom
cancer_inc_female_cns[1:25,] = cancer_inc_female_cns[1:25,]*2 # brain tumor rates for years [1977,2001] should be doubled
cancer_inc_female_all_cancer_except_cns = cancer_inc_female_all_cancer - cancer_inc_female_cns

cancer_inc_years = c(1977:2014) 
cancer_inc_ages = seq(0,95,5)

# Load data on CT usage:
# There are 12 tables stored there each of which contains 34 rows, one for
# each year of CT (in the interval [1979-2012]) but varied number of columns. 

# Tables probabilitydataaboveage10 and probabilitydatabelowage10 contain 26 columns: 
# col1 - year of CT, 
# col2- age_group (1- if age at CT <10 years or 2 -if age at CT >=10 years) 
# col3 - probability of >=1 CT 
# col4-col30 - probability of respectively 1,2,3,4,5,6,7,8,9,10,11,12,16,21,13,19,14,15,17,25,18,22,20 (in this order!) 

# Tables transmergeddata1CTaboveage10 and transmergeddata1CTbelowage10 contain 5 columns: 
# col1 - year of CT, 
# col2- age_group (1- if age at CT <10 years or 2 -if age at CT >=10 years) 
# col3 - probaility of a head CT  
# col4- probability of an abdomen CT
# col5 - probability of a chest CT

# Tables transmergeddata2CTaboveage10 and transmergeddata2CTbelowage10 contain 8 columns:
# col1 - category of year of CT, 
# col2- age_group (1- if age at CT <10 years or 2 -if age at CT >=10 years) 
# col3 - probaility of a 2 abdomen CTs  
# col4- probability of 1 abd and 1 head CT
# col5 - probability of 1 head and 1 chest CTs
# col6- probability of 2 head CTs
# col7- probability of 1 abdomen and 1 chest CT
# col8- probability of 2 chest CTs

# Tables transmergeddata3CTaboveage10 and transmergeddata3CTbelowage10 contain 12 columns:
# col1 - year of CT, 
# col2- age_group (1- if age at CT <10 years or 2 -if age at CT >=10 years) 
# col3-col12 - probailities of 12 different combinations of 3 CTs where (assuming h-head, c-chect and a-abdomen CT) the order of
# combinations is: haa, hha, hhh, hhc, hcc, acc, ccc, aac, hac, aaa

# Tables transmergeddata4CTaboveage10 and transmergeddata4CTbelowage10 contain 17 columns:
# col1 - year of CT, 
# col2- age_group (1- if age at CT <10 years or 2 -if age at CT >=10 years) 
# col3-col17 - probailities of 15 different combinations of 4 CTs where (assuming h-head, c-chect and a-abdomen CT) the order of
# combinations is: aaaa, hhhh, cccc, aacc, hacc, hhaa, hhha, hhac, accc,
# aaac, hhcc, hhhc, haaa, hccc, haac

# Tables transmergeddata5CTaboveage10 and transmergeddata5CTbelowage10 contain 23 columns:
# col1 - year of CT, 
# col2- age_group (1- if age at CT <10 years or 2 -if age at CT >=10 years) 
# col3-col23 - probailities of 21 different combinations of 4 CTs where (assuming h-head, c-chect and a-abdomen CT) the order of
# combinations is: hhhhh, hhhhc, acccc, aaccc, aaacc, hhccc, hhhaa, ccccc,
# hhacc, haacc, hhhha, aaaaa, hhaac, haaac, hhhac, hcccc, haccc, hhhcc,
# aaaac, hhaaa, haaaa

probabilitydataaboveage10 = as.data.frame(read.csv("X:/Department Medizin/Biometrie/Projekte/CT/subprojects/simulation_study_bias/datasets_for_simulations/probabilitydataaboveage10.csv"))
probabilitydatabelowage10 = as.data.frame(read.csv("X:/Department Medizin/Biometrie/Projekte/CT/subprojects/simulation_study_bias/datasets_for_simulations/probabilitydatabelowage10.csv"))

CT_years = c(1979:2012)
transmergeddata1CTbelowage10 = as.data.frame(read.csv("X:/Department Medizin/Biometrie/Projekte/CT/subprojects/simulation_study_bias/datasets_for_simulations/transmergeddata1CTbelowage10.csv"))
transmergeddata1CTaboveage10 = as.data.frame(read.csv("X:/Department Medizin/Biometrie/Projekte/CT/subprojects/simulation_study_bias/datasets_for_simulations/transmergeddata1CTaboveage10.csv"))
transmergeddata2CTbelowage10 = as.data.frame(read.csv("X:/Department Medizin/Biometrie/Projekte/CT/subprojects/simulation_study_bias/datasets_for_simulations/transmergeddata2CTbelowage10.csv"))
transmergeddata2CTaboveage10 = as.data.frame(read.csv("X:/Department Medizin/Biometrie/Projekte/CT/subprojects/simulation_study_bias/datasets_for_simulations/transmergeddata2CTaboveage10.csv"))
transmergeddata3CTbelowage10 = as.data.frame(read.csv("X:/Department Medizin/Biometrie/Projekte/CT/subprojects/simulation_study_bias/datasets_for_simulations/transmergeddata3CTbelowage10.csv"))
transmergeddata3CTaboveage10 = as.data.frame(read.csv("X:/Department Medizin/Biometrie/Projekte/CT/subprojects/simulation_study_bias/datasets_for_simulations/transmergeddata3CTaboveage10.csv"))
transmergeddata4CTbelowage10 = as.data.frame(read.csv("X:/Department Medizin/Biometrie/Projekte/CT/subprojects/simulation_study_bias/datasets_for_simulations/transmergeddata4CTbelowage10.csv"))
transmergeddata4CTaboveage10 = as.data.frame(read.csv("X:/Department Medizin/Biometrie/Projekte/CT/subprojects/simulation_study_bias/datasets_for_simulations/transmergeddata4CTaboveage10.csv"))
transmergeddata5CTbelowage10 = as.data.frame(read.csv("X:/Department Medizin/Biometrie/Projekte/CT/subprojects/simulation_study_bias/datasets_for_simulations/transmergeddata5CTbelowage10.csv"))
transmergeddata5CTaboveage10 = as.data.frame(read.csv("X:/Department Medizin/Biometrie/Projekte/CT/subprojects/simulation_study_bias/datasets_for_simulations/transmergeddata5CTaboveage10.csv"))

# data for 1981 was unavailable - 1982 data was used instead
transmergeddata5CTbelowage10 = rbind(transmergeddata5CTbelowage10[1:2,],
                                     unlist(c(1981, transmergeddata5CTbelowage10[3, 2:23])),
                                     transmergeddata5CTbelowage10[3:33,])

# 1979 data was unavailable - 1980 data was used instead
# 1983 data was unavailable - 1984 data was used instead
# 1988 data was unavailable - 1989 data was used instead
transmergeddata5CTaboveage10 = rbind(unlist(c(1979, transmergeddata5CTaboveage10[1, 2:23])),
                                     transmergeddata5CTaboveage10[1:3,],
                                     unlist(c(1983, transmergeddata5CTaboveage10[4, 2:23])),
                                     transmergeddata5CTaboveage10[4:7,],
                                     unlist(c(1988, transmergeddata5CTaboveage10[8, 2:23])),
                                     transmergeddata5CTaboveage10[8:31,])

# Load radiation dose data (Kim's doses)
# This includes 6 matrices, one for each combination of sex (male/female),
# calendar year of CT (pre 2001/ post 2001) and body part scanned (head/chest/abdomen). 
# Each matrix has 23 rows, each of which corresponds to a different age at exposure 0, 1, .., 22 years.   
# Each matrix has 2 columns- the first one contains absorbed doses (in mGy) to the brain whereas the second one contains absorbed doses to the red bone marrow.

scan_ages = c(0:22)

abdomen_scan_doses_females_post2001 = as.data.frame(read_excel("X:/Department Medizin/Biometrie/Projekte/CT/subprojects/simulation_study_bias/datasets_for_simulations/abdomen_scan_doses_females_post2001.xlsx", col_names = F))
abdomen_scan_doses_females_pre2001 = as.data.frame(read_excel("X:/Department Medizin/Biometrie/Projekte/CT/subprojects/simulation_study_bias/datasets_for_simulations/abdomen_scan_doses_females_pre2001.xlsx", col_names = F))
abdomen_scan_doses_males_post2001 = as.data.frame(read_excel("X:/Department Medizin/Biometrie/Projekte/CT/subprojects/simulation_study_bias/datasets_for_simulations/abdomen_scan_doses_males_post2001.xlsx", col_names = F))
abdomen_scan_doses_males_pre2001 = as.data.frame(read_excel("X:/Department Medizin/Biometrie/Projekte/CT/subprojects/simulation_study_bias/datasets_for_simulations/abdomen_scan_doses_males_pre2001.xlsx", col_names = F))

head_scan_doses_females_post2001 = as.data.frame(read_excel("X:/Department Medizin/Biometrie/Projekte/CT/subprojects/simulation_study_bias/datasets_for_simulations/head_scan_doses_females_post2001.xlsx", col_names = F))
head_scan_doses_females_pre2001 = as.data.frame(read_excel("X:/Department Medizin/Biometrie/Projekte/CT/subprojects/simulation_study_bias/datasets_for_simulations/head_scan_doses_females_pre2001.xlsx", col_names = F))
head_scan_doses_males_post2001 = as.data.frame(read_excel("X:/Department Medizin/Biometrie/Projekte/CT/subprojects/simulation_study_bias/datasets_for_simulations/head_scan_doses_males_post2001.xlsx", col_names = F))
head_scan_doses_males_pre2001 = as.data.frame(read_excel("X:/Department Medizin/Biometrie/Projekte/CT/subprojects/simulation_study_bias/datasets_for_simulations/head_scan_doses_males_pre2001.xlsx", col_names = F))

chest_scan_doses_females_post2001 = as.data.frame(read_excel("X:/Department Medizin/Biometrie/Projekte/CT/subprojects/simulation_study_bias/datasets_for_simulations/chest_scan_doses_females_post2001.xlsx", col_names = F))
chest_scan_doses_females_pre2001 = as.data.frame(read_excel("X:/Department Medizin/Biometrie/Projekte/CT/subprojects/simulation_study_bias/datasets_for_simulations/chest_scan_doses_females_pre2001.xlsx", col_names = F))
chest_scan_doses_males_post2001 = as.data.frame(read_excel("X:/Department Medizin/Biometrie/Projekte/CT/subprojects/simulation_study_bias/datasets_for_simulations/chest_scan_doses_males_post2001.xlsx", col_names = F))
chest_scan_doses_males_pre2001 = as.data.frame(read_excel("X:/Department Medizin/Biometrie/Projekte/CT/subprojects/simulation_study_bias/datasets_for_simulations/chest_scan_doses_males_pre2001.xlsx", col_names = F))

# Data simulation part:
source("X:/Department Medizin/Biometrie/Projekte/CT/subprojects/simulation_study_bias/functions.R")
for (p in 1:n_betas){   # loop over different true parameter values
  check = 1 # counter for simul runs
  tic("entire simulation")
  while (check <= no_iterat){
    set.seed(check)
    tic("one simulation")
    tic("data_generation_single_dataset")
    # (2) Simulate subjects' characteristics at study entry 
    sex = matrix(0, N, 1) #gender (1-male, 0-female)
    n_males = round(N*0.55)
    sex[1:n_males,1] = 1
    # Simulate year at first exposure (YearInit). Entry is always January 1
    year_freq = c(355, 312, 262, 268, 394, 775, 1072, 1191, 1185, 1330, 1439, 1319, 1425, 1534,
                  1589, 1938, 2225, 2406, 2859, 2945, 3478, 4277, 4449, 4919, 5097, 5252, 6927,
                  8208, 8962, 8715, 8956, 9086, 8890, 8563)
    year_prob = year_freq / sum(year_freq)
    YearInit = sample(1979:2012, size = N, replace = T, prob = year_prob)
    # Simulate age at first exposure (AgeInit; uniform on 0-18 years): 
    AgeAtEntry = runif(N, 0, 18)
    # Simulate CTs at study entry (first how many CTs and then what combination of CTs)
    # Save number of CTs of different type at study entry and in each subsequent year of follow-up:
    CT_type = matrix(0, N, 3*(FollowUpYrs + 1)) # col1- stores no of head CTs, col2- stores no of chest CTs, col3- stores no of abd CTs
    # Save radiation dose to the target organ and age at exposure at study entry and in each subsequent year of follow-up (we assume that all CTs in a given year occur at the same time point):
    DoseCT = matrix(0, N, FollowUpYrs + 1) #sum of doses from all three scan types in each year of follow-up including scans done at study entry
    cumDoseCT = matrix(-1, N, FollowUpYrs + 1) #cumulative sum of doses from all three scan types in each year of follow-up including scans done at study entry
    AgeCT = matrix(0, N, FollowUpYrs + 1)
    # (3) Simulate subjects' histories until death, cancer occurrence or end of study:
    lag_exp = ifelse(organ == 1, 5, 2) # lag period for exposure of 2 years for leukaemia and 5 for brain tumor
    beta = betas[p] #radiation dose effect
    cancer_cause = matrix(0, N, FollowUpYrs) # 1 - cancer other than leukemia/brain tumor, 2 - leukemia/brain tumor
    age_at_death = matrix(0, N, FollowUpYrs) # store age at death
    status = 9*matrix(1, N, FollowUpYrs+1) #status at each year of follow-up including entry (0-censored due to death, leukemia/brain tumor, other cancer or end of study=31 Dec 2012, whichever comes first), 1-alive); 9 means this cell has not been evaluated
    status[,1] = rep(1, N, 1) # everyone is alive at study entry
    end_status = matrix(0, N, 1) # 0- dead, 1- cancer, 2- alive at the end of study 
    outcome = matrix(0, N, 1) #1-leukemia/brain tumor (event), 0-censored
    age_at_event = matrix(0, N, 1) #store ages at leukemia/brain tumor (event) or death/other cancer diagnosis/end of study (censoring)
    for (id in 1:N){
      first_CT_yr = which(CT_years == YearInit[id])
      n_CTs = min(5, sample(c(1:12, 16, 21, 13, 19, 14, 15, 17, 25, 18, 22, 20), size = 1, replace = T,
                            prob = (probabilitydatabelowage10[first_CT_yr,4:dim(probabilitydatabelowage10)[2]]))*(AgeAtEntry[id] < 10) +
                    sample(c(1:12, 16, 21, 13, 19, 14, 15, 17, 25, 18, 22, 20), size = 1, replace = T,
                           prob = (probabilitydataaboveage10[first_CT_yr,4:dim(probabilitydataaboveage10)[2]]))*(AgeAtEntry[id] >= 10)) #max no of CTs per year is 5
      n_CT_type = exposure_probability_Dec2015(n_CTs, CT_years,YearInit[id], AgeAtEntry[id], transmergeddata1CTaboveage10,
                                               transmergeddata1CTbelowage10, transmergeddata2CTaboveage10,
                                               transmergeddata2CTbelowage10, transmergeddata3CTaboveage10,
                                               transmergeddata3CTbelowage10, transmergeddata4CTaboveage10,
                                               transmergeddata4CTbelowage10, transmergeddata5CTaboveage10, transmergeddata5CTbelowage10)
      n_headCTs = n_CT_type[1]
      n_abdCTs = n_CT_type[2]
      n_chestCTs = n_CT_type[3]
      
      CT_type[id, 1:3] = c(n_headCTs, n_chestCTs, n_abdCTs)
      result = dose_calculation_Dec2015(organ, scan_ages, YearInit[id], AgeAtEntry[id], sex[id],
                                        n_headCTs, n_chestCTs, n_abdCTs, head_scan_doses_males_pre2001, chest_scan_doses_males_pre2001,
                                        abdomen_scan_doses_males_pre2001, head_scan_doses_males_post2001, chest_scan_doses_males_post2001,
                                        abdomen_scan_doses_males_post2001, head_scan_doses_females_pre2001, chest_scan_doses_females_pre2001,
                                        abdomen_scan_doses_females_pre2001, head_scan_doses_females_post2001, chest_scan_doses_females_post2001,
                                        abdomen_scan_doses_females_post2001)
      dose = result$DoseCT
      Age_CT = result$AgeCT
      DoseCT[id,1] = dose
      cumDoseCT[id,1] = dose
      temp_cum = dose
      AgeCT[id,1] = Age_CT
      for (yr in 2:(2012 - YearInit[id] + 2)){ # each person has his/her event follow-up period #yr starts from 2 and goes max to 35 (=34 fu years)
        if (status[id, yr - 1] == 1){ # if alive at the beginning of the current year
          surv_prob = min(1, survival_prob(AgeAtEntry[id] + yr - 1, sex[id]))
          alive = rbinom(1, 1, surv_prob) # 0 if dead, 1 if alive
          mid_yr_age = mean(c(AgeAtEntry[id] + yr - 2, AgeAtEntry[id] + yr - 1))
          if (alive == 0){ # if dead
            status[id,yr] = 0 
            age_at_death[id, yr-1] = mid_yr_age # age at death is the mid age in a given year
            age_at_event[id] = mid_yr_age
            end_status[id] = 0
            cumDoseCT[id,yr] = temp_cum
          }
          else { #if alive
            if (mid_yr_age >= 18){# there is an age limit on exposure follow-up (max age at exposure in Dutch CT study is 18 years)
              DoseCT[id,yr] = 0
              AgeCT[id,yr] = 0
              cumDoseCT[id,yr] = temp_cum
            }
            else{ #whether or not person has a CT in the current year:
              CT_exposure = rbinom(1, 1, probabilitydatabelowage10[which(CT_years == YearInit[id] + yr - 2), 3]) * (mid_yr_age < 10) + 
                rbinom(1, 1, probabilitydataaboveage10[which(CT_years == YearInit[id] + yr - 2), 3]) * (mid_yr_age>=10)
              # exposure indicator (1- exposed, 0- unexposed)
              if (CT_exposure == 1){ # if exposed in a given year
                # max no of CTs per year is 5 # in the first year of fu we are in the same year of YearInit 
                n_CTs = min(5, sample(c(1:12, 16, 21, 13, 19, 14, 15, 17, 25, 18, 22, 20), size = 1, replace = T,
                                      prob = probabilitydatabelowage10[which(CT_years == YearInit[id] + yr - 2), 4:dim(probabilitydatabelowage10)[2]])*(mid_yr_age < 10) +
                              sample(c(1:12, 16, 21, 13, 19, 14, 15, 17, 25, 18, 22, 20), size = 1, replace = T,
                                     prob = probabilitydataaboveage10[which(CT_years == YearInit[id] + yr - 2), 4:dim(probabilitydataaboveage10)[2]])*(mid_yr_age >= 10))
                
                n_CT_type = exposure_probability_Dec2015(n_CTs, CT_years, YearInit[id] + yr - 2, mid_yr_age, transmergeddata1CTaboveage10,
                                                         transmergeddata1CTbelowage10, transmergeddata2CTaboveage10,
                                                         transmergeddata2CTbelowage10, transmergeddata3CTaboveage10,
                                                         transmergeddata3CTbelowage10, transmergeddata4CTaboveage10,
                                                         transmergeddata4CTbelowage10, transmergeddata5CTaboveage10,
                                                         transmergeddata5CTbelowage10)
                n_headCTs = n_CT_type[1]
                n_abdCTs = n_CT_type[2]
                n_chestCTs = n_CT_type[3]
                
                result = dose_calculation_Dec2015(organ,scan_ages,YearInit[id] + yr - 2, mid_yr_age, sex[id],
                                                  n_headCTs, n_chestCTs, n_abdCTs, head_scan_doses_males_pre2001, 
                                                  chest_scan_doses_males_pre2001, abdomen_scan_doses_males_pre2001, 
                                                  head_scan_doses_males_post2001, chest_scan_doses_males_post2001, 
                                                  abdomen_scan_doses_males_post2001, head_scan_doses_females_pre2001,
                                                  chest_scan_doses_females_pre2001, abdomen_scan_doses_females_pre2001,
                                                  head_scan_doses_females_post2001, chest_scan_doses_females_post2001,
                                                  abdomen_scan_doses_females_post2001)
                dose = result$DoseCT
                Age_CT = result$AgeCT
                DoseCT[id, yr] = dose
                temp_cum = temp_cum + dose
                cumDoseCT[id,yr] = temp_cum
                AgeCT[id, yr] = Age_CT - 0.1
                CT_type[id, 3 * (yr-1) + c(1:3)] = c(n_headCTs, n_chestCTs, n_abdCTs)
              }
              else if (CT_exposure == 0){ # if unexposed
                DoseCT[id,yr] = 0
                cumDoseCT[id,yr] = temp_cum
                AgeCT[id,yr] = 0
                CT_type[id, 3 * (yr-1) + c(1:3)] = c(0, 0, 0)
              }
            }
            age_cat = max(which(cancer_inc_ages <= mid_yr_age))
            year_cat = max(which(cancer_inc_years <= YearInit[id] + yr - 2))
            if (sex[id] == 1){ # if male
              other_ann_canc_risk = (organ == 2) * cancer_inc_male_all_cancer_except_leukemia[year_cat, age_cat] + 
                (organ == 1) * cancer_inc_male_all_cancer_except_cns[year_cat, age_cat]
              ann_CT_related_risk = ((organ == 2) * cancer_inc_male_leuk[year_cat, age_cat] + (organ == 1) * cancer_inc_male_cns[year_cat, age_cat]) * 
                # (1 + beta * ifelse(yr - lag_exp > 0, sum(DoseCT[id, 1:(yr - lag_exp)]), 0))
                (1 + beta * sum(DoseCT[id, AgeCT[id,] <= mid_yr_age - lag_exp])) #ERR with lagging, column 2 is 1 fu year and so on
              other_cancer_cause = rbinom(1, 1, other_ann_canc_risk) 
              CT_related_cancer_cause = rbinom(1, 1, ann_CT_related_risk)
              if (CT_related_cancer_cause == 1){
                status[id,yr] = 0 
                outcome[id] = 1 
                age_at_event[id] = mid_yr_age
                end_status[id] = 1
              }
              if (other_cancer_cause == 1 & outcome[id]!=1){
                status[id,yr] = 0 
                outcome[id] = 0
                age_at_event[id] = mid_yr_age
                end_status[id] = 1
              }
              if (other_cancer_cause == 0 & CT_related_cancer_cause == 0){
                status[id,yr] = 1
              }
            }
            else{ #if female
              other_ann_canc_risk = (organ == 2) * cancer_inc_female_all_cancer_except_leukemia[year_cat, age_cat] + 
                (organ == 1) * cancer_inc_female_all_cancer_except_cns[year_cat, age_cat]
              ann_CT_related_risk = ((organ == 2) * cancer_inc_female_leuk[year_cat, age_cat] + (organ == 1) * cancer_inc_female_cns[year_cat, age_cat]) * 
                # (1 + beta * ifelse(yr - lag_exp > 0, sum(DoseCT[id, 1:(yr - lag_exp)]), 0))
                (1 + beta * sum(DoseCT[id, AgeCT[id,] <= mid_yr_age - lag_exp])) #ERR with lagging, column 2 is 1 fu year and so on
              other_cancer_cause = rbinom(1, 1, other_ann_canc_risk)
              CT_related_cancer_cause = rbinom(1, 1, ann_CT_related_risk)
              if (CT_related_cancer_cause == 1){
                status[id, yr] = 0
                outcome[id] = 1 
                age_at_event[id] = mid_yr_age
                end_status[id] = 1
              }
              if (other_cancer_cause == 1 & outcome[id] != 1){
                status[id, yr] = 0
                outcome[id] = 0 
                age_at_event[id] = mid_yr_age
              }
              if (other_cancer_cause == 0 & CT_related_cancer_cause == 0){
                status[id, yr] = 1
              }
            }
          }
        }
        else
          cumDoseCT[id,yr] = temp_cum
      }
    }
    # Those who are alive at the end of their follow-up are assigned age at
    # study end and event indicator saying they have not experienced the event of interest during follow-up: 
    for (k in 1:N){
      if (status[k, (2012 - YearInit[k] + 2)] == 1){
        age_at_event[k] = AgeAtEntry[k] + 2013 - YearInit[k]
        end_status[k] = 2
      }
    }
    
    #compute lagged cumulative dose
    cumDoseCT_lagged = cbind(matrix(0, dim(cumDoseCT)[1], lag_exp), cumDoseCT[, 1 : (dim(cumDoseCT)[2] - lag_exp)])
    
    #create complete age matrix
    agect_complete = AgeCT
    for (age_index in 2:dim(agect_complete)[2]){
      agect_complete[, age_index] = agect_complete[,1] + age_index - 1.5 - 0.1
    }
    
    final_data = as.data.frame(cbind(c(1:N), sex, YearInit, AgeAtEntry, age_at_event, outcome, agect_complete, DoseCT, cumDoseCT_lagged)) # collect data on patient's histories across the strata
    agect_names = rep("a", 35)
    agectcomplete_names = rep("a", 35)
    dosect_names = rep("a", 35)
    cumdosect_names = rep("a", 35)
    cumdosectlagged_names = rep("a", 35)
    for(ag in 0:34){agect_names[ag+1] = paste("AgeCT", ag, sep = "")
    agectcomplete_names[ag+1] = paste("AgeCTcomplete", ag, sep = "")
    dosect_names[ag+1] = paste("DoseCT", ag, sep = "")
    cumdosect_names[ag+1] = paste("cumdoseCT", ag, sep = "")
    cumdosectlagged_names[ag+1] = paste("cumdoseCT", ag, sep = "")}
    colnames(final_data) = c("ID", "sex", "YearInit", "AgeAtEntry", "AgeAtEvent",
                             "Outcome", agectcomplete_names, dosect_names, cumdosectlagged_names)
    
    #create birth year column
    final_data$birthyear = floor(final_data$YearInit - final_data$AgeAtEntry)
    
    #risk set generation
    cases = final_data[final_data$Outcome == 1,]
    
    c = 1
    Nset = 80
    
    case = cases[c,]
    temp_age = cases$AgeAtEvent[c]
    temp_sex = cases$sex[c]
    temp_id = cases$ID[c]
    temp_birth = cases$birthyear[c]
    
    possible_matches = final_data[temp_age >= final_data$AgeAtEntry & temp_age <= final_data$AgeAtEvent &
                                    final_data$ID != temp_id & final_data$sex == temp_sex & 
                                    temp_birth == final_data$birthyear, ]
    set.seed(c)
    if (dim(possible_matches)[1] <= Nset){
      final_data_rsets = rbind(case, possible_matches)
      final_data_rsets$case = 0 #cannot use Outcome since a later case can be a control until he becomes case
      final_data_rsets$case[1] = 1
      rset = rep(1, dim(possible_matches)[1] + 1)
    } else{
      final_data_rsets = rbind(case, possible_matches[sample(1:nrow(possible_matches), Nset), ])
      final_data_rsets$case = 0
      final_data_rsets$case[1] = 1
      rset = rep(1, Nset + 1)
    }
    
    colnames(final_data_rsets) = c(colnames(final_data), "case")
    
    if (dim(cases)[1] > 1){
      for (c in 2:(dim(cases)[1])){
        case = cases[c,]
        temp_age = cases$AgeAtEvent[c]
        temp_sex = cases$sex[c]
        temp_id = cases$ID[c]
        temp_birth = cases$birthyear[c]
        # A future case can be a control until they don't become case
        possible_matches = final_data[temp_age >= final_data$AgeAtEntry & temp_age <= final_data$AgeAtEvent &
                                        final_data$ID != temp_id & final_data$sex == temp_sex & 
                                        temp_birth == final_data$birthyear, ]
        set.seed(c)
        if (dim(possible_matches)[1] <= Nset){
          temp_dataset = rbind(case, possible_matches)
          temp_dataset$case = 0
          temp_dataset$case[1] = 1
          final_data_rsets = rbind(final_data_rsets, temp_dataset)
          rset = c(rset, rep(c, dim(possible_matches)[1] + 1))
        } else{
          temp_dataset = rbind(case, possible_matches[sample(1:nrow(possible_matches), Nset), ])
          temp_dataset$case = 0
          temp_dataset$case[1] = 1
          final_data_rsets = rbind(final_data_rsets, temp_dataset )
          rset = c(rset, rep(c, Nset + 1))
        }
      }
    }
    final_data_rsets$rset = rset
    event_age = final_data_rsets$AgeAtEvent[final_data_rsets$rset == 1 & final_data_rsets$case == 1]
    iter = 1 #iterates over rsets
    for (k in 1:dim(final_data_rsets)[1]){
      if(final_data_rsets$rset[k] == iter){
        final_data_rsets$cumdose[k] = final_data_rsets[k, ifelse(final_data_rsets$AgeAtEntry[k] + lag_exp < event_age,
                                                                 max(which(final_data_rsets[k, 7:(7 + 34)] < event_age & final_data_rsets[k, 77:(77 + 34)] >= 0)) + 35*2 + 6,
                                                                 1 + 35*2 + 6)] # 6 columns before ageCT0, if FU < 2 years cumdose is 0
      }
      else{
        iter = iter + 1
        event_age = final_data_rsets$AgeAtEvent[final_data_rsets$rset == iter & final_data_rsets$case == 1]
        final_data_rsets$cumdose[k] = final_data_rsets[k, ifelse(final_data_rsets$AgeAtEntry[k] + lag_exp < event_age,
                                                                 max(which(final_data_rsets[k, 7:(7 + 34)] < event_age & final_data_rsets[k, 77:(77 + 34)] >= 0)) + 35*2 + 6,
                                                                 1 + 35*2 + 6)]
      }
    }
    final_data_rsets = subset(final_data_rsets, select = c(rset, cumdose, case, sex))
    final_data_rsets$loc = 1 #to use meandose method in linERR package
    Dmax = max(final_data_rsets$cumdose)
    time_data = toc()
    # (4) Partial summary of simulated cohort
    # summary number of CT related cancer cases and person-years based on entire simulated cohort:
    no_events_res[check, p] = length(which(final_data[,"Outcome"] == 1)) # number of events
    average_person_years_res[check, p] = mean(final_data[, "AgeAtEvent"] - final_data[, "AgeAtEntry"]) #average number of person years (age_at_event - AgeAtEntry)
    end_status_matrix[check, c(1, 2, 3)*p] = table(end_status)
    n_headCTs_matrix[check, p] = sum(CT_type[,seq(1,105,3)])
    n_chestCTs_matrix[check, p] = sum(CT_type[,seq(2,105,3)])
    n_abdCTs_matrix[check, p] = sum(CT_type[,seq(3,105,3)])
    
    # analysis
    temp_linerr = linearERR(final_data_rsets, set = 1, doses = c(2,2), status = 3, loc = 5, repar = F, ccmethod = "meandose")
    temp_linerr_f = linearERRfirth(final_data_rsets, set = 1, doses = c(2,2), status = 3, loc = 5, repar = F,
                                   ccmethod = "meandose", initpars = temp_linerr$MLE$coef,
                                   lowerlim = -1, upperlim = temp_linerr$MLE$coef + 0.05)
    threshold[check, p] = -1/Dmax
    estimate_at_threshold = as.numeric(temp_linerr$MLE$coef) - (-1/Dmax + 0.0001) < 1e-6
    if(estimate_at_threshold){ #if the estimate is the threshold
      thresh_flag[check, p] = 1
      nothresh = linearERRfit_nothresh(final_data_rsets, set = 1, doses = c(2,2), status = 3, loc = 5, repar = F, ccmethod = "meandose")
      beta_est_no_thresh[check, p] = nothresh$fit$par
    }
    err = abs(temp_linerr_f$par - (temp_linerr$MLE$coef + 0.05))
    kk = 1
    estimate_f_at_upper = err < 1e-6 #0-1, indicates if estimate is at upper bound or not
    if (estimate_f_at_upper & !estimate_at_threshold){
      while (err < 1e-6){
        temp_linerr_f = linearERRfirth(final_data_rsets, set = 1, doses = c(2,2), status = 3, loc = 5, repar = F,
                                       ccmethod = "meandose", initpars = temp_linerr$MLE$coef,
                                       lowerlim = -0.1, upperlim = temp_linerr$MLE$coef + 0.05 - 0.005 * kk)
        err = abs(temp_linerr_f$par - (temp_linerr$MLE$coef + 0.05 - 0.005 * kk))
        kk = kk + 1
      }
      beta_est[check, p] = temp_linerr$MLE$coef
      beta_est_f[check, p] = temp_linerr_f$par
    } else{
      beta_est[check, p] = temp_linerr$MLE$coef
      beta_est_f[check, p] = temp_linerr_f$par
    }
    if (estimate_at_threshold){#if estimate is threshold Firth will be below threshold
      beta_est_f[check, p] = temp_linerr$MLE$coef
    }
    beta_sd[check, p] = sqrt(solve(linERRscore(temp_linerr$MLE$coef, final_data_rsets, set = 1, doses = c(2,2), status = 3, loc = 5, repar = F, ccmethod = "meandose")$infomat))
    beta_sd_f[check, p] = sqrt(solve(linERRscore(temp_linerr_f$par, final_data_rsets, set = 1, doses = c(2,2), status = 3, loc = 5, repar = F, ccmethod = "meandose")$infomat))
    convergence_info[check, p] = temp_linerr$MLE$convergence
    convergence_info_corrected[check, p] = temp_linerr_f$convergence
    p_value[check, p] = temp_linerr$MLE$dosepval
    
    #coverage
    temp_linerr_loglik = linearERRfit(final_data_rsets, set = 1, doses = c(2,2), status = 3, loc = 5, repar = F, ccmethod = "meandose" ) #need proflik function
    coverage[check, p] = ifelse(temp_linerr_loglik$proflik(beta) - temp_linerr$MLE$fitobj$fit$value < 1.92, 1, 0)  #if the true value is contained in the 95% CI, the difference of the logliks is <1.92 (dev = -2loglik, dev<3.84 if 1 dof)
    
    #score test linear ERR
    temp_constrained_0 = linearERR_constrained(final_data_rsets, set = 1, doses = c(2,2), status = 3,
                                               loc = 5, repar = F, ccmethod = "meandose",
                                               constrained_value = 0)
    temp_constrained = linearERR_constrained(final_data_rsets, set = 1, doses = c(2,2), status = 3,
                                             loc = 5, repar = F, ccmethod = "meandose",
                                             constrained_value = beta)
    temp_score_0 = temp_constrained_0$MLE$score
    temp_score_beta = temp_constrained$MLE$score
    infomat_0 = linERRscore(temp_constrained_0$MLE$coef, final_data_rsets, set = 1, doses = c(2,2), status = 3, loc = 5, repar = F,
                            ccmethod = "meandose")$infomat
    infomat_beta = linERRscore(temp_constrained$MLE$coef, final_data_rsets, set = 1, doses = c(2,2), status = 3, loc = 5, repar = F,
                               ccmethod = "meandose")$infomat
    score_test_0 = temp_score_0^2 * (1/infomat_0) #t(temp_score_0) %*% solve(infomat_0) %*% (temp_score_0)
    score_test_beta = temp_score_beta^2 * (1/infomat_beta) #t(temp_score_beta) %*% solve(infomat_beta) %*% (temp_score_beta)
    p_value_score[check, p] = ifelse(score_test_0 > 3.84, 1, 0) #1 if significant, 0 otherwise
    coverage_score[check, p] = ifelse(score_test_beta < 3.84, 1, 0)
    
    #score test Firth (infomatrix from linerrscore)
    temp_firth_0 = linearERRfirth(final_data_rsets, set = 1, doses = c(2,2), status = 3, loc = 5, repar = F,
                                  ccmethod = "meandose", lowerlim = -0.0001, upperlim = 0.0001)
    temp_firth_beta = linearERRfirth(final_data_rsets, set = 1, doses = c(2,2), status = 3, loc = 5, repar = F,
                                     ccmethod = "meandose", initpars = beta,
                                     lowerlim = beta - 0.0001, upperlim = beta + 0.0001)
    temp_firth_score_0 = linERRscore(temp_firth_0$par, final_data_rsets, set = 1, doses = c(2,2), status = 3, loc = 5, repar = F,
                                     ccmethod = "meandose")
    temp_firth_score_beta = linERRscore(temp_firth_beta$par, final_data_rsets, set = 1, doses = c(2,2), status = 3, loc = 5, repar = F,
                                        ccmethod = "meandose")
    score_test_firth_0 = t(temp_firth_score_0$U + temp_firth_score_0$A) %*% solve(temp_firth_score_0$infomat) %*% (temp_firth_score_0$U + temp_firth_score_0$A)
    score_test_firth_beta = t(temp_firth_score_beta$U + temp_firth_score_beta$A) %*% solve(temp_firth_score_beta$infomat) %*% (temp_firth_score_beta$U + temp_firth_score_beta$A)
    p_value_f[check, p] = ifelse(score_test_firth_0 > 3.84, 1, 0) #1 if significant, 0 otherwise
    coverage_f[check, p] = ifelse(score_test_firth_beta < 3.84, 1, 0)
    
    time_analysis = toc()
    print(check)
    check = check + 1
  }
}
time_simul = toc()