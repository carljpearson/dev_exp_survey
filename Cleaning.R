library(tidyverse)
library(googlesheets)

df18 <- read_csv("/Users/carlpearson/Documents/data_internal/dev_exp/dev_exp_survey_2018_10_06.csv",col_names = T)

df19 <- read_csv("/Users/carlpearson/Documents/data_internal/dev_exp/dev_exp_survey_early_2019_10_3.csv",col_names = T)

df19 <- df19 %>% select(-Q41_1)

#see mismatch ols
setdiff(df18,df19)

#clean df18
dfc18 <- df18[3:nrow(df18),] #remove excess headers

dfc18 <- dfc18 %>% 
  filter(Status!="Survey Preview") %>% #remove preview data
  select(
    respondent_role = Q1,
    responding_behalf_internal = Q34,
    responding_behalf_external = Q22,
    team_size = Q2,
    dev_exp_lvl = Q5,
    openshift_exp_lvl = Q3,
    kube_exp_lvl = Q4,
    running_apps = Q6,
    sat_getting.started = Q7_1,
    sat_int.local.dev = Q7_2,
    sat_dev.test.against = Q7_3,
    sat_image.build.s2i = Q7_4,
    satad_time.build.run = Q36_1,
    satad_framework.runtime.avail = Q36_2,
    satad_debug = Q36_3,
    satad_feedback.prog.err = Q36_4,
    satad_health.perf.vis = Q36_5,
    satad_proj.env.org = Q36_6,
    satad_cicd.int = Q36_7,
    sathelp_docs = Q38_1,
    sathelp_blog = Q38_2,
    sathelp_tutorial = Q38_3,
    challenge_open = Q8,
    new_app_create = Q9,
    new_app_create_open = Q9_6_TEXT,
    migrate = Q10,
    migrate_open = Q10_9_TEXT,
    language.tools = Q11,
    language.tools_open = Q11_24_TEXT,
    frameworks = Q39,
    frameworks_open = Q39_84_TEXT,
    node.js.spec = Q40
         ) %>%
  mutate(
          #get survey id
         survey_id = 2018,
         #remove NAs for uniting to single var
         responding_behalf_internal = if_else(is.na(responding_behalf_internal),"",responding_behalf_internal),
         responding_behalf_external = if_else(is.na(responding_behalf_external),"",responding_behalf_external)
         ) %>%
  unite(responding_behalf, responding_behalf_internal:responding_behalf_external,remove=T,sep = "") #unite identifier variables

#clean 19
dfc19 <- df19[3:nrow(df19),]
dfc19 <- dfc19 %>% 
  filter(Status!="Survey Preview") %>% #remove preview data
  select(
    respondent_role = Q1,
    responding_behalf_internal = Q34,
    responding_behalf_external = Q22,
    team_size = Q2,
    dev_exp_lvl = Q5,
    openshift_exp_lvl = Q3,
    kube_exp_lvl = Q4,
    running_apps = Q6,
    sat_getting.starteed = Q7_1,
    sat_int.local.dev = Q7_2,
    sat_dev.test.against = Q7_3,
    sat_image.build.s2i = Q7_4,
    satad_time.build.run = Q36_1,
    satad_framework.runtime.avail = Q36_2,
    satad_debug = Q36_3,
    satad_feedback.prog.err = Q36_4,
    satad_health.perf.vis = Q36_5,
    satad_proj.env.org = Q36_6,
    satad_cicd.int = Q36_7,
    sathelp_docs = Q38_1,
    sathelp_blog = Q38_2,
    sathelp_tutorial = Q38_3,
    challenge_open = Q8,
    new_app_create = Q9,
    new_app_create_open = Q9_6_TEXT,
    migrate = Q10,
    migrate_open = Q10_9_TEXT,
    language.tools = Q11,
    language.tools_open = Q11_24_TEXT,
    frameworks = Q39,
    frameworks_open = Q39_84_TEXT,
    frameworks_node.js.spec = Q41,
    frameworks_node.js.open = Q41_1
    
    
  ) %>%
  mutate(
    #get survey id
    survey_id = 2019,
    #remove NAs for uniting to single var
    responding_behalf_internal = if_else(is.na(responding_behalf_internal),"",responding_behalf_internal),
    responding_behalf_external = if_else(is.na(responding_behalf_external),"",responding_behalf_external)
  ) %>%
  unite(responding_behalf, responding_behalf_internal:responding_behalf_external,remove=T,sep = "") #unite identifier variables





df18n<-df18
df18n[1,19:20] <- c("responding_internal","responding_external")
colnames(df18n) <- df18n[1,]
df18n <- df18n[3:nrow(df18n),]
df18n$year <-2018


df19n<-df19
df19n[1,19:20] <- c("responding_internal","responding_external")
colnames(df19n) <- df19n[1,]
df19n <- df19n[3:nrow(df19n),]
df19n$year <-2019

setdiff(colnames(df18n),colnames(df19n))
setdiff(colnames(df19n),colnames(df18n))

df18n <- df18n %>%
  rename(
    "What is your role?"  = "What is your title/role?",
  )



