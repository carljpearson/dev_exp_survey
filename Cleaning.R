library(tidyverse)
library(googlesheets)

df18 <- read_csv("/Users/carlpearson/Documents/data_internal/dev_exp/dev_exp_survey_2018_10_06.csv",col_names = T)

df19 <- read_csv("/Users/carlpearson/Documents/data_internal/dev_exp/dev_exp_survey_early_2019_10_21.csv",col_names = T)

df19 <- df19 %>% select(-Q41_1)


zval=1.96

#see mismatch ols
setdiff(df18,df19)


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

#rename role question
df18n <- df18n %>%
  rename(
    "What is your role?"  = "What is your title/role?",
  )

#combine dataframes
df <- bind_rows(df19n,df18n) %>%
  mutate(id=as.numeric(as.factor(paste0(`Start Date`,`Duration (in seconds)`)))) #make unnique identifier


#remove previews
df <- df %>% 
  filter(`Response Type` !="Survey Preview") %>%
  mutate(
    #remove NAs for uniting to single var
    year=as.factor(year),
    `What is your role?` = as.factor(`What is your role?`),
    responding_internal = if_else(is.na(responding_internal),"",responding_internal),
    responding_external = if_else(is.na(responding_external),"",responding_external)
  ) %>%
  unite(responding_behalf, responding_internal:responding_external,remove=T,sep = "") #unite identifier variables 
  
#relevel customer
df$`What is your role?` <- factor(df$`What is your role?`, levels=c(
                                                              "I am NOT a Red Hat OpenShift Customer",
                                                              "I am a Red Hat OpenShift Customer",
                                                              "I am a Red Hat Consultant",
                                                              "I am a Red Hat Solution Architect",
                                                              "I am a Red Hat Account Manager"
                                                              ))

df <- df %>% mutate(
              company = case_when(
                `What is your role?` == "I am NOT a Red Hat OpenShift Customer" ~ "external",
                `What is your role?` == "I am a Red Hat OpenShift Customer" ~ "external",
                `What is your role?` == "I am a Red Hat Consultant" ~ "internal",
                `What is your role?` == "I am a Red Hat Solution Architect" ~ "internal",
                `What is your role?` == "I am a Red Hat Account Manager" ~ "internal"
)) 

#team size cleaning
df <- df %>% 
  rename(team_size=`What is your development team size?`) %>% #temp rename to work with case_when
  mutate(team_size = case_when( #recode variable factors
                                   team_size == "1 person"  ~ "1 person",
                                   team_size == "1" ~ "1 person",
                                   team_size == "2 - 8 people" ~ "2 - 8 people",
                                   team_size =="2 - 8" ~ "2 - 8 people",
                                   team_size == "9 - 20 people" ~ "9 - 20 people",
                                   team_size =="9 - 20" ~ "9 - 20 people",
                                   team_size == "21 or more people" ~ "21 or more people",
                                   team_size == "20 +" ~ "21 or more people"
                                   ),
         team_size = factor(team_size, levels=c( #properly order factors
           "1 person",
           "2 - 8 people",
           "9 - 20 people",
           "21 or more people"
         ))
         ) %>%

  rename(`What is your development team size?` = team_size)

#dev experience cleaning

df <- df %>% 
  rename(temp=`What is your, or your team's, average overall development experience?`) %>% #temp rename to work with case_when
  mutate(temp = case_when( #recode variable factors
    temp == "6 -10 years"  ~ "6 - 10 years",
    temp == "11 - 15 years" ~ "11 - 15 years",
    temp == "11 -15 years" ~ "11 - 15 years",
    temp =="Less than 1 year" ~ "Less than 1 year",
    temp == "15 + years" ~ "More than 15 years",
    temp =="More than 15 years" ~ "More than 15 years",
    temp =="1 - 5 years" ~ "1 - 5 years"
  ),
  temp = factor(temp, levels=c( #properly order factors
    "Less than 1 year",
    "1 - 5 years",
    "6 - 10 years",
    "11 - 15 years",
    "More than 15 years"
  ))
  ) %>%
  
  rename(`What is your, or your team's, average overall development experience?` = temp) #un-rename

#openshift experience cleaning

df <- df %>% 
  rename(temp=`What is your, or your team's, average overall OpenShift experience?`) %>% #temp rename to work with case_when
  mutate(temp = case_when( #recode variable factors
    temp == "Less than 1 year"  ~ "Less than 1 year",
    temp == "Between 1 - 2 years" ~ "Between 1 - 2 years",
    temp == "1 - 2 years" ~ "Between 1 - 2 years",
    temp == "Between 2 - 3 years" ~ "Between 2 - 3 years",
    temp =="2 + years" ~ "Between 2 - 3 years",
    temp =="More than 3 years" ~ "More than 3 years"
  ),
  temp = factor(temp, levels=c( #properly order factors
    "Less than 1 year",
    "Between 1 - 2 years",
    "Between 2 - 3 years",
    "More than 3 years"
  ))
  ) %>%
  
  rename(`What is your, or your team's, average overall OpenShift experience?` = temp) #un-rename

#kube
df <- df %>% 
  rename(temp=`What is your, or your team's, average Kubernetes experience?`) %>% #temp rename to work with case_when
  mutate(temp = case_when( #recode variable factors
    temp == "Less than 1 year"  ~ "Less than 1 year",
    temp == "Between 1 - 2 years" ~ "Between 1 - 2 years",
    temp == "1 - 2 years" ~ "Between 1 - 2 years",
    temp == "Between 2 - 3 years" ~ "Between 2 - 3 years",
    temp =="2 + years" ~ "Between 2 - 3 years",
    temp =="More than 3 years" ~ "More than 3 years"
  ),
  temp = factor(temp, levels=c( #properly order factors
    "Less than 1 year",
    "Between 1 - 2 years",
    "Between 2 - 3 years",
    "More than 3 years"
  ))
  ) %>%
  
  rename(`What is your, or your team's, average Kubernetes experience?` = temp) #un-rename
  
#split out apps running

df <- df %>%
  #select(id,`What type of application are you running on OpenShift? (Check all that apply)`) %>%
  rename(temp=`What type of application are you running on OpenShift? (Check all that apply)`) %>%
  mutate(
    
    Running.Apps_Stateless.Application =  na_if(  if_else(grepl("Stateless application",temp),"Yes","no" ),"no"),
    Running.Apps_Traditional.Application =  na_if(  if_else(grepl("Traditional application",temp),"Yes","no" ),"no"),
    Running.Apps_Microservices =  na_if(  if_else(grepl("Microservices",temp),"Yes","no" ),"no"),
    Running.Apps_Single.Page.Application =  na_if(  if_else(grepl("Single Page",temp),"Yes","no" ),"no"),
    Running.Apps_Serverless =  na_if(  if_else(grepl("Serverless",temp),"Yes","no" ),"no"),
    Running.Apps_Traditional.Application =  na_if(  if_else(grepl("Traditional application",temp),"Yes","no" ),"no"),
    Running.Apps_Traditional.Application =  na_if(  if_else(grepl("Traditional application",temp),"Yes","no" ),"no"),
    Running.Apps_Traditional.Application =  na_if(  if_else(grepl("Traditional application",temp),"Yes","no" ),"no")
    
  ) %>%
  rename(`What type of application are you running on OpenShift? (Check all that apply)`=temp) 

#version
df <- df %>% 
  mutate(
    `What OpenShift version do you have the most recent experience with?` = factor(`What OpenShift version do you have the most recent experience with?`, levels=c( #properly order factors
    "3.9 or earlier",
    "3.10",
    "3.11",
    "4.1",
    "4.2"
  ))
  )

#umux-lite
df <- df %>% 
  select(id,contains("PLease rate your agreement")) %>%
  rename(
    umux.effe = `Please rate your agreement with the statements below regarding OpenShift version [QID46-ChoiceGroup-SelectedChoices]. - OpenShift's capabilities meet my requirements.`,
    umux.ease = `Please rate your agreement with the statements below regarding OpenShift version [QID46-ChoiceGroup-SelectedChoices]. - OpenShift is easy to use.` 
         ) %>%
  pivot_longer(contains("umux"),names_to="umux") %>%
  mutate(value=case_when(
    value == "Strongly agree" ~ 5,
    value == "Somewhat agree" ~ 4,
    value == "Neither agree nor disagree" ~ 3,
    value == "Strongly disagree" ~ 2,
    value == "Strongly disagree" ~ 1
  )) %>%
  pivot_wider(names_from = umux, values_from = value) %>%
  inner_join(df) %>%
  mutate(umux.lite=(umux.ease+umux.effe)/2)





#satisfaction



#viz and analysis -----

#compare response counts
df %>%
  group_by(year) %>%
  count()

#role/title over year
df %>% 
  select(`What is your role?`,year) %>%
  group_by(`What is your role?`,year,.drop=F) %>%
  count(.drop=F) %>%
  ggplot(aes(y=n,x=`What is your role?`,fill=year)) +
  geom_bar(stat="identity",position = "dodge") +
  scale_fill_brewer(palette = "Set2") +
  #theme for deck
  coord_flip() +
  ggthemes::theme_tufte(base_family = "sans") +
  labs(
    y="Count",
    x="Role",
    title="Roles/Titles of respondents by count"
  )

#role/title over year percentage
df %>% 
  #group to count responses
  group_by(`What is your role?`,year,.drop=F) %>%
  summarize(count=n()) %>%
  #regroup for totals by year
  ungroup() %>% group_by(year) %>%
  #CIs
  mutate(n=sum(count),prop = count / sum(count)) %>%
  mutate(prop = count / n) %>% #exact proportion from succesess/trials
  mutate(laplace = (count + 1) / (n + 2)) %>% #laplace point estimate
  mutate(p_adj = (n * prop + (zval * zval) / 2) / (n + (zval * zval))) %>% #adjust p for wald calculation
  mutate(n_adj = n + (zval * zval)) %>% #adjust n for wald calculation
  mutate(marg =  zval * sqrt(p_adj * (1 - p_adj) / n_adj)) %>% #wald margin value
  mutate(lowerci = p_adj - marg) %>% #lower wald ci
  mutate(lowerci = ifelse(lowerci <= 0, 0, lowerci)) %>% #keep lower ci above 0
  mutate(upperci = p_adj + marg) %>% #upper wald ci
  mutate(upperci = ifelse(upperci >= 1, 1, upperci)) %>% #keep upper ci below 1
  #begin plot of vars
  ggplot(aes(y=prop,x=`What is your role?`,fill=year)) +
  geom_bar(stat="identity",position = "dodge") + #add bars
  geom_errorbar(aes(ymin=lowerci,ymax=upperci),color="gray",position="dodge") + #add adjusted wald CIs
  #geom_text(aes(label=count),position=position_dodge(width=.9)) +
  scale_y_continuous(limits=c(0,1),labels = scales::percent) + #Make scale 0-1 and %
  #theme for deck
  coord_flip() + #flip y
  scale_fill_brewer(palette = "Set2") + #colors
  ggthemes::theme_tufte(base_family = "sans") + #remove excess ink
  #labels
  labs( 
    y="Percentage",
    x="",
    title="Proportions of Roles/Titles across Years"
  ) +
  guides(fill=guide_legend(title="Survey Year")) 

ggsave("/Users/carlpearson/Documents/r_github/dev_exp_survey/Plots/role_proportions.png",device="png",
       width=10,height=8,bg="transparent")

#role over year percentage no CI but count
df %>% 
  #group to count responses
  group_by(`What is your role?`,year,.drop=F) %>%
  summarize(count=n()) %>%
  #regroup for totals by year
  ungroup() %>% group_by(year) %>%
  #CIs
  mutate(n=sum(count),prop = count / sum(count)) %>%
  #begin plot of vars
  ggplot(aes(y=prop,x=`What is your role?`,fill=year)) +
  geom_bar(stat="identity",position = "dodge") + #add bars
 # geom_errorbar(aes(ymin=lowerci,ymax=upperci),color="gray",position="dodge") + #add adjusted wald CIs
  geom_text(aes(label=count,y=.02),position=position_dodge(width=.9)) +
  scale_y_continuous(limits=c(0,1),labels = scales::percent) + #Make scale 0-1 and %
  #theme for deck
  coord_flip() + #flip y
  scale_fill_brewer(palette = "Set2") + #colors
  ggthemes::theme_tufte(base_family = "sans") + #remove excess ink
  #labels
  labs( 
    y="Percentage",
    x="",
    title="Proportions of Roles/Titles across Years",
    subtitle="Numbers indicate raw counts"
  ) +
  guides(fill=guide_legend(title="Survey Year")) 

ggsave("/Users/carlpearson/Documents/r_github/dev_exp_survey/Plots/role_proportions_count.png",device="png",
       width=10,height=8,bg="transparent")


#role over responding on behalf of
df %>% 
  filter(year=="2019") %>% #only 2019
  #group to count responses
  group_by(`What is your role?`,responding_behalf,.drop=F) %>%
  summarize(count=n()) %>%
  #begin plot of vars
  ggplot(aes(y=count,x=`What is your role?`,fill=responding_behalf)) +
  geom_bar(stat="identity",position = "dodge") + #add bars
  # geom_errorbar(aes(ymin=lowerci,ymax=upperci),color="gray",position="dodge") + #add adjusted wald CIs
  geom_text(aes(label=count,y=.2),position=position_dodge(width=.9)) +
  #theme for deck
  coord_flip() + #flip y
  scale_fill_brewer(palette = "Set2") + #colors
  ggthemes::theme_tufte(base_family = "sans") + #remove excess ink
  #labels
  labs( 
    y="Count",
    x="",
    title="Who Respondents Give Answers For",
    subtitle="2019 Only"
  ) +
  guides(fill=guide_legend(title="Responding on Behalf of")) 

ggsave("/Users/carlpearson/Documents/r_github/dev_exp_survey/Plots/role_proportions_behalf.png",device="png",
       width=10,height=8,bg="transparent")


#role over year percentage no CI but count


#Experience levels
#dev experience
df %>% 
  #group to count responses
  group_by(`What is your, or your team's, average overall development experience?`,year,.drop=F) %>%
  summarize(count=n()) %>%
  na.omit() %>%
  #regroup for totals by year
  ungroup() %>% group_by(year) %>%
  #CIs
  mutate(n=sum(count),prop = count / sum(count)) %>%
  #begin plot of vars
  ggplot(aes(y=prop,x=`What is your, or your team's, average overall development experience?`,fill=year)) +
  geom_bar(stat="identity",position = "dodge") + #add bars
  # geom_errorbar(aes(ymin=lowerci,ymax=upperci),color="gray",position="dodge") + #add adjusted wald CIs
  geom_text(aes(label=count,y=.01),position=position_dodge(width=.9)) +
  scale_y_continuous(
                      #limits=c(0,1),
                      labels = scales::percent) + #Make scale 0-1 and %
  #theme for deck
  coord_flip() + #flip y
  scale_fill_brewer(palette = "Set2") + #colors
  ggthemes::theme_tufte(base_family = "sans") + #remove excess ink
  #labels
  labs( 
    y="Percentage",
    x="",
    title="Development Experience across Years",
    subtitle="Numbers indicate raw counts"
  ) +
  guides(fill=guide_legend(title="Survey Year")) 

ggsave("/Users/carlpearson/Documents/r_github/dev_exp_survey/Plots/dev_exp_count.png",device="png",
       width=10,height=8,bg="transparent")

#openshift experience
df %>% 
  #group to count responses
  group_by(`What is your, or your team's, average overall OpenShift experience?`,year,.drop=F) %>%
  summarize(count=n()) %>%
  #regroup for totals by year
  ungroup() %>% group_by(year) %>%
  #CIs
  mutate(n=sum(count),prop = count / sum(count)) %>%
  #begin plot of vars
  ggplot(aes(y=prop,x=`What is your, or your team's, average overall OpenShift experience?`,fill=year)) +
  geom_bar(stat="identity",position = "dodge") + #add bars
  # geom_errorbar(aes(ymin=lowerci,ymax=upperci),color="gray",position="dodge") + #add adjusted wald CIs
  geom_text(aes(label=count,y=.01),position=position_dodge(width=.9)) +
  scale_y_continuous(labels = scales::percent) + #Make scale 0-1 and %
  #theme for deck
  coord_flip() + #flip y
  scale_fill_brewer(palette = "Set2") + #colors
  ggthemes::theme_tufte(base_family = "sans") + #remove excess ink
  labs(  #labels
    y="Percentage",
    x="",
    title="OpenShift Experience Across Years",
    subtitle="Numbers indicate raw counts",
    caption="\"2+ years\" in 2018 survey recoded to \"2-3 years\""
  ) +
  guides(fill=guide_legend(title="Survey Year")) 

ggsave("/Users/carlpearson/Documents/r_github/dev_exp_survey/Plots/ipenshift_exp_count.png",device="png",
       width=10,height=8,bg="transparent")




#openshift experience
df %>% 
  #group to count responses
  group_by(`What is your, or your team's, average Kubernetes experience?`,year,.drop=F) %>%
  summarize(count=n()) %>%
  #regroup for totals by year
  ungroup() %>% group_by(year) %>%
  #CIs
  mutate(n=sum(count),prop = count / sum(count)) %>%
  #begin plot of vars
  ggplot(aes(y=prop,x=`What is your, or your team's, average Kubernetes experience?`,fill=year)) +
  geom_bar(stat="identity",position = "dodge") + #add bars
  # geom_errorbar(aes(ymin=lowerci,ymax=upperci),color="gray",position="dodge") + #add adjusted wald CIs
  geom_text(aes(label=count,y=.01),position=position_dodge(width=.9)) +
  scale_y_continuous( labels = scales::percent) + #Make scale 0-1 and %
  #theme for deck
  coord_flip() + #flip y
  scale_fill_brewer(palette = "Set2") + #colors
  ggthemes::theme_tufte(base_family = "sans") + #remove excess ink
  labs(  #labels
    y="Percentage",
    x="",
    title="Kubernetes Experience Across Years",
    subtitle="Numbers indicate raw counts",
    caption="\"2+ years\" in 2018 survey recoded to \"2-3 years\""
  ) +
  guides(fill=guide_legend(title="Survey Year")) 

ggsave("/Users/carlpearson/Documents/r_github/dev_exp_survey/Plots/kube_exp_count.png",device="png",
       width=10,height=8,bg="transparent")




#running apps

#general

df %>% 
  select(id,year,contains("Running.Apps_")) %>% #get only relevant vars
  pivot_longer(contains("Running.Apps_"),
               names_to="Running_Apps", #change col name
               values_drop_na=T) %>% #drop NAs
  mutate( #clean up value names
    Running_Apps = gsub("Running.Apps_","",Running_Apps),
    Running_Apps = gsub("\\."," ",Running_Apps)
  ) %>%
  #group to count responses
  group_by(Running_Apps,year,.drop=F) %>%
  summarize(count=n()) %>%
  #regroup for totals by year
  ungroup() %>% group_by(year) %>%
  #CIs
  mutate(n=sum(count),prop = count / sum(count)) %>%
  mutate(prop = count / n) %>% #exact proportion from succesess/trials
  mutate(laplace = (count + 1) / (n + 2)) %>% #laplace point estimate
  mutate(p_adj = (n * prop + (zval * zval) / 2) / (n + (zval * zval))) %>% #adjust p for wald calculation
  mutate(n_adj = n + (zval * zval)) %>% #adjust n for wald calculation
  mutate(marg =  zval * sqrt(p_adj * (1 - p_adj) / n_adj)) %>% #wald margin value
  mutate(lowerci = p_adj - marg) %>% #lower wald ci
  mutate(lowerci = ifelse(lowerci <= 0, 0, lowerci)) %>% #keep lower ci above 0
  mutate(upperci = p_adj + marg) %>% #upper wald ci
  mutate(upperci = ifelse(upperci >= 1, 1, upperci)) %>% #keep upper ci below 1
  
  #begin plot of vars
  ggplot(aes(y=prop,x=Running_Apps,fill=year)) +
  geom_bar(stat="identity",position = "dodge") + #add bars
  geom_errorbar(aes(ymin=lowerci,ymax=upperci),color="gray",position="dodge") + #add adjusted wald CIs
  geom_text(aes(label=count,y=.01),position=position_dodge(width=.9)) +
  scale_y_continuous(
    #limits=c(0,1), 
    labels = scales::percent) + #Make scale 0-1 and %
  #theme for deck
  coord_flip() + #flip y
  scale_fill_brewer(palette = "Set2") + #colors
  ggthemes::theme_tufte(base_family = "sans") + #remove excess ink
  #labels
  labs( 
    y="Percentage",
    x="",
    title="Running Apps across years",
    subtitle="Numbers indicate raw counts"
  ) +
  guides(fill=guide_legend(title="Survey Year")) 

ggsave("/Users/carlpearson/Documents/r_github/dev_exp_survey/Plots/running_apps_year.png",device="png",
       width=10,height=8,bg="transparent")

#gteam_size

df %>% 
  select(id,year,contains("Running.Apps_"), `What is your development team size?`) %>% #get only relevant vars
  pivot_longer(contains("Running.Apps_"),
               names_to="Running_Apps", #change col name
               values_drop_na=T) %>% #drop NAs
  mutate( #clean up value names
    Running_Apps = gsub("Running.Apps_","",Running_Apps),
    Running_Apps = gsub("\\."," ",Running_Apps)
  ) %>%
  #group to count responses
  group_by(Running_Apps,year,`What is your development team size?`,.drop=F) %>%
  summarize(count=n()) %>%
  #regroup for totals by year
  ungroup() %>% group_by(year,`What is your development team size?`) %>%
  #CIs
  mutate(n=sum(count),prop = count / sum(count)) %>%
  mutate(prop = count / n) %>% #exact proportion from succesess/trials
  mutate(laplace = (count + 1) / (n + 2)) %>% #laplace point estimate
  mutate(p_adj = (n * prop + (zval * zval) / 2) / (n + (zval * zval))) %>% #adjust p for wald calculation
  mutate(n_adj = n + (zval * zval)) %>% #adjust n for wald calculation
  mutate(marg =  zval * sqrt(p_adj * (1 - p_adj) / n_adj)) %>% #wald margin value
  mutate(lowerci = p_adj - marg) %>% #lower wald ci
  mutate(lowerci = ifelse(lowerci <= 0, 0, lowerci)) %>% #keep lower ci above 0
  mutate(upperci = p_adj + marg) %>% #upper wald ci
  mutate(upperci = ifelse(upperci >= 1, 1, upperci)) %>% #keep upper ci below 1
  
  #begin plot of vars
  ggplot(aes(y=prop,x=Running_Apps,fill=year)) +
  geom_bar(stat="identity",position = "dodge") + #add bars
  geom_errorbar(aes(ymin=lowerci,ymax=upperci),color="lightgray",position="dodge") + #add adjusted wald CIs
  geom_text(aes(label=count,y=.03),hjust=1.1,position=position_dodge(width=.9)) +
  scale_y_continuous(
    #limits=c(0,1), 
    labels = scales::percent) + #Make scale 0-1 and %
  facet_wrap(~`What is your development team size?`) +
  #theme for deck
 coord_flip() + #flip y
  scale_fill_brewer(palette = "Set2") + #colors
  ggthemes::theme_tufte(base_family = "sans") + #remove excess ink
  #labels
  labs( 
    y="Percentage",
    x="",
    title="Running Apps across Year and Team Size",
    subtitle="Numbers indicate raw counts"
  ) +
  guides(fill=guide_legend(title="Survey Year")) 

ggsave("/Users/carlpearson/Documents/r_github/dev_exp_survey/Plots/running_apps_year_size.png",device="png",
       width=10,height=8,bg="transparent")


#version
df %>% 
  filter(year=="2019") %>%
  #group to count responses
  group_by(`What OpenShift version do you have the most recent experience with?`) %>%
  summarize(count=n()) %>%
  na.omit() %>%
  mutate(n=sum(count),prop = count / sum(count)) %>%
  #begin plot of vars
  ggplot(aes(y=prop,x=`What OpenShift version do you have the most recent experience with?`)) +
  geom_bar(stat="identity",position = "dodge") + #add bars
  # geom_errorbar(aes(ymin=lowerci,ymax=upperci),color="gray",position="dodge") + #add adjusted wald CIs
  geom_text(aes(label=count,y=.01),position=position_dodge(width=.9),color="white") +
  scale_y_continuous( labels = scales::percent) + #Make scale 0-1 and %
  #theme for deck
  coord_flip() + #flip y
  scale_fill_brewer(palette = "Set2") + #colors
  ggthemes::theme_tufte(base_family = "sans") + #remove excess ink
  labs(  #labels
    y="Percentage",
    x="",
    title="Version Recently Used The Most",
    subtitle="Numbers indicate raw counts",
    caption="2019 only"
  ) 

ggsave("/Users/carlpearson/Documents/r_github/dev_exp_survey/Plots/version_prop.png",device="png",
       width=10,height=8,bg="transparent")


#umux

df %>% 
  filter(year=="2019") %>%
  #group to count responses
  ggplot(aes(umux.lite)) +
  geom_histogram(bins=10) +
  coord_cartesian(xlim=c(1,5)) +
  geom_vline(xintercept = 4,color="mediumspringgreen") +
  #theme for deck
  ggthemes::theme_tufte(base_family = "sans") + #remove excess ink
  labs(  #labels
    y="Count",
    x="",
    title="UMUX-lite score",
    subtitle = "Green line indicates \'good\' score benchmark",
    caption = "2019 scores only"
  ) 

aggsave("/Users/carlpearson/Documents/r_github/dev_exp_survey/Plots/umux.png",device="png",
       width=10,height=8,bg="transparent")

df %>% 
  filter(year=="2019") %>%
  #group to count responses
  group_by(company) %>%
  summarise(umux.lite.avg = mean(umux.lite),sd=sd(umux.lite,na.rm = T),n=n(),se=sd/sqrt(n),marg=se*zval) %>%
  ggplot(aes(x=company,y=umux.lite.avg)) +
  geom_bar(stat="identity") +
  coord_cartesian(xlim=c(1,5)) +
  geom_hline(yintercept = 4,color="mediumspringgreen") +
  #theme for deck
  ggthemes::theme_tufte(base_family = "sans") + #remove excess ink
  labs(  #labels
    y="Count",
    x="",
    title="UMUX-lite score",
    subtitle = "Green line indicates \'good\' score benchmark",
    caption = "2019 scores only"
  ) 

aggsave("/Users/carlpearson/Documents/r_github/dev_exp_survey/Plots/umux.png",device="png",
        width=10,height=8,bg="transparent")

#umux across version

df %>% 
  filter(year=="2019",
         !is.na(umux.lite)) %>%
  group_by(`What OpenShift version do you have the most recent experience with?`) %>%
  summarise(umux.lite.avg = mean(umux.lite),sd=sd(umux.lite,na.rm = T),n=n(),se=sd/sqrt(n),marg=se*zval) %>%
  filter(n > 5 ) %>%
  #group to count responses
  ggplot(aes(y=umux.lite.avg,x=`What OpenShift version do you have the most recent experience with?`,fill=`What OpenShift version do you have the most recent experience with?`)) +
  geom_bar(stat="identity",position = "dodge") + #add bars
  geom_errorbar(aes(ymin=umux.lite.avg-marg,ymax=umux.lite.avg+marg),color="gray",position="dodge") + #add adjusted wald CIs
  geom_hline(aes(yintercept=4),color="mediumspringgreen") +
  #theme for deck
  coord_flip(ylim = c(1,5)) + #flip y
  ggthemes::theme_tufte(base_family = "sans") + #remove excess ink
 # scale_fill_brewer(palette = "RdPu") + #colors
  labs(  #labels
    y="UMUX-lite",
    x="",
    title="UMUX-lite score",
    subtitle = "Green line indicates \'good\' score benchmark",
    caption = "2019 scores only, n > 5 for inclusion"
  ) +
  guides(fill=FALSE, color=FALSE)


#satisfaction
library(likert)

#dev and testing


df %>%
 filter(year=="2019") %>% #2019 only
  select(id,contains("developing and testing")) %>% #choose vars
  pivot_longer(contains("developing and testing")) %>% #elongate
  mutate( #clean up variable names
    name=gsub("Please rate your satisfaction with the developing and testing your code on OpenShift\\. - ","",name),
  #  value=replace_na(value,"N/A"),
    #value=factor(value,c("Very Satisfied","Satisfied","Unsatisfied","Very Unsatisfied","N/A"))
   value = na_if(value,"N/A"),
   value=factor(value,c("Very Satisfied","Satisfied","Unsatisfied","Very Unsatisfied"))
  ) %>%
  pivot_wider(names_from = name,values_from = value) %>%
  select(-id) %>%
  as.data.frame() %>%
  sjPlot::plot_likert(
    cat.neutral = "N/A"
    
    ) +
  ggthemes::theme_tufte(base_family = "sans") +
  labs(  #labels
    y="Response Percentage",
    x="",
    title="Satisfacation of Developing and Testing - 2019",
    # subtitle = "",
    caption = "NAs removed"
  )


ggsave("/Users/carlpearson/Documents/r_github/dev_exp_survey/Plots/sat-developandtest19.png",device="png",
       width=12,height=8,bg="transparent")

df %>%
  filter(year=="2018") %>% #2019 only
  select(id,contains("developing and testing")) %>% #choose vars
  pivot_longer(contains("developing and testing")) %>% #elongate
  mutate( #clean up variable names
    name=gsub("Please rate your satisfaction with the developing and testing your code on OpenShift\\. - ","",name),
   # value=replace_na(value,"N/A"),
   # value=factor(value,c("Very Satisfied","Satisfied","Unsatisfied","Very Unsatisfied","N/A"))
    value = na_if(value,"N/A"),
    value=factor(value,c("Very Satisfied","Satisfied","Unsatisfied","Very Unsatisfied"))
  ) %>%
  pivot_wider(names_from = name,values_from = value) %>%
  select(-id) %>%
  as.data.frame() %>%
  sjPlot::plot_likert(
    cat.neutral = "N/A"
    
  ) +
  ggthemes::theme_tufte(base_family = "sans") +
  labs(  #labels
    y="Response Percentage",
    x="",
    title="Satisfacation of Developing and Testing - 2019",
    # subtitle = "",
    caption = "NAs removed"
  )


ggsave("/Users/carlpearson/Documents/r_github/dev_exp_survey/Plots/sat-developandtest18.png",device="png",
       width=12,height=8,bg="transparent")

#utilizing services

#with na
df %>%
  filter(year=="2019") %>% #2019 only
  select(id,contains("utilizing services")) %>% #choose vars
  pivot_longer(contains("utilizing services")) %>% #elongate
  mutate( #clean up variable names
    name=gsub("Please rate your satisfaction with utilizing services (databases, message brokers, etc) on OpenShift\\. - ","",name),
      value=replace_na(value,"N/A"),
    value=factor(value,c("Very Satisfied","Satisfied","Unsatisfied","Very Unsatisfied","N/A"))
    #value = na_if(value,"N/A"),
   # value=factor(value,c("Very Satisfied","Satisfied","Unsatisfied","Very Unsatisfied"))
  ) %>%
  pivot_wider(names_from = name,values_from = value) %>%
  select(-id) %>%
  as.data.frame() %>%
  sjPlot::plot_likert(
    cat.neutral = "N/A"
    
  ) +
  ggthemes::theme_tufte(base_family = "sans") +
  labs(  #labels
    y="Response Percentage",
    x="",
    title="Utilizing Services- 2019"
  )


ggsave("/Users/carlpearson/Documents/r_github/dev_exp_survey/Plots/sat-utilize-services.png",device="png",
       width=12,height=8,bg="transparent")

#no na
df %>%
  filter(year=="2018") %>% #2019 only
  select(id,contains("developing and testing")) %>% #choose vars
  pivot_longer(contains("developing and testing")) %>% #elongate
  mutate( #clean up variable names
    name=gsub("Please rate your satisfaction with the developing and testing your code on OpenShift\\. - ","",name),
    # value=replace_na(value,"N/A"),
    # value=factor(value,c("Very Satisfied","Satisfied","Unsatisfied","Very Unsatisfied","N/A"))
    value = na_if(value,"N/A"),
    value=factor(value,c("Very Satisfied","Satisfied","Unsatisfied","Very Unsatisfied"))
  ) %>%
  pivot_wider(names_from = name,values_from = value) %>%
  select(-id) %>%
  as.data.frame() %>%
  sjPlot::plot_likert(
    cat.neutral = "N/A"
    
  ) +
  ggthemes::theme_tufte(base_family = "sans") +
  labs(  #labels
    y="Response Percentage",
    x="",
    title="Satisfacation of Developing and Testing - 2019",
    # subtitle = "",
    caption = "NAs removed"
  )

ggsave("/Users/carlpearson/Documents/r_github/dev_exp_survey/Plots/sat-utilize-services2.png",device="png",
       width=12,height=8,bg="transparent")

#app dev

#2019
df %>%
  filter(year=="2019") %>% #2019 only
  select(id,contains("satisfaction with the application development")) %>% #choose vars
  pivot_longer(contains("satisfaction with the application development")) %>% #elongate
  mutate( #clean up variable names
    name=gsub("Please rate your satisfaction with the application development features on OpenShift\\. - ","",name),
    # value=replace_na(value,"N/A"),
    # value=factor(value,c("Very Satisfied","Satisfied","Unsatisfied","Very Unsatisfied","N/A"))
    value = na_if(value,"N/A"),
    value=factor(value,c("Very Satisfied","Satisfied","Unsatisfied","Very Unsatisfied"))
  ) %>%
  pivot_wider(names_from = name,values_from = value) %>%
  select(-id) %>%
  as.data.frame() %>%
  sjPlot::plot_likert(
    cat.neutral = "N/A"
    
  ) +
  ggthemes::theme_tufte(base_family = "sans") +
  labs(  #labels
    y="Response Percentage",
    x="",
    title="Satisfacation of Application Development Features - 2019",
    # subtitle = "",
    caption = "NAs removed"
  )

ggsave("/Users/carlpearson/Documents/r_github/dev_exp_survey/Plots/sat-appdev19.png",device="png",
       width=12,height=8,bg="transparent")

#2018
df %>%
  filter(year=="2018") %>% #2019 only
  select(id,contains("satisfaction with the application development")) %>% #choose vars
  pivot_longer(contains("satisfaction with the application development")) %>% #elongate
  mutate( #clean up variable names
    name=gsub("Please rate your satisfaction with the application development features on OpenShift\\. - ","",name),
    # value=replace_na(value,"N/A"),
    # value=factor(value,c("Very Satisfied","Satisfied","Unsatisfied","Very Unsatisfied","N/A"))
    value = na_if(value,"N/A"),
    value=factor(value,c("Very Satisfied","Satisfied","Unsatisfied","Very Unsatisfied"))
  ) %>%
  pivot_wider(names_from = name,values_from = value) %>%
  select(-id) %>%
  as.data.frame() %>%
  sjPlot::plot_likert(
    cat.neutral = "N/A"
  ) +
  ggthemes::theme_tufte(base_family = "sans") +
  labs(  #labels
    y="Response Percentage",
    x="",
    title="Satisfacation of Application Development Features - 2018",
    # subtitle = "",
    caption = "NAs removed"
  )

ggsave("/Users/carlpearson/Documents/r_github/dev_exp_survey/Plots/sat-appdev18.png",device="png",
       width=12,height=8,bg="transparent")

#resources for OpenShift

#2019
df %>%
  filter(year=="2019") %>% #2019 only
  select(id,contains("resources for OpenShift")) %>% #choose vars
  pivot_longer(contains("resources for OpenShift")) %>% #elongate
  mutate( #clean up variable names
    name=gsub("Please rate your satisfaction with these resources for OpenShift - ","",name),
    # value=replace_na(value,"N/A"),
    # value=factor(value,c("Very Satisfied","Satisfied","Unsatisfied","Very Unsatisfied","N/A"))
    value = na_if(value,"N/A"),
    value=factor(value,c("Very Satisfied","Satisfied","Unsatisfied","Very Unsatisfied"))
  ) %>%
  pivot_wider(names_from = name,values_from = value) %>%
  select(-id) %>%
  as.data.frame() %>%
  sjPlot::plot_likert(
    cat.neutral = "N/A"
    
  ) +
  ggthemes::theme_tufte(base_family = "sans") +
  labs(  #labels
    y="Response Percentage",
    x="",
    title="Satisfacation of Resources for OpenShift - 2019",
    # subtitle = "",
    caption = "NAs removed"
  )

ggsave("/Users/carlpearson/Documents/r_github/dev_exp_survey/Plots/sat-resources19.png",device="png",
       width=12,height=8,bg="transparent")

#2018
df %>%
  filter(year=="2018") %>% #2019 only
  select(id,contains("resources for OpenShift")) %>% #choose vars
  pivot_longer(contains("resources for OpenShift")) %>% #elongate
  mutate( #clean up variable names
    name=gsub("Please rate your satisfaction with these resources for OpenShift - ","",name),
    # value=replace_na(value,"N/A"),
    # value=factor(value,c("Very Satisfied","Satisfied","Unsatisfied","Very Unsatisfied","N/A"))
    value = na_if(value,"N/A"),
    value=factor(value,c("Very Satisfied","Satisfied","Unsatisfied","Very Unsatisfied"))
  ) %>%
  pivot_wider(names_from = name,values_from = value) %>%
  select(-id) %>%
  as.data.frame() %>%
  sjPlot::plot_likert(
    cat.neutral = "N/A"
    
  ) +
  ggthemes::theme_tufte(base_family = "sans") +
  labs(  #labels
    y="Response Percentage",
    x="",
    title="Satisfacation of Resources for OpenShift - 2018",
    # subtitle = "",
    caption = "NAs removed"
  )

ggsave("/Users/carlpearson/Documents/r_github/dev_exp_survey/Plots/sat-resources18.png",device="png",
       width=12,height=8,bg="transparent")

#getting started

n18 <- nrow(df[df$year=="2018",])
n19 <- nrow(df[df$year=="2019",])


df %>%
  select(id,year,`How do you get started creating new applications for OpenShift? (Check all that apply) - Selected Choice`) %>%
  separate(`How do you get started creating new applications for OpenShift? (Check all that apply) - Selected Choice`,into = paste0("gs",1:10),sep = ",") %>%
  group_by(year) %>%
  mutate(total=n_distinct(id)) %>%
  pivot_longer(contains("gs")) %>%
  na.omit(value) %>%
  group_by(value,year,total) %>%
  summarize(count=n()) %>%
  group_by(year) %>%
  mutate(prop = count / total) %>%
  
  

