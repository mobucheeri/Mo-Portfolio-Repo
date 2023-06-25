# Medical Insurance EDA Project

## Closed Claims, 2019-2022

```{r echo=FALSE, warning=FALSE, results='hide', message=FALSE}

library(tidyverse)
library(dplyr)
library(lubridate)
library(stringr)
library(ggplot2)
library(data.table)
library(tidyr)
library(janitor)
library(rpart)
library(rpart.plot)
library(randomForest)
library(readxl)
library(knitr)
library(kableExtra)
library(randomForest)
library(shiny)
```

```{r Closed Claim Raw Dataset, echo=FALSE, warning=FALSE, message=FALSE}

setwd('/Users/mo/Desktop/Mo-Portfolio-Repo/Project 1 Datasets')

### Cleaned Closed Claim Dataset 2019-2022
ClosedClaim_2019 <- read_xlsx('/Users/mo/Desktop/Mo-Portfolio-Repo/Project 1 Datasets/Closed Claims 2019-2022/ClosedClaim_2019.xlsx')
ClosedClaim_2020 <- read_xlsx('/Users/mo/Desktop/Mo-Portfolio-Repo/Project 1 Datasets/Closed Claims 2019-2022/ClosedClaim_2020.xlsx')
ClosedClaim_2021 <- read_xlsx('/Users/mo/Desktop/Mo-Portfolio-Repo/Project 1 Datasets/Closed Claims 2019-2022/ClosedClaim_2021.xlsx')
ClosedClaim_2022 <- read_xlsx('/Users/mo/Desktop/Mo-Portfolio-Repo/Project 1 Datasets/Closed Claims 2019-2022/ClosedClaim_2022.xlsx')
Provider_Network <- read_xlsx('/Users/mo/Desktop/Mo-Portfolio-Repo/Project 1 Datasets/Provider Network.xlsx')
Production <- read_xlsx('/Users/mo/Desktop/Mo-Portfolio-Repo/Project 1 Datasets/GroupID.xlsx')
GroupID <- Production

ClosedClaim_2020$BRANCH<- as.character(ClosedClaim_2020$BRANCH)
ClosedClaim_2021$BRANCH<- as.character(ClosedClaim_2021$BRANCH)
ClosedClaim_2022$BRANCH<- as.character(ClosedClaim_2022$BRANCH)

ClosedClaim_2020$`BROKER NAME`<- as.character(ClosedClaim_2020$`BROKER NAME`)
ClosedClaim_2021$`BROKER NAME`<- as.character(ClosedClaim_2021$`BROKER NAME`)
ClosedClaim_2022$`BROKER NAME`<- as.character(ClosedClaim_2022$`BROKER NAME`)

ClosedClaim_2020$`STAFF`<- as.character(ClosedClaim_2020$`STAFF`)
ClosedClaim_2021$`STAFF`<- as.character(ClosedClaim_2021$`STAFF`)
ClosedClaim_2022$`STAFF`<- as.character(ClosedClaim_2022$`STAFF`)

ClosedClaim_2019$`DOCTOR CODE`<- as.character(ClosedClaim_2019$`DOCTOR CODE`)
ClosedClaim_2020$`DOCTOR CODE`<- as.character(ClosedClaim_2020$`DOCTOR CODE`)
ClosedClaim_2021$`DOCTOR CODE`<- as.character(ClosedClaim_2021$`DOCTOR CODE`)
ClosedClaim_2022$`DOCTOR CODE`<- as.character(ClosedClaim_2022$`DOCTOR CODE`)

ClosedClaim_2019$`LETTER`<- as.character(ClosedClaim_2019$`LETTER`)
ClosedClaim_2020$`LETTER`<- as.character(ClosedClaim_2020$`LETTER`)
ClosedClaim_2021$`LETTER`<- as.character(ClosedClaim_2021$`LETTER`)
ClosedClaim_2022$`LETTER`<- as.character(ClosedClaim_2022$`LETTER`)

ClosedClaim_2019$`CHECK`<- as.character(ClosedClaim_2019$`CHECK`)
ClosedClaim_2020$`CHECK`<- as.character(ClosedClaim_2020$`CHECK`)
ClosedClaim_2021$`CHECK`<- as.character(ClosedClaim_2021$`CHECK`)
ClosedClaim_2022$`CHECK`<- as.character(ClosedClaim_2022$`CHECK`)

ClosedClaim_2019$`LETTER DATE`<- as.character(ClosedClaim_2019$`LETTER DATE`)
ClosedClaim_2020$`LETTER DATE`<- as.character(ClosedClaim_2020$`LETTER DATE`)
ClosedClaim_2021$`LETTER DATE`<- as.character(ClosedClaim_2021$`LETTER DATE`)
ClosedClaim_2022$`LETTER DATE`<- as.character(ClosedClaim_2022$`LETTER DATE`)

ClosedClaim_2019$`CHECK DATE`<- as.character(ClosedClaim_2019$`CHECK DATE`)
ClosedClaim_2020$`CHECK DATE`<- as.character(ClosedClaim_2020$`CHECK DATE`)
ClosedClaim_2021$`CHECK DATE`<- as.character(ClosedClaim_2021$`CHECK DATE`)
ClosedClaim_2022$`CHECK DATE`<- as.character(ClosedClaim_2022$`CHECK DATE`)

ClosedClaim_2019$`INVOICE`<- as.character(ClosedClaim_2019$`INVOICE`)
ClosedClaim_2020$`INVOICE`<- as.character(ClosedClaim_2020$`INVOICE`)
ClosedClaim_2021$`INVOICE`<- as.character(ClosedClaim_2021$`INVOICE`)
ClosedClaim_2022$`INVOICE`<- as.character(ClosedClaim_2022$`INVOICE`)

ClosedClaim_2019$`LOS`<- as.numeric(ClosedClaim_2019$`LOS`)
ClosedClaim_2020$`LOS`<- as.numeric(ClosedClaim_2020$`LOS`)
ClosedClaim_2021$`LOS`<- as.numeric(ClosedClaim_2021$`LOS`)
ClosedClaim_2022$`LOS`<- as.numeric(ClosedClaim_2022$`LOS`)

ClosedClaim_2019$`ESTIMATED`<- as.numeric(ClosedClaim_2019$`ESTIMATED`)
ClosedClaim_2020$`ESTIMATED`<- as.numeric(ClosedClaim_2020$`ESTIMATED`)
ClosedClaim_2021$`ESTIMATED`<- as.numeric(ClosedClaim_2021$`ESTIMATED`)
ClosedClaim_2022$`ESTIMATED`<- as.numeric(ClosedClaim_2022$`ESTIMATED`)

ClosedClaim_2019$`REFERENCE`<- as.character(ClosedClaim_2019$`REFERENCE`)
ClosedClaim_2020$`REFERENCE`<- as.character(ClosedClaim_2020$`REFERENCE`)
ClosedClaim_2022$`REFERENCE`<- as.character(ClosedClaim_2022$`REFERENCE`)

ClosedClaim<- bind_rows(ClosedClaim_2019,
                        ClosedClaim_2020,
                        ClosedClaim_2021,
                        ClosedClaim_2022)

ClosedClaim$`Admission Year`<- year(ClosedClaim$`ADMISSION DATE`)
ClosedClaim$`Admission Month`<- month(ClosedClaim$`ADMISSION DATE`)
ClosedClaim$`GENDER`<-as.character(ClosedClaim$`GENDER`)

rm(list=c("ClosedClaim_2019","ClosedClaim_2020","ClosedClaim_2021","ClosedClaim_2022", "Production"))

ClosedClaim <- clean_names(ClosedClaim)
ClosedClaim<- inner_join(x = Provider_Network, y = ClosedClaim, by = "provider")
ClosedClaim<- inner_join(x = GroupID, y = ClosedClaim, by = "group_id")
colnames(ClosedClaim)[40]<-"length_of_stay"

head(ClosedClaim)%>%kable(caption = 'Table 1: GLOBMED Closed Claim 2019-2022 Raw Data', position = "center")%>%kable_classic_2(full_width=F, html_font = "Cambria")%>%kable_styling(full_width = F, bootstrap_options = c("striped", "hover", "condensed"), position = "left", font_size = 12, fixed_thead = T)%>%column_spec(1, bold = T, border_right = T, color = "black", background = "lightgrey")
```

### Data Cleaning

```{r Data Cleaning, echo=FALSE, warning=FALSE, message=FALSE}

# Grouped Variables

Variables_grouped<-ClosedClaim%>%select("ind", "group_id", "group_id_description", "workplace_type", "provider", "provider_name", "type", "network", "doctor_name", "underwriting_year", "age", "age_band", "gender", "nationality", "categ", "subcateg_id", "subcateg_desc", "visa_type", "visa_type_description", "admission_month", "admission_year", "relation", "service", "service_description", "length_of_stay","mcn_nbr", "ssnbr_visanbr", "cov_risknet")

#### N/A's

Variables_grouped$length_of_stay<- as.numeric(Variables_grouped$length_of_stay)

Variables_grouped$`visa_type` <- replace(Variables_grouped$`visa_type`, is.na(Variables_grouped$`visa_type`), "N/A")

Variables_grouped <- Variables_grouped%>%
  mutate_if(is.character, ~replace_na(., "Not Applicable"))

Variables_grouped$length_of_stay <- replace(Variables_grouped$length_of_stay, is.na(Variables_grouped$length_of_stay), 0)

Variables_grouped$`doctor_name` <- str_replace(Variables_grouped$`doctor_name`, "NOT X AVAILABLE", "Unknown")

#### Age Band

Variables_grouped$`age_band`<- cut(Variables_grouped$age,breaks = c(0,5,18,19,30,35,40,45,50,55,60,65,70,80,90),right = F)
Variables_grouped$`age_band`<-as.character(Variables_grouped$`age_band`)

#### Relation

Variables_grouped$`relation`<- ifelse(ClosedClaim$principal > ClosedClaim$ind, "Principal", ifelse(ClosedClaim$principal < ClosedClaim$ind, "Family Member", "Principal"))

#### Nationality

Variables_grouped$nationality<- ifelse(Variables_grouped$nationality=="UNSPECIFIED", "NON-BAHRAINI",ifelse(Variables_grouped$nationality=="BAHRAIN","BAHRAINI","NON-BAHRAINI"))

#### DRC and Subcateg

Variables_grouped<- Variables_grouped%>%mutate(benefit= `subcateg_desc`)
Variables_grouped[grepl("515",ClosedClaim$drc)|
           grepl("516",ClosedClaim$drc)|
           grepl("517",ClosedClaim$drc)|
           grepl("518",ClosedClaim$drc)|
           grepl("527",ClosedClaim$drc),"benefit"]<- "Maternity"

Variables_grouped$pre_existing_indicator<- "NonPreExisting"
Variables_grouped$pre_existing_indicator[grepl("664",ClosedClaim$drc)]<- "PreExisting"

#### Claim Count and Service Count

Variables_grouped<- Variables_grouped%>%group_by(ssnbr_visanbr)%>%mutate(service_count=1)%>%ungroup()

Variables_grouped<- Variables_grouped%>%group_by(`mcn_nbr`)%>%mutate(claim_count=cumsum(1:n()))%>%ungroup()

Variables_grouped$claim_count[Variables_grouped$claim_count>1]<-0  

#### Variables Grouped Cleaned

head(Variables_grouped)%>%kable(caption = "Table 2: Cleaned Data Closed Claims 2019-2022")%>%kable_classic_2(full_width=F, html_font = "Cambria")%>%kable_styling(full_width = F, bootstrap_options = c("striped", "hover", "condensed"), position = "left", font_size = 12, fixed_thead = T)%>%column_spec(1, bold = T, border_right = T, color = "black", background = "lightgrey")
```

### Predictor Variables

#### Table of Final Predictor Variables

```{r Predictor Variables, echo=FALSE, warning=FALSE, message=FALSE}

Variables_summary<-Variables_grouped%>%group_by_("ind","group_id_description", "workplace_type", "provider", "provider_name", "type", "network", "doctor_name", "underwriting_year", "age", "age_band", "gender", "nationality", "categ", "benefit", "pre_existing_indicator", "visa_type_description", "admission_month", "admission_year", "relation", "length_of_stay")%>%
  mutate(grossclaim=sum(`cov_risknet`), claimcount=sum(`claim_count`), servicecount=service_count, severity=sum(`cov_risknet`)/sum(`claim_count`))%>%ungroup()

Variables_summary$rs<-rowSums(Variables_summary[,c("grossclaim")])

Variables_summary<-Variables_summary%>%filter(rs != 0)
Variables_summary$rs<- NULL

Variables_summary<-Variables_summary[,-26:-27]
Variables_summary<-Variables_summary[,-29:-30]
Variables_summary$severity[is.infinite(Variables_summary$severity)]<-NA


head(Variables_summary)%>%kable(caption = "Table 3: Predictor Variables")%>%kable_classic_2(full_width=F, html_font = "Cambria")%>%kable_styling(full_width = F, bootstrap_options = c("striped", "hover", "condensed"), position = "left", font_size = 12, fixed_thead = T)%>%column_spec(1, bold = T, border_right = T, color = "black", background = "lightgrey")

```

### Exploratory Data Analysis

##### Gross Claim Analysis
###### Each table in the Gross Claim Analysis shows the Gross Claim per Predictor Variable per year (2019-2022)

```{r Gross Claim Analysis, warning=FALSE, message=FALSE, echo=FALSE, results='asis'}

#### Gross Claim per Variable, 2019-2022

var<- c("group_id_description", "workplace_type", "provider_name", "type", "network", "doctor_name", "underwriting_year", "age", "age_band", "gender", "nationality", "categ", "benefit", "pre_existing_indicator",  "visa_type_description", "relation", "length_of_stay")

for(i in var){
  a<-Variables_summary%>%group_by_(i,"admission_year")%>%summarise(grossclaim=sum(grossclaim))%>%ungroup()
  a<-a%>%arrange(admission_year)
  a<-pivot_wider(a,id_cols = i, names_from = "admission_year",values_from = "grossclaim" ,values_fill = 0)
  colnames(a)[2:5]<- paste0("gross_claim_",colnames(a)[2:5])
  a$`gross_claim_2019`<-prettyNum(a$`gross_claim_2019`, big.mark = ',')
  a$`gross_claim_2020`<-prettyNum(a$`gross_claim_2020`, big.mark = ',')
  a$`gross_claim_2021`<-prettyNum(a$`gross_claim_2021`, big.mark = ',')
  a$`gross_claim_2022`<-prettyNum(a$`gross_claim_2022`, big.mark = ',')
  print(knitr::kable(a)%>%kable_classic_2(full_width=F, html_font = "Cambria")%>%kable_styling(full_width = F, bootstrap_options = c("striped", "hover", "condensed"), position = "left", font_size = 12, fixed_thead = T)%>%column_spec(1, bold = T, border_right = T, color = "black", background = "lightgrey"))
}

#### Histogram of Gross Claim, 2019-2022

Variables_summary%>%ggplot(aes(x=grossclaim,fill=as.factor(admission_year)))+geom_histogram(alpha=0.5)+scale_x_log10()+labs(x = "Gross Claim", title = "Gross Claim 2019-2022")+scale_fill_discrete(name = "Admission Year")

#### Boxplot of Gross Claim by Provider Network, 2019-2022

Variables_summary%>%group_by_("provider_name", "admission_year", "network")%>%summarise(grossclaim=sum(grossclaim))%>%ggplot(aes(x=network, y=grossclaim, fill=network, color=network))+geom_boxplot(alpha=0.5)+labs(x = "Network", y = "Gross Claim", title = "Log Scaled Boxplot for Gross Claim by Provider Network, 2019-2022")+facet_wrap("admission_year", scales = "free_y")+scale_y_log10()

#### Histogram of Gross Claim by Nationality, 2019-2022

Variables_summary%>%ggplot(aes(x=grossclaim,fill=nationality))+geom_histogram(alpha=0.5)+scale_x_log10()+labs(x = "Gross Claim", title = "Gross Claim by Nationality, 2019-2022")+scale_fill_discrete(name = "Nationality")+facet_wrap("admission_year")

#### Histogram of In and Out Patient Gross Claim, 2019-2022

Variables_summary%>%filter(categ=="In-Patient")%>%ggplot(aes(x=grossclaim,fill=as.factor(admission_year)))+geom_histogram(alpha=0.5)+scale_x_log10()+labs(title = "In-Patient Gross Claim, 2019-2022", x = "Gross Claim")

Variables_summary%>%filter(categ=="Out-patient")%>%ggplot(aes(x=grossclaim,fill=as.factor(admission_year)))+geom_histogram(alpha=0.5)+scale_x_log10()+labs(title = "Out-Patient Gross Claim, 2019-2022", x = "Gross Claim")

#### Q-Q Plot Comparing Normal Distribution of Gross Claim and In-Patient/        ### Out-Patient Claims, 2019-2022

Variables_summary%>%group_by_("categ", "admission_year", "ind")%>%summarise(grossclaim=sum(grossclaim))%>%ggplot(aes(sample = grossclaim, color = categ))+stat_qq()+stat_qq_line()+facet_wrap("admission_year")+labs(title = "Normally Distributed Q-Q Plot on Gross Claim and In-Patient/Out-Patient Claims", x= "z-scores", y = "Claim Count")

#### Distribution of Gross Claim, by Age, 2019-2022

Variables_summary%>%ggplot(aes(x=age, y=grossclaim, color = admission_year))+geom_col(alpha=0.5)+scale_y_log10()+labs(x = "Age", y="Gross Claim", title = "Gross Claim by Age, 2019-2022")+facet_wrap("admission_year", scales = "free_y")

#### Distribution of Gross Claim, by Gender, 2019-2022

Variables_summary%>%ggplot(aes(x=grossclaim,fill=gender))+geom_histogram(alpha=0.5)+scale_x_log10()+labs(x = "Gross Claim", title = "Gross Claim by Gender, 2019-2022")+scale_fill_discrete(name = "Gender")+facet_wrap("admission_year", scales = "free_y")

#### Scatter Plot of Gross Claim and Age by Gender, 2019-2022

Variables_summary%>%group_by_("gender", "admission_year", "age")%>%summarise(grossclaim=sum(grossclaim))%>%ggplot(aes(x=age, y=grossclaim, color=gender, fill=gender))+geom_point()+geom_smooth()+labs(x = "Age", y = "Gross Claim", title = "Gross Claim and Age by Gender, 2019-2022")+facet_wrap("admission_year", scales = "free_y")

#### Distribution of Gross Claim by Relation, 2019-2022

Variables_summary%>%ggplot(aes(x=grossclaim,fill=relation))+geom_histogram(alpha=0.5)+scale_x_log10()+labs(x = "Gross Claim", title = "Gross Claim by Relation, 2019-2022")+scale_fill_discrete(name = "Relation")+facet_wrap("admission_year", scales = "free_y")

#### Scatter Plot of Gross Claim and Age by Relation, 2019-2022

Variables_summary%>%group_by_("relation", "admission_year", "age")%>%summarise(grossclaim=sum(grossclaim))%>%ggplot(aes(x=age, y=grossclaim, color=relation, fill=relation))+geom_point()+geom_smooth()+labs(x = "Age", y = "Gross Claim", title = "Gross Claim and Age by Relation, 2019-2022")+facet_wrap("admission_year", scales = "free_y")

#### Scatter Plot of Gross Claim and Age by Pre-Existing Indicator, 2019-2022

Variables_summary%>%group_by_("pre_existing_indicator", "admission_year", "age")%>%summarise(grossclaim=sum(grossclaim))%>%ggplot(aes(x=age, y=grossclaim, color=pre_existing_indicator, fill=pre_existing_indicator))+geom_point()+geom_smooth()+labs(x = "Age", y = "Gross Claim", title = "Gross Claim and Age by Pre-Existing Indicator, 2019-2022")+facet_wrap("admission_year", scales = "free_y")

#### Line Graph of Gross Claim and Benefit by Age, 2019-2022

Variables_summary%>%group_by_("benefit", "admission_year", "age")%>%summarise(grossclaim=sum(grossclaim))%>%ggplot(aes(x=age, y=grossclaim, color=benefit, fill=benefit))+geom_line(alpha=0.15)+geom_smooth()+labs(x = "Age", y = "Gross Claim", title = "Gross Claim and Age by Benefit, 2019-2022")+facet_wrap("admission_year", scales = "free_y")

#### Scatter Plot of Gross Claim by Length of Stay, 2019-2022 

Variables_summary%>%group_by_("length_of_stay", "admission_year", "categ")%>%summarise(grossclaim=sum(grossclaim))%>%arrange(admission_year)%>%ggplot(aes(x=length_of_stay,y=grossclaim,color=admission_year))+geom_point()+facet_wrap("admission_year", scales = "free_y")+labs(title = "Gross Claim by Length of Stay, 2019-2022", color = "Admission Year")+geom_smooth()

```

#### Claim Count Analysis
##### Each table in the Claim Count Analysis is the Claim Count per Predictor Variable (where applicable) per year (2019-2022)

```{r Claim Count Analysis, echo=FALSE, warning=FALSE, message=FALSE, results='asis'}

#### Claim Count

for(i in var){
  b<-Variables_summary%>%group_by_(i,"admission_year")%>%summarise(claimcount=sum(claimcount))
  b<- b%>%arrange(admission_year)
  b<-pivot_wider(b,id_cols = i, names_from = "admission_year",values_from = "claimcount" ,values_fill = 0)
  colnames(b)[2:5]<- paste0("claim_count_",colnames(b)[2:5])
  b$claim_count_2019<-prettyNum(b$claim_count_2019, big.mark = ',')
  b$claim_count_2020<-prettyNum(b$claim_count_2020, big.mark = ',')
  b$claim_count_2021<-prettyNum(b$claim_count_2021, big.mark = ',')
  b$claim_count_2022<-prettyNum(b$claim_count_2022, big.mark = ',')
  print(knitr::kable(b)%>%kable_classic_2(full_width=F, html_font = "Cambria")%>%kable_styling(full_width = F, bootstrap_options = c("striped", "hover", "condensed"), position = "left", font_size = 12, fixed_thead = T)%>%column_spec(1, bold = T, border_right = T, color = "black", background = "lightgrey"))
}

Variables_summary%>%ggplot(aes(x=claimcount,fill=gender))+geom_histogram(alpha=0.5)+scale_y_log10()+labs(x = "Claim Count", title = "Claim Count by Gender, 2019-2022")+scale_fill_discrete(name = "Gender")+facet_wrap("admission_year", scales = "free_y")

Variables_summary%>%group_by_("pre_existing_indicator", "admission_year", "age")%>%summarise(claimcount=sum(claimcount))%>%ggplot(aes(x=age, y=claimcount, color=pre_existing_indicator, fill=pre_existing_indicator))+geom_point()+geom_smooth()+labs(x = "Age", y = "Claim Count", title = "Claim Count and Pre-Existing Indicator by Age, 2019-2022")+facet_wrap("admission_year", scales = "free_y")
```

#### Severity of Claims Analysis

##### Each table in the Severity Analysis is the Severity of the Claims per Predictor Variable per year (2019-2022)

```{r Severity Analysis, echo=FALSE, warning=FALSE, results='asis', message=FALSE}

#### Severity of Claims per Variable, 2019-2022

for(i in var){
  c<-Variables_summary%>%group_by_(i,"admission_year")%>%summarise(severity=sum(grossclaim)/sum(claimcount))
   c<- c%>%arrange(admission_year)
  c<-pivot_wider(c,id_cols = i, names_from = "admission_year",values_from = "severity" ,values_fill = 0)
  colnames(c)[2:5]<- paste0("severity_",colnames(c)[2:5])
  c$severity_2019<-prettyNum(c$severity_2019, big.mark = ',')
  c$severity_2020<-prettyNum(c$severity_2020, big.mark = ',')
  c$severity_2021<-prettyNum(c$severity_2021, big.mark = ',')
  c$severity_2022<-prettyNum(c$severity_2022, big.mark = ',')
  print(knitr::kable(c)%>%kable_classic_2(full_width=F, html_font = "Cambria")%>%kable_styling(full_width = F, bootstrap_options = c("striped", "hover", "condensed"), position = "left", font_size = 12, fixed_thead = T)%>%column_spec(1, bold = T, border_right = T, color = "black", background = "lightgrey"))
}

#### Histogram of Severity (log scaled), 2019-2022

Variables_summary%>%ggplot(aes(x=severity,fill=as.factor(admission_year)))+geom_histogram(alpha=0.5)+scale_y_log10()+labs(x = "Severity", title = "Severity 2019-2022")+scale_fill_discrete(name = "Admission Year")

#### Histogram of Severity of Claim in In and Out Patient (log scaled), 2019-2022

Variables_summary%>%filter(categ=="In-Patient")%>%ggplot(aes(x=severity,fill=as.factor(admission_year)))+geom_histogram(alpha=0.5)+scale_x_log10()+labs(title = "In-Patient Severity, 2019-2022", x = "Severity")+scale_fill_discrete(name = "Admission Year")

Variables_summary%>%filter(categ=="Out-patient")%>%ggplot(aes(x=severity,fill=as.factor(admission_year)))+geom_histogram(alpha=0.5)+scale_x_log10()+labs(title = "Out-Patient Severity, 2019-2022", x = "Severity")+scale_fill_discrete(name = "Admission Year")

#### Q-Q Plot Comparing Normal Distribution of Severity by In-Patient/Out-Patient ### Claims, 2019-2022

Variables_summary%>%group_by_("categ", "admission_year", "ind")%>%summarise(severity=sum(grossclaim)/sum(claimcount))%>%ggplot(aes(sample = severity, color = categ))+stat_qq()+stat_qq_line()+facet_wrap("admission_year")+labs(title = "Normally Distributed Q-Q Plot on Severity and In-Patient/Out-Patient Claims", x = "z-scores", y = "Severity", color = "Category")

#### Line Graph Comparing In-Patient Percentage per Network, 2019-2022

Variables_summary%>%group_by(`admission_year`, `network`)%>%summarise(total = n(), percent_in_patient = mean(`categ` == "In-Patient"))%>%arrange(desc(percent_in_patient))%>%filter(network %in% c("Silver", "Gold", "Diamond", "Platinum", "Restricted"))%>%ungroup()%>%ggplot(aes(x=admission_year,y=percent_in_patient, color = network))+geom_point()+geom_smooth()+labs(title = "In-Patient Percentage per Nework, 2019-2022", x = "Admission Year", y = "Percentage of In-Patient Intake")

#### Histogram of Severity of Claim by Gender (log scaled), 2019-2022

Variables_summary%>%ggplot(aes(x=severity,fill=gender))+geom_histogram(alpha=0.5)+scale_y_log10()+labs(x = "Severity", title = "Severity by Gender, 2019-2022")+scale_fill_discrete(name = "Gender")+facet_wrap("admission_year")

#### Column Chart of Distribution of Severity (log scaled) by Age and Gender, 2019-2022

Variables_summary%>%ggplot(aes(x=age,y=severity,fill=gender))+geom_col(alpha=0.5)+scale_y_log10()+labs(x = "Age", y="Severity", title = "Severity by Age and Gender, 2019-2022")+scale_fill_discrete(name = "Gender")+facet_wrap("admission_year")

#### Q-Q Plot Comparing Normal Distribution of Severity by Gender, 2019-2022

Variables_summary%>%group_by_("gender", "admission_year", "ind")%>%summarise(severity=sum(grossclaim)/sum(claimcount))%>%ggplot(aes(sample = severity, color = gender))+stat_qq()+stat_qq_line()+facet_wrap("admission_year", scales = "free_y")+labs(title = "Normally Distributed Q-Q Plot on Severity by Gender", x = "z-scores", y = "Severity", color = "Gender")

#### Distribution of Severity by Nationality (log scaled), 2019-2022

Variables_summary%>%ggplot(aes(x=severity,fill=nationality))+geom_histogram(alpha=0.5)+scale_y_log10()+labs(x = "Severity", title = "Severity by Nationality, 2019-2022")+scale_fill_discrete(name = "Nationality")+facet_wrap("admission_year", scales = "free_y")

#### Scatter plot of Severity of Claim by Length of Stay, 2019-2022

Variables_summary%>%group_by_("length_of_stay", "admission_year", "categ")%>%summarise(severity=sum(grossclaim)/sum(claimcount))%>%arrange(admission_year)%>%ggplot(aes(x=length_of_stay,y=severity,color=admission_year))+geom_point()+facet_wrap("admission_year", scales = "free_y")+labs(title = "Severity of Claims by Length of Stay, 2019-2022", color = "Admission Year")+geom_smooth()

#### Scatter Plot of Severity of Claim and Pre-Existing Condition by Age, 2019-2022

Variables_summary%>%group_by_("pre_existing_indicator", "admission_year", "age")%>%summarise(severity=sum(grossclaim)/sum(claimcount))%>%ggplot(aes(x=age, y=severity, color=pre_existing_indicator, fill=pre_existing_indicator))+geom_point(alpha=0.5)+geom_smooth()+labs(x = "Age", y = "Severity", title = "Severity of Claim by Pre-Existing Indicator and Age, 2019-2022")+facet_wrap("admission_year", scales = "free_y")

#### Area Graph of the Severity of Claim and Benefit by Age, 2019-2022

Variables_summary%>%group_by_("benefit", "admission_year", "age")%>%summarise(severity=sum(grossclaim)/sum(claimcount))%>%ggplot(aes(x=age, y=severity, color=benefit, fill=benefit))+geom_line(alpha=0.15)+geom_smooth()+labs(x = "Age", y = "Severity", title = "Severity of Claim by Benefit and Age, 2019-2022")+facet_wrap("admission_year", scales = "free_y")+scale_y_log10()

#### Scatter Plot of the Severity of Claim by Principal and Family Member, 2019-2022

Variables_summary%>%group_by_("relation", "admission_year", "age")%>%summarise(severity=sum(grossclaim/sum(claimcount)))%>%ggplot(aes(x=age, y=severity, color=relation, fill=relation))+geom_point(alpha=0.5)+geom_smooth()+labs(x = "Age", y = "Severity", title = "Severity by Relation and Age, 2019-2022")+facet_wrap("admission_year")
```

#### Models: Decision Tree Model

```{r, echo=FALSE, warning=FALSE, results='hide', message=FALSE}

set.seed(123)
sample_size = floor(0.8*nrow(Variables_summary))
select_rows = sample(seq_len(nrow(Variables_summary)), size = sample_size)
ClosedClaim2_training = Variables_summary[`select_rows`,]
ClosedClaim2_testing = Variables_summary[-`select_rows`,]
nrow(ClosedClaim2_training)
nrow(ClosedClaim2_testing)

complex_model<-rpart(severity ~ `admission_year` + `type` + `network` + `underwriting_year` + `age` + `gender` + `nationality` + `categ` + `benefit` + `pre_existing_indicator` + `relation` + `length_of_stay`, cp = 0.001, data = ClosedClaim2_training)

options(repr.plot.width = 20, repr.plot.height = 10)
rpart.plot(complex_model, type = 1, digits = 5, fallen.leaves = TRUE)
```

