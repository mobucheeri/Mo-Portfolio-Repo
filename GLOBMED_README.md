---
title: "GLOBMED Closed Claims 2019-2023"
author: "Mohamed Bucheeri"
date: "02-08-2023"
output:
  html_document:
    df_print: paged
---
# 1. Project Overview

This project analyses GLOBMED closed claims data from 2019 to 2023 to identify patterns in claim severity, frequency, and various demographic and medical variables. Using statistical techniques, this project provides strategic insights to enhance risk assessment and resource management in the medical insurance domain.

# 2. Key Libraries

This project employs a variety of R libraries to support data processing, analysis, and visualisation. Key libraries include:

```{r echo=TRUE, message=FALSE, warning=FALSE, results='hide'}

# These are the required libraries

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
library(readxl)
library(knitr)
library(kableExtra)
library(shiny)
library(writexl)
library(tinytex)
```
# 3. Data Import and Cleaning
Data from multiple years is imported, processed, and cleaned to ensure consistency. The data includes closed claims from 2019 to 2023, provider networks, and ICD-10 medical codes for disease classification. Key cleaning steps include:

- Converting column types for consistency.
- Addressing missing data by assigning NA values to appropriate categories (e.g., “N/A” or “Unknown”).
- Standardising demographic fields such as gender, nationality, and relation type for consistency.

```{r Closed Claim Raw Dataset, echo=TRUE, warning=FALSE, message=FALSE}

# Importing all the necessary data-sets

ClosedClaim_2019 <- read_xlsx('C:/Users/au-trn01/OneDrive - BKIC BAHRAIN/Closed Claim GLOBMED/GLOBMED Files/Claim/ClosedClaim_2019.xlsx')
ClosedClaim_2020 <- read_xlsx('C:/Users/au-trn01/OneDrive - BKIC BAHRAIN/Closed Claim GLOBMED/GLOBMED Files/Claim/ClosedClaim_2020.xlsx')
ClosedClaim_2021 <- read_xlsx('C:/Users/au-trn01/OneDrive - BKIC BAHRAIN/Closed Claim GLOBMED/GLOBMED Files/Claim/ClosedClaim_2021.xlsx')
ClosedClaim_2022 <- read_xlsx('C:/Users/au-trn01/OneDrive - BKIC BAHRAIN/Closed Claim GLOBMED/GLOBMED Files/Claim/ClosedClaim_2022.xlsx')
ClosedClaim_2023 <- read_xlsx('C:/Users/au-trn01/OneDrive - BKIC BAHRAIN/Closed Claim GLOBMED/GLOBMED Files/Claim/ClosedClaim_2023.xlsx')
Provider_Network <- read_xlsx('C:/Users/au-trn01/OneDrive - BKIC BAHRAIN/Closed Claim GLOBMED/GLOBMED Files/Poduction Variables/Provider Network.xlsx')
Production <- read_xlsx('C:/Users/au-trn01/OneDrive - BKIC BAHRAIN/Closed Claim GLOBMED/GLOBMED Files/Poduction Variables/GroupID.xlsx')
GroupID <- Production
icd10_code <- read_xlsx('C:/Users/au-trn01/OneDrive - BKIC BAHRAIN/Closed Claim GLOBMED/GLOBMED Files/icd10_code.xlsx')

ClosedClaim_2020$`BROKER NAME`<- as.character(ClosedClaim_2020$`BROKER NAME`)
ClosedClaim_2021$`BROKER NAME`<- as.character(ClosedClaim_2021$`BROKER NAME`)
ClosedClaim_2022$`BROKER NAME`<- as.character(ClosedClaim_2022$`BROKER NAME`)
ClosedClaim_2023$`BROKER NAME`<- as.character(ClosedClaim_2023$`BROKER NAME`)
ClosedClaim_2020$`STAFF`<- as.character(ClosedClaim_2020$`STAFF`)
ClosedClaim_2021$`STAFF`<- as.character(ClosedClaim_2021$`STAFF`)
ClosedClaim_2022$`STAFF`<- as.character(ClosedClaim_2022$`STAFF`)
ClosedClaim_2023$`STAFF`<- as.character(ClosedClaim_2023$`STAFF`)
ClosedClaim_2019$`DOCTOR CODE`<- as.character(ClosedClaim_2019$`DOCTOR CODE`)
ClosedClaim_2020$`DOCTOR CODE`<- as.character(ClosedClaim_2020$`DOCTOR CODE`)
ClosedClaim_2021$`DOCTOR CODE`<- as.character(ClosedClaim_2021$`DOCTOR CODE`)
ClosedClaim_2022$`DOCTOR CODE`<- as.character(ClosedClaim_2022$`DOCTOR CODE`)
ClosedClaim_2023$`DOCTOR CODE`<- as.character(ClosedClaim_2023$`DOCTOR CODE`)
ClosedClaim_2019$`LETTER`<- as.character(ClosedClaim_2019$`LETTER`)
ClosedClaim_2020$`LETTER`<- as.character(ClosedClaim_2020$`LETTER`)
ClosedClaim_2021$`LETTER`<- as.character(ClosedClaim_2021$`LETTER`)
ClosedClaim_2022$`LETTER`<- as.character(ClosedClaim_2022$`LETTER`)
ClosedClaim_2023$`LETTER`<- as.character(ClosedClaim_2023$`LETTER`)
ClosedClaim_2019$`CHECK`<- as.character(ClosedClaim_2019$`CHECK`)
ClosedClaim_2020$`CHECK`<- as.character(ClosedClaim_2020$`CHECK`)
ClosedClaim_2021$`CHECK`<- as.character(ClosedClaim_2021$`CHECK`)
ClosedClaim_2022$`CHECK`<- as.character(ClosedClaim_2022$`CHECK`)
ClosedClaim_2023$`CHECK`<- as.character(ClosedClaim_2023$`CHECK`)
ClosedClaim_2019$`LETTER DATE`<- as.character(ClosedClaim_2019$`LETTER DATE`)
ClosedClaim_2020$`LETTER DATE`<- as.character(ClosedClaim_2020$`LETTER DATE`)
ClosedClaim_2021$`LETTER DATE`<- as.character(ClosedClaim_2021$`LETTER DATE`)
ClosedClaim_2022$`LETTER DATE`<- as.character(ClosedClaim_2022$`LETTER DATE`)
ClosedClaim_2023$`LETTER DATE`<- as.character(ClosedClaim_2023$`LETTER DATE`)
ClosedClaim_2019$`CHECK DATE`<- as.character(ClosedClaim_2019$`CHECK DATE`)
ClosedClaim_2020$`CHECK DATE`<- as.character(ClosedClaim_2020$`CHECK DATE`)
ClosedClaim_2021$`CHECK DATE`<- as.character(ClosedClaim_2021$`CHECK DATE`)
ClosedClaim_2022$`CHECK DATE`<- as.character(ClosedClaim_2022$`CHECK DATE`)
ClosedClaim_2023$`CHECK DATE`<- as.character(ClosedClaim_2023$`CHECK DATE`)
ClosedClaim_2019$`INVOICE`<- as.character(ClosedClaim_2019$`INVOICE`)
ClosedClaim_2020$`INVOICE`<- as.character(ClosedClaim_2020$`INVOICE`)
ClosedClaim_2021$`INVOICE`<- as.character(ClosedClaim_2021$`INVOICE`)
ClosedClaim_2022$`INVOICE`<- as.character(ClosedClaim_2022$`INVOICE`)
ClosedClaim_2023$`INVOICE`<- as.character(ClosedClaim_2023$`INVOICE`)
ClosedClaim_2019$`LOS`<- as.numeric(ClosedClaim_2019$`LOS`)
ClosedClaim_2020$`LOS`<- as.numeric(ClosedClaim_2020$`LOS`)
ClosedClaim_2021$`LOS`<- as.numeric(ClosedClaim_2021$`LOS`)
ClosedClaim_2022$`LOS`<- as.numeric(ClosedClaim_2022$`LOS`)
ClosedClaim_2023$`LOS`<- as.numeric(ClosedClaim_2023$`LOS`)
ClosedClaim_2019$`ESTIMATED`<- as.numeric(ClosedClaim_2019$`ESTIMATED`)
ClosedClaim_2020$`ESTIMATED`<- as.numeric(ClosedClaim_2020$`ESTIMATED`)
ClosedClaim_2021$`ESTIMATED`<- as.numeric(ClosedClaim_2021$`ESTIMATED`)
ClosedClaim_2022$`ESTIMATED`<- as.numeric(ClosedClaim_2022$`ESTIMATED`)
ClosedClaim_2023$`ESTIMATED`<- as.numeric(ClosedClaim_2023$`ESTIMATED`)
ClosedClaim_2019$`REFERENCE`<- as.character(ClosedClaim_2019$`REFERENCE`)
ClosedClaim_2020$`REFERENCE`<- as.character(ClosedClaim_2020$`REFERENCE`)
ClosedClaim_2022$`REFERENCE`<- as.character(ClosedClaim_2022$`REFERENCE`)
ClosedClaim_2023$`REFERENCE`<- as.character(ClosedClaim_2023$`REFERENCE`)
ClosedClaim_2019$`BRANCH`<- as.character(ClosedClaim_2019$`BRANCH`)
ClosedClaim_2020$`BRANCH`<- as.character(ClosedClaim_2020$`BRANCH`)
ClosedClaim_2022$`BRANCH`<- as.character(ClosedClaim_2022$`BRANCH`)
ClosedClaim_2023$`BRANCH`<- as.character(ClosedClaim_2023$`BRANCH`)
ClosedClaim_2019$`MEDICAL FILE`<- as.character(ClosedClaim_2019$`MEDICAL FILE`)
ClosedClaim_2020$`MEDICAL FILE`<- as.character(ClosedClaim_2020$`MEDICAL FILE`)
ClosedClaim_2021$`MEDICAL FILE`<- as.character(ClosedClaim_2021$`MEDICAL FILE`)
ClosedClaim_2022$`MEDICAL FILE`<- as.character(ClosedClaim_2022$`MEDICAL FILE`)
ClosedClaim_2023$`MEDICAL FILE`<- as.character(ClosedClaim_2023$`MEDICAL FILE`)

ClosedClaim<- bind_rows(ClosedClaim_2019,
                        ClosedClaim_2020,
                        ClosedClaim_2021,
                        ClosedClaim_2022,
                        ClosedClaim_2023)

ClosedClaim$`Admission Year`<- year(ClosedClaim$`ADMISSION DATE`)
ClosedClaim$`Admission Month`<- month(ClosedClaim$`ADMISSION DATE`)
ClosedClaim$`GENDER`<-as.character(ClosedClaim$`GENDER`)

rm(list=c("ClosedClaim_2019","ClosedClaim_2020","ClosedClaim_2021","ClosedClaim_2022", "ClosedClaim_2023", "Production"))

ClosedClaim <- clean_names(ClosedClaim)

# Provider<-ClosedClaim%>%select(provider_name)
# Provider<-unique(Provider)

# write_xlsx(Provider, path = 'C:/Users/au-trn01/OneDrive - BKIC BAHRAIN/Medical/GLOBMED/GMD Provider.xlsx')

ClosedClaim<- inner_join(Provider_Network,ClosedClaim, by = "provider")
ClosedClaim<- inner_join(x = GroupID, y = ClosedClaim, by = "group_id")
colnames(ClosedClaim)[40]<-"length_of_stay"
```

# 4. ICD Code Grouped
The ICD (International Classification of Diseases) codes are standardised identifiers for health conditions, enabling consistent classification of diseases. In this section, we map closed claim data to grouped ICD blocks, allowing for a more accessible analysis of claims by disease type. This process involves extracting a disease block code, replacing specific codes with "N/A," then joining it with a reference dataset to categorize the claims data.

## Code Explanation
```{r, echo=TRUE, warning=FALSE, message=FALSE}
# Step 1: Extract the ICD disease block from the first character of the 'disease' column
ClosedClaim <- ClosedClaim %>% mutate(icd_disease_block = substr(ClosedClaim$disease, 1, 1))

# Step 2: Replace specific values in 'icd_disease_block' with "N/A" for categories not needed
ClosedClaim$icd_disease_block <- str_replace(ClosedClaim$icd_disease_block, "2", "N/A")
ClosedClaim$icd_disease_block <- str_replace(ClosedClaim$icd_disease_block, "3", "N/A")
ClosedClaim$icd_disease_block <- str_replace(ClosedClaim$icd_disease_block, "4", "N/A")
ClosedClaim$icd_disease_block <- str_replace(ClosedClaim$icd_disease_block, "7", "N/A")
ClosedClaim$icd_disease_block <- str_replace(ClosedClaim$icd_disease_block, "8", "N/A")

# Step 3: Join the 'ClosedClaim' data with 'icd10_code' to map grouped ICD descriptions
ClosedClaim <- left_join(ClosedClaim, icd10_code, by = 'icd_disease_block')

# Step 4: Remove unnecessary datasets to free memory
rm(list = c("Provider_Network", "GroupID", "icd10_code"))

# Step 5: Display the first few rows of the cleaned and merged 'ClosedClaim' data
head(ClosedClaim) %>%
  kable(caption = 'Table 1: GLOBMED Closed Claim 2019-2023 Raw Data', position = "center") %>%
  kable_classic_2(full_width = F, html_font = "Cambria") %>%
  kable_styling(full_width = F, bootstrap_options = c("striped", "hover", "condensed"), 
                position = "left", font_size = 12, fixed_thead = T) %>%
  column_spec(1, bold = T, border_right = T, color = "black", background = "lightgrey")
```

# 5. Data Cleaning
This section focuses on preparing the claims data for analysis by handling missing values, standardizing variable formats, and deriving valuable groupings. Data cleaning is essential to ensure accurate analysis and 
interpretation.

```{r Data Cleaning, echo=TRUE, warning=FALSE, message=FALSE}

# Step 1: Selecting Relevant Variables
# Selecting specific columns from the 'ClosedClaim' dataset to create 'Variables_grouped'
Variables_grouped <- ClosedClaim %>% 
  select("ind", "group_id", "group_id_description", "workplace_type", "provider", 
         "provider_name", "type", "network", "doctor_name", "underwriting_year", 
         "age", "age_band", "gender", "nationality", "categ", "chapter", 
         "icd_disease_block", "start_of_block", "end_of_block", "icd_grouped", 
         "icd_disease_description", "subcateg_id", "subcateg_desc", "visa_type", 
         "visa_type_description", "admission_month", "admission_year", "relation", 
         "service", "service_description", "length_of_stay", "mcn_nbr", 
         "ssnbr_visanbr", "cov_risknet")

# Step 2: Handling Missing Values
# Converting 'length_of_stay' to numeric and filling NA values in 'visa_type' and character fields
Variables_grouped$length_of_stay <- as.numeric(Variables_grouped$length_of_stay)
Variables_grouped$`visa_type` <- replace(Variables_grouped$`visa_type`, is.na(Variables_grouped$`visa_type`), "N/A")
Variables_grouped <- Variables_grouped %>% mutate_if(is.character, ~replace_na(., "Not Applicable"))
Variables_grouped$length_of_stay <- replace(Variables_grouped$length_of_stay, is.na(Variables_grouped$length_of_stay), 0)

# Replacing unavailable doctor names with 'Unknown'
Variables_grouped$`doctor_name` <- str_replace(Variables_grouped$`doctor_name`, "NOT X AVAILABLE", "Unknown")

# Step 3: Creating Age Bands
# Assigning age into categories (bands) for analysis
Variables_grouped$`age_band` <- cut(Variables_grouped$age, breaks = c(0,5,18,19,30,35,40,45,50,55,60,65,70,80,90), right = F)
Variables_grouped$`age_band` <- as.character(Variables_grouped$`age_band`)

# Step 4: Setting Relationship Type
# Defining 'relation' based on the principal and individual claims
Variables_grouped$`relation` <- ifelse(ClosedClaim$principal > ClosedClaim$ind, "Principal", 
                                       ifelse(ClosedClaim$principal < ClosedClaim$ind, "Family Member", "Principal"))

# Step 5: Categorizing Nationality
# Assigning specified nationality groups
Variables_grouped$nationality <- ifelse(Variables_grouped$nationality == "UNSPECIFIED", "NON-BAHRAINI",
                                        ifelse(Variables_grouped$nationality == "BAHRAIN", "BAHRAINI", "NON-BAHRAINI"))

# Step 6: Identifying Maternity and Pre-Existing Conditions
# Setting 'benefit' to 'Maternity' if the claim contains specific DRC codes, and assigning pre-existing indicators
Variables_grouped <- Variables_grouped %>% mutate(benefit = `subcateg_desc`)
Variables_grouped[grepl("515|516|517|518|527", ClosedClaim$drc), "benefit"] <- "Maternity"
Variables_grouped$pre_existing_indicator <- "NonPreExisting"
Variables_grouped$pre_existing_indicator[grepl("664", ClosedClaim$drc)] <- "PreExisting"

# Step 7: Calculating Service and Claim Counts
# Grouping claims by 'ssnbr_visanbr' and counting services, then by 'mcn_nbr' for cumulative claim counts
Variables_grouped <- Variables_grouped %>% group_by(ssnbr_visanbr) %>% mutate(service_count = 1) %>% ungroup()
Variables_grouped <- Variables_grouped %>% group_by(`mcn_nbr`) %>% mutate(claim_count = cumsum(1:n())) %>% ungroup()
Variables_grouped$claim_count[Variables_grouped$claim_count > 1] <- 0

# Step 8: Removing Temporary Data
# Cleaning up the environment by removing the original 'ClosedClaim' dataset
rm(ClosedClaim)

# Step 9: Displaying Cleaned Data
# Displaying the first rows of the cleaned data
head(Variables_grouped) %>%
  kable(caption = "Table 2: Cleaned Data Closed Claims 2019-2023") %>%
  kable_classic_2(full_width = F, html_font = "Cambria") %>%
  kable_styling(full_width = F, bootstrap_options = c("striped", "hover", "condensed"), 
                position = "left", font_size = 12, fixed_thead = T) %>%
  column_spec(1, bold = T, border_right = T, color = "black", background = "lightgrey")
```
## Summary of Steps:
1. Selecting Variables: Only relevant columns are retained for analysis.
2. Handling Missing Values: Missing values are replaced with relevant indicators, such as “N/A” or “Not Applicable.”
3. Age Band Creation: The age_band variable categorizes age for segmented analysis.
4. Relationship Assignment: Determines each claim's relation (Principal or Family Member).
5. Nationality Grouping: Nationality is classified into defined categories.
6. Maternity and Pre-Existing Conditions: Specific claim codes identify maternity benefits and pre-existing conditions.
7. Claim and Service Counts: Counts services and claims to add to data granularity.
8. Data Cleanup: Temporary data are removed to optimize memory.
9. Data Display: A preview of the cleaned dataset is provided in a formatted table.

This data-cleaning process transforms raw data into a structured format suitable for further analysis, ensuring consistency and usability for subsequent steps in the project.


```{r Predictor Variables, echo=TRUE, warning=FALSE, message=FALSE}

Variables_summary<-Variables_grouped%>%group_by_("group_id_description", "workplace_type", "provider_name", "type", "network", "age", "age_band", "gender", "nationality", "categ", "visa_type", "visa_type_description", "chapter","icd_disease_block","icd_grouped", "icd_disease_description", "doctor_name", "benefit", "pre_existing_indicator", "underwriting_year","admission_month", "admission_year", "relation", "length_of_stay")%>%
  mutate(grossclaim=sum(`cov_risknet`), claimcount=sum(`claim_count`), servicecount=service_count, severity=sum(`cov_risknet`)/sum(`claim_count`))%>%ungroup()

Variables_summary$rs<-rowSums(Variables_summary[,c("grossclaim")])

Variables_summary<-Variables_summary%>%filter(rs != 0)
Variables_summary$rs<- NULL

Variables_summary<-Variables_summary[,-32:-33]
Variables_summary<-Variables_summary[,-35:-36]
Variables_summary$severity[is.infinite(Variables_summary$severity)]<-NA

head(Variables_summary)%>%kable(caption = "Table 3: Predictor Variables")%>%kable_classic_2(full_width=F, html_font = "Cambria")%>%kable_styling(full_width = F, bootstrap_options = c("striped", "hover", "condensed"), position = "left", font_size = 12, fixed_thead = T)%>%column_spec(1, bold = T, border_right = T, color = "black", background = "lightgrey")

rm("Variables_grouped")
```

# 6. Exploratory Data Analysis (EDA)
This section of the analysis explores gross claim trends from 2019 to 2023. By examining the distribution, trends, and relationships within the dataset, we gain insights into factors influencing gross claim amounts.

Key Analyses
- Distribution Analysis: Visualizes the distribution of gross claims across demographic variables such as age and gender.
- Trend Analysis: Line plots highlight gross claim trends across years.
- Cross-Variable Comparisons: Examines relationships between variables such as age, gender, and length of stay.
  
```{r Gross Claim Analysis, warning=FALSE, message=FALSE, echo=TRUE, results='asis'}

# Gross Claim Analysis by Variable, 2019-2023

var <- c("group_id_description", "workplace_type", "provider_name", "type", "network", "doctor_name", 
         "underwriting_year", "age", "age_band", "gender", "icd_grouped", "icd_disease_description", 
         "nationality", "categ", "benefit", "pre_existing_indicator", "visa_type_description", 
         "relation", "length_of_stay")

for (i in var) {
  # Group data by variable `i` and admission year, then calculate total gross claims
  a <- Variables_summary %>% 
       group_by_(i, "admission_year") %>% 
       summarise(grossclaim = sum(grossclaim)) %>% 
       ungroup() %>%
       arrange(admission_year)
  
  # Reshape data to have years as columns and fill missing values with 0
  a <- pivot_wider(a, id_cols = i, names_from = "admission_year", values_from = "grossclaim", values_fill = 0)
  
  # Rename columns for readability
  colnames(a)[2:6] <- paste0("gross_claim_", colnames(a)[2:6])
  
  # Format claim counts for easy readability
  for (year in 2019:2023) {
    a[[paste0("gross_claim_", year)]] <- prettyNum(a[[paste0("gross_claim_", year)]], big.mark = ',')
  }
  
  # Display formatted table with custom styling
  print(knitr::kable(a) %>% 
          kable_classic_2(full_width = FALSE, html_font = "Cambria") %>% 
          kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover", "condensed"), 
                        position = "left", font_size = 12, fixed_thead = TRUE) %>% 
          column_spec(1, bold = TRUE, border_right = TRUE, color = "black", background = "lightgrey"))
}
```
## Visualisations:

```{r Gross Claim Analysis Visualisations, warning=FALSE, message=FALSE, echo=TRUE, results='asis'}

### Histogram of Gross Claim, 2019-2023

Variables_summary%>%ggplot(aes(x=grossclaim,fill=as.factor(admission_year)))+geom_histogram(alpha=0.5)+scale_x_log10()+labs(x = "Gross Claim", title = "Gross Claim 2019-2023")+scale_fill_discrete(name = "Admission Year")

### Boxplot of Gross Claim by Provider Network, 2019-2023

Variables_summary%>%group_by_("provider_name", "admission_year", "network")%>%summarise(grossclaim=sum(grossclaim))%>%ggplot(aes(x=network, y=grossclaim, fill=network, color=network))+geom_boxplot(alpha=0.5)+labs(x = "Network", y = "Gross Claim", title = "Log Scaled Boxplot for Gross Claim by Provider Network, 2019-2023")+facet_wrap("admission_year", scales = "free_y")+scale_y_log10()

### Histogram of Gross Claim by Nationality, 2019-2023

Variables_summary%>%ggplot(aes(x=grossclaim,fill=nationality))+geom_histogram(alpha=0.5)+scale_x_log10()+labs(x = "Gross Claim", title = "Gross Claim by Nationality, 2019-2023")+scale_fill_discrete(name = "Nationality")+facet_wrap("admission_year")

### Histogram of In and Out Patient Gross Claim, 2019-2023

Variables_summary%>%filter(categ=="In-Patient")%>%ggplot(aes(x=grossclaim,fill=as.factor(admission_year)))+geom_histogram(alpha=0.5)+scale_x_log10()+labs(title = "In-Patient Gross Claim, 2019-2023", x = "Gross Claim")

Variables_summary%>%filter(categ=="Out-patient")%>%ggplot(aes(x=grossclaim,fill=as.factor(admission_year)))+geom_histogram(alpha=0.5)+scale_x_log10()+labs(title = "Out-Patient Gross Claim, 2019-2023", x = "Gross Claim")

### Q-Q Plot Comparing Normal Distribution of Gross Claim and In-Patient/        ### Out-Patient Claims, 2019-2023

Variables_summary%>%group_by_("categ", "admission_year", "ind")%>%summarise(grossclaim=sum(grossclaim))%>%ggplot(aes(sample = grossclaim, color = categ))+stat_qq()+stat_qq_line()+facet_wrap("admission_year")+labs(title = "Normally Distributed Q-Q Plot on Gross Claim and In-Patient/Out-Patient Claims", x= "z-scores", y = "Claim Count")

### Distribution of Gross Claim, by Age, 2019-2023

Variables_summary%>%ggplot(aes(x=age, y=grossclaim, color = admission_year))+geom_col(alpha=0.5)+scale_y_log10()+labs(x = "Age", y="Gross Claim", title = "Gross Claim by Age, 2019-2023")+facet_wrap("admission_year", scales = "free_y")

### Distribution of Gross Claim, by Gender, 2019-2023

Variables_summary%>%ggplot(aes(x=grossclaim,fill=gender))+geom_histogram(alpha=0.5)+scale_x_log10()+labs(x = "Gross Claim", title = "Gross Claim by Gender, 2019-2023")+scale_fill_discrete(name = "Gender")+facet_wrap("admission_year", scales = "free_y")

### Scatter Plot of Gross Claim and Age by Gender, 2019-2023

Variables_summary%>%group_by_("gender", "admission_year", "age")%>%summarise(grossclaim=sum(grossclaim))%>%ggplot(aes(x=age, y=grossclaim, color=gender, fill=gender))+geom_point()+geom_smooth()+labs(x = "Age", y = "Gross Claim", title = "Gross Claim and Age by Gender, 2019-2023")+facet_wrap("admission_year", scales = "free_y")

### Distribution of Gross Claim by Relation, 2019-2023

Variables_summary%>%ggplot(aes(x=grossclaim,fill=relation))+geom_histogram(alpha=0.5)+scale_x_log10()+labs(x = "Gross Claim", title = "Gross Claim by Relation, 2019-2023")+scale_fill_discrete(name = "Relation")+facet_wrap("admission_year", scales = "free_y")

### Scatter Plot of Gross Claim and Age by Relation, 2019-2023

Variables_summary%>%group_by_("relation", "admission_year", "age")%>%summarise(grossclaim=sum(grossclaim))%>%ggplot(aes(x=age, y=grossclaim, color=relation, fill=relation))+geom_point()+geom_smooth()+labs(x = "Age", y = "Gross Claim", title = "Gross Claim and Age by Relation, 2019-2023")+facet_wrap("admission_year", scales = "free_y")

### Scatter Plot of Gross Claim and Age by Pre-Existing Indicator, 2019-2023

Variables_summary%>%group_by_("pre_existing_indicator", "admission_year", "age")%>%summarise(grossclaim=sum(grossclaim))%>%ggplot(aes(x=age, y=grossclaim, color=pre_existing_indicator, fill=pre_existing_indicator))+geom_point()+geom_smooth()+labs(x = "Age", y = "Gross Claim", title = "Gross Claim and Age by Pre-Existing Indicator, 2019-2023")+facet_wrap("admission_year", scales = "free_y")

### Line Graph of Gross Claim and Benefit by Age, 2019-2023

Variables_summary%>%group_by_("benefit", "admission_year", "age")%>%summarise(grossclaim=sum(grossclaim))%>%ggplot(aes(x=age, y=grossclaim, color=benefit, fill=benefit))+geom_line(alpha=0.15)+geom_smooth()+labs(x = "Age", y = "Gross Claim", title = "Gross Claim and Age by Benefit, 2019-2023")+facet_wrap("admission_year", scales = "free_y")

### Scatter Plot of Gross Claim by Length of Stay, 2019-2023 

Variables_summary%>%group_by_("length_of_stay", "admission_year", "categ")%>%summarise(grossclaim=sum(grossclaim))%>%arrange(admission_year)%>%ggplot(aes(x=length_of_stay,y=grossclaim,color=admission_year))+geom_point()+facet_wrap("admission_year", scales = "free_y")+labs(title = "Gross Claim by Length of Stay, 2019-2023", color = "Admission Year")+geom_smooth()

```
## Summary
The EDA provides a comprehensive view of claim patterns over time. The visualisations and analyses of gross claim distribution by demographics and categories (such as inpatient vs. outpatient) allow for detailed insights. These findings help identify trends and outliers, guiding strategic decision-making and resource allocation within the organisation.

# 7. Claim Count Analysis

This section provides a detailed analysis of claim counts across different variables and demographics. Using grouped data by admission_year, we summarise claim counts for variables such as gender, age, and pre-existing conditions. We visualise the distribution of claim counts with histograms and scatter plots.

```{r Claim Count Analysis, echo=TRUE, warning=FALSE, message=FALSE, results='asis'}
# Claim Count Analysis
for(i in var){
  # Group data by variable `i` and admission year, then sum the claim counts
  b <- Variables_summary %>% 
       group_by_(i, "admission_year") %>% 
       summarise(claimcount = sum(claimcount)) %>% 
       arrange(admission_year)
  
  # Reshape data for readability, filling missing values with 0
  b <- pivot_wider(b, id_cols = i, names_from = "admission_year", values_from = "claimcount", values_fill = 0)
  
  # Rename columns for clarity
  colnames(b)[2:6] <- paste0("claim_count_", colnames(b)[2:6])
  
  # Format claim counts with comma separators for readability
  b$claim_count_2019 <- prettyNum(b$claim_count_2019, big.mark = ',')
  b$claim_count_2020 <- prettyNum(b$claim_count_2020, big.mark = ',')
  b$claim_count_2021 <- prettyNum(b$claim_count_2021, big.mark = ',')
  b$claim_count_2022 <- prettyNum(b$claim_count_2022, big.mark = ',')
  b$claim_count_2023 <- prettyNum(b$claim_count_2023, big.mark = ',')
  
  # Display the table with customized styling
  print(knitr::kable(b) %>% 
          kable_classic_2(full_width = F, html_font = "Cambria") %>% 
          kable_styling(full_width = F, bootstrap_options = c("striped", "hover", "condensed"), position = "left", font_size = 12, fixed_thead = T) %>% 
          column_spec(1, bold = T, border_right = T, color = "black", background = "lightgrey"))
}
```
# 8. Visualisations II

The analysis includes various visualisations created with ggplot2:

- Gross Claim Distribution: Gross claims per variable (e.g., nationality, gender) across years.
- Severity of Claims Analysis: Severity of claims by demographic variables like age and pre-existing conditions.
- Trend Analysis: Severity of claims over time segmented by admission year and provider network.

```{r Severity Analysis, echo=TRUE, warning=FALSE, results='asis', message=FALSE}

# Severity of Claims per Variable, 2019-2023

for(i in var){
  c<-Variables_summary%>%group_by_(i,"admission_year")%>%summarise(severity=sum(grossclaim)/sum(claimcount))
   c<- c%>%arrange(admission_year)
  c<-pivot_wider(c,id_cols = i, names_from = "admission_year",values_from = "severity" ,values_fill = 0)
  colnames(c)[2:6]<- paste0("severity_",colnames(c)[2:6])
  c$severity_2019<-prettyNum(c$severity_2019, big.mark = ',')
  c$severity_2020<-prettyNum(c$severity_2020, big.mark = ',')
  c$severity_2021<-prettyNum(c$severity_2021, big.mark = ',')
  c$severity_2022<-prettyNum(c$severity_2022, big.mark = ',')
  c$severity_2023<-prettyNum(c$severity_2023, big.mark = ',')
  print(knitr::kable(c)%>%kable_classic_2(full_width=F, html_font = "Cambria")%>%kable_styling(full_width = F, bootstrap_options = c("striped", "hover", "condensed"), position = "left", font_size = 12, fixed_thead = T)%>%column_spec(1, bold = T, border_right = T, color = "black", background = "lightgrey"))
}

### Histogram of Severity (log scaled), 2019-2023

Variables_summary%>%ggplot(aes(x=severity,fill=as.factor(admission_year)))+geom_histogram(alpha=0.5)+scale_y_log10()+labs(x = "Severity", title = "Severity 2019-2023")+scale_fill_discrete(name = "Admission Year")

### Histogram of Severity of Claim in In and Out Patient (log scaled), 2019-2023

Variables_summary%>%filter(categ=="In-Patient")%>%ggplot(aes(x=severity,fill=as.factor(admission_year)))+geom_histogram(alpha=0.5)+scale_x_log10()+labs(title = "In-Patient Severity, 2019-2023", x = "Severity")+scale_fill_discrete(name = "Admission Year")

Variables_summary%>%filter(categ=="Out-patient")%>%ggplot(aes(x=severity,fill=as.factor(admission_year)))+geom_histogram(alpha=0.5)+scale_x_log10()+labs(title = "Out-Patient Severity, 2019-2023", x = "Severity")+scale_fill_discrete(name = "Admission Year")

### Q-Q Plot Comparing Normal Distribution of Severity by In-Patient/Out-Patient ### Claims, 2019-2023

Variables_summary%>%group_by_("categ", "admission_year", "ind")%>%summarise(severity=sum(grossclaim)/sum(claimcount))%>%ggplot(aes(sample = severity, color = categ))+stat_qq()+stat_qq_line()+facet_wrap("admission_year")+labs(title = "Normally Distributed Q-Q Plot on Severity and In-Patient/Out-Patient Claims", x = "z-scores", y = "Severity", color = "Category")

### Line Graph Comparing In-Patient Percentage per Network, 2019-2023

Variables_summary%>%group_by(`admission_year`, `network`)%>%summarise(total = n(), percent_in_patient = mean(`categ` == "In-Patient"))%>%arrange(desc(percent_in_patient))%>%filter(network %in% c("Silver", "Gold", "Diamond", "Platinum", "Restricted"))%>%ungroup()%>%ggplot(aes(x=admission_year,y=percent_in_patient, color = network))+geom_point()+geom_smooth()+labs(title = "In-Patient Percentage per Nework, 2019-2023", x = "Admission Year", y = "Percentage of In-Patient Intake")

### Histogram of Severity of Claim by Gender (log scaled), 2019-2023

Variables_summary%>%ggplot(aes(x=severity,fill=gender))+geom_histogram(alpha=0.5)+scale_y_log10()+labs(x = "Severity", title = "Severity by Gender, 2019-2023")+scale_fill_discrete(name = "Gender")+facet_wrap("admission_year")

### Column Chart of Distribution of Severity (log scaled) by Age and Gender, 2019-2023

Variables_summary%>%ggplot(aes(x=age,y=severity,fill=gender))+geom_col(alpha=0.5)+scale_y_log10()+labs(x = "Age", y="Severity", title = "Severity by Age and Gender, 2019-2023")+scale_fill_discrete(name = "Gender")+facet_wrap("admission_year")

### Q-Q Plot Comparing Normal Distribution of Severity by Gender, 2019-2023

Variables_summary%>%group_by_("gender", "admission_year", "ind")%>%summarise(severity=sum(grossclaim)/sum(claimcount))%>%ggplot(aes(sample = severity, color = gender))+stat_qq()+stat_qq_line()+facet_wrap("admission_year", scales = "free_y")+labs(title = "Normally Distributed Q-Q Plot on Severity by Gender", x = "z-scores", y = "Severity", color = "Gender")

### Distribution of Severity by Nationality (log scaled), 2019-2023

Variables_summary%>%ggplot(aes(x=severity,fill=nationality))+geom_histogram(alpha=0.5)+scale_y_log10()+labs(x = "Severity", title = "Severity by Nationality, 2019-2023")+scale_fill_discrete(name = "Nationality")+facet_wrap("admission_year", scales = "free_y")

### Scatter plot of Severity of Claim by Length of Stay, 2019-2023

Variables_summary%>%group_by_("length_of_stay", "admission_year", "categ")%>%summarise(severity=sum(grossclaim)/sum(claimcount))%>%arrange(admission_year)%>%ggplot(aes(x=length_of_stay,y=severity,color=admission_year))+geom_point()+facet_wrap("admission_year", scales = "free_y")+labs(title = "Severity of Claims by Length of Stay, 2019-2023", color = "Admission Year")+geom_smooth()

### Scatter Plot of Severity of Claim and Pre-Existing Condition by Age, 2019-2023

Variables_summary%>%group_by_("pre_existing_indicator", "admission_year", "age")%>%summarise(severity=sum(grossclaim)/sum(claimcount))%>%ggplot(aes(x=age, y=severity, color=pre_existing_indicator, fill=pre_existing_indicator))+geom_point(alpha=0.5)+geom_smooth()+labs(x = "Age", y = "Severity", title = "Severity of Claim by Pre-Existing Indicator and Age, 2019-2023")+facet_wrap("admission_year", scales = "free_y")

### Area Graph of the Severity of Claim and Benefit by Age, 2019-2023

Variables_summary%>%group_by_("benefit", "admission_year", "age")%>%summarise(severity=sum(grossclaim)/sum(claimcount))%>%ggplot(aes(x=age, y=severity, color=benefit, fill=benefit))+geom_line(alpha=0.15)+geom_smooth()+labs(x = "Age", y = "Severity", title = "Severity of Claim by Benefit and Age, 2019-2023")+facet_wrap("admission_year", scales = "free_y")+scale_y_log10()

### Scatter Plot of the Severity of Claim by Principal and Family Member, 2019-2023

Variables_summary%>%group_by_("relation", "admission_year", "age")%>%summarise(severity=sum(grossclaim/sum(claimcount)))%>%ggplot(aes(x=age, y=severity, color=relation, fill=relation))+geom_point(alpha=0.5)+geom_smooth()+labs(x = "Age", y = "Severity", title = "Severity by Relation and Age, 2019-2023")+facet_wrap("admission_year")
```
# 9. Predictive Modelling

A decision tree model is developed to predict claim severity using factors such as admission year, network, age, and gender. Modelling steps include:

1. Splitting the data into training and testing sets with an 80/20 ratio.
2. Training a decision tree model with rpart, using a complexity parameter (cp) of 0.001.
3. Visualising the model to identify critical predictors for claim severity.

```{r, echo=TRUE, warning=FALSE, message=FALSE}

# Here is the code to create a Decision Tree Model

set.seed(123)
sample_size = floor(0.8*nrow(Variables_summary))
select_rows = sample(seq_len(nrow(Variables_summary)), size = sample_size)
ClosedClaim2_training = Variables_summary[`select_rows`,]
ClosedClaim2_testing = Variables_summary[-`select_rows`,]
nrow(ClosedClaim2_training)
nrow(ClosedClaim2_testing)

complex_model<-rpart(severity ~ `admission_year` + `type` + `network` + `underwriting_year` + `age` + `gender` + `nationality` + `categ` + `benefit` + `pre_existing_indicator` + `icd_grouped` + `relation` + `length_of_stay`, cp = 0.001, data = ClosedClaim2_training)

options(repr.plot.width = 40, repr.plot.height = 10)
rpart.plot(complex_model, type = 1, digits = 5, fallen.leaves = TRUE)
```
# 8. Future Enhancements

Future directions for this analysis include:

1. Expanded Modelling: Experimenting with alternative models like random forests and logistic regression.
2. Advanced Cleaning: Incorporating additional validation techniques to enhance model accuracy.
3. Real-Time Reporting: Implementing a more advanced Shiny dashboard to support dynamic and real-time data analytics.
