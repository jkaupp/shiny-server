library(rio)
library(readxl)
suppressMessages(library(zoo))
library(magrittr)
library(plyr)
suppressMessages(library(dplyr))
suppressMessages(library(tidyr))
library(stringr)

#nsse_data <- "/Users/Jake/ownCloud/FEAS/QUQAP/Unit Data/OIRP/01_04-SF-Student Learning Experience-NSSE National 2014 - Means - Program Groups - Engineering (Mar 2015).xlsx"
# lookup <- data_frame(sheet = c(excel_sheets(nsse_data)), dept = c("National", "APSC", "BMEE", "CHEE", "CIVL", "CMPE", "ELEC", "ENVR", "MECH", "METL", "MINE", "OTHR"))
# 
# Queens<-c("APSC", "CHEE", "CIVL", "ELEC", "CMPE", "MECH", "MINE")
# 
#  Comparators <-c(
# "Alberta",
# "Calgary",
# "Dalhousie",
# "Laval",
# "Guelph",
# "Manitoba",
# "McGill",
# "McMaster",
# "Ottawa",
# "Queen's",
# "Saskatchewan",
# "UBC",
# "Waterloo",
# "Western"
# )

nsse_data <- "./data/NSSE.xlsx"


nsse_indicators <-
  data_frame(
    Indicator = c("CL","DD","ET","HO","LS","QI","QR","RI","SE","SF"), Description =
      c(
        "Collaborative Learning","Discussions with Diverse Others","Effective Teaching Practises","Higher-Order Learning","Learning Strategies","Quality of Interactions","Quantiative Reasoning","Reflective & Integrative Learning","Supportive Environment","Student-Faculty Interaction"
      )
  )


# Function to read the OIRP nsse summary data and convert it to a useful form.
nsse_extract <- function(sheet) {
  import(nsse_data, sheet = sheet) %>%
    set_colnames(c("Prompt", "Drop", "Year", "Variable", .[3, c(5:ncol(.))])) %>%
    select(-Drop) %>% 
    mutate(Dept = .[2,1]) %>% 
    slice(-1:-3) %>% 
    mutate(Prompt = na.locf(Prompt),
           Year = na.locf(Year)) %>%
    gather("School", "Value", -Prompt, -Year, -Variable, -Dept) %>%
    mutate(Value = ifelse(Value == ".", NA, Value)) %>%
    spread(Variable, Value) %>%
    mutate(
      Item = str_extract(Prompt, "\\d+\\w"),
      Code = ifelse(
        grepl("Indicator", Prompt), "Engagement Indicator", str_extract(Prompt, "(?:\\()\\w+(?:\\))") %>%
          str_replace("\\(","") %>%
          str_replace("\\)","")
      ),
      Indicator = ifelse(
        grepl("HIP", Prompt), "HIP",
        ifelse(
          grepl("Indicator", Prompt), str_extract(Prompt, "[A-Z]{2}"), str_extract(Code, "[A-Z]{2}")
        )
      ),
      Prompt = str_replace(Prompt, "\\d+\\w\\.", "") %>%
        str_replace(.,  "\\(.+?\\)", ""),
      School = as.character(School)
    )
}


# Apply the function to the raw nsse data and change variable types ----
nsse_results <- lapply(excel_sheets(nsse_data),nsse_extract) %>%
  bind_rows() %>%
  mutate(
    Mean = as.numeric(Mean),
    Year = factor(Year, levels=c("FY","SY"),labels=c("First Year","Senior Year")),
    School = factor(School),
    `Std Dev` = as.numeric(`Std Dev`),
    N = as.numeric(N),
    Color = ifelse(School=="Hogwarts","#3B9AB2","#333333"),
    Code = factor(Code)
  ) %>%
  dplyr::rename(sd=`Std Dev`) %>% 
  left_join(nsse_indicators, by="Indicator")

institution_list <- unique(as.character(nsse_results$School))
department_list <- unique(as.character(nsse_results$Dept))

