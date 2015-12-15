library(rio)
library(readxl)
library(zoo)
library(magrittr)
library(plyr)
library(dplyr)
library(tidyr)
library(stringr)

# Directory and File Declarations

nsse_data <- file.path(".","data","NSSE.xlsx")


# Create the Full Description of the NSSE Indicators
nsse_indicators <-
  data_frame(
    Indicator = c("CL","DD","ET","HO","LS","QI","QR","RI","SE","SF"), Description =
      c(
        "Collaborative Learning","Discussions with Diverse Others","Effective Teaching Practises","Higher-Order Learning","Learning Strategies","Quality of Interactions","Quantiative Reasoning","Reflective & Integrative Learning","Supportive Environment","Student-Faculty Interaction"
      )
  )


# Create Report Layouts by Indicator for Program Breakdown----
CL_layout <- rbind(c(5,5,5,5),
                   c(1,1,2,2),
                   c(3,3,4,4))

QI_layout <- rbind(c(6,6,6,6),
                   c(1,1,2,2),
                   c(3,3,4,4),
                   c(5,5,NA,NA))

SE_layout <- rbind(c(9,9,9,9),
                   c(1,1,2,2),
                   c(3,3,4,4),
                   c(5,5,6,6),
                   c(7,7,8,8))

RI_layout <- rbind(c(8,8,8,8),
                   c(1,1,2,2),
                   c(3,3,4,4),
                   c(5,5,6,6),
                   c(7,7,NA,NA))

SF_layout <- rbind(c(5,5,5,5),
                   c(1,1,2,2),
                   c(3,3,4,4))

HO_layout <- rbind(c(5,5,5,5),
                   c(1,1,2,2),
                   c(3,3,4,4))

ET_layout <- rbind(c(6,6,6,6),
                   c(1,1,2,2),
                   c(3,3,4,4),
                   c(5,5,NA,NA))

QR_layout <- rbind(c(4,4,4,4),
                   c(1,1,2,2),
                   c(3,3,NA,NA))

DD_layout <- rbind(c(5,5,5,5),
                   c(1,1,2,2),
                   c(3,3,4,4))

LS_layout <- rbind(c(4,4,4,4),
                   c(1,1,2,2),
                   c(3,3,NA,NA))

# Create the Layout Frame for gridArrange
layout_frame <-
  list(
    CL_layout = CL_layout,QI_layout = QI_layout,SE_layout = SE_layout,RI_layout =
      RI_layout,SF_layout = SF_layout,HO_layout = HO_layout,ET_layout = ET_layout,QR_layout =
      QR_layout,DD_layout = DD_layout,LS_layout = LS_layout
  )




# Function to read the OIRP nsse summary data and convert it to a useful form.
nsse_extract <- function(df, sheet) {
  import(df, sheet = sheet) %>%
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

institution_list <-
  c(
    "Jordan College", "Battle School", "Unseen University", "Wayside School",
    "Brakebills", "Bayside", "Hampden College", "Xavier Institute",
    "Starfleet Academy"
  )

department_list <- c("Underwater Basket Weaving", "Handwavium Processing", "Macaroni Art"
)

nsse_results <- lapply(excel_sheets(nsse_data), nsse_extract, df = nsse_data) %>%
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

