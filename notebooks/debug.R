#RAW_DATA_PATH <- "../data/raw/datenabzug_sp500.xlsx"
RAW_DATA_PATH <- "/Users/mhf/Library/Mobile Documents/com~apple~CloudDocs/TU-Wien/Interdisciplinary-Project/sales-forecasting/data/raw/datenabzug_sp500.xlsx"

OUTPUT_BASE_PATH <- "../data/processed"
SALES_OUTPUT_PATH <- paste(OUTPUT_BASE_PATH, "sales.csv", sep = "/")

load_transform_sales_quarter <- function(quarter) {
  # read and transpose
  df <- read_excel(RAW_DATA_PATH, sheet = paste0("Sales Q", quarter))
  
  df <- df[startsWith(df$Name, paste0("Q", quarter)), ] 
  
  df <- t(df) %>%
    as.data.frame()  %>%
    rownames_to_column("Name") %>%
    filter(startsWith(Name, paste0("Q", quarter)))
  
  colnames(df) <- df[1, ]
  df <- slice(df,-1)
  
  # extract company name and variable
  df <- df %>%
    mutate(
      company = str_extract(Name, "^(.*?)[^-]*"),
      variable = str_extract(Name, "(?<= - ).*(?= -)")
    )
  
  # pivot dataframe
  df <- df %>% select(-Name) %>%
    pivot_longer(
      cols = -c(company, variable),
      names_to = "Name",
      values_to = "interim_sales"
    )
  
  # extract year and quarter from Name column
  df <- df %>% mutate(year = str_extract(Name, "\\s.*"),
                      quarter = substr(Name, 2, 3)) %>% select(-Name)
  
  # remove rows with missing quarter values
  df <- df %>% filter(interim_sales != "NA")
  
  # remove variable column
  df <- df %>% select(-variable)
  
  return(df)
}

# apply function to all 4 quarters
df_sales <- map_dfr(1, load_transform_sales_quarter)
cat(
  paste(
    "Transformed interim sales dataframe contains ",
    nrow(df_sales),
    " records for ",
    length(unique(df_sales$company)),
    " companies from year ",
    min(df_sales$year),
    " to ",
    max(df_sales$year),
    ".",
    sep = ""
  )
)