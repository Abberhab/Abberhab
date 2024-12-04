library(eurostat)
library(dkstat)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(jsonlite)
library(readxl)
library(rvest)
library(rstudioapi)
library(xlsx)


#### Henter mælkeproduktion data fra eurostat ####

dato_threshold = "2000-01"

eu27_lande <- c("AT", "BE", "BG", "CY", "CZ", "DE", "DK", "EE", "EL", "ES", "FI", "FR",
                "HR", "HU", "IE", "IT", "LT", "LU", "LV", "MT", "NL", "PL", "PT", "RO",
                "SE", "SI", "SK")

start_dato = "2000-01"
end_dato = "2024-09"
online_code = "apro_mk_colm"

#lav en liste med en dataframe af hvert land hvor hver sub_listes index er landets index i lande_koder
data = list()

for (i in 1:length(eu27_lande)){
  data[[i]] <- get_eurostat(id = online_code, time_format = "date", filters = list(geo = eu27_lande[i]))
  
  clean_eurostat_cache()
  }

# fjerner "dairyprod ", "freq" kolonner for hvert listeelement
for (i in 1:length(data)){
  data[[i]] = data[[i]][, -c(1, 2)]
}

# laver en dataframe for hvert land med tid i kolonne 1 og hver unik unit fra kolonne 2 - 4
df_list <- list()
  
for (i in 1:length(data)){

test_1 <- as.data.frame(pivot_wider(data = data[[i]], names_from = "unit", values_from = "values", values_fn = ~ sum(.x,na.rm = TRUE)))

test_1$time <- as.character(test_1$time)

# indsæt landekoden
test_2 <- test_1 %>% select(-geo) %>% rename_with(~ paste0(., "_", test_1[1,1]), -time)

df_list[[i]] <- test_2

# navngiver hvert dataframes unikke geo 
names(df_list)[i] <- unique(test_1$geo)

}


# Filtrer derefter til 2008 Q1 og senere
Samlet_list <- map(df_list, ~ .x %>%
                          filter(time >= dato_threshold))

# Kombiner til en samlet data frame
samlet_df <- reduce(Samlet_list, full_join, by = "time")



##### Fjerner den sidste række, da den indeholder ikke registrerede værdier ######
samlet_df <- samlet_df[-nrow(samlet_df),]

last_12_rows <- tail(samlet_df, 12)

# Tjek hvilke kolonner der indeholder værdien 0.00 i de sidste 12 rækker
columns_with_zero <- sapply(last_12_rows, function(column) {
  any(column == 0.00, na.rm = TRUE)
})

# Filtrér kolonnenavne, hvor 0.00 findes
columns_with_zero <- names(columns_with_zero[columns_with_zero])

# Print resultatet
print(columns_with_zero)

#### TBC ####



#### Henter Valuta for alle eu lande ####
DNVALD_meta = dst_meta("DNVALD")

valuta_liste = list()

for (i in 1:nrow(DNVALD_meta$values$VALUTA[2])){
  valuta_kurs = DNVALD_meta$values$VALUTA[[2]][[i]]
  
  DNVALD_filtre = list(VALUTA = valuta_kurs,
                       KURTYP = "Valutakurser (DKK pr. 100 enheder valuta)", 
                       Tid = "*")
  
  DNVALD_data <- dst_get_data(
    table = "DNVALD",
    query = DNVALD_filtre,
    lang = "da"
  )
  
  test_1 <- as.data.frame(pivot_wider(data = DNVALD_data, names_from = "VALUTA", values_from = "value", values_fn = ~ sum(.x,na.rm = TRUE)))
  
  test_1 <- test_1[,-1] 
  
  valuta_liste[[i]] <- test_1
  
  names(valuta_liste)[i] <- colnames(test_1[2])
  
  print(paste0("Antal valutakurser nået: ", i, " / ", nrow(DNVALD_meta$values$VALUTA[2])))

}

# Omdan TID i alle dataframes til character
valuta_liste <- map(valuta_liste, ~ .x %>%
                      mutate(TID = as.character(TID)))


Samlet_valuta <- list()

for (i in seq_along(valuta_liste)) {
  
  # Extract the current element
  test_3 <- valuta_liste[[i]]
  
  # Remove the last three symbols in the TID column (keeping only year-month)
  test_3$TID <- substr(test_3$TID, 1, 7)
  
  # Rename the columns for consistency
  colnames(test_3) <- c("TID", "kurs")
  
  # Calculate monthly averages
  test_3 <- test_3 %>%
    group_by(TID) %>%
    summarise(
      Gennemsnitskurs = mean(kurs, na.rm = TRUE),  # Calculate mean
      .groups = "drop"
    )
  
  # Add to the list
  Samlet_valuta[[i]] <- as.data.frame(test_3)
  
  # Name the list element after the corresponding valuta
  names(Samlet_valuta)[i] <- names(valuta_liste)[i]
  
  # Print progress
  print(paste0("Antal valutakurser nået: ", i, " / ", length(valuta_liste)))
}


# Filtrer fra 2000-01 fra Samlet_valuta
Samlet_valuta = map(Samlet_valuta, ~ .x %>%
                      filter(TID >= dato_threshold))

Samlet_valuta <- lapply(names(Samlet_valuta), function(df_name) {
  df <- Samlet_valuta[[df_name]]
  colnames(df)[-which(colnames(df) == "TID")] <- paste0(df_name, "_", colnames(df)[-which(colnames(df) == "TID")])
  return(df)
})

# Merge all dataframes by "TID"
samlet_valuta_df <- Reduce(function(x, y) merge(x, y, by = "TID", all = TRUE), Samlet_valuta)

# Fjerner kolonner hvor 50% eller mere af værdierne er 0.00

# Calculate the percentage of zeros in each column
zero_percentage <- colMeans(samlet_valuta_df == 0, na.rm = TRUE)
write.xlsx(zero_percentage,file = "Valutaer med mere end 0 pct r.xlsx")


# Filter out columns with more than 50% zeros
filtreret_valuta_df <- samlet_valuta_df[, zero_percentage < 0.5]

#### Mælkepriser - Euro ####

######### henter mælkepriser på nettet ########
library(rvest)

# URL til siden
url = 'https://agriculture.ec.europa.eu/data-and-analysis/markets/overviews/market-observatories/milk_en'

# Læs HTML-indhold fra siden
html = read_html(url)

# Find alle href-links
links = html %>% html_nodes("a") %>% html_attr("href")

# Filtrer for links der indeholder .xlsx
xlsx_links = links[grepl("\\.xlsx$", links)]

# Hvis links er relative, gør dem absolutte
xlsx_links = ifelse(startsWith(xlsx_links, "http"), xlsx_links, paste0("https://agriculture.ec.europa.eu", xlsx_links))

# Brug det første link (eller vælg korrekt link, hvis der er flere)
link = xlsx_links[1]

#opsætter R til at hente fra datamappen hvor R filen er gemt (lav en ny mappe navngivet Data)
R_mappe <- dirname(getActiveDocumentContext()$path)
setwd(R_mappe)

# Download filen
download.file(link, destfile = "milk_prices.xlsx", mode = "wb")



######### Indlæser mælkepriser fra xlsx ########
rå_mælks_priser <- read_excel("milk_prices.xlsx", 
                          sheet = "Raw Milk Prices", skip = 6)

# Øko_mælke_priser <- read_excel("~/Documents/DataAnalyse/Bachelor/milk_prices.xlsx", 
#                               sheet = "Organic milk prices", skip = 4)

Data_mappe <- file.path(R_mappe, "Udtræk")
setwd(Data_mappe)

# Ændre første kolonnenavne
colnames(rå_mælks_priser)[1] <- "Tid"

# Fjern sidste kolonner fra 31 og frem
rå_mælks_priser <- rå_mælks_priser[, 1:30]

######### Data cleaning - rå mælke priser ########

# omskriver "m" til "-" i tid kolonnen
rå_mælks_priser$Tid <- gsub("m", "-", rå_mælks_priser$Tid)

# tilføj "_Euro" til alle kolonnenavne, undtagen den første
colnames(rå_mælks_priser)[-1] <- paste0(colnames(rå_mælks_priser)[-1], "_Euro")

# Fjerner sidste række, da den indeholder ikke kan anvende dem
rå_mælks_priser <- rå_mælks_priser[-nrow(rå_mælks_priser),]

# Filtrer til dato_threshold
samlet_råmælks_priser <- rå_mælks_priser %>%
  filter(Tid >= dato_threshold)

#### Sammel alt data i en "total" dataframe ####

# Ændre det første kolonnenavn til "Tid" i samlede dataframes
colnames(samlet_df)[1] <- "Tid"
colnames(samlet_valuta_df)[1] <- "Tid"
colnames(samlet_råmælks_priser)[1] <- "Tid"

# maks af samlet_df$tid
max_tid <- max(samlet_df$Tid)

samlet_råmælks_priser <- rå_mælks_priser %>%
  filter(Tid <= max_tid)

samlet_valuta_df <- samlet_valuta_df %>%
  filter(Tid <= max_tid)

samlet_df <- samlet_df %>%
  filter(Tid <= max_tid)

filtreret_valuta_df <- filtreret_valuta_df %>%
  filter(TID <= max_tid)

# lav en dataframe med max og min tid for samlet_råmælks_priser, samlet_valuta_df og samlet_df

# Merge mælkeproduktion, valuta og mælkepriser
uafhængige_variabler <- Reduce(function(x, y) merge(x, y, by = "Tid", all = TRUE), list(samlet_df, samlet_valuta_df, samlet_råmælks_priser))

# filtrer på tid
uafhængige_variabler <- uafhængige_variabler %>%
  filter(Tid >= dato_threshold)
