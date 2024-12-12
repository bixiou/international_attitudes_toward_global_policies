# Alternatively, just load co2_pop and go to Export maps and table
# readRDS("../data/map_GCS/co2_pop.rds")

##### Population data #####
years <- c(2005, seq(2010, 2100, 10))
# pop <- read.csv("../data/map_GCS/future population by age 2022.csv") # https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2022_PopulationByAge5GroupSex_Medium.zip
# pop <- pop[, c("Location", "ISO2_code", "ISO3_code", "Time", "AgeGrpStart", "PopTotal")]
# pop <- pop[pop$Time %in% c(2015, 2019, 2023, 2030, years),]
# write.csv(pop, "../data/map_GCS/future population by age 2022.csv", row.names = F) # Reduces from 98 Mo to 3 Mo
pop <- read.csv("../data/map_GCS/future population by age 2022.csv")
pop_adult <- pop[!(pop$AgeGrpStart %in% c(0, 5, 10)),] # Population aged 15 or above, in thousands
names(pop_adult) <- c("country", "ISO2_code", "code", "year", "AgeGrpStart", "adult")
pop_adult_iso3 <- aggregate(adult ~ year + code, data = pop_adult, FUN = sum)
names(pop) <- c("country", "ISO2_code", "code", "year", "AgeGrpStart", "pop")
pop_iso3 <- aggregate(pop ~ year + code, data = pop, FUN = sum)
pop_iso3 <- merge(pop_iso3, pop_adult_iso3)
iso2to3 <- setNames(pop$code, pop$ISO2_code)
iso2to3["NA"] <- "NAM"


##### CO2 emissions data #####
# source: https://ourworldindata.org/co2-emissions#how-do-consumption-based-emissions-compare-to-production-based-emissions CO2 emissions from fossil fuels and industry. Land use change is not included.
co2 <- read.csv("../data/map_GCS/production-vs-consumption-co2-emissions_our-world-in-data.csv") # Peters et al. (2012) 
co2 <- co2[co2$Year %in% c(2015, 2019),]
temp <- co2[co2$Year == 2019,]
temp$Year <- 2023
co2 <- rbind(co2, temp)
names(co2) <- c("country", "code", "year", "territorial", "footprint")


##### GDP pc data #####
GDPpc <- read.csv("../data/map_GCS/GDPpc_2015$_nominal.csv") # GDP per capita (constant 2015 US$) https://data.worldbank.org/indicator/NY.GDP.PCAP.KD March 1, 2023 (last year available is 2021)
GDPpc$code <- GDPpc$Country.Code
GDPpc <- rbind(data.frame("code" = GDPpc$code, "gdp_pc" = GDPpc$X2015, "year" = 2015), data.frame("code" = GDPpc$code, "gdp_pc" = GDPpc$X2019, "year" = 2019))
GDPpc$gdp_pc[GDPpc$code %in% c("ERI", "PRK", "SSD", "VEN", "YEM")] <- c(715, 654, 467, 3640, 702) # Impute data from other sources for Eritrea (IMF, 2023), North Korea (IMF, 2021), South Sudan (IMF, 2023), Venezuela (IMF, 2023), Yemen (WB, 2018)
co2 <- merge(co2, GDPpc)


##### Merge datasets #####
# co2$year[co2$year == 2016] <- 2030 # Beware, before we estimate 2030 emissions, they will be equal to 2016 ones!
for (y in c(years)) {
  temp_y <- co2[co2$year == 2015,]
  temp_y$year <- y
  for (v in c("territorial", "footprint")) temp_y[[v]] <- NA
  co2 <- rbind(co2, temp_y)
}
co2_pop <- merge(pop_iso3[pop_iso3$code != "",], co2[co2$code != "",], all = T)
country_code <- setNames(co2$country[co2$year == 2019], co2$code[co2$year == 2019])
co2_pop$country <- country_code[co2_pop$code]
co2_pop$adult <- 1000 * co2_pop$adult
co2_pop$pop <- 1000 * co2_pop$pop
co2_pop$emissions <- co2_pop$footprint
co2_pop$missing_footprint <- is.na(co2_pop$footprint)
co2_pop$emissions[is.na(co2_pop$footprint)] <- co2_pop$territorial[is.na(co2_pop$footprint)] # imputing territorial emissions for those countries
co2_pop <- co2_pop %>% group_by(code) %>% pivot_wider(id_cols = c("code", "country"),  names_from = year, 
           values_from = c("territorial", "footprint", "emissions", "adult", "pop", "gdp_pc", "missing_footprint"), names_glue = "{.value}_{year}") %>% ungroup()
# /!\ Beware, data is valid only in 2015 and 2019 for gdp_pc (and it is in nominal)
co2_pop$missing_footprint <- co2_pop$missing_footprint_2019
co2_pop <- co2_pop[, !colnames(co2_pop) %in% c(paste0("missing_footprint_", c(2015, 2019, 2023, years)), paste0("gdp_pc_", years))]
co2_pop <- co2_pop[!co2_pop$country %in% c("World", "Kosovo", NA), sapply(names(co2_pop), function(v) any(!is.na(co2_pop[[v]])))]
co2_pop$gdp_2019 <- co2_pop$gdp_pc_2019 * co2_pop$pop_2019
co2_pop$country_map <- co2_pop$country
co2_pop$country_map[co2_pop$country == "United States"] <- "USA"
co2_pop$country_map[co2_pop$country == "United Kingdom"] <- "UK"
co2_pop$country_map[co2_pop$country == "Democratic Republic of Congo"] <- "Democratic Republic of the Congo"
co2_pop$country_map[co2_pop$country == "Congo"] <- "Republic of Congo"
co2_pop$country_map[co2_pop$country == "Cote d'Ivoire"] <- "Ivory Coast"
co2_pop$country_map[co2_pop$country == "Czechia"] <- "Czech Republic"
co2_pop$country_map[co2_pop$country == "Eswatini"] <- "Swaziland"
co2_pop$share_territorial_2019 <- co2_pop$territorial_2019/sum(co2_pop$territorial_2019)
# rm(pop, co2)  
decrit(co2_pop$missing_footprint) # 98 out of 218 countries missing (while no missing for territorial)
# co2_pop$country[co2_pop$missing_footprint]


##### Constants #####
euro_per_dollar <- 0.94
pound_per_dollar <- .82
LCU_per_dollar <- c(rep(euro_per_dollar, 3), pound_per_dollar, 1)
co2_emissions_2030 <- 26.3e9
(R <- carbon_tax_revenues_2030 <- 2367e9) # in $, Based on $90/tCO2 
(A <- adult_pop_2015 <- sum(co2_pop$adult_2015)) # 5.45G (entire pop (not just 15+) 7.42G)
(adult_pop_2019 <- sum(co2_pop$adult_2019)) # 5.75G (entire pop (not just 15+) 7.76G)
(F_ <- adult_pop_2030 <- sum(co2_pop$adult_2030)) # 6.57G (entire pop (not just 15+) 8.54G)
(E <- co2_emissions_2015 <- sum(co2_pop$territorial_2015)) # 34.4G (footprint: 33.4G + NA)
(E <- co2_emissions_2019 <- sum(co2_pop$territorial_2019)) # 35.8G
# countries_survey_oecd <- c("AUS", "CAN", "DNK", "FRA", "DEU", "ITA", "JPN", "MEX", "POL", "KOR", "ESP", "TUR", "GBR", "USA", "BRA", "CHN", "IND", "IDN", "ZAF", "UKR")
# revenues_pa_oecd <- c(134, 105, 68, 46, 61, 42, 60, 34, 42, 72, 39, 40, 59, 128, 18, 38, 13, 16, 50, 32) # cf. oecd_climate/questionnairesboard row 63 and oecd_climate/questionnaires/net_gain_global_tax
# mean_gain_oecd <- 30 - revenues_pa_oecd
# emission_share_2015_oecd <- c(426.4, 547.9, 59.4, 445, 853.4, 423, 1361, 485.5, 273.8, 584.8, 293.8, 374.9, 575.8, 5794.5, 475.4, 7977.9, 1918.8, 484.6, 313.5, 262)/32276 # OECD bit.ly/37kSVUx EU28 12.3%, G20 82.7% 2015 latest date available
# names(mean_gain_oecd) <- names(emission_share_2015_oecd) <- names(revenues_pa_oecd) <- countries_survey_oecd
#             2020   2030   2040   2050                                Past/global basic income/estimate of a global basic income.pdf
# Emissions   34.3   26.3   18.8   13.1 GtCO2                          2DS (66% chance) scenario in Energy Technology Perspectives 2017 http://www.iea.org/etp2017/summary/
# Price         40     90    120    145 $/tCO2                         Stern & Stiglitz (2017), Table 3
# Revenues    1372   2367   2256   1900 G$/year
# Basic income  20     30     26     21 $/month                        Median UN ≥15 pop
# PPP          1.4    2.1    1.8    1.5 Sub-Saharan Africa PPP $/day   2.1: ratio of GDP pc of Sub-Saharan Africa in PPP and value (World Bank)
# Share income 1.4    1.7    1.2    0.7 % of Gross World Product       2020: 98T$, then assuming 3.5% growth
# Emissions pa   6      4    2.6    1.7 tCO2/year per adult above 15
# Adult pop    5.7    6.6    7.2    7.5 G adult above 15
# /!\ The 2020 Gross World Product (84.7T$) and future growth may well be lower than assumed (98T$, 3.5%), hence higher Share income.
# /!\ Units of price are not specified in Stern & Stiglitz (2017), but probably constant 2017 dollar.


##### Compute net gain per capita #####
# Assumption: emissions per adult (>15) will evolve in the same way in all countries
# There are discrepancies between OECD data (used in OECD survey) and Global Carbon Project data (used here as it covers all countries, cf. Peters al. (2012)). Discrepancies are always within +/- 20%.
# The results also slightly change if the baseline year 2019 is used instead of 2015, and if we compute the net gain for (i.e. divide by population of) year = 2030 vs. 2015, cf. deprecated/draft_map_GCS_incidence.R for an analysis
# For consistency with the OECD survey, we use 2015 as a baseline.
compute_gain <- function(year = 2030, base_year = 2019, type = "mean", df = co2_pop, return_data = T) {
  df[[paste0("demographic_evolution_", base_year)]] <- (df$adult_2030/df[[paste0("adult_", base_year)]]) * (sum(df[[paste0("adult_", base_year)]])/adult_pop_2030)
  df[[paste0("emissions_pa_", base_year)]] <- df[[paste0("emissions_", base_year)]]/df[[paste0("adult_", base_year)]]
  df[[paste0("share_emissions_", base_year)]] <- df[[paste0("emissions_", base_year)]]/sum(df[[paste0("emissions_", base_year)]])
  df[[paste0("share_emissions_2030_base_", base_year)]] <- df[[paste0("share_emissions_", base_year)]] * df[[paste0("demographic_evolution_", base_year)]]
  df[[paste0("share_emissions_2030_base_", base_year)]] <- df[[paste0("share_emissions_2030_base_", base_year)]]/sum(df[[paste0("share_emissions_2030_base_", base_year)]])
  df$emissions_2030 <- df[[paste0("share_emissions_2030_base_", base_year)]] * co2_emissions_2030
  df$emissions_pa_2030 <- df$emissions_2030 / df$adult_2030
  df[[paste0("revenues_pa_", year)]] <- df[[paste0("share_emissions_2030_base_", base_year)]] * carbon_tax_revenues_2030 / df[[paste0("adult_", year)]]/12
  
  df[[paste0(type, "_gain_", year)]] <- 30 - (1 - 0.1*(type == "median"))*df[[paste0("revenues_pa_", year)]]
  # Accounts for opting out (using 2019 GDP pc)
  df[[paste0("optout_right_", year)]] <- 2 - pmax(1, pmin(2, df$gdp_pc_2019 / wtd.mean(df$gdp_pc_2019, df$pop_2019)))
  temp <- rep(T, nrow(df)) 
  basic_income <- wtd.mean(df[[paste0("revenues_pa_", year)]], df[[paste0("adult_", year)]])
  df[[paste0("large_footprint_", year)]] <- (df[[paste0("revenues_pa_", year)]] > basic_income)
  df[[paste0("participation_rate_", year)]] <- 1 - df[[paste0("large_footprint_", year)]] * df[[paste0("optout_right_", year)]]
  while (any(temp != df[[paste0("large_footprint_", year)]])) {
    temp <- df[[paste0("large_footprint_", year)]]
    basic_income <- wtd.mean(df[[paste0("revenues_pa_", year)]], df[[paste0("participation_rate_", year)]] * df[[paste0("adult_", year)]])
    df[[paste0("large_footprint_", year)]] <- (df[[paste0("revenues_pa_", year)]] > basic_income)
    df[[paste0("participation_rate_", year)]] <- 1 - df[[paste0("large_footprint_", year)]] * df[[paste0("optout_right_", year)]]
  }
  # basic_income <- 30 * wtd.mean(df[[paste0("participation_rate_", year)]], df[[paste0("revenues_pa_", year)]] * df[[paste0("adult_", year)]])
  print(paste0("Basic income adjusted for opting-out countries: ", round(basic_income, 1), " (down from ", round(wtd.mean(df[[paste0("revenues_pa_", year)]], df[[paste0("adult_", year)]]), 1), ")"))
  df[[paste0(type, "_gain_adj_", year)]] <- df[[paste0("participation_rate_", year)]] * (basic_income - (1 - 0.1*(type == "median"))*df[[paste0("revenues_pa_", year)]])
  if (return_data) return(df) else return(df[[paste0(type, "_gain_", year)]])
}
co2_pop <- compute_gain(year = 2015, base_year = 2015, type = "median") # creates median_gain_2015. Adj: basic income 36 -> 32
co2_pop <- compute_gain(year = 2019, base_year = 2019, type = "mean") # Adj: 34 -> 29
co2_pop <- compute_gain(year = 2030, base_year = 2019, type = "mean") # creates mean_gain_2030. Adj: 30 -> 25


##### Poverty gaps #####
extract_last_year <- function(df = pg, cols = names(df), var_name = NULL, pattern_gsub = "[^0-9]", keep = names(df)) {
  df$last_year <- NA
  for (j in cols) df$last_year <- ifelse(is.na(df[[j]]), df$last_year, rep(j, nrow(df)))
  if (!is.null(var_name)) df[[var_name]] <- NA
  if (!is.null(var_name)) df[[var_name]][!is.na(df$last_year)] <- sapply(which(!is.na(df$last_year)), function(i) df[[df$last_year[i]]][i])
  df$last_year <- as.numeric(gsub(pattern_gsub, "", df$last_year))
  if (!is.null(var_name)) return(df[, unique(c(keep, "last_year", var_name))])
  else return(df$last_year)
}
pg2 <- read.csv("../data/map_GCS/poverty_gap_2-15.csv") # Poverty gap at $2.15 a day (2017 PPP) (%) https://data.worldbank.org/indicator/SI.POV.GAPS March 1, 2023. World average: 2.6% i.e. (less than - bc PPP) 163G$ = 2.15*365*8e9*.026
pg2$code <- pg2$Country.Code
pg2 <- extract_last_year(df = pg2, cols = paste0("X", 1960:2021), var_name = "pg2", keep = c("Country.Name", "code"))
GDPpcPPP <- read.csv("../data/map_GCS/GDPpc_2017$_PPP.csv") # GDP per capita, PPP (constant 2017 international $) https://data.worldbank.org/indicator/NY.GDP.PCAP.PP.KD March 1, 2023  (last year available is 2021)
GDPpcPPP$code <- GDPpcPPP$Country.Code
GDPpcPPP$GDPpcPPP[!is.na(pg2$last_year)] <- sapply(which(!is.na(pg2$last_year)), function(i) GDPpcPPP[[paste0("X", pg2$last_year[i])]][i])
pg <- merge(pg2, GDPpcPPP[, c("Country.Name", "code", "GDPpcPPP")])


##### SSPs #####
ssp_countries <- read.xlsx("../data/map_GCS/SSP_cmip6_iam_model_region_mapping.xlsx")
names(ssp_countries)[1] <- "code"
ssp_countries$R5_region <- sub("R5", "R5.2", ssp_countries$R5_region)
co2_pop <- merge(pg[,c("code", "GDPpcPPP")], co2_pop)
co2_pop$gdp_ppp_now <- co2_pop$GDPpcPPP * co2_pop$pop_2020
co2_pop <- merge(ssp_countries[,c("code", "IMAGE.REGION", "MESSAGE-GLOBIOM.REGION", "R5_region")], co2_pop)
pop_un <- list()
pop.R5 <- co2_pop %>% group_by(R5_region) %>% summarise_at(names(co2_pop)[grepl("adult_|pop_|gdp_|gdp_ppp_", names(co2_pop))], sum, na.rm = T)
pop_un[["IMAGE"]] <- co2_pop %>% group_by(IMAGE.REGION) %>% summarise_at(names(co2_pop)[grepl("adult_|pop_|gdp_|gdp_ppp_", names(co2_pop))], sum, na.rm = T)
pop_un[["MESSAGE-GLOBIOM"]] <- co2_pop %>% group_by(`MESSAGE-GLOBIOM.REGION`) %>% summarise_at(names(co2_pop)[grepl("adult_|pop_|gdp_|gdp_ppp_", names(co2_pop))], sum, na.rm = T)
names(pop.R5)[1] <- names(pop_un[["IMAGE"]])[1] <- names(pop_un[["MESSAGE-GLOBIOM"]])[1] <- "region"
pop_un[["IMAGE"]] <- rbind(pop_un[["IMAGE"]], pop.R5)
pop_un[["MESSAGE-GLOBIOM"]] <- rbind(pop_un[["MESSAGE-GLOBIOM"]], pop.R5)

# View(co2_pop[, c("code", "country", "IMAGE.REGION", "MESSAGE-GLOBIOM.REGION")])
co2_pop$`MESSAGE-GLOBIOM.REGION`[is.na(co2_pop$`MESSAGE-GLOBIOM.REGION`)] <- "LAM"
regions <- unique(co2_pop$`MESSAGE-GLOBIOM.REGION`)
message_region_by_image <- c("BRA" = "LAM", "CAN" = "NAM", "CEU" = "EEU", "CHN" = "CPA", "EAF" = "AFR", "INDIA" = "SAS", "INDO" = "PAS", "JAP" = "PAO", "KOR" = "PAS", "ME" = "MEA", "MEX" = "LAM", "NAF" = "MEA", "OCE" = "PAO", "RCAM" = "LAM", "RSAF" = "AFR", "RSAM" = "LAM", "RSAS" = "SAS", "RUS" = "FSU", "SAF" = "AFR", "SEAS" = "PAS", "STAN" = "FSU", "TUR" = "WEU", "UKR" = "FSU", "USA" = "NAM", "WAF" = "AFR", "WEU" = "WEU")
image_regions <- names(message_region_by_image)
message_regions <- unique(message_region_by_image) # this is regions sorted differently
message_region_by_code <- setNames(co2_pop$`MESSAGE-GLOBIOM.REGION`, co2_pop$code)
message_region_by_code[c("Dem USA", "Non-Dem USA")] <- "NAM"
image_region_by_code <- setNames(co2_pop$IMAGE.REGION, co2_pop$code)
big_region_by_message <- c("LAM" = "lam", "NAM" = "oecd", "EEU" = "oecd", "CPA" = "asia", "AFR" = "maf", "SAS" = "asia", "PAS" = "asia", "PAO" = "oecd", "MEA" = "maf", "FSU" = "ref", "WEU" = "oecd")
# big_region_by_image <- c("BRA" = "lam", "CAN" = "oecd", "CEU" = "oecd", "CHN" = "asia", "EAF" = "maf", "INDIA" = "asia", "INDO" = "asia", "JAP" = "oecd", "KOR" = "asia", "ME" = "maf", "MEX" = "lam", "NAF" = "maf", "OCE" = "oecd", "RCAM" = "lam", "RSAF" = "maf", "RSAM" = "lam", "RSAS" = "asia", "RUS" = "ref", "SAF" = "maf", "SEAS" = "asia", "STAN" = "ref", "TUR" = "oecd", "UKR" = "ref", "USA" = "oecd", "WAF" = "maf", "WEU" = "oecd")
big_region_by_image <- setNames(big_region_by_message[message_region_by_image[image_regions]], image_regions)
big_region_by_code <- setNames(big_region_by_message[message_region_by_code[co2_pop$code]], co2_pop$code)
big_regions <- c("asia", "lam", "maf", "oecd", "ref")


SSPs <- read.csv("../data/map_GCS/SSPs.csv") # https://secure.iiasa.ac.at/web-apps/ene/SspDb/download/iam_v2/SSP_IAM_V2_201811.csv.zip
SSPs_countries <- read.csv("../data/map_GCS/SSP_CMIP6.csv") # https://secure.iiasa.ac.at/web-apps/ene/SspDb/download/cmip6/SSP_CMIP6_201811.csv.zip

vars_SSPs <- c("emissions_pc" = "Emissions|CO2", "fossil_emissions" = "Emissions|CO2|Fossil Fuels and Industry", # Mt CO2/yr => tCO2/yr/pers
               "energy_pc" = "Final Energy", # EJ/yr => GJ/yr/pers
               "gdp_ppp" = "GDP|PPP", # billion US$2005/yr => US$2005/yr, "conso" = "Consumption"
               "pop" = "Population", # million => pers
               "carbon_price" = "Price|Carbon") # US$2005/t CO2
ssp <- list() # 2 min
for (i in unique(SSPs$SCENARIO)) { # unique(SSPs$MODEL)
  ssp[[i]] <- list()
  for (j in c("IMAGE", "MESSAGE-GLOBIOM")) {
    # puts the database in the right form
    ssp[[i]][[j]] <- data.frame(region = c(paste0("iam_", unique(SSPs$REGION)), unique(SSPs_countries$REGION[SSPs_countries$SCENARIO %in% c("SSP1-19", "SSP2-45")]))) # unique(SSPs_countries$REGION)
    # loads GDP data (current one, not projections)
    for (y in years) {
      for (v in names(vars_SSPs)) {
        # print(paste(i, j, v, y))
        if (sum(SSPs$MODEL == j & SSPs$SCENARIO == i & SSPs$VARIABLE == vars_SSPs[v]) > 0 && all(SSPs$REGION[SSPs$MODEL == j & SSPs$SCENARIO == i & SSPs$VARIABLE == vars_SSPs[v]] == unique(SSPs$REGION))) {
          ssp[[i]][[j]][[paste0(v, "_", y)]] <- c(SSPs[SSPs$MODEL == j & SSPs$SCENARIO == i & SSPs$VARIABLE == vars_SSPs[v], paste0("X", y)], rep(NA, 42))
        }
      }
      # merge SSPs (aggregated into 5 regions) with SSPs_countries
      if (sum(SSPs_countries$VARIABLE == "CMIP6 Emissions|CO2" & SSPs_countries$MODEL == j & SSPs_countries$SCENARIO == i & !is.na(SSPs_countries[[paste0("X", y)]])) > 0) {
        temp <- SSPs_countries[SSPs_countries$MODEL == j & SSPs_countries$SCENARIO == i & SSPs_countries$VARIABLE == "CMIP6 Emissions|CO2", paste0("X", y)]
        names(temp) <- SSPs_countries$REGION[SSPs_countries$MODEL == j & SSPs_countries$SCENARIO == i & SSPs_countries$VARIABLE == "CMIP6 Emissions|CO2"]
        ssp[[i]][[j]][[paste0("emissions_pc_", y)]][7:48] <- temp[ssp[[i]][[j]]$region][7:48]
      }
      if (y == years[1]) {
        ssp[[i]][[j]]$gdp_ppp_now <- ssp[[i]][[j]]$gdp_2019 <- rep(NA, 48)
        ssp[[i]][[j]]$gdp_ppp_now[match.nona(pop_un[[j]]$region, ssp[[i]][[j]]$region)] <- pop_un[[j]]$gdp_ppp_now[pop_un[[j]]$region %in% ssp[[i]][[j]]$region] 
        # ssp[[i]][[j]]$gdp_2019[match.nona(pop_un[[j]]$region, ssp[[i]][[j]]$region)] <- pop_un[[j]]$gdp_2019[pop_un[[j]]$region %in% ssp[[i]][[j]]$region] 
        for (r in c(big_regions)) ssp[[i]][[j]]$gdp_ppp_now[ssp[[i]][[j]]$region == paste0("iam_R5.2", toupper(r))] <- sum(ssp[[i]][[j]]$gdp_ppp_now[ssp[[i]][[j]]$region %in% c(image_regions[big_region_by_image == r], message_regions[big_region_by_message == r])], na.rm = T)
        # for (r in c(big_regions)) ssp[[i]][[j]]$gdp_2019[ssp[[i]][[j]]$region == paste0("iam_R5.2", toupper(r))] <- sum(ssp[[i]][[j]]$gdp_2019[ssp[[i]][[j]]$region %in% c(image_regions[big_region_by_image == r], message_regions[big_region_by_message == r])], na.rm = T)
      }
      if (paste0("emissions_pc_", y) %in% names(ssp[[i]][[j]])) {
        ctries <- !multi_grepl(c(toupper(big_regions), "World"), ssp[[i]][[j]]$region)
        # Defines population, adult and gdp (using external pop_un coming from co2_pop) for disaggregated countries (it was only given for the 5 regions)
        ssp[[i]][[j]][[paste0("emissions_", y)]] <- 1e6 * ssp[[i]][[j]][[paste0("emissions_pc_", y)]]
        ssp[[i]][[j]][[paste0("pop_", y)]] <- 1e6 * ssp[[i]][[j]][[paste0("pop_", y)]]
        ssp[[i]][[j]][[paste0("pop_", y)]][match.nona(pop_un[[j]]$region, ssp[[i]][[j]]$region)] <- pop_un[[j]][[paste0("pop_", y)]][pop_un[[j]]$region %in% ssp[[i]][[j]]$region] 
        # Adjust pop_ of CMIP6 to match the region sum of IAM
        for (r in c(big_regions)) ssp[[i]][[j]][[paste0("pop_", y)]][ssp[[i]][[j]]$region %in% c(message_regions[big_region_by_message == r], image_regions[big_region_by_image == r])] <- ssp[[i]][[j]][[paste0("pop_", y)]][ssp[[i]][[j]]$region %in% c(message_regions[big_region_by_message == r], image_regions[big_region_by_image == r])] * 
          ssp[[i]][[j]][[paste0("pop_", y)]][ssp[[i]][[j]]$region == paste0("iam_R5.2", toupper(r))] / ssp[[i]][[j]][[paste0("pop_", y)]][ssp[[i]][[j]]$region == paste0("R5.2", toupper(r))] # sum(ssp[[i]][[j]][[paste0("pop_", y)]][ssp[[i]][[j]]$region %in% c(message_regions[big_region_by_message == r], image_regions[big_region_by_image == r])], na.rm = T)
        # Fill
        ssp[[i]][[j]][[paste0("adult_", y)]] <- NA
        ssp[[i]][[j]][[paste0("adult_", y)]][match(paste0("iam_", unique(SSPs$REGION)[-6]), ssp[[i]][[j]]$region)] <- (pop_un[[j]][[paste0("adult_", y)]]/pop_un[[j]][[paste0("pop_", y)]])[match(unique(SSPs$REGION)[-6], pop_un[[j]]$region)] * ssp[[i]][[j]][[paste0("pop_", y)]][match(paste0("iam_", unique(SSPs$REGION)[-6]), ssp[[i]][[j]]$region)]
        ssp[[i]][[j]][[paste0("adult_", y)]][ssp[[i]][[j]]$region == "iam_World"] <- sum(ssp[[i]][[j]][[paste0("adult_", y)]][match(paste0("iam_", unique(SSPs$REGION)[-6]), ssp[[i]][[j]]$region)], na.rm = T)
        ssp[[i]][[j]][[paste0("adult_", y)]][match.nona(pop_un[[j]]$region, ssp[[i]][[j]]$region)] <- pop_un[[j]][[paste0("adult_", y)]][pop_un[[j]]$region %in% ssp[[i]][[j]]$region]
        # Adjust to IAM
        for (r in c(big_regions)) ssp[[i]][[j]][[paste0("adult_", y)]][ssp[[i]][[j]]$region %in% c(message_regions[big_region_by_message == r], image_regions[big_region_by_image == r])] <- ssp[[i]][[j]][[paste0("adult_", y)]][ssp[[i]][[j]]$region %in% c(message_regions[big_region_by_message == r], image_regions[big_region_by_image == r])] * 
          ssp[[i]][[j]][[paste0("adult_", y)]][ssp[[i]][[j]]$region == paste0("iam_R5.2", toupper(r))] / sum(ssp[[i]][[j]][[paste0("adult_", y)]][ssp[[i]][[j]]$region %in% c(message_regions[big_region_by_message == r], image_regions[big_region_by_image == r])], na.rm = T)
        # ssp[[i]][[j]][[paste0("gdp_ppp_", y)]] <- 1e9 * ssp[[i]][[j]][[paste0("gdp_ppp_", y)]]
        for (r in c(big_regions)) {
          region_r <- ssp[[i]][[j]]$region %in% c(message_regions[big_region_by_message == r], image_regions[big_region_by_image == r])
          growth_factor_ry <- ssp[[i]][[j]][[paste0("gdp_ppp_", y)]][ssp[[i]][[j]]$region == paste0("iam_R5.2", toupper(r))]/sum(((ssp[[i]][[j]]$gdp_ppp_now/ssp[[i]][[j]]$pop_2020) * ssp[[i]][[j]][[paste0("pop_", y)]])[region_r], na.rm = T)
          ssp[[i]][[j]][[paste0("gdp_ppp_pc_", y)]][region_r] <- growth_factor_ry * (ssp[[i]][[j]]$gdp_ppp_now/ssp[[i]][[j]]$pop_2020)[region_r]
          ssp[[i]][[j]][[paste0("gdp_ppp_", y)]][region_r] <- (ssp[[i]][[j]][[paste0("gdp_ppp_pc_", y)]] * ssp[[i]][[j]][[paste0("pop_", y)]])[region_r]
        } 
        # ssp[[i]][[j]][[paste0("gdp_", y)]] <- rep(NA, 48)
        # for (r in c(big_regions)) ssp[[i]][[j]][[paste0("gdp_", y)]][ssp[[i]][[j]]$region %in% c(message_regions[big_region_by_message == r], image_regions[big_region_by_image == r])] <- ssp[[i]][[j]]$gdp_2019[ssp[[i]][[j]]$region %in% c(message_regions[big_region_by_message == r], image_regions[big_region_by_image == r])] * 
        #   ssp[[i]][[j]][[paste0("gdp_ppp_", y)]][ssp[[i]][[j]]$region == paste0("iam_R5.2", toupper(r))] / ssp[[i]][[j]]$gdp_2019[ssp[[i]][[j]]$region == paste0("iam_R5.2", toupper(r))]
        if (i %in% c("SSP1-19", "SSP1-26")) { # extends to all MESSAGE regions
          for (v in c("emissions_", "pop_", "adult_")) ssp[[i]][[j]][[paste0(v, y)]][ssp[[i]][[j]]$region == "AFR"] <- sum(ssp[[i]][[j]][[paste0(v, y)]][ssp[[i]][[j]]$region %in% c("WAF", "EAF", "SAF", "RSAF")])
          for (v in c("emissions_", "pop_", "adult_")) ssp[[i]][[j]][[paste0(v, y)]][ssp[[i]][[j]]$region == "LAM"] <- sum(ssp[[i]][[j]][[paste0(v, y)]][ssp[[i]][[j]]$region %in% c("MEX", "RCAM", "RSAM")])
        }
        ssp[[i]][[j]][[paste0("emissions_pc_", y)]] <- ssp[[i]][[j]][[paste0("emissions_", y)]]/ssp[[i]][[j]][[paste0("pop_", y)]]
        ssp[[i]][[j]][[paste0("emissions_pa_", y)]] <- ssp[[i]][[j]][[paste0("emissions_", y)]]/ssp[[i]][[j]][[paste0("adult_", y)]]
        for (var in c("energy_pc_", "gdp_ppp_")) ssp[[i]][[j]][[paste0(var, y)]] <- 1e9 * ssp[[i]][[j]][[paste0(var, y)]]
        # ssp[[i]][[j]][[paste0("gdp_pc_", y)]] <- ssp[[i]][[j]][[paste0("gdp_", y)]]/ssp[[i]][[j]][[paste0("pop_", y)]]
        # ssp[[i]][[j]][[paste0("gdp_pa_", y)]] <- ssp[[i]][[j]][[paste0("gdp_", y)]]/ssp[[i]][[j]][[paste0("adult_", y)]]
        ssp[[i]][[j]][[paste0("gdp_ppp_pc_", y)]] <- ssp[[i]][[j]][[paste0("gdp_ppp_", y)]]/ssp[[i]][[j]][[paste0("pop_", y)]]
        ssp[[i]][[j]][[paste0("gdp_ppp_pa_", y)]] <- ssp[[i]][[j]][[paste0("gdp_ppp_", y)]]/ssp[[i]][[j]][[paste0("adult_", y)]]
      }
    }
    ssp[[i]][[j]]$region[1:6] <- c("asia", "lam", "maf", "oecd", "ref", "world")
  }
}

ssp2_26 <- ssp$`SSP2-26`$IMAGE

ssps <- "ssp2_26"
names(ssps) <- "SSP2-2.6 (1.8 °C)"

carbon_price <- revenues_pc <- world_emissions <- world_emissions_pc <- world_population <- revenues_over_gdp <- list()
for (s in ssps) {
  carbon_price[[s]] <- setNames(sapply(years, function(y) d(s)[[paste0("carbon_price_", y)]][d(s)$region == "asia"]), years)
  world_emissions[[s]] <- setNames(sapply(years, function(y) d(s)[[paste0("emissions_", y)]][d(s)$region == "world"]), years)
  world_population[[s]] <- setNames(sapply(years, function(y) d(s)[[paste0("pop_", y)]][d(s)$region == "world"]), years)
  world_emissions_pc[[s]] <- world_emissions[[s]]/world_population[[s]]
  revenues_over_gdp[[s]] <- carbon_price[[s]] * sapply(years, function(y) d(s)[[paste0("emissions_pc_", y)]][d(s)$region == "world"]) / sapply(years, function(y) d(s)[[paste0("gdp_ppp_pc_", y)]][d(s)$region == "world"])
  revenues_pc[[s]] <- carbon_price[[s]] * sapply(years, function(y) d(s)[[paste0("emissions_pc_", y)]][d(s)$region == "world"])
  for (y in years) eval(str2expression(paste0(s, "$gain_pc_", y, " <- (revenues_pc$", s, "['", y, "'] - carbon_price$", s, "['", y, "'] * ", s, "$emissions_pc_", y, ") /12"))) # Average gain in $/month
}



##### Global Energy Assessment #####
# This is the one we use (because SSPs I had are not disaggregated enough). /!\ It's territorial, not footprint.
# GEA: Emissions (2°C >50% chance), population and GDP pc until 2100 disaggregated on 11 countries (unlike SSP where only emissions are disaggregated in more than 5 regions), but without a price trajectory.
gea_emissions <- read.xlsx("../data/map_GCS/GEA_efficiency_emissions_regions.xlsx")
gea_pop <- read.xlsx("../data/map_GCS/GEA_efficiency_pop_regions.xlsx")
gea_gdp_ppp <- read.xlsx("../data/map_GCS/GEA_efficiency_GDP_PPP_regions.xlsx")
gea_gdp_mer <- read.xlsx("../data/map_GCS/GEA_efficiency_GDP_MER_regions.xlsx")

gea <- list() # GEA model is done with MESSAGE. It contains three additional regions: ASIA (CPA+SAS+PAS), MAF (AFR+MEA), REF (FSU+EEU), OECD90
for (i in c("IMAGE", "GEA")) {
  gea[[i]] <- data.frame(region = unique(gea_pop$Region))
  temp <- ssp2_26 # I make it temp to avoid problems with intersect(image_regions, message_regions) == "WEU"
  for (y in years) {
    for (v in c("gdp_ppp", "gdp_mer")) gea[[i]][[paste0(v, "_", y)]] <- 1e9 * d(paste0("gea_", v))[[as.character(y)]][d(paste0("gea_", v))$Model == i]
    for (v in c("emissions", "pop")) gea[[i]][[paste0(v, "_", y)]] <- 1e6 * d(paste0("gea_", v))[[as.character(y)]][d(paste0("gea_", v))$Model == i]
    for (r in message_regions) temp[[paste0("adult_", y)]][temp$region == r] <- sum(temp[[paste0("adult_", y)]][message_region_by_image[temp$region] == r], na.rm = T)
    gea[[i]][[paste0("adult_", y)]][match.nona(temp$region, gea[[i]]$region)] <- temp[[paste0("adult_", y)]][temp$region %in% gea[[i]]$region]
    for (v in c("emissions", "gdp_ppp", "gdp_mer")) gea[[i]][[paste0(v, "_pc_", y)]] <- gea[[i]][[paste0(v, "_", y)]]/gea[[i]][[paste0("pop_", y)]]
    for (v in c("emissions", "gdp_ppp", "gdp_mer")) gea[[i]][[paste0(v, "_pa_", y)]] <- gea[[i]][[paste0(v, "_", y)]]/gea[[i]][[paste0("adult_", y)]]
    for (v in c("gdp_ppp_pc_", "gdp_mer_pc_")) gea[[i]][paste0(v, "over_mean_", y)] <- gea[[i]][paste0(v, y)]/wtd.mean(gea[[i]][paste0(v, y)], gea[[i]][[paste0("pop_", y)]])
  }
  gea[[i]]$region[gea[[i]]$region == "World"] <- "world"
  gea[[i]] <- gea[[i]][!gea[[i]]$region %in% c("ASIA", "MAF", "REF", "OECD90", "North", "South"), ]
  for (y in years) for (v in c("adult")) gea[[i]][[paste0(v, "_", y)]][gea[[i]]$region == "world"] <- sum(gea[[i]][[paste0(v, "_", y)]][gea[[i]]$region != "world"], na.rm = T) # , "emissions", "pop", "gdp_ppp", "gdp_mer"
}
world_emissions_pc$gea_gea <- setNames(gea$GEA[gea$GEA$region == "world", grepl("emissions_pc", names(gea$GEA))], years)
world_emissions_pc$gea_image <- setNames(gea$IMAGE[gea$IMAGE$region == "world", grepl("emissions_pc", names(gea$IMAGE))], years)
# GEA scenario is closest to ssp1_26 (higher emissions than ssp2_26 before 2050, larger negative emissions after), gea_image has almost no negative emissions => use gea_gea
# I impute the price trajectory of ssp2_26 (at least three times higher than ssp1_26) to be conservative although emissions_pc are much closer to ssp1_26
carbon_price$gea_gea <- carbon_price$ssp2_26

world_population$gea_gea <- setNames(gea$GEA[gea$GEA$region == "world", grepl("pop_", names(gea$GEA))], years)
world_population$gea_image <- setNames(gea$IMAGE[gea$IMAGE$region == "world", grepl("pop_", names(gea$IMAGE))], years)
world_emissions$gea_gea <- setNames(world_emissions_pc$gea_gea * world_population$gea_gea, years)
world_emissions$gea_image <- setNames(world_emissions_pc$gea_image * world_population$gea_image, years)
gea_gea <- gea$GEA


##### GDR / CERF #####
# Two methods to compare GDRs to equal pc (with the common limitation that we have to use a point estimate in 2030 as they don't provide figures beyond that date):
#>1. Use "our" gea_gea figures, and adjust the GDR allocation to the updated world_emissions$gea_gea["2030"] = 30.9Gt instead of their 1°5 29.4Gt (our closest figure is ssp2_26: 28.7Gt), LED 24.4Gt (our closest is ssp2_26: 28.7Gt in 2030) or their 2DS 35.95Gt (closest gea_image: 35Gt)
# 2. Use their LED 1.5° scenario (which is the NGOs baseline), gdp and pop figures for 2030 and compute our net gains in their setting. The issue with this method is that their LED 1.5°C trajectory is only disaggregated into North vs. South (and not sure it results from a uniform global price) so I don't know how to impute emissions per country.
# 3. Use "our" gea_gea figures for emissions, and theirs for gdp and pop. It shouldn't change much from ours, so let's simply do method 1 and use data from their scenario 1.5°C, closest to our gea_gea.
cerc_1d5 <- read.xlsx("../data/map_GCS/cerc_1D5.xlsx")
cerc_1d5 <- cerc_1d5[cerc_1d5$year == 2030, c("code", "emissions_baseline", "rci", "allocation_MtCO2")] # , "pop_mln", "gdp_blnUSDMER", "gdp_blnUSDPPP"
names(cerc_1d5)[!names(cerc_1d5) %in% c("code", "country", "year")] <- paste0(names(cerc_1d5)[!names(cerc_1d5) %in% c("code", "country", "year")], "_2030")
cerc_1d5$emissions_baseline_2030 <- 1e6 * cerc_1d5$emissions_baseline_2030
world_emissions_reduction_2030_cerc <- ((cerc_1d5$emissions_baseline_2030 - 1e6 * cerc_1d5$allocation_MtCO2_2030)/cerc_1d5$rci_2030)[1] # It's a constant vector
world_emissions_baseline_2030_cerc <- cerc_1d5$emissions_baseline_2030[cerc_1d5$code == "WORLD"]
world_emissions_1d5_2030_cerc <- world_emissions_baseline_2030_cerc - world_emissions_reduction_2030_cerc
world_emissions_1d5_2030 <- world_emissions$gea_gea[["2030"]]
world_emissions_reduction_2030 <- world_emissions_baseline_2030_cerc - world_emissions_1d5_2030
co2_pop <- merge(cerc_1d5[, 1:3], co2_pop) # This discards Taiwan, which is in cerc_1d5
co2_pop$gdr_pa_2030_cerc <- (co2_pop$emissions_baseline_2030 - co2_pop$rci_2030 * world_emissions_reduction_2030_cerc)/co2_pop$adult_2030
co2_pop$gdr_pa_2030 <- (co2_pop$emissions_baseline_2030 - co2_pop$rci_2030 * world_emissions_reduction_2030)/co2_pop$adult_2030
# wtd.mean(co2_pop$gdr_pa_2030_cerc, co2_pop$adult_2030)

# Disaggregate by country emissions, gdp, gdp_pc

compute_npv <- function(var = "gain_pa_", discount_rate = discount, data = co2_pop) {
  rate <- (1+discount_rate)^10
  return(rowSums(sapply(2:10, function(i) { return(10*data[[paste0(var, 2000+10*i)]]/rate^(i-2)) })))
}

compute_gain_given_parties <- function(parties = df$code, df = co2_pop, return = "df", discount = .03, ssp_name = "gea_gea") {
  if ("Dem USA" %in% parties & !"USA" %in% parties) parties <- c(parties, "USA")
  basic_income <- basic_income_adj <- c()
  for (y in seq(2020, 2100, 10)) {
    yr <- as.character(y)
    df[[paste0("participation_rate_", y)]] <- (1 - df[[paste0("large_footprint_", y)]] * df[[paste0("optout_right_", y)]]) * (df$code %in% parties)
    temp <- rep(T, nrow(df)) # df$code %in% parties # average_revenues is average emissions_pa * carbon_price while basic_income is adjusted for participation_rate due to opt-out and anti-regressive mechanism
    while (any(temp != df[[paste0("large_footprint_", y)]])) {
      temp <- df[[paste0("large_footprint_", y)]]
      basic_income[yr] <- wtd.mean(df[[paste0("revenues_pa_", y)]], df[[paste0("participation_rate_", y)]] * df[[paste0("adult_", y)]])
      df[[paste0("large_footprint_", y)]] <- (df[[paste0("revenues_pa_", y)]] > basic_income[yr])
      df[[paste0("participation_rate_", y)]] <- (1 - df[[paste0("large_footprint_", y)]] * df[[paste0("optout_right_", y)]]) * (df$code %in% parties)
    }
    df[[paste0("gain_optout_", y)]] <- df[[paste0("participation_rate_", y)]] * (basic_income[yr] - df[[paste0("revenues_pa_", y)]])
    # Adjusted to avoid high-income receiving money. Pb: GDP in PPP of Europe is not more than twice the world average 2050-2070.
    # To estimate future emissions and GDP, I make the assumption that emissions_pc/GDPpc evolve in the same way in all big regions. Pb: this assumption is at odd with SSP1, where GDPpc converge across regions.
    y_bar <- wtd.mean(df[[paste0("gdp_pc_", y)]], df[[paste0("participation_rate_", y)]] * df[[paste0("pop_", y)]])
    e_bar <- wtd.mean(df[[paste0("emissions_pa_", y)]], df[[paste0("participation_rate_", y)]] * df[[paste0("adult_", y)]])
    lambda <- pmax(0, pmin(1, (2.2*y_bar - df[[paste0("gdp_pc_", y)]])/((2.2-2)*y_bar))) # lambda = 1 means full basic income, lambda = 0 means basic income is proportional to emissions (if they are below 1.3*average)
    lambda[is.na(lambda)] <- 1
    df[[paste0("share_basic_income_", y)]] <- df[[paste0("participation_rate_", y)]] * (lambda + pmin(1, df[[paste0("emissions_pa_", y)]]/(1.3*e_bar))*(1-lambda))
    df[[paste0("gain_adj_", y)]][df[[paste0("emissions_pa_", y)]] < 1.3*e_bar] <- (basic_income[yr] * df[[paste0("share_basic_income_", y)]] -
                                                                                     df[[paste0("participation_rate_", y)]] * df[[paste0("revenues_pa_", y)]])[df[[paste0("emissions_pa_", y)]] < 1.3*e_bar]
    basic_income_adj[yr] <- basic_income[yr] * (1 + wtd.mean(df[[paste0("participation_rate_", y)]] - df[[paste0("share_basic_income_", y)]], df[[paste0("adult_", y)]]))
    df[[paste0("gain_adj_", y)]][lambda == 1 | df[[paste0("emissions_pa_", y)]] >= 1.3*e_bar] <- (df[[paste0("participation_rate_", y)]] * (basic_income_adj[yr] - df[[paste0("revenues_pa_", y)]]))[lambda == 1 | df[[paste0("emissions_pa_", y)]] >= 1.3*e_bar]
    df[[paste0("gain_adj_over_gdp_", y)]] <- df[[paste0("gain_adj_", y)]]/df[[paste0("gdp_pc_", y)]]
  }

  # GDR: find emissions allocations on website and allocate total_revenues[[ssp_name]][yr]. They go only until 2030. Either I recover the GDRs from them (or their code) and apply them here, or I add the per-capita allocation to their code.
  df$gain_gdr_2030 <- (carbon_price[[ssp_name]][["2030"]] * df$gdr_pa_2030  - df$revenues_pa_2030)
  df$gain_gdr_over_gdp_2030 <- df$gain_gdr_2030/df$gdp_pa_2030
  df$diff_gain_gdr_gcs_2030 <- df$gain_gdr_2030 - df$gain_pa_2030
  df$diff_gain_gdr_gcs_over_gdp_2030 <- df$diff_gain_gdr_gcs_2030/df$gdp_pa_2030
  df$diff_gain_gdr_gcs_adj_2030 <- df$gain_gdr_2030 - df$gain_adj_2030
  df$diff_gain_gdr_gcs_adj_over_gdp_2030 <- df$diff_gain_gdr_gcs_adj_2030/df$gdp_pa_2030

  df$npv_pa_gcs <- compute_npv("gain_pa_", discount = discount, data = df)
  df$npv_pa_gcs_adj <- compute_npv("gain_adj_", discount = discount, data = df)
  df$npv_over_gdp_gcs <- df$npv_pa_gcs/compute_npv("gdp_pa_", discount = discount, data = df) # this formula corresponds to the % loss in consumption computed in Balanced Growth Equivalent of Stern et al. (07)
  df$npv_over_gdp_gcs_adj <- df$npv_pa_gcs_adj/compute_npv("gdp_pa_", discount = discount, data = df)

  return(df)
}

total_revenues <- average_revenues <- average_revenues_bis <- basic_income <- basic_income_adj <- list()
create_var_ssp <- function(ssp, df = co2_pop, base_year = 2019, CC_convergence = 2040, discount = .03, opt_out_threshold = 1.5, full_part_threshold = 2, scenario = "all_countries") { # message is only for ssp2_ref , region = message_region_by_code
  ssp_name <- deparse(substitute(ssp))
  if (grepl("ssp1", ssp_name)) model <- "IMAGE"
  else if (ssp_name %in% c("ssp2_ref", "ssp2_26_country") | grepl("gea", ssp_name)) model <- "MESSAGE"
  else model <- "big"
  recoded_countries <- c("BWA", "GAB", "GNQ", "ZAF", "NAM")
  if (grepl("gea", ssp_name)) message_region_by_code_original <- message_region_by_code
  if (grepl("gea", ssp_name)) message_region_by_code[recoded_countries] <- "MEA" # c("Botswana", "Gabon", "Equatorial Guinea", "South Africa", "Namibia)
  region <- if (model == "big") big_region_by_code else { if (model == "IMAGE") image_region_by_code else message_region_by_code }
  regions <- if (model == "big") big_regions else { if (model == "IMAGE") image_regions else message_regions }
  total_revenues[[ssp_name]] <- average_revenues[[ssp_name]] <- average_revenues_bis[[ssp_name]] <- basic_income[[ssp_name]] <- basic_income_adj[[ssp_name]] <- c()
  if (!exists("scenarios_parties") & scenario == "all_countries") parties <- df$code
  else parties <- scenarios_parties[[scenario]]
  if ("Dem USA" %in% parties & !"USA" %in% parties) parties <- c(parties, "USA")

  if ("Dem USA" %in% parties) { # split USA for scenario == "optimistic" into Dem USA (the 12 States + DC with Democratic lead > 10 pp) and Non-Dem USA
    for (y in years) for (v in paste0(c("pop_", "adult_"), y)) df[[v]][df$code == "USA"] <- .3429 * df[[v]][df$code == "USA"]
    for (v in c("gdp_pc_2019", "GDPpcPPP")) df[[v]][df$code == "USA"] <- (.4082/.3429) * df[[v]][df$code == "USA"]
    df[[paste0("emissions_pa_", base_year)]][df$code == "USA"] <- (.2149/.3429) * df[[paste0("emissions_pa_", base_year)]][df$code == "USA"]
  }

  for (y in years) {
    yr <- as.character(y)
    for (v in paste0(c("pop_", "adult_", "emissions_", "gdp_"), y)) if (!v %in% names(df)) df[[v]] <- NA
    if (!paste0("gdp_", y) %in% names(ssp)) {
      if (paste0("gdp_mer_", y) %in% names(ssp)) {
        ssp[[paste0("gdp_", y)]] <- ssp[[paste0("gdp_mer_", y)]]
        df$gdp_pc_base_year <- df$gdp_pc_2019 # manage missing values (Venezuela, Yemen, South Sudan, North Korea, Eritrea, fix Western Sahara)
      } else {
        ssp[[paste0("gdp_", y)]] <- ssp[[paste0("gdp_ppp_", y)]]
        df$gdp_pc_base_year <- df$GDPpcPPP  
      }
    } else df$gdp_pc_base_year <- df$GDPpcPPP
    if (grepl("gea", ssp_name)) {
      for (v in paste0(c("pop_", "adult_"), y)) ssp[[v]][ssp$region == "MEA"] <- ssp[[v]][ssp$region == "MEA"] * (1 + sum(df[[v]][df$code %in% recoded_countries], na.rm = T)/sum(df[[v]][message_region_by_code_original[df$code] == "MEA"], na.rm = T))
      for (v in paste0(c("pop_", "adult_"), y)) ssp[[v]][ssp$region == "AFR"] <- ssp[[v]][ssp$region == "AFR"] * (1 - sum(df[[v]][df$code %in% recoded_countries], na.rm = T)/sum(df[[v]][message_region_by_code_original[df$code] == "AFR"], na.rm = T))
      for (v in c("emissions_", "gdp_")) ssp[[paste0(v, y)]][ssp$region == "MEA"] <- ssp[[paste0(v, y)]][ssp$region == "MEA"] * (1 + sum(df[[paste0(v, 2019)]][df$code %in% recoded_countries], na.rm = T)/sum(df[[paste0(v, 2019)]][message_region_by_code_original[df$code] == "MEA"], na.rm = T))
      for (v in c("emissions_", "gdp_")) ssp[[paste0(v, y)]][ssp$region == "AFR"] <- ssp[[paste0(v, y)]][ssp$region == "AFR"] * (1 - sum(df[[paste0(v, 2019)]][df$code %in% recoded_countries], na.rm = T)/sum(df[[paste0(v, 2019)]][message_region_by_code_original[df$code] == "AFR"], na.rm = T))
    }
    for (r in regions) { # Country downscaling
      region_r <- region[df$code] == r # df$code %in% region[r]
      # /!\ The line below overwrites UN's pop projections
      for (v in paste0(c("pop_", "adult_"), y)) df[[v]][region_r] <- df[[v]][region_r] * ssp[[v]][ssp$region == r] / sum(df[[v]][region_r], na.rm = T)
      reduction_factor_ry <- ssp[[paste0("emissions_", y)]][ssp$region == r]/sum((df[[paste0("emissions_pa_", base_year)]] * df[[paste0("adult_", y)]])[region_r])
      df[[paste0("emissions_pa_", y)]][region_r] <- reduction_factor_ry * df[[paste0("emissions_pa_", base_year)]][region_r] # df$emissions_2019[region_r] * ssp[[paste0("emissions_", y)]][ssp$region == r] / sum(df$emissions_2019[region_r], na.rm = T)
      df[[paste0("emissions_", y)]][region_r] <- (df[[paste0("emissions_pa_", y)]] * df[[paste0("adult_", y)]])[region_r]
      growth_factor_ry <- ssp[[paste0("gdp_", y)]][ssp$region == r]/sum((df$gdp_pc_base_year * df[[paste0("pop_", y)]])[region_r], na.rm = T)
      df[[paste0("gdp_pc_", y)]][region_r] <- growth_factor_ry * df$gdp_pc_base_year[region_r]
      df[[paste0("gdp_", y)]][region_r] <- (df[[paste0("gdp_pc_", y)]] * df[[paste0("pop_", y)]])[region_r] # df$gdp_ppp_now[region_r] * ssp[[paste0("gdp_ppp_", y)]][ssp$region == r] / sum(df$gdp_ppp_now[region_r], na.rm = T)
    }
    df[[paste0("gdp_pc_over_mean_", y)]] <- df[[paste0("gdp_pc_", y)]]/wtd.mean(df[[paste0("gdp_pc_", y)]], df[[paste0("pop_", y)]])
    df[[paste0("gdp_pa_", y)]] <- df[[paste0("gdp_", y)]]/df[[paste0("adult_", y)]]
    df[[paste0("emissions_pc_", y)]] <- df[[paste0("emissions_", y)]]/df[[paste0("pop_", y)]]

    # Unadjusted mean gain pa
    if (y > 2015) {
      df[[paste0("revenues_pa_", y)]] <- carbon_price[[ssp_name]][yr] * pmax(0, df[[paste0("emissions_pa_", y)]]) # /12
      total_revenues[[ssp_name]][yr] <- carbon_price[[ssp_name]][yr] * sum(df[[paste0("emissions_", y)]], na.rm = T) # ssp[[paste0("emissions_", y)]][ssp$region == "world"]
      if (total_revenues[[ssp_name]][yr] < 0) df[[paste0("revenues_pa_", y)]] <- 0

      # GCS
      df[[paste0("gain_pa_", y)]] <- (total_revenues[[ssp_name]][yr]/sum(df[[paste0("adult_", y)]], na.rm = T) - df[[paste0("revenues_pa_", y)]]) # /ssp[[paste0("adult_", y)]][ssp$region == "world"]
      df[[paste0("gain_over_gdp_", y)]] <- df[[paste0("gain_pa_", y)]]/df[[paste0("gdp_pc_", y)]]
      # Adjusted for opt out
      df[[paste0("optout_right_", y)]] <- (full_part_threshold - pmax(opt_out_threshold, pmin(full_part_threshold, df[[paste0("gdp_pc_", y)]] / wtd.mean(df[[paste0("gdp_pc_", y)]], df[[paste0("pop_", y)]]))))/(full_part_threshold - opt_out_threshold)
      # Accounts for non-universal participation
      average_revenues_bis[[ssp_name]][yr] <- total_revenues[[ssp_name]][yr]/ssp[[paste0("adult_", y)]][ssp$region == "world"] # a bit different from average_revenues because in ssp, world emissions is not the sum of regional emissions (be it image_ or big_ regions)
      average_revenues[[ssp_name]][yr] <- wtd.mean(df[[paste0("revenues_pa_", y)]], df[[paste0("adult_", y)]])
      df[[paste0("large_footprint_", y)]] <- (df[[paste0("revenues_pa_", y)]] > average_revenues[[ssp_name]][yr])

      # C&C: define climate debt/credit until convergence date
    }
  }

  df_parties <- compute_gain_given_parties(parties, df = df, return = "df", ssp_name = ssp_name)
  for (y in seq(2020, 2100, 10)) {
    yr <- as.character(y)
    basic_income[[ssp_name]][yr] <- wtd.mean(df_parties[[paste0("revenues_pa_", y)]], df_parties[[paste0("participation_rate_", y)]] * df_parties[[paste0("adult_", y)]])
    basic_income_adj[[ssp_name]][yr] <- basic_income[[ssp_name]][yr] * (1 + wtd.mean(df_parties[[paste0("participation_rate_", y)]] - df_parties[[paste0("share_basic_income_", y)]], df_parties[[paste0("adult_", y)]]))
  }

  total_revenues[[ssp_name]] <<- total_revenues[[ssp_name]]
  average_revenues[[ssp_name]] <<- average_revenues[[ssp_name]]
  average_revenues_bis[[ssp_name]] <<- average_revenues_bis[[ssp_name]]

  if (length(setdiff(df$code, parties)) == 0) {
    basic_income[[ssp_name]] <<- basic_income[[ssp_name]]
    basic_income_adj[[ssp_name]] <<- basic_income_adj[[ssp_name]]
    df <- df_parties
  } else {
    for (v in names(df)[grepl(c("^gain_adj_|^gain_adj_over_gdp_|^npv_pa_gcs_adj|^npv_over_gdp_gcs_adj|^diff_gain_gdr_gcs_adj"), names(df))]) df[[paste0("S", scenario, "_", v)]] <- df_parties[[v]]
    for (v in names(df)[grepl(c("^gain_adj_|^gain_adj_over_gdp_|^npv_pa_gcs_adj|^npv_over_gdp_gcs_adj"), names(df))]) df[[paste0("S", scenario, "_", v)]][!df$code %in% parties] <- NA

    # if ("Dem USA" %in% parties) for (v in names(df)) if (is.numeric(df[[v]]) & !grepl(paste0("S", scenario), v)) df[[v]][df$code %in% c("Dem USA", "Non-Dem USA")] <- 0
  }
  basic_income[[scenario]] <<- basic_income[[ssp_name]]
  basic_income_adj[[scenario]] <<- basic_income_adj[[ssp_name]]
  return(df)
}

co2_pop <- create_var_ssp(gea_gea, opt_out_threshold = 1.5) 

# Net median gain in our 5 countries of interest
(median_gain_2015_LCU <- LCU_per_dollar*co2_pop$median_gain_2015[sapply(c("FRA", "DEU", "ESP", "GBR", "USA"), function(c) which(co2_pop$code == c))])


##### Export maps and table #####

thresholds_map <- c(-Inf, -70, -30, -20, -10, 0, 10, 15, 20, 25, Inf)
# Figure S9: figures/maps/gain_gdr_over_gdp_2030.pdf
plot_world_map("gain_gdr_over_gdp_2030", breaks = c(-Inf, -.04, -.02, -.01, -.005, -1e-10, 0, .03, .1, .2, .5, Inf), format = c('png', 'pdf'), legend_x = .07, trim = T, # svg, pdf 12*c(-Inf, -70, -30, -20, -10, -.1/12, .1/12, 5, 10, 15, 20, Inf)
               labels =  sub("≤", "<", agg_thresholds(c(0), 100*c(-Inf, -.04, -.02, -.01, -.005, 0, 0, .03, .1, .2, .5, Inf), sep = " to ", return = "levels")), df = co2_pop,
               legend = paste0("Gains per adult\nfrom GDRs\nin 2030 (in % of GDP)"), #fill_na = T,
               save = T)
# Figure S10: figures/maps/diff_gain_gdr_gcs_over_gdp_2030.pdf
plot_world_map("diff_gain_gdr_gcs_over_gdp_2030", breaks = c(-Inf, -.3, -.1, -.05, -.02, -1e-10, 0, .005, .01, .05, .1, Inf), format = c('png', 'pdf'), legend_x = .07, trim = T, # svg, pdf 12*c(-Inf, -70, -30, -20, -10, -.1/12, .1/12, 5, 10, 15, 20, Inf)
               labels =  sub("≤", "<", agg_thresholds(c(0), 100*c(-Inf, -.3, -.1, -.05, -.02, 0, 0, .005, .01, .05, .1, Inf), sep = " to ", return = "levels")), df = co2_pop,
               legend = paste0("Difference in\nnet gains:\nGDRs - equal pc\nin 2030 (in % of GDP)"), #fill_na = T,
               save = T)

# Figure S48: figures/maps/median_gain_2015.pdf
plot_world_map("median_gain_2015", breaks = thresholds_map, format = c('png', 'svg', 'pdf'), trim = T, # svg, pdf
               labels = sub("≤", "<", agg_thresholds(c(0), thresholds_map, sep = " to ", return = "levels")), 
               legend = "Median net\ngain per capita\nfrom the GCS\n(in $/month)", df = co2_pop, #fill_na = T,
               save = T, width = 1160, height = 560) # c(min(co2_pop$mean_gain_2030), max(co2_pop$mean_gain_2030)) limits = c(-30, 30), 

# Table S4: tables/gain_gcs.tex, created in code_global/map_GCS_incidence.R
min_pop_table_gain_gcs <- 20e6
table_gain_gcs <- sort(setNames(co2_pop$mean_gain_2030[co2_pop$adult_2019 > min_pop_table_gain_gcs], co2_pop$country[co2_pop$adult_2019 > min_pop_table_gain_gcs]))
temp <- sort(setNames(co2_pop$emissions_pa_2019[co2_pop$adult_2019 > min_pop_table_gain_gcs], co2_pop$country[co2_pop$adult_2019 > min_pop_table_gain_gcs])) # China has larger footprint than France!
table_gain_gcs <- cbind(table_gain_gcs, temp[names(table_gain_gcs)])
row.names(table_gain_gcs)[row.names(table_gain_gcs) %in% co2_pop$country[co2_pop$missing_footprint]] <- paste0(row.names(table_gain_gcs)[row.names(table_gain_gcs) %in% co2_pop$country[co2_pop$missing_footprint]], "*")
row.names(table_gain_gcs)[row.names(table_gain_gcs) %in% c("Democratic Republic of Congo*", "Democratic Republic of the Congo*")] <- "DRC*"
cat(paste(kbl(table_gain_gcs, "latex", caption = "Estimated net gain from the GCS in 2030 and carbon footprint by country.", position = "b", escape = F, booktabs = T, digits = c(0, 1), linesep = rep("", nrow(table_gain_gcs)-1), longtable = T, label = "gain_gcs.tex",
              col.names = c("\\makecell{Mean\\\\net gain\\\\from\\\\the GCS\\\\(\\$/month)}", "\\makecell{CO$_\\text{2}$\\\\footprint\\\\per adult\\\\in 2019\\\\(tCO$_\\text{2}$/y)}")), collapse="\n"), file = "../tables/gain_gcs.tex") 

rm(SSPs, SSPs_countries, ssp, pg, ssp2_26, ssp_countries, pop, pop_adult, pop_adult_iso3, pop_iso3, pop.R5, pg, pg2, GDPpc, GDPpcPPP, co2, gea, gea_emissions, gea_gea, gea_pop, gea_gdp_mer)
# rm(co2_pop)