## 경제대학 학술대회 ACE
## 구재영, 권민주



# Setting Working Directory
setwd('/Users/koojy/Desktop/CONTEST/2024/7. 2024 경제대학 학술대회 ACE')

# Loading Libraries
library(readxl) ; library(urca) ; library(sandwich) ; library(lmtest)
library(vars) ; library(svars) ; library(ggplot2) ; library(tseries)
library(xts) ; library(date) ; library(lubridate) ; library(tsDyn)
library(zoo) ; library(strucchange) ; library(BVAR) ; library(mvtnorm)
library(reshape2) ; library(tidyr) ; library(dplyr)




#############################
# Loading Data - World Bank #
#############################

WB = read.csv("WB_dt.csv")

colnames(WB) <- gsub("^X", "", colnames(WB))
colnames(WB) <- gsub("\\.\\.YR.*", "", colnames(WB))
colnames(WB)[5:ncol(WB)] <- as.numeric(colnames(WB)[5:ncol(WB)])

WB[WB == ".."] <- NA


name_mapping <- c(
  "Current health expenditure per capita (current US$)" = "HEXPEN",
  "Current health expenditure (% of GDP)" = "HEXPENt",
  "Unemployment, total (% of total labor force) (national estimate)" = "UNEMP",
  "GDP (constant 2015 US$)" = "rGDP",
  "GDP growth (annual %)" = "GDPg",
  "Population ages 65 and above (% of total population)" = "AGING",
  "Gross savings (% of GDP)" = "SAVING%",
  "Gross savings (current US$)" = "SAVING"
)

WB$Series.Name <- name_mapping[WB$Series.Name]

WB <- WB %>%
  pivot_longer(cols = matches("^(19|20)\\d{2}$"),
               names_to = "Year", 
               values_to = "Value")

WB <- WB %>%
  pivot_wider(names_from = Series.Name, values_from = Value)

colnames(WB)[colnames(WB) == "Country.Name"] <- "Country"
WB <- WB %>%
  arrange(Country)



WB2 = read_excel("WB2.xlsx")

colnames(WB2) <- gsub("\\.\\.YR.*", "", colnames(WB2))
colnames(WB2) <- gsub("^.*\\[YR", "", colnames(WB2))
colnames(WB2) <- gsub("\\]", "", colnames(WB2))
colnames(WB2)[5:ncol(WB2)] <- as.numeric(colnames(WB2)[5:ncol(WB2)])

WB2[WB2 == ".."] <- NA
WB2 <- WB2[1:1519,]
unique(WB2$`Series Name`)


name_mapping <- c(
  "Current health expenditure per capita (current US$)" = "HEXPEN",
  "GDP growth (annual %)" = "GDPg",
  "Population ages 65 and above (% of total population)" = "AGING",
  "Gross domestic savings (% of GDP)" = "DSAVE",
  "Gross savings (% of GDP)" = "SAVING",
  "Gross national expenditure (% of GDP)" = "EXPEN",
  "Life expectancy at birth, total (years)" = "LEXP"
)

WB2$`Series Name` <- name_mapping[WB2$`Series Name`]

WB2_long <- WB2 %>%
  pivot_longer(cols = matches("^(19|20)\\d{2}$"),
               names_to = "Year", 
               values_to = "Value")
WB2_long <- WB2_long %>%
  filter(Year >= 2005 & Year <= 2021)
WB2_long <- WB2_long[,c("Country Name", "Series Name", "Year", "Value")]

WB2 <- WB2_long %>%
  pivot_wider(names_from = `Series Name`, values_from = Value)

colnames(WB2)[colnames(WB2) == "Country Name"] <- "Country"

na_wb2 <- WB2 %>%
  group_by(Country) %>%
  summarise(across(c("HEXPEN", "GDPg", "AGING", "DSAVE",
                     "SAVING", "EXPEN", "LEXP"), ~ sum(is.na(.)), .names = "{.col}"))



#######################
# Loading Data - OECD #
#######################

OECD <- read.csv("OECD.csv")

OECD <- OECD %>%
  arrange(Country, TIME_PERIOD, Indicator)

# 필요없는 행 제거
OECD <- OECD %>%
  filter(!(Indicator == "Employment rate" & SUBJECT != "TOT"))
OECD <- OECD %>%
  filter(!(Indicator == "GDP per hour worked" & MEASURE != "IDX2015"))
OECD <- OECD %>%
  filter(!(Indicator == "Industrial production" & SUBJECT != "TOT"))
OECD <- OECD %>%
  filter(INDICATOR != "EMPAGE")
OECD <- OECD %>%
  filter(!(INDICATOR == "EMP" & MEASURE != "PC_WKGPOP"))

OECD$INDICATOR <- ifelse(OECD$INDICATOR == "INDPROD", "IIP", OECD$INDICATOR)
OECD$INDICATOR <- ifelse(OECD$INDICATOR == "GDPHRWKD", "LABOR", OECD$INDICATOR)

OECD <- OECD[, c("Country", "TIME_PERIOD", "INDICATOR", "OBS_VALUE")]

OECD <- OECD %>%
  pivot_wider(names_from = INDICATOR, values_from = OBS_VALUE)

colnames(OECD) <- c("Country", "Year", "EMP", "LABOR", "IIP")








##############
# Employment #
##############

EMP <- read.csv("EMP.csv")
EMP <- subset(EMP, sex.label == "Sex: Total" & classif1.label == "Age (Youth, adults): 15+")
EMP = cbind(EMP[, 1], EMP[,6:7])
colnames(EMP) <- c("Country", "Year", "EMP")
EMP <- EMP %>%
  arrange(Country, Year)

EMP_edit <- EMP %>%
  # 각 Country와 Year에 대해 모든 조합 생성 (완전한 데이터 프레임 생성)
  complete(Country, Year = 2005:2021) %>%
  arrange(Country, Year)

na_emp <- EMP_edit %>%
  group_by(Country) %>%
  summarise(missing_count = sum(is.na(EMP)))

(country_fullEMP = as.list(na_emp[na_emp$missing_count == 0, "Country"]))
# 65개국



#######################
# GDP per Hour Worked #
#######################

LAB <- read.csv("LABOR.csv")
LAB <- cbind(LAB[, 1], LAB[,4:5])
colnames(LAB) <- c("Country", "Year", "LABOR")
LAB <- LAB %>%
  arrange(Country, Year)

LAB_edit <- LAB %>%
  complete(Country, Year = 2005:2021) %>%
  arrange(Country, Year)

na_labor <- LAB_edit %>%
  group_by(Country) %>%
  summarise(missing_count = sum(is.na(LABOR)))

(country_labor = as.list(na_labor[na_labor$missing_count == 0, "Country"]))
# 274개 (World, Region 포함)



##############
# Final Data #
##############

unique(WB2$Country) ; unique(EMP_edit$Country) ; unique(LAB_edit$Country)

# 1. 국가명 표기 통일
country_name_mapping <- c(
  "Korea, Rep." = "South Korea",
  "Korea (the Democratic People's Republic of)" = "North Korea",
  "United States" = "United States of America",
  "Russian Federation" = "Russia",
  "Egypt, Arab Rep." = "Egypt",
  "Iran, Islamic Rep." = "Iran",
  "Venezuela, RB" = "Venezuela",
  "Hong Kong SAR, China" = "Hong Kong",
  "Yemen, Rep." = "Yemen",
  "Congo, Rep." = "Congo",
  "Congo, Dem. Rep." = "Democratic Republic of the Congo",
  "Côte d'Ivoire" = "Ivory Coast",
  "Gambia, The" = "Gambia",
  "Bahamas, The" = "Bahamas",
  "Gambia, The" = "Gambia",
  "Brunei Darussalam" = "Brunei",
  "China, Hong Kong SAR" = "Hong Kong",
  "Macao SAR, China" = "Macao",
  "Lao PDR" = "Laos",
  "Myanmar" = "Burma",
  "Moldova" = "Republic of Moldova",
  "Slovak Republic" = "Slovakia",
  "Viet Nam" = "Vietnam",
  "Syrian Arab Republic" = "Syria",
  "United Kingdom" = "United Kingdom of Great Britain and Northern Ireland",
  "United Arab Emirates" = "UAE",
  "Macao SAR, China" = "Macao",
  "Czechia" = "Czech Republic",
  "Eswatini" = "Swaziland",
  "Bolivia (Plurinational State of)" = "Bolivia",
  "Micronesia, Fed. Sts." = "Micronesia",
  "Saint Kitts and Nevis" = "St. Kitts and Nevis",
  "Saint Lucia" = "St. Lucia",
  "Saint Vincent and the Grenadines" = "St. Vincent and the Grenadines",
  "Trinidad and Tobago" = "Trinidad & Tobago",
  "United States" = "United States of America",
  "Venezuela (Bolivarian Republic of)" = "Venezuela",
  "Venezuela, RB" = "Venezuela",
  "Iran (Islamic Republic of)" = "Iran",
  "West Bank and Gaza" = "Palestine"
)

# 매핑 적용
WB2$Country <- recode(WB2$Country, !!!country_name_mapping)
EMP_edit$Country <- recode(EMP_edit$Country, !!!country_name_mapping)
LAB_edit$Country <- recode(LAB_edit$Country, !!!country_name_mapping)

# 2. 공통된 국가명 리스트 추출
common_countries <- Reduce(intersect, list(WB2$Country, EMP_edit$Country, LAB_edit$Country))

# 3. 공통 국가에 대해 데이터 병합
# 먼저 공통 국가로 필터링
WB2_filtered <- WB2 %>% filter(Country %in% common_countries)
EMP_filtered <- EMP_edit %>% filter(Country %in% common_countries)
LAB_filtered <- LAB_edit %>% filter(Country %in% common_countries)

# Year 열을 숫자형으로 변환
WB2_filtered$Year <- as.integer(as.character(WB2_filtered$Year))
EMP_filtered$Year <- as.integer(as.character(EMP_filtered$Year))
LAB_filtered$Year <- as.integer(as.character(LAB_filtered$Year))

# Country와 Year 기준으로 데이터 병합
merged_data <- WB2_filtered %>%
  inner_join(EMP_filtered, by = c("Country", "Year")) %>%
  inner_join(LAB_filtered, by = c("Country", "Year"))

unique(merged_data$Country)

write.csv(merged_data, "DATA_new.csv")







# 국가별로 결측치 개수를 계산하는 코드
missing_by_country <- sapply(split(merged_data, merged_data$Country), function(df) {
  sapply(df, function(x) sum(is.na(x)))
})

missing_by_country <- t(missing_by_country)  # 행과 열을 바꿔서 보기 쉽게 변환
missing_by_country = as.data.frame(missing_by_country)
write.csv(missing_by_country, "NA_DATA_n.csv")

# 변수별로 결측치 개수를 계산하는 코드
missing_by_variable <- sapply(merged_data, function(x) sum(is.na(x)))
missing_by_variable = as.data.frame(missing_by_variable)




###########
# RESULTS #
###########

library(dplyr) ; library(tidyr) ; library(lmtest) ; library(sandwich) ; library(purrr)

result = read.csv("results.csv")
unique(result$country)
unique(result$year)
colnames(result)
### [1] "v1"         "country"    "year"       "hexpen"     "gdpg"       "aging"      "dsave"    ### [8] "saving"     "expen"      "lexp"       "emp"        "labor"      "country_id" "D_labor"   
### [15] "D_hexpen"   "X_cat"  

fem1 <- lm(lexp ~ aging + factor(year) + factor(country), data = result)
fem1.CSE <- vcovHC(fem1, type = "HC1", cluster = "country_i")
summary(fem1, robust = TRUE)
coeftest(fem1, vcov = fem1.CSE)

plot(result$lexp, result$aging)
cor(result$lexp, result$aging)

lm1 <- lm(lexp ~ aging, data = result)
summary(lm1)

fem2 <- lm(lexp ~ aging + hexpen + factor(year) + factor(country), data = result)
fem2.CSE <- vcovHC(fem2, type = "HC1", cluster = "country_i")
summary(fem2, robust = TRUE)
coeftest(fem2, vcov = fem2.CSE)

fem3 <- lm(lexp ~ aging + hexpen + saving + factor(year) + factor(country), data = result)
fem3.CSE <- vcovHC(fem3, type = "HC1", cluster = "country_i")
summary(fem3, robust = TRUE)
coeftest(fem3, vcov = fem3.CSE)

fem4 <- lm(lexp ~ aging + saving + factor(year) + factor(country), data = result)
fem4.CSE <- vcovHC(fem4, type = "HC1", cluster = "country_i")
summary(fem4, robust = TRUE)
coeftest(fem4, vcov = fem4.CSE)

fem5 <- lm(aging ~ lexp + saving + expen + emp + D_hexpen + D_labor
           + factor(year) + factor(country), data = result)
fem5.CSE <- vcovHC(fem5, type = "HC1", cluster = "country_i")
summary(fem5, robust = TRUE)
coeftest(fem5, vcov = fem5.CSE)


