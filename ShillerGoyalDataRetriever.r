library(httr) # downloading the xls(x) files
library(readxl) # reading xls(x) files
library(dplyr) # data formatting

# Read Shiller data ----
GET("http://www.econ.yale.edu/~shiller/data/ie_data.xls",
    write_disk(temp <- tempfile(fileext = ".xls")))
shillerdata <- read_xls(temp, sheet = 3, skip = 7)

# Format the years and months correctly
current_year <- format(Sys.time(), "%Y")
corrected_dates <- expand.grid(1:12, 1871:current_year)
last_month <- length(grep(current_year, shillerdata$Date))
months_to_be_cut_off <- 12 - last_month
corrected_dates <- head(corrected_dates,
                        nrow(corrected_dates)- months_to_be_cut_off)

# Add leading zeros
corrected_dates$Var1 <- sprintf("%02d", as.numeric(corrected_dates$Var1))
dates <- as.data.frame(paste(corrected_dates$Var2,
                             corrected_dates$Var1, sep = "-"))
names(dates) <- "dates"

# Remove possible excess rows & add corrected dates back
shillerdata <- head(shillerdata, nrow(dates))
shillerdata <- cbind(dates, shillerdata)
shillerdata$Date <- NULL

# Read Goyal data ----
GET("http://www.hec.unil.ch/agoyal/docs/PredictorData2017.xlsx",
    write_disk(temp <- tempfile(fileext = ".xls")))
goyaldata <- read_xlsx(temp, sheet = 1)
goyaldata <- select(goyaldata, c("yyyymm", "infl", "b/m"))

# Make dates into same format as above and prepare names for joining
goyaldata$yyyymm <- paste(substr(goyaldata$yyyymm, 1, 4),
                          substr(goyaldata$yyyymm, 5, 6), sep = "-")
names(goyaldata) <- c("dates", "infl", "bm")

full_data <- full_join(shillerdata, goyaldata, by = "dates")

# Replace written NAs with real NAs ----
full_data <- full_data %>% 
  mutate(bm = ifelse(bm == "NaN", NA, bm),
         infl = ifelse(infl == "NaN", NA, as.numeric(infl) * 12),
         CAPE = ifelse(CAPE == "NA", NA, as.numeric(CAPE)))

# Calculate total returns
# First calculate the monthly returns
full_data$diff <- lag(lead(full_data$P) / full_data$P)
# Then calculate an index including dividends
full_data$index <- NA
# First observation
full_data$index[2] <- (full_data$P[1] +
                       full_data$D[1] / 12) *
                       full_data$diff[2]

# Calculate real returns
for (i in 1:I(nrow(full_data) - 2)) {
  full_data$index[i + 2] <- (full_data$index[i + 1] +
                             full_data$D[i + 1] / 12) *
                             full_data$diff[i + 2]
  }
# Calculate ten year returns
full_data$tenyear <- (lead(full_data$index, 12 * 10) /
                        full_data$index)^(1 / 10)

# Calculate real total returns
# First calculate inflation
full_data$cpichange <- lag(lead(full_data$CPI) / full_data$CPI)
# Calculate monthly real returns
full_data$indexinfl <- full_data$diff / full_data$cpichange
full_data$index_real <- NA
# First observation
full_data$index_real[2] <- (full_data$P[2] +
                            full_data$D[2] / 12) *
                            full_data$indexinfl[2]
# TODO 
# Calculate real returns including reinvested dividends
for (i in 1:I(nrow(full_data) - 2)){
  full_data$index_real[i + 2] <- (full_data$index_real[i + 1] +
                                  full_data$D[i + 2] / 12) *
                                  full_data$indexinfl[i + 2]
  }
# Then calculate ten-year real total returns
full_data$tenyear_real <- (lead(full_data$index_real, 12 * 10) / full_data$index_real) ^ (1 / 10)

# Return only full data
rm(list = setdiff(ls(), "full_data"))