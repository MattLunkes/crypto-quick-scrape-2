############################################################
###      Crypto Quick Scrape part II, by Matt Lunkes     ###
############################################################


#### Overview: ######################################## ####

# Set-up  

# Step 1: Obtain price data from CMC's API
# Step 2: Re-create sort columns & rerun rest of part I 
# Step 3: Prepare urls for historical data scrape
# Step 4: Scrape historical coin data

# Analyze data in R
# Step 5: Simple correlations with BTC
# Step 6: Trailing 30-day correlations with BTC
# Step 7: Ingest index data
# Step 8: Calculate betas relative to CCi30 index
# Step 9: Calculate betas relative to S&P 500
# Step 10: Compare the S&P500 & CCi30 directly


# Set-up  ############################################# ####
############################################################

# Load primary packages
library(rvest)
library(httr)
library(jsonlite)
library(quantmod)

# Load graphing packages
library(ggplot2)
library(ggthemes)	#<-- adds awesome chart themes
library(reshape2)	#<-- preps data for easier ggplot2 ingest (also written by HW)


# Step 1: Obtain price data from CMC's API  ########### ####
############################################################

### Use httr and jsonLITE to obtain market data
cmc_api <- GET("https://api.coinmarketcap.com/v1/ticker/?limit=0")
cmc_prep <- content(cmc_api, as = "text")
cmc <- fromJSON(cmc_prep)

# Check the new version of cmc
class(cmc)
str(cmc)


# Step 2: Re-create sort columns & rerun rest of part I ####
############################################################

# They're less messed up w/ special characters, but you still need to RE-Create sort columns
cmc$Mkt_Cap_sort <- as.numeric(cmc$market_cap_usd)
cmc$Price_sort <- as.numeric(cmc$price_usd)
cmc$Circ_Supply_sort <- as.numeric(cmc$available_supply)

# Check out the new version of cmc
head(cmc)

# Re-run steps 4 - 5 in part I (starts at line 71)
browseURL("https://github.com/MattLunkes/crypto-quick-scrape/blob/master/crypto_quick_scrape.R")


# Step 3: Prepare urls for historical data scrape ##### ####
############################################################

# Prepare a vector of coin IDs
sqlch_cn <- 20 #<- Squelch the # of coins to reduce noise
coin <- cmc$id[1:sqlch_cn]

# Set a date range
trail <- 180
end_date <- gsub("-","", Sys.Date())
start_date <- gsub("-","", Sys.Date()-trail)

# Compile the above info into a Vector of URLS
urls <- paste("https://coinmarketcap.com/currencies/",coin,"/historical-data/?start=",start_date,"&end=",end_date, sep = "")


# Step 4: Scrape historical coin data  ################ ####
############################################################

# Read-IN urls, grab HLOC tables (via the xpath node), & convert to data.frames
# Use a quick custom function to bulkify the rvest functions
scrape <- function(i) {
	x <- read_html(i)
	x <- x %>% html_nodes(xpath = '//*[@id="historical-data"]/div/div[3]/table')
	x <- x %>% html_table() %>% data.frame()
}

# Use lapply() to run all or your urls through scrape()
ohlc <- lapply(urls, scrape)
names(ohlc) <- coin #<- name all of your list elements (they'll be in order of "coin")

# remove those w/out the length of time required by Trail
ohlc <- ohlc[sapply(ohlc, function(i) nrow(i) >= trail)]

# Pull out the Close prices for each coin
# Also, resequence the dates as actual Sys.Date() values, instead of characters
prices <- cbind(as.data.frame(seq(Sys.Date()-1, Sys.Date()-trail, by="-1 day")), as.data.frame(lapply(ohlc,"[[",5)))
# NOTE: the beginning of the date sequence needs to be Today - 1, because the cmc historical tables only run up to YESTERDAY
colnames(prices) <- c("Date", names(ohlc))
head(prices[,1:6])

# Create a day-over-day returns matrix based on prices
returns <- as.data.frame(matrix(, nrow = nrow(prices), ncol = ncol(prices)))
returns[,1] <- prices[,1]
names(returns) <- names(prices)

for(i in 1:nrow(returns)) {
	if (i < nrow(returns)) {
		returns[i,2:ncol(returns)] <- prices[i,2:ncol(prices)] / prices[i+1,2:ncol(prices)] - 1
	} else {
		returns[i,2:ncol(returns)] <- NA
	}
}

returns <- na.omit(returns)
rownames(returns) <- NULL

# Spot check:
head(prices[,1:6])
head(returns[,1:6])


# Step 5: Simple correlations with BTC  ############### ####
############################################################

# Tee up a new data.frame consisting of all the non-BTC coins from returns
btc_corr <- as.data.frame(names(returns)[3:ncol(returns)])
names(btc_corr) <- "coin"

# Loop through and find each's correlation with Btc
for(i in 1:nrow(btc_corr)) {
	x <- cor(returns$bitcoin, returns[,2+i])
	btc_corr$Corr_w_btc[i] <- x
}

# Check it out!
btc_corr

# Plot it
ggplot(data=btc_corr, aes(x=coin, y=Corr_w_btc, fill=Corr_w_btc)) +
	geom_col() +
	labs(title = "Correlation with BTC Returns", subtitle = paste("Last 180 Days from ",Sys.Date()), x = "Coin", y = "Correlation with BTC") +
	ylim(0,1) +
	theme_economist() +
	theme(axis.text.x=element_text(angle=45,hjust=1,vjust=1), legend.position="none")


# Step 6: Trailing 30-day correlations with BTC  ###### ####
############################################################

# Build a matrix to track each coin's correlation with Bitcoin
# Since we're focusing on 30 day trail, our data.frame needs to omit the last 30 rows of "returns", for which we don't have enough data to compute a trailing 30-day correlation
tr_corr <- as.data.frame(matrix(, nrow = (nrow(returns)-30), ncol = ncol(returns)))
tr_corr[,1] 		<- returns[1:(nrow(returns)-30),1] #<- Date column!
colnames(tr_corr) 	<- colnames(returns)
head(tr_corr)

# Loop through to calculate trailing correlations
for(i in 1:(nrow(returns)-30)) {
	for(j in 1:(ncol(returns)-1)) {
	tr_corr[i,j+1] <- cor(returns[i:(i+29),2],returns[i:(i+29),j+1])
	}
}

# Check the results for the top 10 non-BTC coins
head(tr_corr[,c(1,3:12)])

# Expand that to 10 coins and reshape (i.e. 1 row per observation of a coin correlation )
tr_melt <- melt(tr_corr[1:100,c(1,3:12)], id="Date")  

# Check out the stucture, and then some random rows:
str(tr_melt)
tr_melt[c(3,234,600),]

# Plot it out!
ggplot(data=tr_melt, aes(x=Date, y=value, color=variable)) +
    geom_line() +
	labs(title = "Trailing 30-day Correlations with BTC Returns", subtitle = paste("Last 150 Days ending on",Sys.Date()), x = "Date", y = "Correlation with BTC") +
	ylim(-1,1) +
	theme_economist()


# Step 7: Ingest index data  ########################## ####
############################################################

# Let's get some ßs!! 
# First, we need some index data (grabbing for cryptos, and then also the S&P)
cci30_raw <- read.csv(url("https://cci30.com/ajax/getIndexHistory.php"))
sp500_raw <- as.data.frame(getSymbols("^GSPC",auto.assign = FALSE, from = (Sys.Date()-trail), to = Sys.Date()))
str(cci30_raw)
str(sp500_raw) #<-- Columns are a bit different; we'll fix that when we clean up

# Adjust and calculate returns for each
# Start with the crypto index:
# Replace the Date column with actual Sys.Date() values
cci30_Date <- strsplit( as.character(cci30_raw$Date),"T")
cci30_Date <- as.Date(sapply(cci30_Date,`[`,1))
cci30_raw$Date <- cci30_Date

# Loop through to calculate the CCi30's day-to-day return
for(i in 1:nrow(cci30_raw)) {
	if (i>1) {
		cci30_raw$cci30_rtn[i] <- cci30_raw$Close[i] / cci30_raw$Close[i-1] -1
	} else {
		cci30_raw$cci30_rtn[i]   <- NA
	}
}

cci30 <- na.omit(cci30_raw) # Remove any NAs

# Then work on the S&P
# First, pull out the dates, and fix up the raw feed a bit:
sp500 <- cbind(sp500_raw, as.Date(rownames(sp500_raw)) )
colnames(sp500) <- c("Open", "High", "Low", "Close", "Volume", "Adjusted", "Date")
rownames(sp500) <- NULL

# Loop through to calculate the S&P's day-to-day return
# Use the non-raw matrix to avoid the date issues:
for(i in 1:nrow(sp500)) {
	if (i>1) {
		sp500$sp500_rtn[i] <- sp500$Close[i] / sp500$Close[i-1] -1
	} else {
		sp500$sp500_rtn[i]   <- NA
	}
}

sp500 <- na.omit(sp500) # Remove any NAs

# Check out each new data frame's structure:
str(cci30)
str(sp500)
# S&P only trades M-F. We'll account for this later with the merge() function.


# Step 8: Calculate betas relative to CCi30 index  #### ####
############################################################

### bitcoin
# Merge returns with cci30 rtn data
bitcoin_cci30_rtn <- merge(x = returns[,c("Date","bitcoin")], y = cci30[,c("Date","cci30_rtn")], by = "Date", all.x = TRUE)
colnames(bitcoin_cci30_rtn) <- c("Date", "bitcoin_rtn", "cci30_rtn") #<- clean up the colname

# Check it out and calculate slope (beta)
str(bitcoin_cci30_rtn)
bitcoin_cci30_beta_cov  <- cov(bitcoin_cci30_rtn$cci30_rtn, bitcoin_cci30_rtn$bitcoin_rtn) / var(bitcoin_cci30_rtn$cci30_rtn)
bitcoin_cci30_beta_slope <- coef(lm(bitcoin_cci30_rtn$bitcoin_rtn ~ bitcoin_cci30_rtn$cci30_rtn))[2]
bitcoin_cci30_beta_cov
bitcoin_cci30_beta_slope

# Plot it out!!
ggplot(bitcoin_cci30_rtn, aes(x=cci30_rtn, y=bitcoin_rtn)) +
	geom_point() +
	geom_smooth(method=lm, fullrange = T, se = FALSE) +
	xlim(-0.2,0.2)+
	ylim(-0.2,0.2)+
	labs(title = "Beta: Bitcoin vs CCi30 Crypto Index", subtitle = paste("Last 180 days ending on", Sys.Date(),"; ",paste("ß = ",bitcoin_cci30_beta_cov)), x = "CCi30 Returns", y = "Bitcoin Returns") +
	theme_economist()

# Try it with another coin!
### ripple
# Merge returns with cci30 rtn data
ripple_cci30_rtn <- merge(x = returns[,c("Date","ripple")], y = cci30[,c("Date","cci30_rtn")], by = "Date", all.x = TRUE)
colnames(ripple_cci30_rtn) <- c("Date", "ripple_rtn", "cci30_rtn") #<- clean up the colname

# Check it out and calculate slope (beta)
str(ripple_cci30_rtn)
ripple_cci30_beta_cov  <- cov(ripple_cci30_rtn$cci30_rtn, ripple_cci30_rtn$ripple_rtn) / var(ripple_cci30_rtn$cci30_rtn)
ripple_cci30_beta_slope <- coef(lm(ripple_cci30_rtn$ripple_rtn ~ ripple_cci30_rtn$cci30_rtn))[2]
ripple_cci30_beta_cov
ripple_cci30_beta_slope

# Plot it out!!
ggplot(ripple_cci30_rtn, aes(x=cci30_rtn, y=ripple_rtn)) +
	geom_point() +
	geom_smooth(method=lm, fullrange = T, se = FALSE) +
	xlim(-0.2,0.2)+
	ylim(-0.2,0.2)+
	labs(title = "Beta: Ripple (XRP) vs CCi30 Crypto Index", subtitle = paste("Last 180 days ending on", Sys.Date(),"; ",paste("ß = ",ripple_cci30_beta_cov)), x = "CCi30 Returns", y = "XRP Returns") +
	theme_fivethirtyeight()+theme(plot.title=element_text(size=14), plot.subtitle=element_text(size=8), axis.text=element_text(size=8), axis.title=element_text(size=8))

# One last!
### tron
# Merge returns with cci30 rtn data
tron_cci30_rtn <- merge(x = returns[,c("Date","tron")], y = cci30[,c("Date","cci30_rtn")], by = "Date", all.x = TRUE)
colnames(tron_cci30_rtn) <- c("Date", "tron_rtn", "cci30_rtn") #<- clean up the colname

# Check it out and calculate slope (beta)
str(tron_cci30_rtn)
tron_cci30_beta_cov  <- cov(tron_cci30_rtn$cci30_rtn, tron_cci30_rtn$tron_rtn) / var(tron_cci30_rtn$cci30_rtn)
tron_cci30_beta_slope <- coef(lm(tron_cci30_rtn$tron_rtn ~ tron_cci30_rtn$cci30_rtn))[2]
tron_cci30_beta_cov
tron_cci30_beta_slope

# Plot it out!!
ggplot(tron_cci30_rtn, aes(x=cci30_rtn, y=tron_rtn)) +
	geom_point() +
	geom_smooth(method=lm, fullrange = T, se = FALSE) +
	xlim(-0.2,0.2)+
	ylim(-0.2,0.2)+
	labs(title = "Beta: Tron vs CCi30 Crypto Index", subtitle = paste("Last 180 days ending on", Sys.Date(),"; ",paste("ß = ",tron_cci30_beta_cov)), x = "CCi30 Returns", y = "Tron Returns") +
	theme_wsj()+theme(plot.title=element_text(size=14), plot.subtitle=element_text(size=8), axis.text=element_text(size=8), axis.title=element_text(size=8))

# copy/paste/modify/repeat for other coins!


# Step 9: Calculate betas relative to S&P 500  ######## ####
############################################################

### bitcoin
# Merge with sp500 data = x (because it's only M-F)
bitcoin_sp500_rtn <- merge(x = sp500[,c("Date","sp500_rtn")], y = returns[,c("Date","bitcoin")], by = "Date", all.x = TRUE)
colnames(bitcoin_sp500_rtn) <- c("Date", "sp500_rtn", "bitcoin_rtn") #<- clean up the colname

# Check it out and calculate slope (beta)
str(bitcoin_sp500_rtn)
bitcoin_sp500_beta_cov  <- cov(bitcoin_sp500_rtn$sp500_rtn, bitcoin_sp500_rtn$bitcoin_rtn) / var(bitcoin_sp500_rtn$sp500_rtn)
bitcoin_sp500_beta_slope <- coef(lm(bitcoin_sp500_rtn$bitcoin_rtn ~ bitcoin_sp500_rtn$sp500_rtn))[2]
bitcoin_sp500_beta_cov
bitcoin_sp500_beta_slope

# Plot it out!!
ggplot(bitcoin_sp500_rtn, aes(x=sp500_rtn, y=bitcoin_rtn)) +
	geom_point() +
	geom_smooth(method=lm, fullrange = T, se = FALSE) +
	xlim(-0.3,0.3)+
	ylim(-0.3,0.3)+
	labs(title = "Beta: Bitcoin vs S&P 500", subtitle = paste("Last 180 days ending on", Sys.Date(),"; ",paste("ß = ",bitcoin_sp500_beta_cov)), x = "S&P 500 Returns", y = "Bitcoin Returns") +
	theme_economist()+theme(plot.title=element_text(size=14), plot.subtitle=element_text(size=8), axis.text=element_text(size=8), axis.title=element_text(size=8))

# Try another one!
### ripple
# Merge with sp500 data = x (because it's only M-F)
ripple_sp500_rtn <- merge(x = sp500[,c("Date","sp500_rtn")], y = returns[,c("Date","ripple")], by = "Date", all.x = TRUE)
colnames(ripple_sp500_rtn) <- c("Date", "sp500_rtn", "ripple_rtn") #<- clean up the colname

# Check it out and calculate slope (beta)
str(ripple_sp500_rtn)
ripple_sp500_beta_cov  <- cov(ripple_sp500_rtn$sp500_rtn, ripple_sp500_rtn$ripple_rtn) / var(ripple_sp500_rtn$sp500_rtn)
ripple_sp500_beta_slope <- coef(lm(ripple_sp500_rtn$ripple_rtn ~ ripple_sp500_rtn$sp500_rtn))[2]
ripple_sp500_beta_cov
ripple_sp500_beta_slope

# Plot it out!!
ggplot(ripple_sp500_rtn, aes(x=sp500_rtn, y=ripple_rtn)) +
	geom_point() +
	geom_smooth(method=lm, fullrange = T, se = FALSE) +
	xlim(-0.3,0.3)+
	ylim(-0.3,0.3)+
	labs(title = "Beta: Ripple (XRP) vs S&P 500", subtitle = paste("Last 180 days ending on", Sys.Date(),"; ",paste("ß = ",ripple_sp500_beta_cov)), x = "S&P 500 Returns", y = "Ripple (XRP) Returns") +
	theme_fivethirtyeight()+theme(plot.title=element_text(size=14), plot.subtitle=element_text(size=8), axis.text=element_text(size=8), axis.title=element_text(size=8))

# One last!
### tron
# Merge with sp500 data = x (because it's only M-F)
tron_sp500_rtn <- merge(x = sp500[,c("Date","sp500_rtn")], y = returns[,c("Date","tron")], by = "Date", all.x = TRUE)
colnames(tron_sp500_rtn) <- c("Date", "sp500_rtn", "tron_rtn") #<- clean up the colname

# Check it out and calculate slope (beta)
str(tron_sp500_rtn)
tron_sp500_beta_cov  <- cov(tron_sp500_rtn$sp500_rtn, tron_sp500_rtn$tron_rtn) / var(tron_sp500_rtn$sp500_rtn)
tron_sp500_beta_slope <- coef(lm(tron_sp500_rtn$tron_rtn ~ tron_sp500_rtn$sp500_rtn))[2]
tron_sp500_beta_cov
tron_sp500_beta_slope

# Plot it out!!
ggplot(tron_sp500_rtn, aes(x=sp500_rtn, y=tron_rtn)) +
	geom_point() +
	geom_smooth(method=lm, fullrange = T, se = FALSE) +
	xlim(-2,2)+
	ylim(-2,2)+
	labs(title = "Beta: Tron vs S&P 500", subtitle = paste("Last 180 days ending on", Sys.Date(),"; ",paste("ß = ",tron_sp500_beta_cov)), x = "S&P 500 Returns", y = "Tron Returns") +
	theme_wsj()+theme(plot.title=element_text(size=14), plot.subtitle=element_text(size=8), axis.text=element_text(size=8), axis.title=element_text(size=8))


# Step 10: Compare the S&P500 & CCi30 directly  ######## ####
############################################################

# Run S&P500 vs CCi30 through all your previous analyses
# Go back and get more S&P data
sp500_raw_2 <- as.data.frame(getSymbols("^GSPC",auto.assign = FALSE, from = (Sys.Date()-(365*3)), to = Sys.Date()))
# Fix your new sp500 matrix
sp500_2 <- cbind(sp500_raw_2, as.Date(rownames(sp500_raw_2)) )
colnames(sp500_2) <- c("Open", "High", "Low", "Close", "Volume", "Adjusted", "Date")
rownames(sp500_2) <- NULL

# Find returns again:
for(i in 1:nrow(sp500_2)) {
	if (i>1) {
		sp500_2$sp500_rtn[i] <- sp500_2$Close[i] / sp500_2$Close[i-1] -1
	} else {
		sp500_2$sp500_rtn[i]   <- NA
	}
}

sp500_2 <- na.omit(sp500_2) # Remove any NAs

# Check out each new data frame's structure:
str(cci30)
str(sp500_2)

# Put the returns of each into a (merged) data.frame
cci30_sp500_rtn <- merge(x = sp500_2[,c("Date","sp500_rtn")], y = cci30[,c("Date","cci30_rtn")], by = "Date", all.x = TRUE)
colnames(cci30_sp500_rtn) <- c("Date", "sp500_rtn", "cci30_rtn")
cci30_sp500_rtn <- na.omit(cci30_sp500_rtn)
str(cci30_sp500_rtn)

# Crunch out a Beta for the CCi30
cci30_sp500_beta_cov  <- cov(cci30_sp500_rtn$sp500_rtn, cci30_sp500_rtn$cci30_rtn) / var(cci30_sp500_rtn$sp500_rtn)
cci30_sp500_beta_slope <- coef(lm(cci30_sp500_rtn$cci30_rtn ~ cci30_sp500_rtn$sp500_rtn))[2]
cci30_sp500_beta_cov
cci30_sp500_beta_slope

# Plot it out!!
ggplot(cci30_sp500_rtn, aes(x=sp500_rtn, y=cci30_rtn)) +
	geom_point() +
	geom_smooth(method=lm, fullrange = T, se = FALSE) +
	xlim(-0.3,0.3)+
	ylim(-0.3,0.3)+	
	labs(title = "Beta: CCi30 vs S&P 500", subtitle = paste("Last 3 years ending on", Sys.Date(),"; ",paste("ß = ",cci30_sp500_beta_cov)), x = "S&P 500 Returns", y = "CCi30 Returns") +
	theme_economist()

# Find the model's R2
summary(lm(cci30_sp500_rtn$cci30_rtn ~ cci30_sp500_rtn$sp500_rtn))$r.squared    

# Look at the StdDev of each's daily returns
sd(cci30_sp500_rtn$cci30_rtn)
sd(cci30_sp500_rtn$sp500_rtn)

# Calculate the correlation between the two returns over the last 3 years:
cor(cci30_sp500_rtn$sp500_rtn, cci30_sp500_rtn$cci30_rtn)

# Calculate the trailing correlation between the two
# Copy our merged data.frame:
cci30_sp500_tr_corr <- cci30_sp500_rtn

# Loop through (remember, you're going in a reverse Date order than when you looped through prices)
# Given the longer time period (3 years), maybe look at a trailing 90-day correlation
for(i in 1:nrow(cci30_sp500_rtn)) {
	if (i>89) {
		cci30_sp500_tr_corr$tr_corr[i] <- cor(cci30_sp500_rtn[i:(i-89),2], cci30_sp500_rtn[i:(i-89),3])
	}
	else{
		cci30_sp500_tr_corr$tr_corr[i] <- NA
	}
}

# Rip out the NAs
cci30_sp500_tr_corr <- na.omit(cci30_sp500_tr_corr)
str(cci30_sp500_tr_corr)

# No need to reshape (we're plotting one variable). Plot!
ggplot(data=cci30_sp500_tr_corr, aes(x=Date, y=tr_corr, color=tr_corr)) +
    geom_line() +
	labs(title = "The S&P 500 vs the CCi30: Trailing 90-day Correlation", subtitle = paste("Last 3 Years (approx) ending on",Sys.Date()), x = "Date", y = "Trailing Correlation") +
	ylim(-0.5,0.5) +
	theme_economist()+
	theme(legend.position="none")


############################################################
###                          End                         ###
############################################################