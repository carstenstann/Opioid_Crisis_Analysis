library(readxl)
library(plm)
library(lmtest)
library(ggplot2)
library(dplyr)

# import data ---------------------------------------------------------------------------------------------------------------
state_names <- read_excel(path = "/Users/Carsten/Documents/Freie Universität Berlin/WS 17_18/A.F. International Economics and Area Studies/Workbook1.xlsx",
                          sheet = "Medicare", col_names = FALSE, n_max =1, col_types = "text")

medicare <- read_excel(path = "/Users/Carsten/Documents/Freie Universität Berlin/WS 17_18/A.F. International Economics and Area Studies/Workbook1.xlsx",
                       sheet = "Medicare", col_names = FALSE, col_types = "numeric", skip = 7)

medicaid <- read_excel(path = "/Users/Carsten/Documents/Freie Universität Berlin/WS 17_18/A.F. International Economics and Area Studies/Workbook1.xlsx",
                       sheet = "Medicaid", col_names = FALSE, skip = 7, col_types = "numeric")

opioid_deaths <- read_excel(path ="/Users/Carsten/Documents/Freie Universität Berlin/WS 17_18/A.F. International Economics and Area Studies/PrescriptionOpioidDeaths.xlsx",
                            sheet = "Sheet1", col_names = FALSE, col_types = "numeric", 
                            trim_ws = TRUE, skip = 7)

gdp <- read_excel(path = "/Users/Carsten/Documents/Freie Universität Berlin/WS 17_18/A.F. International Economics and Area Studies/StateGDP.xls",
                  sheet = "Sheet1", col_names = FALSE, col_types = "numeric", skip = 7)

rxrate <- read_excel(path = "/Users/Carsten/Documents/Freie Universität Berlin/WS 17_18/A.F. International Economics and Area Studies/CDCPrescribingRate.xlsx",
                     sheet = "Sheet2", col_names = FALSE, col_types = "numeric", skip = 1)

elderly <- read_excel(path = "/Users/Carsten/Documents/Freie Universität Berlin/WS 17_18/A.F. International Economics and Area Studies/elderly.xls",
                      sheet = "Sheet2", col_names = FALSE, col_types = "numeric", skip = 7)

medicare <- medicare[,-1]
medicaid <- medicaid[,-1]
opioid_deaths <- opioid_deaths[,-1]
gdp <- gdp[,-1]
rxrate <- rxrate[,-1]
elderly <- elderly[,-1]

mm <- medicare + medicaid

# form panel data -----------------------------------------------------------------------
states_panel <- c()
for (i in  1:44)
{
  s <- rep(state_names[[i]], 9) 
  states_panel <- c(states_panel, s)
}
## states_panel <- as.factor(states_panel)

years_panel = rep(2006:2014, 44)

opioid_panel <- c()
for(i in 1:44)
{
  x <- opioid_deaths[,i]
  x<-t(x)
  opioid_panel <- c(opioid_panel, x)
}

medicare_panel<- c()
for(i in 1:44)
{
  x <- medicare[,i]
  x<- t(x)
  medicare_panel <- c(medicare_panel, x)
}

medicaid_panel <- c()
for(i in 1:44){
  x <- medicaid[,i]
  x <- t(x)
  medicaid_panel <- c(medicaid_panel, x)
}

mm_panel <- c()
for(i in 1:44){
  x <- mm[,i]
  x <- t(x)
  mm_panel <- c(mm_panel, x)
}

gdp_panel <- c()
for(i in 1:44)
{
  x <- gdp[,i]
  x <- t(x)
  gdp_panel <- c(gdp_panel, x)
}

rx_panel <- c()
for(i in 1:44)
{
  x<-rxrate[,i]
  x<-t(x)
  rx_panel <- c(rx_panel, x)
}

elderly_panel <- c()
for (i in 1:44)
{
  x <- as.numeric(rep(elderly[1,i],9))
  x<- t(x)
  elderly_panel<- c(elderly_panel, x)
}

panel <- data.frame(states_panel, years_panel, opioid_panel, medicare_panel, medicaid_panel, 
                    mm_panel, gdp_panel, rx_panel, elderly_panel, stringsAsFactors = TRUE)
colnames(panel) <- c("state", "year", "deaths", "medicare","medicaid","medicare_medicaid", "gdp", "prescription_rate", "elderly")

pdata <- pdata.frame(x = panel, index = c("state", "year"))

pool <- plm(log(deaths) ~ log(medicare_medicaid) + log(gdp)+ log (prescription_rate), data = pdata, 
            model = "pooling")

# test for individual effects in the pooling model
plmtest(pool, "individual")


FEplm <- plm(log(deaths) ~ log(medicare_medicaid) + log(gdp) + log(prescription_rate), data = pdata, 
                 model = "within")
REplm <- plm(log(deaths) ~ log(medicare) + log(gdp) + log(prescription_rate) + log(elderly), data = pdata, 
             model = "random")

# compare FEplm and pool model for individual effects
pFtest(FEplm,pool)

# conduct hausmann test, reject null at 5 % level. Choose FE model for p less than .05
hausman <- phtest(x = FEplm, x2 = REplm, model = c("within", "random"))
hausman

FEplm <- plm(log(deaths) ~ log(medicare_medicaid) + log(gdp) + log(prescription_rate), data = pdata, 
                 model = "within", effect ="individual")
FEplm2 <- plm(log(deaths) ~ log(medicare) + log(gdp), data = pdata, 
             model = "within", effect ="individual")

summary(FEplm)
summary(FEplm2)

# Test for Autocorrelation: Breusch-Godfrey Test
# Null Hypothesis: No Serial Correlation. 
pbgtest(FEplm)
pbgtest(FEplm2)
pbgtest(REplm)

#Test for heteroskedasticity: heteterskedasticity present for p less than .05
bptest(FEplm)
bptest(FEplm2)
bptest(REplm)

# Because Serial Correlation and Heteroskedasticity present. Use Newey West HAC Errors
coeftest(FEplm, vcov.=vcovNW)
coeftest(FEplm2, vcov.=vcovNW)
coeftest(REplm, vcov. =vcovNW)

# Plots
state_spending <- ggplot(pdata, mapping = aes(x = factor(state),y = medicare_medicaid/1000000, colour = year))+
  geom_point()+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(size = 7,angle = 45, vjust = 1, hjust=1), 
        legend.title=element_blank())+
  ggtitle("Medicare and Medicaid Spending by State: 2006-2014") +
  labs(x = "State", y = "Spending (Millions of $)")+
  scale_y_continuous(labels=function(n){format(n, big.mark = ",", scientific = FALSE)})
  
ggsave("StateSpending.png", 
       plot = state_spending, 
       device = "png", 
       path = "/Users/Carsten/Documents/GitHub/Opiod_Crisis_Analysis/README_figs/",
       width = 20, height = 10, units = "cm",
       dpi = 320, limitsize = TRUE)


# Aggregate Data using dplyr
a.deaths <- summarise(group_by(pdata, year), sum(deaths))
colnames(a.deaths) <- c("year", "deaths")
a.mm <- summarise(group_by(pdata, year), sum(medicare_medicaid))
colnames(a.mm) <- c("year", "mm")
a.gdp <- summarise(group_by(pdata, year), sum(gdp))
colnames(a.gdp) <- c("year", "gdp")
a.rx <- summarise(group_by(pdata, year), sum(prescription_rate))
colnames(a.rx) <- c("year", "rx")

lm <- lm(log(a.deaths$deaths) ~ log(a.mm$mm) + log(a.gdp$gdp) + log(a.rx$rx))

ggplot(a.deaths, mapping = aes(x = as.Date(year, format = "%Y"), y = deaths))+
  geom_line()

