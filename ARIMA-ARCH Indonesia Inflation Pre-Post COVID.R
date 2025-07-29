# Memuat library yang diperlukan
library(ggplot2)
library(forecast)
library(tseries)
library(rpart)
library(TSA)
library(tidyverse)
library(lubridate)
library(dplyr)
library(BETS)
library(FinTS)
library(fGarch)
library(xts)

# Memuat dataset dan mengonversi kolom tanggal ke format datetime
df <- read.csv("C:/Users/HP/Downloads/Data Inflasi Indonesia.csv")

# Memeriksa informasi dataset
view(df)
str(df)
names(df)

# Memeriksa nilai yang hilang
sum(is.na(df))
head(df$Date)

### Mengonversi dan pengurutan data berdasarkan tanggal
df <- df[order(df$Date),]  # Memastikan data terurut berdasarkan tanggal
df$Date <- as.Date(df$Date)

### Mengonversi persentase menjadi nilai desimal
df$Inflasi <- as.numeric(sub("%", "", df$Inflasi)) / 100

### Membuat time series dari Inflasi
inf_ts <- ts(df$Inflasi, frequency = 12)

### Menampilkan struktur dari time series
print(inf_ts)
plot(inf_ts)
tsdisplay(inf_ts)

### Visualisasi data Inflasi seiring waktu dengan ggplot2
ggplot(df, aes(x = Date, y = Inflasi, group = 1)) +
  geom_line() +
  ggtitle("Inflasi Indonesia 2018-2024") +
  xlab("Tahun") +
  ylab("Inflasi") + 
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

### Uji stasioneritas dengan Augmented Dickey-Fuller test
adf_test <- adf.test(inf_ts, alternative = "stationary")
print(adf_test)  # Menampilkan hasil uji ADF

### Differencing untuk mencapai stasioneritas
datadiff <- diff(inf_ts, differences = 1)
print(datadiff)  # Menampilkan data setelah differencing pertama

### Uji stasioneritas dengan ADF test setelah differencing pertama
adf_test_datadiff <- adf.test(datadiff)
print(adf_test_datadiff)

### Differencing untuk mencapai stasioneritas
datadiff2 <- diff(datadiff, differences = 1)
print(datadiff2)  # Menampilkan data setelah differencing kedua

### Uji stasioneritas dengan ADF test setelah differencing kedua
adf_test_datadiff2 <- adf.test(datadiff2)
print(adf_test_datadiff2)

### Menampilkan time series setelah differencing kedua
tsdisplay(datadiff2)
eacf(datadiff2)

#ARIMA(0,2,1), ARIMA(1,2,1), ARIMA(2,2,0)

### Model ARIMA
model1 <- Arima(inf_ts, order=c(0,2,1))
model2 <- Arima(inf_ts, order=c(1,2,1))
model3 <- Arima(inf_ts, order=c(2,2,0))


### Ringkasan dan perbandingan model
summary(model1)
summary(model2)
summary(model3)


### Kalkulasi AIC dan BIC untuk pemilihan model terbaik
AIC(model1, model2, model3)
BIC(model1, model2, model3)
cbind(model1, model2, model3)

#Model3 ARIMA(2,2,0)

### Memilih model terbaik berdasarkan residuals
fit <- model3

# Uji Stasioneritas Residual Model
adf_test_fit <- adf.test(fit$residuals)
print(adf_test_fit)

# Uji Normalitas Residual Model
shapiro_test_fit <- shapiro.test(fit$residuals)
print(shapiro_test_fit)

# Uji Independensi Residual Model
lbtest_fit <- checkresiduals(fit)

# Uji Efek ARCH pada Residual Model
arch_test <- ArchTest(fit$residuals)
print(arch_test)

model10 = garchFit(~arma(2,0)+garch(1,0), data = datadiff2, trace = F)
summary(model10)

library(rugarch)
# Model ARIMA-GARCH (2,2,0) dengan ARCH(1)
spec_arima_garch <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 0)),  # GARCH(1,1)
  mean.model = list(armaOrder = c(2, 0), include.mean = TRUE),  # ARIMA(2,2)
  distribution.model = "norm"
)

# Fit model ARIMA-GARCH ke data
model_arima_garch <- ugarchfit(spec = spec_arima_garch, data = datadiff2)

# Memuat paket yang diperlukan
library(rugarch)


# Langkah 2: Mendefinisikan spesifikasi ARIMA(2,0)-ARCH(1)
spec_arima_garch <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 0)),  # ARCH(1)
  mean.model = list(armaOrder = c(2, 0), include.mean = TRUE),    # ARMA(2,0)
  distribution.model = "norm"  # Distribusi residual normal
)

# Langkah 3: Fitting model ARIMA-GARCH
fit <- ugarchfit(spec = spec_arima_garch, data = datadiff2)
forecast <- ugarchforecast(fit, n.ahead = 3)

# Melihat hasil forecast
print(forecast)