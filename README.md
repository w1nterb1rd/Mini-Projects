# Mini-Projects
The repository contains various mini projects that apply Linear Regression, Logistic Regression, Random Forest and Time Series analysis. 

The list of mini projects include:-
1. **Boston Housing**
: The data was taken from Kaggle. Supervised learning was applied here to determine the correlation of differents variables such as crime rate, age, distance from employment centres, tax etc. to the price of the house. The correlations were checked using the corrplot library. The data was cleaned and the outliers were detected and treated using boxplots. Linear regression modelling was done and the accuracy was determined using the libraries usdm and lmtest. Metrics package was used to calculate root mean square error.
2. **Credit Card Fraud Detection**
: Logistic regression modelling was done here after cleaning and preparing the data. Based on difference between signup time and purchase time, purchase value, browser, source, age etc. it was to be predicted if a person is prone to credit card fraud or not. Learnt about new libraries such as bit64, sqldf, caret, plyr, usdm and ROCR. The accuracy of the model was checked using Confusion Matrix and ROCR curve. The same dataset was evaluated using the Random Forest Model.
3. **Candy Production**
: Time Series analysis was used to forcast candy production based on the current trend. Packages used include forecast, tseries and lubridate. ARIMA model was used and studied through acf and pacf plots.
