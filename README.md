# StockSystems
The goal of the **StockSystems project** is to define a set of systematic rules to build a diversified portfolio of stocks overtime.
Although the system aims to be generic, it has been designed having in mind a private investor with a long-term view, medium risk-aversion, and investing in EUR.

For ilustration, we will assume a private investor with an initial capital of 50.000 EUR and annual contributions of 5.000 EUR during the next 10 years. 

Although "price momentum" is an important driver of the strategy, the investment philosofy is "[value](https://en.wikipedia.org/wiki/Value_investing) / [quality](https://en.wikipedia.org/wiki/Quality_investing)", meaning that the selection of the stocks as well as the diversification is based on the fundamentals of the business. 


## Capital allocation

We will build an equally weighted portfolio where the number of spots gets determined by:

* **Capital to invest** 
* **Optimal fees strategy** 



* **Maximum number of assets one is able to manage:** Since this system has been designed for a private investor with a "Value" profile, it's hard to imagine portfolios above 20-25 companies. Having more than 25 companies on the portfolio - besides the management difficulties - is not efficient in terms of fees compared with the potential diversification benefits.





## Stock picker: Quality - Value - Payout - Momentum

Multifactor model


Strategy to subset the list of companies which we can invest on.
Quality - Value - Payout - Momentum

### Factor 1: Quality 
What define the quality of a company? A quality company to invest in needs to have a good balance of these 3 elements. 

1. Healthy debts: A company shoud generate cash in order to repay its debt. The **Free Cash Flow to Debt** ratio is a good indicator of how quick a company pays back its debt. The lower this ratio the better.

FCF2D = $\frac{Debt}{Free Cash Flow}$

2. Profitability: 

3. High Margin: We look for companies that generate free cash flow compare to its price. The **Free Cash Flow to Entreprise Value** ratio tells us how many years of cash flow would be needed to the return the 100% of the investment. Again, the lower, the better. Values bellow 1 would mean "free lunch".

FCF2EV = $\frac{EV}{Free Cash Flow}$

### Factor 3:  Payout 
Companies that are committed to reward investors through dividends.
Trailing Annual Dividend Yield
5 Year Average Dividend Yield
Payout Ratio 



## Risk management

### Stop Loss




### Diversification: Country and Sector

### Volatility

## Data sources



