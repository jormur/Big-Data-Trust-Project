# Is Ignorance Bliss?
## Media Exposure and Government Confidence in a Dynamic Landscape

**National Chengchi University**

### Contributors
111266018 萬瓏棠 Valentin

111266012 莫喬丹 Jordan

110266009 謝明穎 Carol

110266003 郭珈君 Amanda

111300353 瑪莉李 Mary

### Project Description

What is the effect of media exposure on civil trust and political leanings?
Are people who are more exposed to news more likely to distrust their government? Do the internet and social media play a key role in affecting people’s trust in their government? Is there a difference in this dynamic before and after the COVID-19 pandemic?

These are the questions that we seek to answer in this research project.

### Data
We plan to use the European Social Survey, which is an academically driven cross-national survey that has been conducted across Europe since its establishment in 2018. Every two years, face-to-face interviews are conducted with newly selected, cross-sectional samples. The survey measures the attitudes, beliefs and behavior patterns of diverse populations in more than thirty nations.

The large size of our datasets (each dataset contained a number of observations in a range from 40.000 to 54.000) was one of the main strengths of our analysis. However, it required us to spend a relevant amount of time and effort on data cleaning.

### File Structure
The main file is found as *code.R*. The ESS dataset is in the present folder as *ESS-Data-Wizard.csv*, while the series of control variables is found in the *Data* folder.

### Methodology
In our study, we aimed to examine the relationship between exposure to news media and reported trust in government using a large dataset consisting of three rounds of survey data collected over a span of six years. With tens of thousands of individuals providing responses to hundreds of questions, our dataset contained a wealth of information, including more than 75 variables.

To address the complexity and potential confounding factors inherent in such a rich dataset, we employed two complementary empirical models: double selection and fixed effects models.

The double selection model was chosen to identify the variables most relevant to our research question and control for potential confounders. Given the large number of variables, it was crucial to employ a method that could effectively handle variable selection while accounting for the multiple testing issue. Double selection is a powerful approach that combines the strengths of the lasso and the post-lasso inference. It automatically selects the most relevant variables and provides reliable estimates by controlling the false discovery rate. By employing double selection, we were able to identify the variable that best captured the effect of exposure to news media on reported trust in government, while mitigating potential biases from other variables in the dataset.

Furthermore, we utilized a fixed effects model to account for unobserved time-invariant heterogeneity among individuals. The fixed effects approach is particularly appropriate for panel data analysis, as it allows us to control for individual-specific characteristics that may remain constant over time but could potentially influence both exposure to news media and trust in government. By including individual fixed effects, we effectively removed the influence of time-invariant factors, focusing on within-individual changes over time. This model specification allowed us to isolate the variation in reported trust in government that can be attributed to changes in exposure to news media, while holding constant individual-specific heterogeneity.

By employing both the double selection and fixed effects models, we ensured a robust analysis that addresses the complexities of our dataset. The double selection model enabled us to identify the key variable of interest, while the fixed effects model accounted for unobserved time-invariant factors. Together, these models provided a comprehensive and rigorous framework for examining the relationship between exposure to news media and reported trust in government, enhancing the validity and reliability of our findings.

### Results
The output of summary(doubleselect) [shown in the table below] shows the estimate, standard error, t-value, and p-value for the effect of the selected variable on the outcome variable (trust in government). In this case, the estimated coefficient for "d1" is 1.276e-05, with a standard error of 1.552e-04. The t-value of 0.082 suggests that the estimated coefficient is not statistically significant (p-value = 0.935). Therefore, based on these results, there is no significant effect of exposure to news media on reported trust in government.

The output of confint(doubleselect) provides the 95% confidence interval for the estimated coefficient. In this case, the confidence interval for "d1" ranges from -0.0002914995 to 0.0003170138. Since the interval includes zero, it further supports the finding that the effect of exposure to news media on trust in government is not statistically significant.

For thoroughness, we also conducted a fixed effects model to account for country and time effects. The estimates of that model are shown in the table below. Once again, it shows very minute estimates.

In summary, based on the double selection analysis, the selected variable (exposure to news media) does not have a statistically significant effect on reported trust in government.

### References
- Demography of Europe—More women than men. (n.d.). Demography of Europe. Retrieved June 15, 2023, from https://www.istat.it/demografiadelleuropa/bloc-1b.html?lang=it

- European Social Survey European Research Infrastructure (ESS ERIC). (2023). ESS10 integrated file, edition 3.0 [Data set]. Sikt - Norwegian Agency for Shared Services in Education and Research. https://doi.org/10.18712/ESS10E03_0

- Numbeo. (n.d.). Crime Index by Country 2023 [Data set]. Retrieved June 15, 2023, from https://www.numbeo.com/crime/rankings_by_country.jsp

- The Economist Intelligence Unit. (n.d.). Democracy Index 2022 [Data set]. Retrieved June 15, 2023, from https://www.eiu.com/n/campaigns/democracy-index-2022/

- Transparency International. (2022). 2021 Corruption Perceptions Index [Data set]. https://www.transparency.org/en/cpi/2021

- United Nations. (n.d.). Human Development Index. In Human Development Reports. United Nations. Retrieved June 15, 2023, from https://hdr.undp.org/data-center/human-development-index

- World Bank. (n.d.-a). European Union: Age distribution of inhabitants from 2011 to 2021 [Graph] [Data set]. Retrieved June 15, 2023, from https://www.statista.com/statistics/253408/age-distribution-in-the-european-union-eu/

- World Bank. (n.d.-b). World Development Indicators | DataBank [Data set]. Retrieved June 15, 2023, from https://databank.worldbank.org/reports.aspx?source=2&series=SI.POV.DDAY&country=CHE,CZE,EST,FIN,FRA,HUN,ISL,ITA,LTU,NLD,NOR,PRT,SVN#

- World Health Organization. (n.d.-a). Global Health Expenditure Database [Data set]. Retrieved June 15, 2023, from https://apps.who.int/nha/database/Select/Indicators/en

- World Health Organization. (n.d.-b). Indicators [Data set]. Retrieved June 15, 2023, from https://www.who.int/data/gho/data/indicators

- World Health Organization. (n.d.-c). The National health Workforce Accounts database [Data set]. Retrieved June 15, 2023, from https://www.who.int/data/gho/data/themes/topics/health-workforce

