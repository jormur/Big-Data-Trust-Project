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
The main file is found as *code.R*. The ESS dataset is in the present folder as *ESS-Data-Wizard.csv*, while the series of control variables is found in the *Data* folder. It is best to clone the file structure to your local system to run the code without error.

### Methodology
In our study, we aimed to examine the relationship between exposure to news media and reported trust in government using a large dataset consisting of three rounds of survey data collected over a span of six years. With tens of thousands of individuals providing responses to hundreds of questions, our dataset contained a wealth of information, including more than 75 variables.

To address the complexity and potential confounding factors inherent in such a rich dataset, we employed two complementary empirical models: double selection and fixed effects models.

The double selection model was chosen to identify the variables most relevant to our research question and control for potential confounders. Given the large number of variables, it was crucial to employ a method that could effectively handle variable selection while accounting for the multiple testing issue. Double selection is a powerful approach that combines the strengths of the lasso and the post-lasso inference. It automatically selects the most relevant variables and provides reliable estimates by controlling the false discovery rate. By employing double selection, we were able to identify the variable that best captured the effect of exposure to news media on reported trust in government, while mitigating potential biases from other variables in the dataset.

Furthermore, we utilized a fixed effects model to account for unobserved time-invariant heterogeneity among individuals. The fixed effects approach is particularly appropriate for panel data analysis, as it allows us to control for individual-specific characteristics that may remain constant over time but could potentially influence both exposure to news media and trust in government. By including individual fixed effects, we effectively removed the influence of time-invariant factors, focusing on within-individual changes over time. This model specification allowed us to isolate the variation in reported trust in government that can be attributed to changes in exposure to news media, while holding constant individual-specific heterogeneity.

By employing both the double selection and fixed effects models, we ensured a robust analysis that addresses the complexities of our dataset. The double selection model enabled us to identify the key variable of interest, while the fixed effects model accounted for unobserved time-invariant factors. Together, these models provided a comprehensive and rigorous framework for examining the relationship between exposure to news media and reported trust in government, enhancing the validity and reliability of our findings.

### Results
The analysis of the effect of media exposure on trust in various institutions yielded interesting findings. The estimated effects of media exposure on trust levels in different institutions were examined using the double selection method. The results indicate that none of the estimated effects were statistically significant.

For the variables related to trust, the measured trust levels followed a decreasing order: police, legal system, United Nations, parliament, EU, political parties, and politicians. Notably, all trust levels showed an overall increasing trend over the observed years, except for trust in the legal system, which exhibited a slight wavering and a lesser increase compared to the other institutions.

The lack of statistically significant relationships between media exposure and trust in these institutions challenges the prevailing notion that media plays a direct and influential role in shaping trust levels. It suggests that other factors beyond media exposure may have a more significant impact on trust formation.

The distinct trend observed in trust in the legal system, where it did not increase as significantly as other institutions, is intriguing. This discrepancy might be attributed to specific factors related to the legal system itself or contextual influences that affect public perceptions of trust in this particular institution. Further qualitative investigation and analysis of contextual factors can shed light on these dynamics and provide a deeper understanding of the nuances surrounding trust in the legal system.

Regarding trust in the European Parliament, the estimated effect was also not statistically significant. This result diverges from the overall increasing trend observed in trust levels for the other institutions. The implications of this finding could suggest that media exposure might have a different impact on trust in the European Parliament compared to other institutions. Factors such as the perception of the European Union, political dynamics at the European level, or the influence of other social and cultural factors may contribute to this distinct result. Exploring these factors in future research can enhance our understanding of the complexities involved in shaping trust in the European Parliament and its relationship with media exposure.

The analysis reveals that media exposure does not demonstrate a statistically significant effect on trust levels in various institutions. The differing trends in trust levels, particularly in the legal system and the European Parliament, warrant further investigation to uncover the underlying dynamics that contribute to these patterns. The findings highlight the need for a comprehensive and multidimensional approach to understanding the intricate relationship between media, trust, and public perceptions in different institutional contexts.

For thoroughness, we also conducted a fixed effects model to account for country and time effects. The estimates of that model are shown in the table below for trust in European parliament (as that had the greatest deviation for the previous regression estimates. 

The fixed effects model was applied to analyze the relationship between trust in the European Parliament and various factors. The estimated effects of different variables on trust in the European Parliament are presented below. However, it should be noted that not all of the estimated effects were statistically significant.

Among the country-specific fixed effects, the coefficients for several countries were negative but did not reach statistical significance. These include the Czech Republic, Estonia, Finland, France, Hungary, Italy, Lithuania, Netherlands, Norway, Portugal, and Slovenia. The negative coefficients suggest that individuals from these countries tended to exhibit lower levels of trust in the European Parliament compared to the reference country.

On the other hand, the coefficient for Iceland was positive and statistically significant, indicating that individuals from Iceland had higher levels of trust in the European Parliament compared to the reference country. This finding suggests that contextual factors specific to Iceland may have contributed to the higher trust levels observed.

Regarding the other variables, several exhibited statistically significant effects on trust in the European Parliament. Notably, variables such as net use time, education level, government satisfaction, race discrimination, and death rate showed significant positive effects on trust. This implies that individuals who spent more time using the internet, had higher education levels, expressed greater satisfaction with the government, reported less race discrimination, and lived in countries with lower death rates tended to have higher levels of trust in the European Parliament.

Conversely, variables such as the perception of media influence, political polarization, and economic indicators did not exhibit statistically significant effects on trust in the European Parliament. This suggests that factors beyond media exposure, political polarization, and economic conditions play a more prominent role in shaping trust in this particular institution.

It is important to highlight that the lack of statistical significance for certain variables should not undermine their potential influence on trust in the European Parliament. The non-significant estimates may be attributed to factors such as limited sample size, measurement errors, or the complex interplay of various determinants of trust.

The findings from the fixed effects model provide insights into the factors associated with trust in the European Parliament. The positive influence of factors such as net use time, education level, government satisfaction, race discrimination, and death rate indicates the multidimensionality of trust formation in this context. Further research is needed to explore the underlying mechanisms and contextual factors that drive trust dynamics in the European Parliament.

In conclusion, the fixed effects model revealed both significant and non-significant effects of various factors on trust in the European Parliament. The analysis underscores the complexity of trust formation and highlights the importance of considering country-specific factors, individual characteristics, and contextual influences when examining trust in supranational institutions.

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

