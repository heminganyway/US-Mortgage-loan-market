# US-Mortgage-loan-market
1. Data Munging – data input

hmd_init() will load the data and return a data frame that joins loan table and institution table.

We add a new column ‘loan_group’ into this table:
Low: loan_amount_000 less than 25% quantile of all loans

Middle: loan_amount_000 between 25% quantile and 75% quantile of all loans

High: loan_amount_000 between 75% quantile and (75% quantile + 1.5*Inter Quantile) of all loans

Extremely_high : loan_amount_000 greater than (75% quantile + 1.5*Inter Quantile) of all loans


Data Munging – data export

hmda_to_json() will export the data into json format, both states and conventional_conforming are optional arguments.

export format such as:
hmda_to_json(data = initiate)
hmda_to_json(data = initiate, states = c('DC','DE'))
hmda_to_json(data = initiate, conventional_conforming = 'conventional')
hmda_to_json(data = initiate, states = c('DC'), conventional_conforming = 'conventional')
hmda_to_json(data = initiate, states = c('DC'), conventional_conforming = 'non_conventional')
hmda_to_json(data = initiate, states = c('DC'), conventional_conforming = 'conforming')
hmda_to_json(data = initiate, states = c('DC'), conventional_conforming = 'non_conforming')
hmda_to_json(data = initiate, states = c('DC'), conventional_conforming = 'conventional_and_conforming')





2. Quality check

large_loan_byMSA: data frame stores large loans, which are statistical outliers regards to loan amount in its MSA and MD area.

large_loan_byRespondent: data frame stores the number of large loans owned by each respondent.

risky_loan: data frame stores risky loans, which are statistical outliers regards to loan to income ratio.

risky_loan_byRespondent: data frame stores the number of risky loans owned by each respondent.

large_risky_loan: data frame stores large and risky loans 

risky_loan_distribution: data frame stores the distribution of risky loans




3. Craft a visual data narrative

mkt_size: data frame stores market size by state and year

exlude_mkt_size: data frame stores market size for DC, DE and WV by year

conform_mkt_size: data frame stores conforming market size by state and year

conventional_mkt_size: data frame stores conventional market size by state and year

non_conventional_mkt_size: data frame stores non-conventional market size by state and year

jumbo_mkt_size: data frame stores jumbo market size by state and year

exlude_jumbo_mkt_size: data frame stores jumbo market size for DC, DE and WV by year






4. Interactive dashboard for competitors’ market share

This dashboard allows VP to see competitor's trend of market share, assets, and distribution of their business in different loan groups.

Input: Respondent name, State

Output: 
1. A table describes respondent’s market share, its assets, and distribution of their business in different loan groups in each state in each year.
2. A histogram represents the trend of its market share in this state these years.


To be noted: not all respondents have business in all states all years.
