library(dplyr)
library(ggplot2)
library(readr)
library(jsonlite)
library(shiny)


# 1. Data Munging -- data loaded

hmda_init <- function(){
  # read files
  loans <- read.csv('2012_to_2014_loans_data.csv')
  institution <- read.csv('2012_to_2014_institutions_data.csv')
  
  # join two tables
  loan_instituion <- loans %>%
  inner_join(institution, by = c('Respondent_ID' = 'Respondent_ID', 'As_of_Year' = 'As_of_Year', 'Agency_Code' = 'Agency_Code'))
  
  # create loan_amount_000 groups using statistical method
  bucket <- function(a,na.rm = TRUE) {
  qnt <- quantile(a,probs = c(.25,.75),na.rm = na.rm)
  value <- 1.5 * IQR(a,na.rm = na.rm)
  b <- a
  b[a <= qnt[1]] <- 'low'
  b[a > qnt[1] & a <= qnt[2] ] <- 'middle'
  b[a > qnt[2] & a <= (qnt[2]+value)] <- 'high'
  b[a > (qnt[2]+value) ] <- 'extremly high'
  return(b)
  }

loan_instituion$loan_group <- loan_instituion$Loan_Amount_000

loan_instituion$loan_group <- bucket(loan_instituion$loan_group)

return (loan_instituion)
}

# 'initiate' will become the dataframe you want
initiate <- hmda_init()






# Data Munging -- export function

# export format such as:
# hmda_to_json(data = initiate)
# hmda_to_json(data = initiate, states = c('DC','DE'))
# hmda_to_json(data = initiate, conventional_conforming = 'conventional')
# hmda_to_json(data = initiate, states = c('DC'), conventional_conforming = 'conventional')
# hmda_to_json(data = initiate, states = c('DC'), conventional_conforming = 'non_conventional')
# hmda_to_json(data = initiate, states = c('DC'), conventional_conforming = 'conforming')
# hmda_to_json(data = initiate, states = c('DC'), conventional_conforming = 'non_conforming')
# hmda_to_json(data = initiate, states = c('DC'), conventional_conforming = 'conventional_and_conforming')


hmda_to_json <- function(data,states, conventional_conforming){
   if (missing(conventional_conforming) & missing(states)){
      data %>%
        toJSON() %>%
        write('initiate.json') 
  }
  
   else if (missing(states)){
    if(conventional_conforming == 'conventional'){
          final_data <- data %>%
            filter(Conventional_Status == 'Conventional')
          final_data %>%
            toJSON() %>%
            write('conventional.json')  
    }
    else if(conventional_conforming == 'non_conventional'){
          final_data <- data %>%
            filter(Conventional_Status != 'Conventional')
          final_data %>%
            toJSON() %>%
            write('non_conventional.json')  
    }
    else if(conventional_conforming == 'conforming'){
          final_data <- data %>%
            filter(Conforming_Status == 'Conforming')
          final_data %>%
            toJSON() %>%
            write('conforming.json')  
    }
    else if(conventional_conforming =='non_conforming'){
          final_data <- data %>%
            filter(Conforming_Status != 'Conforming')
          final_data %>%
            toJSON() %>%
            write('non_conforming.json')  
    }
    else if(conventional_conforming == 'conforming_and_conventional'){
          final_data <- data %>%
            filter(Conventional_Conforming_Flag == 'Y')
          final_data %>%
            toJSON() %>%
            write('conventional_conforming.json')  
    }
  }

  else if (missing(conventional_conforming)){
    final_data <- data %>%
      filter(State %in% states)
    final_data %>%
      toJSON() %>%
      write('market_by_states.json')
  }
  
  
  else {
    if(conventional_conforming == 'conventional'){
          final_data <- data %>%
            filter(State %in% states,Conventional_Status == 'Conventional')
          final_data %>%
            toJSON() %>%
            write('conventional_bystate.json')  
    }
    else if(conventional_conforming == 'non_conventional'){
          final_data <- data %>%
            filter(State %in% states,Conventional_Status != 'Conventional')
          final_data %>%
            toJSON() %>%
            write('non_conventional_bystate.json')  
    }
    else if(conventional_conforming == 'conforming'){
          final_data <- data %>%
            filter(State %in% states,Conforming_Status == 'Conforming')
          final_data %>%
            toJSON() %>%
            write('conforming_bystate.json')  
    }
    else if(conventional_conforming == 'non_conforming'){
          final_data <- data %>%
            filter(State %in% states,Conforming_Status != 'Conforming')
          final_data %>%
            toJSON() %>%
            write('non_conforming_bystate.json')  
    }
    else if(conventional_conforming == 'conventional_and_conforming'){
          final_data <- data %>%
            filter(State %in% states,Conventional_Conforming_Flag == 'Y')
          final_data %>%
            toJSON() %>%
            write('conventional_conforming_bystate.json')  
    }
  }

}




# Quality Check
# Statisticaly large loans in its' MSA_MD area, as different MSA_MD area have different housing price and conforming limit
large_loan_byMSA <- initiate%>%
  group_by(MSA_MD_Description)%>%
  mutate(limit = quantile(Loan_Amount_000,probs = 0.75) + 3*IQR(Loan_Amount_000))%>%
  filter(Loan_Amount_000 > limit)%>%
  select(MSA_MD_Description,Loan_Amount_000,Respondent_Name_TS)

large_loan_byMSA

# total number of large loans owned by each respondent.
large_loan_byRespondent <- large_loan_byMSA%>%
  group_by(Respondent_Name_TS)%>%
  summarise(count = n())%>%
  select(Respondent_Name_TS,count)%>%
  arrange(desc(count))

large_loan_byRespondent


# Additional column for quality assessment: loan_to_income_ratio (Loan_Amount_000/Applicant_Income_000), because this reflects applicant's ability of paying back

initiate$Loan_Amount_000 <- as.numeric(initiate$Loan_Amount_000)
initiate$Applicant_Income_000 <- as.numeric(initiate$Applicant_Income_000)
initiate$loan_to_income_ratio <- round(initiate$Loan_Amount_000/initiate$Applicant_Income_000,2)


# the critical value for statistical outliers for loan_to_income_ratio equals to 8.69. Which means the applicant will not be able to pay back the principal in almost 9 years time, I assume these are the risky loans
quantile(initiate$loan_to_income_ratio,probs = 0.75) + 3*IQR(initiate$loan_to_income_ratio)

risky_loan <- initiate%>%
  filter(loan_to_income_ratio > quantile(initiate$loan_to_income_ratio,probs = 0.75) + 3*IQR(initiate$loan_to_income_ratio))%>%
  select(Loan_Amount_000,Respondent_Name_TS,loan_to_income_ratio,loan_group)

risky_loan



# the total number of those risky loans owned by each respondent.
risky_loan_byRespondent <- risky_loan%>%
  group_by(Respondent_Name_TS)%>%
  summarise(count = n())%>%
  arrange(desc(count))

risky_loan_byRespondent



# Loans that that are both large and risky
large_risky_loan <- initiate%>%
  filter(loan_to_income_ratio > quantile(initiate$loan_to_income_ratio,probs = 0.75) + 3*IQR(initiate$loan_to_income_ratio))%>%
  group_by(MSA_MD_Description)%>%
  mutate(limit = quantile(Loan_Amount_000,probs = 0.75) + 3*IQR(Loan_Amount_000))%>%
  filter(Loan_Amount_000 > limit)%>%
  ungroup()%>%
  select(Loan_Amount_000,Respondent_Name_TS,loan_to_income_ratio)

large_risky_loan

# Recommendation
# We thought there should be a big intersection between large loans and risky loans. However, there are 11,854 large loans and 4,621 risky loans, and only 403 risky large loans. Which means only around 10% of risky loans are from large loans. 

# the distribution of risky loans in different loan groups:
risky_loan_distribution <- risky_loan%>%summarise(low_loan_pct = round(mean(loan_group == 'low')*100,2),
            middle_loan_pct = round(mean(loan_group == 'middle')*100,2),
            high_loan_pct = round(mean(loan_group == 'high')*100,2),
            extremly_high_loan_pct = round(mean(loan_group == 'extremly high')*100,2))

risky_loan_distribution

# The table shows that almost 60% of risky loans come from the middle group of loan! These statistics demonstrate that loan amount is not always correlated with risk. For example, jumbo loans are intended for excellent borrowers with excellent credit looking to finance loan amounts greater than the amount allowed by Fannie Mae or Freddie Mac, according to our findings, jumbo loans are not having to be more risker than conforming loans.


# According to our findings in next part, the market size for the jumbo market is the only market that shrinks slowly, and our quality assessment shows that most of the risky loans are coming from the middle group of loans. Therefore, my recommendation for Change Financial is to focus its business on the jumbo market.




# Hypothesis: It is risky to enter this home loan market

# overall market size 
mkt_size <- initiate %>%
  group_by(As_of_Year,State) %>%
  summarise(sum_of_loan = sum(Loan_Amount_000))

ggplot(mkt_size,aes(As_of_Year, sum_of_loan)) +
  geom_histogram(stat = 'identity')+
  facet_grid(.~State)+
  ggtitle('overall market size')


# Take a close look of mkt size for DC, WV and DE
exlude_mkt_size <- mkt_size%>%
  filter(State != 'MD' & State != 'VA')

ggplot(exlude_mkt_size,aes(x = As_of_Year, y = sum_of_loan)) +
  geom_histogram(stat = 'identity')+
  facet_grid(.~State)+
  ggtitle('overall market size - closer look for DC, DE and WV')

# Rank of the market size: VA > MD > DC > WV > DE.
# The mkt size in all five states is decreasing these years. 




# conforming mkt
conform_mkt_size <- initiate %>%
  filter(Conforming_Status == 'Conforming') %>%
  group_by(As_of_Year,State) %>%
  summarise(sum_of_loan = sum(Loan_Amount_000))

ggplot(conform_mkt_size,aes(x = As_of_Year, y = sum_of_loan)) +
  geom_histogram(stat = 'identity')+
  facet_grid(.~State)+
  ggtitle('conforming loan market')


# conventional
conventional_mkt_size <- initiate %>%
  filter(Conventional_Status == 'Conventional') %>%
  group_by(As_of_Year,State) %>%
  summarise(sum_of_loan = sum(Loan_Amount_000))

ggplot(conventional_mkt_size,aes(x = As_of_Year, y = sum_of_loan)) +
  geom_histogram(stat = 'identity')+
  facet_grid(.~State)+
  ggtitle('conventional loan market')

non_conventional_mkt_size <- initiate %>%
  filter(Conventional_Status != 'Conventional') %>%
  group_by(As_of_Year,State) %>%
  summarise(sum_of_loan = sum(Loan_Amount_000))

ggplot(non_conventional_mkt_size,aes(x = As_of_Year, y = sum_of_loan)) +
  geom_histogram(stat = 'identity')+
  facet_grid(.~State)+
  ggtitle('non_conventional loan market')


# jumbo market
jumbo_mkt_size <- initiate %>%
  filter(Conforming_Status != 'Conforming') %>%
  group_by(As_of_Year,State) %>%
  summarise(sum_of_loan = sum(Loan_Amount_000))

ggplot(jumbo_mkt_size,aes(x = As_of_Year, y = sum_of_loan)) +
  geom_histogram(stat = 'identity')+
  facet_grid(.~State)+
  ggtitle('jumbo loan market')

exlude_jumbo_mkt_size <- jumbo_mkt_size%>%
  filter(State != 'MD' & State != 'VA')

ggplot(exlude_jumbo_mkt_size,aes(x = As_of_Year, y = sum_of_loan)) +
  geom_histogram(stat = 'identity')+
  facet_grid(.~State)+
  ggtitle('jumbo loan market - closer look for DC, DE and WV')

# the market size for jumbo market shrink slightly, and even increased in WV.

# To prove or disprove our hypothesis we need more information: Although the overall market size is shrinking, the jumbo loan market decreases slowly compared to other markets, and even increase in WV. Therefore, we need more information about what type of loan Change financial want to focus on for its business and take the profitability and risk of jumbo loans into consideration before we can prove whether it is a good time for Change Financial to enter a new geography.



# Summarization of key insights:
# Rank of the market size: VA > MD > DC > WV > DE. Overall, the demand for the home loan decreased sharply from 2012 to 2014, except for WV, the market size in which only decreased slowly. However, in the jumbo loan market, the trend of decreasing is much more gentle. However, It’s worth noting that jumbo market size is relatively small when compared to both conforming and conventional markets.



# R shiny interactive dashboard
# This dashboard allows us to see competitor's trend of market share, assets, and distribution of their business in different loan groups.

# To be noted: not all respondents have business in all states all years.

inter_table <- initiate %>%
  select(As_of_Year, Respondent_Name_TS,State,Loan_Amount_000,Assets_000_Panel,loan_group)%>%
  group_by(As_of_Year, Respondent_Name_TS,State,Assets_000_Panel) %>%
  summarise(sum_loan = sum(Loan_Amount_000),low_loan_pct = round(mean(loan_group == 'low')*100,2),
            middle_loan_pct = round(mean(loan_group == 'middle')*100,2),
            high_loan_pct = round(mean(loan_group == 'high')*100,2),
            extremly_high_loan_pct = round(mean(loan_group == 'extremly high')*100,2))



ui <- fluidPage(
  selectInput(inputId = 'respondent',
              label = 'choose a respondent',
              choices =  inter_table$Respondent_Name_TS),
  uiOutput("secondSelection"),
  tableOutput('table'),
  plotOutput('hist')
)

server <- function(input,output){
   output$secondSelection <- renderUI({
           selectInput(inputId = 'state',
              label = 'choose a state',
              choices = inter_table$State[inter_table$Respondent_Name_TS == input$respondent])
   })
   
  output$table <- renderTable({
    inter_table%>%
      filter(State == input$state)%>%
      group_by(As_of_Year,State)%>%
      mutate(mkt_share_percentage = (sum_loan/sum(sum_loan))*100)%>%
      filter(Respondent_Name_TS == input$respondent)%>%
      select(Respondent_Name_TS,As_of_Year,mkt_share_percentage,Assets_000_Panel,low_loan_pct,middle_loan_pct,high_loan_pct,extremly_high_loan_pct)
  },digits = 4)
  
  output$hist <- renderPlot({
    ggplot(data = inter_table%>%
                      filter(State == input$state)%>%
                      group_by(As_of_Year,State)%>%
                      mutate(mkt_share_percentage = (sum_loan/sum(sum_loan))*100)%>%
                      filter(Respondent_Name_TS == input$respondent),
                        aes(x = As_of_Year,y = mkt_share_percentage)) +
    geom_histogram(stat = 'identity')+
    ggtitle('Percentage of market share by year')
  })
}

shinyApp(server = server, ui = ui)

