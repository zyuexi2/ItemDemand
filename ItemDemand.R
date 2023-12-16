library(tidyverse)
library(tidymodels)
library(modeltime)
library(timetk)
library(vroom)



item <- vroom("/Users/cicizeng/Desktop/STA348/ItemDemand/train.csv") 
itemTest <- vroom("/Users/cicizeng/Desktop/STA348/ItemDemand/test.csv") 


n.stores <- max(item$store)
n.items <- max(item$item)


it <- 0
## Double Loop over all store-item combos
for(s in 1:n.stores){
  for(i in 1:n.items){
    
    ## Increment the progress bar
    it <- it + 1
    
    ## Subset the data
    train <- item %>%
      filter(store==s, item==i) %>%
      select(date, sales)
    test <- itemTest %>%
      filter(store==s, item==i) %>%
      select(id, date)
    
    ## CV Split
    cv_splits <- time_series_split(train, assess="3 months", cumulative = TRUE)
    
    ## Define and Tune Prophet Model
    prophet_model <- prophet_reg() %>%
      set_engine(engine = "prophet") %>%
      fit(sales ~ date, data = training(cv_splits))
    cv_results <- modeltime_calibrate(prophet_model, 
                                      new_data = testing(cv_splits))
    
    ## Refit the data and forecast
    preds <- cv_results %>%
      modeltime_refit(data = train) %>%
      modeltime_forecast(h = "3 months") %>%
      rename(date=.index, sales=.value) %>%
      select(date, sales) %>% 
      full_join(., y=test, by="date") %>%
      select(id, sales)
    
    ## If first store-item save, otherwise bind
    if(it==1){
      all_preds <- preds
    } else {
      all_preds <- bind_rows(all_preds,
                             preds)
    }
    
    
  }
}

all_preds <- all_preds %>%
  arrange(id)