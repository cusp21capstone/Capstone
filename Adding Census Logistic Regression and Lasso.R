library(sf)
library(tidycensus)
api_key = #API KEY GOES HERE
safegraph_dat_path = #SAFEGRAPH DATA PATH GOES HERE

tidycensus::census_api_key(api_key)
ny <- tidycensus::get_acs(state = "NY", geography = "tract",
                          variables = c(median_income ="B06011_001"), geometry = TRUE) 
total_business <- read.csv(safegraph_dat_path) %>%
  filter(!is.na(latitude) & !is.na(longitude) & longitude != "Flushing")  %>%
  st_as_sf(coords = c("longitude", "latitude"),
           crs = st_crs(ny))

total_business_geo = st_join(total_business, ny) %>% as_tibble()

ny2 <- tidycensus::get_acs(state = "NY", geography = "tract",
                           variables = c(pop_under_poverty_level = "B05010_002",
                                         total_pop = "B01003_001",labor_force_pop = "C18120_002",
                                         unemployed_pop = "C18120_006",median_income ="B06011_001"
                                         
                           ), geometry = TRUE) %>% as_tibble() %>% select(GEOID,variable,estimate) %>%
  pivot_wider(id_cols=GEOID,names_from = variable,values_from = estimate)

total_business2 = left_join(total_business_geo,ny2,by="GEOID") %>% select(-c("variable","estimate","moe")) %>%
  mutate(percentage_poverty = pop_under_poverty_level/total_pop, percentage_unemployed = unemployed_pop/labor_force_pop )


list_to_exclude = total_business %>% filter(closed_on %in% c("Dec-19","Jan-20","Jul-19","Sep-19") ) %>% distinct(placekey,.keep_all = TRUE)



total_business_geo = st_join(total_business, ny) %>% as_tibble()

ny2 <- tidycensus::get_acs(state = "NY", geography = "tract",
                           variables = c(pop_under_poverty_level = "B05010_002",
                                         total_pop = "B01003_001",labor_force_pop = "C18120_002",
                                         unemployed_pop = "C18120_006",median_income ="B06011_001"
                                         
                           ), geometry = TRUE) %>% as_tibble() %>% select(GEOID,variable,estimate) %>%
  pivot_wider(id_cols=GEOID,names_from = variable,values_from = estimate)

total_business2 = left_join(total_business_geo,ny2,by="GEOID") %>% select(-c("variable","estimate","moe")) %>%
  mutate(percentage_poverty = pop_under_poverty_level/total_pop, percentage_unemployed = unemployed_pop/labor_force_pop )

capstone_dat_path = #data with Shu's additional variables added

capstone_data <- read.csv(capstone_dat_path)


total_business_unique = total_business2 %>%
  distinct(location_name,NAME,.keep_all=TRUE) %>% mutate(from_ari = 1) %>% 
  select(location_name,postal_code,percentage_poverty,percentage_unemployed,median_income,from_ari)

full_dat = full_join(capstone_data,total_business_unique,by=c("location_name","postal_code")) %>%
  filter(!is.na(from_ari) & is.finite(change_.)) %>% 
  mutate_at(c("distance_to_subway","distance_to_busstop","X500_same","X500_different", "mix_index","commercial_percent","building_d","st_width","distance_to_hospital","distance_to_pharmacy",
              "change_.","percentage_poverty","percentage_unemployed","median_income"),
            ~(scale(.) %>% as.vector)) %>% rename("change_in_visit_number" = change_.) %>% filter(!placekey %in% list_to_exclude$placekey)
full_dat$is_closed = ifelse(full_dat$status=="close",1,0)  


mylogit <- glm(is_closed ~ sub_category + distance_to_subway + distance_to_busstop + X500_same + X500_different + mix_index + commercial_percent +
                 st_width + distance_to_hospital + distance_to_pharmacy +
                 change_in_visit_number + percentage_poverty + percentage_unemployed + median_income   ,
               data = full_dat, family = "binomial")

summ(mylogit)
z = plot_coefs(mylogit)
broomed_logit = tidy(mylogit) %>%
  mutate(significance = ifelse(p.value<0.05,"Significant", "Not Significant"),
         variables = factor(term)) 
library(forcats)
broomed_logit$variables = fct_relevel(broomed_logit$variables, "(Intercept)","change_in_visit_number","commercial_percent","distance_to_subway","X500_different")



ggplot(broomed_logit, aes(x=variables,y= estimate, color = significance))+
  geom_point()+
  geom_pointrange(aes(ymin = estimate - std.error, ymax =  estimate + std.error))+
  labs(title = "Coefficients of logistic regression model",
       x = "Model Features",y = "Model Coefficient",caption = "Note: Outcome variable = 1 represented closed business\n
       Value = 0 represented business that remained open") + coord_flip()



full_dat$prediction <- predict(mylogit, newdata = full_dat, type = "response")


ggplot(data=full_dat,aes(x=prediction,y=is_closed)) + geom_point()

library(pROC)
roc_obj <- roc(full_dat$is_closed, full_dat$prediction, plot = TRUE, print.auc = TRUE)

coords(roc_obj, "best", ret = "threshold")

full_dat$predicted_class = ifelse(full_dat$prediction >= 0.08825188,1,0)

deter_poi_path = #DETER PATH GOES HERE

deter_pois = read.dbf(deter_poi_path, as.is = FALSE)


check_pois = left_join(deter_pois,full_dat,by="placekey")

check_colin = full_dat %>%
  select(distance_to_subway,distance_to_busstop, X500_same,X500_different,mix_index,commercial_percent,
         st_width,distance_to_hospital,distance_to_pharmacy,
         change_in_visit_number ,percentage_poverty,percentage_unemployed, median_income,accurate_prediction ) %>% cor()
res <- cor(check_colin)
round(res, 2)


precision <- posPredValue(as.factor(full_dat$predicted_class), as.factor(full_dat$is_closed), positive="1")
recall <- sensitivity(as.factor(full_dat$predicted_class), as.factor(full_dat$is_closed), positive="1")

F1 <- (2 * precision * recall) / (precision + recall)

confusionMatrix(as.factor(full_dat$predicted_class), as.factor(full_dat$is_closed))



set.seed(123)
training.samples <- full_dat$is_closed %>% 
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- full_dat[training.samples, ] %>% 
  dplyr::select(sub_category,distance_to_subway,distance_to_busstop, X500_same,X500_different,mix_index,commercial_percent,
                st_width,distance_to_hospital,distance_to_pharmacy,
                change_in_visit_number ,percentage_poverty,percentage_unemployed, median_income,is_closed )

train.data =  model.matrix(~0+train.data[,'sub_category']) %>% as_tibble() %>% cbind(train.data) %>%
  dplyr::select(-sub_category,-`train.data[, \"sub_category\"]Convenience Stores`)


test.data <- full_dat[-training.samples, ] %>% dplyr::select(sub_category,distance_to_subway,distance_to_busstop, X500_same,X500_different,mix_index,commercial_percent,
                                                             st_width,distance_to_hospital,distance_to_pharmacy,
                                                             change_in_visit_number ,percentage_poverty,percentage_unemployed, median_income,is_closed ) 

test.data =  model.matrix(~0+test.data[,'sub_category']) %>% as_tibble() %>% cbind(test.data) %>%
  dplyr::select(-sub_category,-`test.data[, \"sub_category\"]Convenience Stores`)


train_x = train.data %>% dplyr::select(`train.data[, \"sub_category\"]Full-Service Restaurants`, `train.data[, \"sub_category\"]Limited-Service Restaurants`,
                                       `train.data[, \"sub_category\"]Snack and Nonalcoholic Beverage Bars`,`train.data[, \"sub_category\"]Supermarkets and Other Grocery (except Convenience) Stores`,
                                       distance_to_subway,distance_to_busstop, X500_same,X500_different,mix_index,commercial_percent,
                                       st_width,distance_to_hospital,distance_to_pharmacy,
                                       change_in_visit_number ,percentage_poverty,percentage_unemployed, median_income) %>% as.matrix()

train_y = train.data$is_closed

cv.lasso <- cv.glmnet(train_x,train_y, alpha = 1, family = "binomial")
# Fit the final model on the training data
model <- glmnet(train_x, train_y, alpha = 1, family = "binomial",
                lambda = cv.lasso$lambda.1se)
# Display regression coefficients
coef(model)
# Make predictions on the test data
test_x = test.data %>%  dplyr::select(`test.data[, \"sub_category\"]Full-Service Restaurants`, `test.data[, \"sub_category\"]Limited-Service Restaurants`,
                                      `test.data[, \"sub_category\"]Snack and Nonalcoholic Beverage Bars`,`test.data[, \"sub_category\"]Supermarkets and Other Grocery (except Convenience) Stores`,
                                      distance_to_subway,distance_to_busstop, X500_same,X500_different,mix_index,commercial_percent,
                                      st_width,distance_to_hospital,distance_to_pharmacy,
                                      change_in_visit_number ,percentage_poverty,percentage_unemployed, median_income) %>% as.matrix()

test_y = test.data$is_closed

broomed_lasso = tidy(model)
broomed_lasso$term = ifelse(broomed_lasso$term  == 'train.data[, "sub_category"]Supermarkets and Other Grocery (except Convenience) Stores',
                            'Supermarkets and Other Grocery (except Convenience) Stores',broomed_lasso$term )

ggplot(broomed_lasso, aes(x=term,y= estimate))+
  geom_point()+
  labs(title = "Coefficients of Lasso regression model",
       x = "Model Features",y = "Model Coefficient",caption = "Note: Outcome variable = 1 represented closed business\n
       Value = 0 represented business that remained open") + coord_flip()



probabilities <- model %>% predict(newx = test_x)

roc_obj <- roc(test_y, probabilities, plot = TRUE, print.auc = TRUE)

coords(roc_obj, "best", ret = "threshold")

predicted.classes <- ifelse(probabilities > -1.894859, 1, 0)
# Model accuracy
observed.classes <- test.data$is_closed
mean(predicted.classes == observed.classes)

confusionMatrix(as.factor(predicted.classes), as.factor(observed.classes))

precision <- posPredValue(as.factor(predicted.classes), as.factor(observed.classes), positive="1")
recall <- sensitivity(as.factor(predicted.classes), as.factor(observed.classes), positive="1")

F1 <- (2 * precision * recall) / (precision + recall)
