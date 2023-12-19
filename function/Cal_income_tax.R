Cal_income_tax <- function(
    income.pension # 課税所得
){
  # 所得税
  if(income.pension < 1950000){
    income.tax <- income.pension*.05
  }else if(income.pension < 3300000){
    income.tax <- income.pension*.1 - 97500
  }else if(income.pension < 6950000){
    income.tax <- income.pension*.2 - 427500
  }else if(income.pension < 9000000){
    income.tax <- income.pension*.23 - 636000
  }else if(income.pension < 18000000){
    income.tax <- income.pension*.33 - 1536000
  }else if(income.pension < 40000000){
    income.tax <- income.pension*.4 - 2796000
  }else{
    income.tax <- income.pension*.45 - 4796000
  }
  
  if(income.tax < 0){
    income.tax <- 0
  }
  return(income.tax)
}
