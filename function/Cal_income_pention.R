# 年金収入に対する所得計算
Cal_income_pention <- function(
    Amount.paid,           # 年金収入
    deduction = 430000     # 基礎控除額（所得税48万円/住民税43万円）配偶者控除（所得税38万円/住民税33万円）
){
  # 条件別での年金所得計算
  if(Amount.paid < 1100000){
    income.pension <- 0
  }else if(Amount.paid < 3300000){
    income.pension <- Amount.paid-1100000
  }else if(Amount.paid < 4100000){
    income.pension <- Amount.paid*.75 - 275000 
  }else if(Amount.paid < 7700000){
    income.pension <- Amount.paid*.85 - 685000
  }else if(Amount.paid < 10000000){
    income.pension <- Amount.paid*.95 -1455000
  }else if(Amount.paid >= 10000000){
    income.pension <- Amount.paid - 1955000
  }
  print(paste0("年金収入: ",Amount.paid,"円⇒年金所得: ",income.pension,"円"))
  
  # 所得金額
  income <- income.pension - deduction
  if(income < 0){
    income <- 0
  }
  
  return(income)
}
