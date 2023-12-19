Cal_NHInsurance <- function(
    Amount.paid
    ){
  # 年金所得計算
  income.pension <- 
    Cal_income_pention(Amount.paid = Amount.paid)
  # 国民健康保険料
  if(income.pension < 430000){
    N.H.Insurance <- floor(income.pension*(0.0625+0.0209)/100)*100 + floor((36500+12100)*2*0.3/100)*100
  }else if(income.pension < (430000+290000*2)){
    N.H.Insurance <- floor(income.pension*(0.0625+0.0209)/100)*100 + floor((36500+12100)*2*0.5/100)*100
  }else if(income.pension < (430000+535000*2)){
    N.H.Insurance <- floor(income.pension*(0.0625+0.0209)/100)*100 + floor((36500+12100)*2*0.8/100)*100
  }else{
    N.H.Insurance <- floor(income.pension*(0.0625+0.0209)/100)*100 + floor((36500+12100)*2/100)*100
  }
  return(N.H.Insurance)
}
