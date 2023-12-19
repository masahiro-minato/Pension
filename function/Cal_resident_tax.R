# 住民税計算
Cal_resident_tax <- function(
    income.pension # 課税所得
){
  if(income.pension < (350000*2+310000)){
    resident.tax <- 0
  }else if(income.pension < (350000*2+420000)){
    resident.tax <- 5000
  }else{
    resident.tax <- income.pension*0.1 + 5000
  }
  return(resident.tax)
}