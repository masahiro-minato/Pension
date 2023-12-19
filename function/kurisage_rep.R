kurisage_rep <- function(
    date,
    start.month,
    kosei.n = 1907504,
    kiso.n = 737290,
    haigusya.k = 388900
){
  # 繰り下げ月数
  postponement <- length(seq(as.Date("2024/03/01"), as.Date(start.month), "month"))-1
  # 厚生年金・配偶者加給
  date <- kurisage(
    date = date,
    subject = "kosei", # "kosei" or "kiso"
    start.month = start.month,
    kosei.n = kosei.n,
    kiso.n = kiso.n,
    haigusya.k = haigusya.k
  )
  # 基礎年金
  date <- kurisage(
    date = date,
    subject = "kiso", # "kosei" or "kiso"
    start.month = start.month,
    kosei.n = kosei.n,
    kiso.n = kiso.n,
    haigusya.k = haigusya.k
  )
  # 合計列作成
  Return.date <- 
    date %>% 
    mutate(
      !!paste0("sum.cum.",postponement) := select(.,ends_with(paste0(".cum.",postponement))) %>% rowSums(na.rm=TRUE)
    )
  return(Return.date)
}
