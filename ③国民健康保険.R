# 国民健康保険
Cal_国民健康保険 <- function(
    Nenkin.date.kk
    ){
  # 年金計算
  kosei.n <- 1907504
  kiso.n <- 737290
  haigusya.k <- 388900
  kojin.n <- 801195
  kigyo.n <- 743502
  end.month <- "2055-12-31 JST"
  
  # 年金収入（個人年金以外）
  Amount.paid.mat <- matrix(0, nrow=32, ncol=6)
  colnames(Amount.paid.mat) <- c("0ヶ月","12ヶ月","24ヶ月","36ヶ月","48ヶ月","60ヶ月")
  rownames(Amount.paid.mat) <- seq(2024,2055)
  num <- 0
  for (j in c(1:6)) {
    subject <- paste0("sum.cum.",num)
    Amount.paid.mat[1,j] <- Nenkin.date.kk[[12,subject]]-kojin.n
    num <- num + 12
    n <- 1
    for (i in seq(12, 384-12, by=12)) {
      n <- n+1
      Amount.paid.mat[n,j] <- Nenkin.date.kk[[i+12,subject]]-Nenkin.date.kk[[i,subject]]-kojin.n
    }
  }
  
  # 年金所得から国民健康保険を計算
  # 年金所得用マトリクス
  income.pention.mat <- matrix(0, nrow=32, ncol=6)
  # 国民県保健用マトリクス
  N.H.Insurance.mat <- matrix(0, nrow=32, ncol=6)
  for (j in c(1:6)) {
    for (i in c(1:32)) {
      income.pention.mat[i,j] <- Cal_income_pention(Amount.paid.mat[[i,j]])
      N.H.Insurance.mat[i,j] <- Cal_NHInsurance(Amount.paid.mat[[i,j]])
    }
  }
  # 列名
  colnames(N.H.Insurance.mat) <- c("0ヶ月","12ヶ月","24ヶ月","36ヶ月","48ヶ月","60ヶ月")
  # グローバル変数として保存
  N.H.Insurance.mat <<- N.H.Insurance.mat
  # tibbleへ変換
  N.H.Insurance.tib <- as_tibble(N.H.Insurance.mat, .name_repair = 'unique')
  # 時系列を追加
  N.H.Insurance.tib$X.date <- seq(as.POSIXct("2025-01-01"), as.POSIXct("2056-12-31"), by = "year")
  # 2024年健康保険データを追加
  N.H.Insurance.tib <- 
    N.H.Insurance.tib %>% 
    add_row("0ヶ月" = 312000, "12ヶ月" = 312000, "24ヶ月" = 312000, "36ヶ月" = 312000,
            "48ヶ月" = 312000, "60ヶ月" = 312000, "X.date" = as.POSIXct("2024-01-01"), .before = 1) %>% 
    slice(-33,) %>% 
    select(X.date,ends_with("月"))
  
  # 月次データへ変換
  N.H.Insurance.month.mat <- matrix(0, nrow=32*12, ncol=7)
  for (j in c(2:7)) {
    for (i in c(1:32)) {
      ii <- (i-1)*12
      for (iii in c(1:12)) {
        print(ii)
        iii <- iii+ii
        print(paste0(iii," : ",i," : ",j))
        N.H.Insurance.month.mat[iii,j] <- N.H.Insurance.tib[[i,j]]/12
      }
    }
  }
  
  N.H.Insurance.month.tib <- 
    as_tibble(N.H.Insurance.month.mat, .name_repair = 'unique')
  # 列名
  colnames(N.H.Insurance.month.tib) <- c("X.date","0ヶ月","12ヶ月","24ヶ月","36ヶ月","48ヶ月","60ヶ月")  
  # 時系列を追加
  N.H.Insurance.month.tib$X.date <- seq(as.POSIXct("2024-01-01"), as.POSIXct("2055-12-31"), by = "month")
  
  N.H.Insurance.month.tib <- 
    N.H.Insurance.month.tib %>% 
    mutate(
      cum.0 = cumsum(`0ヶ月`),
      cum.12 = cumsum(`12ヶ月`),
      cum.24 = cumsum(`24ヶ月`),
      cum.36 = cumsum(`36ヶ月`),
      cum.48 = cumsum(`48ヶ月`),
      cum.60 = cumsum(`60ヶ月`),
    ) %>% 
    select(X.date,starts_with("cum."))
  
  N.H.Insurance.month.tib.pivot <- 
    N.H.Insurance.month.tib %>% 
    pivot_longer(cols = starts_with("cum.")) %>% 
    mutate_if(is.numeric, ~replace(., .==0, NA))
  
  # グラフ
  p.NHI <- ggplot(data = N.H.Insurance.month.tib.pivot, aes(x = X.date)) + 
    theme_bw() + 
    labs(title = "国民健康保険料金累計") +
    theme(plot.title = element_text(size = 18,  #font size and adjust
                                    hjust = 0.01,#adjust
    )) +
    ylab("保険額累計") +
    labs(color = "繰下げ月数") +
    scale_y_continuous(
      # breaks=seq(0,140000000,length=8),limits=c(0,140000000),
      labels = label_comma(scale = 1/10000, suffix = '万円')) +
    scale_color_discrete(labels = c("0ヶ月","12ヶ月","24ヶ月","36ヶ月","48ヶ月","60ヶ月")) +
    theme(legend.position = c(0.18, 0.9), legend.justification = c(1, 1)) +
    geom_line(aes(y = value, color = name), linewidth = 1.2) +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_text(size = 16),
          axis.text.x = element_text(angle = 30, hjust = 1, size = 14),
          axis.text.y = element_text(size = 14),
          legend.title = element_text(size = 16),
          legend.text = element_text(size=14)) +
    scale_x_datetime(limits = c(as.POSIXct("2024-01-01 JST"), as.POSIXct(end.month)),
                     date_breaks = "2 year", date_labels = "%Y年%m月") + # 表示期間の設定
    geom_vline(xintercept = as.POSIXct("2029-02-22 JST"), linetype = 2, color = "darkblue") +
    geom_vline(xintercept = as.POSIXct("2039-02-22 JST"), linetype = 2, color = "darkblue") + 
    geom_vline(xintercept = as.POSIXct("2049-02-22 JST"), linetype = 2, color = "darkblue") # 垂直線
  
  plot(p.NHI)
  
  return(N.H.Insurance.month.tib)
}

N.H.Insurance.month.tib <- Cal_国民健康保険(Nenkin.date.kk)

## 手動計算 ----------------------
# 年金計算
kosei.n <- 1907504
kiso.n <- 737290
haigusya.k <- 388900
kojin.n <- 801195
kigyo.n <- 743502

# 繰り下げ月数
# start.months <- c("2025-03-01","2026-03-01","2027-03-01","2028-03-01","2029-03-01")
# postponement <- length(seq(as.Date("2024/01/01"), as.Date("2055-12-31"), "month"))-1
# 384/12

# 年金収入（個人年金以外）
Amount.paid.mat <- matrix(0, nrow=32, ncol=6)
colnames(Amount.paid.mat) <- c("0ヶ月","12ヶ月","24ヶ月","36ヶ月","48ヶ月","60ヶ月")
rownames(Amount.paid.mat) <- seq(2024,2055)
num <- 0
for (j in c(1:6)) {
  subject <- paste0("sum.cum.",num)
  Amount.paid.mat[1,j] <- Nenkin.date.kk[[12,subject]]-kojin.n
  num <- num + 12
  n <- 1
  for (i in seq(12, 384-12, by=12)) {
    n <- n+1
    Amount.paid.mat[n,j] <- Nenkin.date.kk[[i+12,subject]]-Nenkin.date.kk[[i,subject]]-kojin.n
  }
}
Amount.paid.mat
# 年金所得から国民健康保険を計算
# 年金所得用マトリクス
income.pention.mat <- matrix(0, nrow=32, ncol=6)
# 国民県保健用マトリクス
N.H.Insurance.mat <- matrix(0, nrow=32, ncol=6)
for (j in c(1:6)) {
  for (i in c(1:32)) {
    income.pention.mat[i,j] <- Cal_income_pention(Amount.paid.mat[[i,j]])
    N.H.Insurance.mat[i,j] <- Cal_NHInsurance(Amount.paid.mat[[i,j]])
  }
}
# 列名
colnames(N.H.Insurance.mat) <- c("0ヶ月","12ヶ月","24ヶ月","36ヶ月","48ヶ月","60ヶ月")
# tibbleへ変換
N.H.Insurance.tib <- as_tibble(N.H.Insurance.mat)
# 時系列を追加
N.H.Insurance.tib$X.date <- seq(as.POSIXct("2025-01-01"), as.POSIXct("2056-12-31"), by = "year")
# 2024年健康保険データを追加
N.H.Insurance.tib <- 
  N.H.Insurance.tib %>% 
  add_row("0ヶ月" = 312000, "12ヶ月" = 312000, "24ヶ月" = 312000, "36ヶ月" = 312000,
          "48ヶ月" = 312000, "60ヶ月" = 312000, "X.date" = as.POSIXct("2024-01-01"), .before = 1) %>% 
  slice(-33,) %>% 
  select(X.date,ends_with("月"))
# 積算値
# N.H.Insurance.tib.cum <- 
#   N.H.Insurance.tib %>% 
#   mutate(
#     cum.0 = cumsum(`0ヶ月`),
#     cum.12 = cumsum(`12ヶ月`),
#     cum.24 = cumsum(`24ヶ月`),
#     cum.36 = cumsum(`36ヶ月`),
#     cum.48 = cumsum(`48ヶ月`),
#     cum.60 = cumsum(`60ヶ月`),
#   ) %>% 
#   select(X.date,starts_with("cum."))

# 月次データへ変換
N.H.Insurance.month.mat <- matrix(0, nrow=32*12, ncol=7)
for (j in c(2:7)) {
  for (i in c(1:32)) {
    ii <- (i-1)*12
    for (iii in c(1:12)) {
      print(ii)
      iii <- iii+ii
      print(paste0(iii," : ",i," : ",j))
      N.H.Insurance.month.mat[iii,j] <- N.H.Insurance.tib[[i,j]]/12
    }
  }
}

N.H.Insurance.month.tib <- 
  as_tibble(N.H.Insurance.month.mat,.name_repair)
# 列名
colnames(N.H.Insurance.month.tib) <- c("X.date","0ヶ月","12ヶ月","24ヶ月","36ヶ月","48ヶ月","60ヶ月")  
# 時系列を追加
N.H.Insurance.month.tib$X.date <- seq(as.POSIXct("2024-01-01"), as.POSIXct("2055-12-31"), by = "month")

N.H.Insurance.month.tib <- 
  N.H.Insurance.month.tib %>% 
  mutate(
    cum.0 = cumsum(`0ヶ月`),
    cum.12 = cumsum(`12ヶ月`),
    cum.24 = cumsum(`24ヶ月`),
    cum.36 = cumsum(`36ヶ月`),
    cum.48 = cumsum(`48ヶ月`),
    cum.60 = cumsum(`60ヶ月`),
  ) %>% 
  select(X.date,starts_with("cum."))

N.H.Insurance.month.tib.pivot <- 
  N.H.Insurance.month.tib %>% 
  pivot_longer(cols = starts_with("cum.")) %>% 
  mutate_if(is.numeric, ~replace(., .==0, NA))

# グラフ
p.NHI <- ggplot(data = N.H.Insurance.month.tib.pivot, aes(x = X.date)) + 
  theme_bw() + 
  labs(title = "国民健康保険料金累計") +
  theme(plot.title = element_text(size = 18,  #font size and adjust
                                  hjust = 0.01,#adjust
  )) +
  ylab("保険額累計") +
  labs(color = "繰下げ月数") +
  scale_y_continuous(
    # breaks=seq(0,140000000,length=8),limits=c(0,140000000),
    labels = label_comma(scale = 1/10000, suffix = '万円')) +
  scale_color_discrete(labels = c("0ヶ月","12ヶ月","24ヶ月","36ヶ月","48ヶ月","60ヶ月")) +
  theme(legend.position = c(0.18, 0.9), legend.justification = c(1, 1)) +
  geom_line(aes(y = value, color = name), linewidth = 1.2) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = 16),
        axis.text.x = element_text(angle = 30, hjust = 1, size = 14),
        axis.text.y = element_text(size = 14),
        legend.title = element_text(size = 16),
        legend.text = element_text(size=14)) +
  scale_x_datetime(limits = c(as.POSIXct("2024-01-01 JST"), as.POSIXct("2055-12-31")),
                   date_breaks = "2 year", date_labels = "%Y年%m月") + # 表示期間の設定
  geom_vline(xintercept = as.POSIXct("2029-02-22 JST"), linetype = 2, color = "darkblue") +
  geom_vline(xintercept = as.POSIXct("2039-02-22 JST"), linetype = 2, color = "darkblue") + 
  geom_vline(xintercept = as.POSIXct("2049-02-22 JST"), linetype = 2, color = "darkblue") # 垂直線


plot(p.NHI)

# 保存
ggsave("./PDF/国民健康保険料金累計.pdf", plot = p.NHI, device = cairo_pdf, dpi=300, width=10, height=6)

# -------------------------------------------
# 年金所得計算
income.pension <- 
  Cal_income_pention(Amount.paid = 2644773)
# 国民健康保険料
if(income.pension < 430000){
  N.H.Insurance <- floor((income.pension*(0.0625+0.0209) + (36500+12100)*2*0.3)/100)*100
}else if(income.pension < (430000+290000*2)){
  N.H.Insurance <- floor((income.pension*(0.0625+0.0209) + (36500+12100)*2*0.5)/100)*100
}else if(income.pension < (430000+535000*2)){
  N.H.Insurance <- floor((income.pension*(0.0625+0.0209) + (36500+12100)*2*0.8)/100)*100
}else{
  N.H.Insurance <- floor((income.pension*(0.0625+0.0209) + (36500+12100)*2)/100)*100
}

N.H.Insurance <- floor((income.pension*(0.0625+0.0209) + (36500+12100)*2)/100)*100

Cal_NHInsurance(Amount.paid = 2644773)


# 年金所得
Amount.paid <- 4000000
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
# 基礎控除430000円
deduction <- 430000
# 所得金額
income <- income.pension - deduction


400
2295000
350
1920000
340
1845000
330
1770000
320
1670000
310
1570000
300
1470000
250
970000
200
470000