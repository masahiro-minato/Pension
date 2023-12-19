# 住民税 基礎控除 <- 430000 配偶者控除 <- 330000

# 住民税計算
Cal_住民税 <- function(
    Nenkin.date.kk,
    N.H.Insurance.mat
  ){
  # 年金
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
  Amount.paid.mat
  # 年金所得から国民健康保険を計算
  # 年金所得用マトリクス
  income.pention.mat <- matrix(0, nrow=32, ncol=6)
  # 住民税用マトリクス
  resident.tax.mat <- matrix(0, nrow=32, ncol=6)
  # 年金所得年額
  for (j in c(1:6)) {
    for (i in c(1:32)) {
      income.pention.mat[i,j] <- Cal_income_pention(Amount.paid.mat[[i,j]], deduction = (430000+330000))
    }
  }
  # 社会保険料(国民健康保険料)の控除
  income.pention.mat <- 
    income.pention.mat - N.H.Insurance.mat
  # 住民税年額
  for (j in c(1:6)) {
    for (i in c(1:32)) {
      resident.tax.mat[i,j] <- as.integer(Cal_resident_tax(income.pention.mat[[i,j]]))
    }
  }
  # 列名
  colnames(resident.tax.mat) <- c("0ヶ月","12ヶ月","24ヶ月","36ヶ月","48ヶ月","60ヶ月")
  # tibbleへ変換
  resident.tax.tib <- as_tibble(resident.tax.mat)
  # 時系列を追加
  resident.tax.tib$X.date <- seq(as.POSIXct("2025-01-01"), as.POSIXct("2056-12-31"), by = "year")
  resident.tax.tib <- 
    resident.tax.tib %>% 
    select(X.date,everything())
  
  # 月次データへ変換
  resident.tax.month.mat <- matrix(0, nrow=32*12, ncol=7)
  for (j in c(2:7)) {
    for (i in c(1:32)) {
      ii <- (i-1)*12
      for (iii in c(1:12)) {
        print(ii)
        iii <- iii+ii
        print(paste0(iii," : ",i," : ",j))
        resident.tax.month.mat[iii,j] <- resident.tax.tib[[i,j]]/12
      }
    }
  }
  # tibbleへ変換
  resident.tax.month.tib <- 
    as_tibble(resident.tax.month.mat,.name_repair)
  # 列名
  colnames(resident.tax.month.tib) <- c("X.date","0ヶ月","12ヶ月","24ヶ月","36ヶ月","48ヶ月","60ヶ月")  
  # 時系列を追加
  resident.tax.month.tib$X.date <- seq(as.POSIXct("2024-01-01"), as.POSIXct("2055-12-31"), by = "month")
  # 累積値
  resident.tax.month.tib <- 
    resident.tax.month.tib %>% 
    mutate(
      cum.0 = cumsum(`0ヶ月`),
      cum.12 = cumsum(`12ヶ月`),
      cum.24 = cumsum(`24ヶ月`),
      cum.36 = cumsum(`36ヶ月`),
      cum.48 = cumsum(`48ヶ月`),
      cum.60 = cumsum(`60ヶ月`),
    ) %>% 
    select(X.date,starts_with("cum."))
  # グラフ描画用ピボット変換
  resident.tax.month.tib.pivot <- 
    resident.tax.month.tib %>% 
    pivot_longer(cols = starts_with("cum.")) %>% 
    mutate_if(is.numeric, ~replace(., .==0, NA))
  
  # グラフ
  p.residente <- ggplot(data = resident.tax.month.tib.pivot, aes(x = X.date)) + 
    theme_bw() + 
    labs(title = "住民得税累計") +
    theme(plot.title = element_text(size = 18,  #font size and adjust
                                    hjust = 0.01,#adjust
    )) +
    ylab("住民税累計") +
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
  
  plot(p.residente)
  return(resident.tax.month.tib)
}

resident.tax.month.tib <- Cal_住民税(Nenkin.date.kk, N.H.Insurance.mat)

## 手動計算 ------------------
# 年金
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
Amount.paid.mat
# 年金所得から国民健康保険を計算
# 年金所得用マトリクス
income.pention.mat <- matrix(0, nrow=32, ncol=6)
# 住民税用マトリクス
resident.tax.mat <- matrix(0, nrow=32, ncol=6)
# 年金所得年額
for (j in c(1:6)) {
  for (i in c(1:32)) {
    income.pention.mat[i,j] <- Cal_income_pention(Amount.paid.mat[[i,j]], deduction = (430000+330000))
  }
}
# 社会保険料(国民健康保険料)の控除
income.pention.mat <- 
  income.pention.mat - N.H.Insurance.mat
# 住民税年額
for (j in c(1:6)) {
  for (i in c(1:32)) {
    resident.tax.mat[i,j] <- as.integer(Cal_resident_tax(income.pention.mat[[i,j]]))
  }
}
# 列名
colnames(resident.tax.mat) <- c("0ヶ月","12ヶ月","24ヶ月","36ヶ月","48ヶ月","60ヶ月")
# tibbleへ変換
resident.tax.tib <- as_tibble(resident.tax.mat)
# 時系列を追加
resident.tax.tib$X.date <- seq(as.POSIXct("2025-01-01"), as.POSIXct("2056-12-31"), by = "year")
resident.tax.tib <- 
  resident.tax.tib %>% 
  select(X.date,everything())

# 月次データへ変換
resident.tax.month.mat <- matrix(0, nrow=32*12, ncol=7)
for (j in c(2:7)) {
  for (i in c(1:32)) {
    ii <- (i-1)*12
    for (iii in c(1:12)) {
      print(ii)
      iii <- iii+ii
      print(paste0(iii," : ",i," : ",j))
      resident.tax.month.mat[iii,j] <- resident.tax.tib[[i,j]]/12
    }
  }
}
# tibbleへ変換
resident.tax.month.tib <- 
  as_tibble(resident.tax.month.mat,.name_repair)
# 列名
colnames(resident.tax.month.tib) <- c("X.date","0ヶ月","12ヶ月","24ヶ月","36ヶ月","48ヶ月","60ヶ月")  
# 時系列を追加
resident.tax.month.tib$X.date <- seq(as.POSIXct("2024-01-01"), as.POSIXct("2055-12-31"), by = "month")
# 累積値
resident.tax.month.tib <- 
  resident.tax.month.tib %>% 
  mutate(
    cum.0 = cumsum(`0ヶ月`),
    cum.12 = cumsum(`12ヶ月`),
    cum.24 = cumsum(`24ヶ月`),
    cum.36 = cumsum(`36ヶ月`),
    cum.48 = cumsum(`48ヶ月`),
    cum.60 = cumsum(`60ヶ月`),
  ) %>% 
  select(X.date,starts_with("cum."))
# グラフ描画用ピボット変換
resident.tax.month.tib.pivot <- 
  resident.tax.month.tib %>% 
  pivot_longer(cols = starts_with("cum.")) %>% 
  mutate_if(is.numeric, ~replace(., .==0, NA))

# グラフ
p.residente <- ggplot(data = resident.tax.month.tib.pivot, aes(x = X.date)) + 
  theme_bw() + 
  labs(title = "住民得税累計") +
  theme(plot.title = element_text(size = 18,  #font size and adjust
                                  hjust = 0.01,#adjust
  )) +
  ylab("住民税累計") +
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


plot(p.residente)

# 保存
ggsave("./PDF/住民税累計.pdf", plot = p.residente, device = cairo_pdf, dpi=300, width=10, height=6)
