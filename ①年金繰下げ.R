# 年金計算
Cal_年金 <- function(){
  kosei.n <- 1907504
  kiso.n <- 737290
  haigusya.k <- 388900
  
  end.month <- "2055-12-31 JST"
  
  X.date <- seq(as.POSIXct("2024-01-01 JST"), as.POSIXct(end.month), by = "month")
  Nenkin.date <- tibble(X.date = X.date)
  # 老齢基礎年金
  KoX.date <- seq(as.POSIXct("2024-03-01 JST"), as.POSIXct(end.month), by = "month")
  Kosei.date <- tibble(X.date = KoX.date) %>% 
    mutate(
      kosei.0 = as.integer(kosei.n/12),
      kosei.cum.0 = cumsum(kosei.0)
    )
  # 老齢基礎年金
  KiX.date <- seq(as.POSIXct("2024-03-01 JST"), as.POSIXct(end.month), by = "month")
  Kiso.date <- tibble(X.date = KiX.date) %>% 
    mutate(
      kiso.0 = as.integer(kiso.n/12),
      kiso.cum.0 = cumsum(kiso.0)
    )
  # 配偶者加給
  HXL.date <- seq(as.POSIXct("2024-03-01 JST"), as.POSIXct(end.month), by = "month")
  Haigu.date.L <- tibble(X.date = HXL.date)
  HX.date <- seq(as.POSIXct("2024-03-01 JST"), as.POSIXct("2026-06-30 JST"), by = "month")
  Haigu.date <- tibble(X.date = HX.date) %>% 
    mutate(
      haigu.0 = as.integer(haigusya.k/12)
    )%>% 
    full_join(Haigu.date.L) %>% 
    mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>% 
    mutate(
      haigu.cum.0 = cumsum(haigu.0)
    )
  # 結合
  Nenkin.date <- 
    Nenkin.date %>% 
    full_join(Kosei.date) %>% 
    full_join(Kiso.date) %>% 
    full_join(Haigu.date) %>% 
    mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>% 
    mutate(sum.cum.0 = select(.,ends_with(".cum.0")) %>% rowSums(na.rm=TRUE))
  
  start.months <- c("2025-03-01 JST","2026-03-01 JST","2027-03-01 JST","2028-03-01 JST","2029-03-01 JST")
  
  for (start.month in start.months) {
    print(start.month)
    Nenkin.date <- kurisage_rep(
      date = Nenkin.date,
      start.month = start.month,
      kosei.n = 1907504,
      kiso.n = 737290,
      haigusya.k = 388900
    )
  }
  
  # # データ保存
  # write_csv(Nenkin.date, "./csv/Nenkin.date.csv")
  
  # 累積値
  Nenkin.date.sum.pivot <- 
    Nenkin.date %>% 
    select(X.date,starts_with("sum.")) %>% 
    pivot_longer(cols = starts_with("sum.")) %>% 
    mutate_if(is.numeric, ~replace(., .==0, NA))
  
  # グラフ
  p <- ggplot(data = Nenkin.date.sum.pivot, aes(x = X.date)) + 
    theme_bw() + 
    labs(title = "年金受給額累計") +
    theme(plot.title = element_text(size = 18,  #font size and adjust
                                    hjust = 0.01,#adjust
    )) +
    ylab("受給額累計") +
    # xlab("year and month") +
    labs(color = "繰下げ月数") +
    scale_y_continuous(labels = label_comma(scale = 1/10000, suffix = '万円')) +
    scale_color_discrete(labels = c("0ヶ月","12ヶ月","24ヶ月","36ヶ月","48ヶ月","60ヶ月")) +
    theme(legend.position = c(0.18, 0.9), legend.justification = c(1, 1)) +
    geom_line(aes(y = value, color = name), linewidth = 1.2) +
    theme(axis.title.x = element_blank(),
          # axis.title.x = element_text(size = 16),
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
  
  plot(p)
  
  return(Nenkin.date)
}

Nenkin.date <- Cal_年金()

## 手動計算 ----------------------
kosei.n <- 1907504
kiso.n <- 737290
haigusya.k <- 388900

end.month <- "2055-12-31 JST"

X.date <- seq(as.POSIXct("2024-01-01 JST"), as.POSIXct(end.month), by = "month")
Nenkin.date <- tibble(X.date = X.date)
# 老齢基礎年金
KoX.date <- seq(as.POSIXct("2024-03-01 JST"), as.POSIXct(end.month), by = "month")
Kosei.date <- tibble(X.date = KoX.date) %>% 
  mutate(
    kosei.0 = as.integer(kosei.n/12),
    kosei.cum.0 = cumsum(kosei.0)
  )
# 老齢基礎年金
KiX.date <- seq(as.POSIXct("2024-03-01 JST"), as.POSIXct(end.month), by = "month")
Kiso.date <- tibble(X.date = KiX.date) %>% 
  mutate(
    kiso.0 = as.integer(kiso.n/12),
    kiso.cum.0 = cumsum(kiso.0)
  )
# 配偶者加給
HXL.date <- seq(as.POSIXct("2024-03-01 JST"), as.POSIXct(end.month), by = "month")
Haigu.date.L <- tibble(X.date = HXL.date)
HX.date <- seq(as.POSIXct("2024-03-01 JST"), as.POSIXct("2026-06-30 JST"), by = "month")
Haigu.date <- tibble(X.date = HX.date) %>% 
  mutate(
    haigu.0 = as.integer(haigusya.k/12)
  )%>% 
  full_join(Haigu.date.L) %>% 
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>% 
  mutate(
    haigu.cum.0 = cumsum(haigu.0)
  )
# 結合
Nenkin.date <- 
  Nenkin.date %>% 
  full_join(Kosei.date) %>% 
  full_join(Kiso.date) %>% 
  full_join(Haigu.date) %>% 
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>% 
  mutate(sum.cum.0 = select(.,ends_with(".cum.0")) %>% rowSums(na.rm=TRUE))

start.months <- c("2025-03-01 JST","2026-03-01 JST","2027-03-01 JST","2028-03-01 JST","2029-03-01 JST")

for (start.month in start.months) {
  print(start.month)
  Nenkin.date <- kurisage_rep(
    date = Nenkin.date,
    start.month = start.month,
    kosei.n = 1907504,
    kiso.n = 737290,
    haigusya.k = 388900
  )
}

# データ保存
write_csv(Nenkin.date, "./csv/Nenkin.date.csv")

# 累積値
Nenkin.date.sum.pivot <- 
  Nenkin.date %>% 
  select(X.date,starts_with("sum.")) %>% 
  pivot_longer(cols = starts_with("sum.")) %>% 
  mutate_if(is.numeric, ~replace(., .==0, NA))

# グラフ
p <- ggplot(data = Nenkin.date.sum.pivot, aes(x = X.date)) + 
  theme_bw() + 
  labs(title = "年金受給額累計") +
  theme(plot.title = element_text(size = 18,  #font size and adjust
                                  hjust = 0.01,#adjust
  )) +
  ylab("受給額累計") +
  # xlab("year and month") +
  labs(color = "繰下げ月数") +
  scale_y_continuous(labels = label_comma(scale = 1/10000, suffix = '万円')) +
  scale_color_discrete(labels = c("0ヶ月","12ヶ月","24ヶ月","36ヶ月","48ヶ月","60ヶ月")) +
  theme(legend.position = c(0.18, 0.9), legend.justification = c(1, 1)) +
  geom_line(aes(y = value, color = name), linewidth = 1.2) +
  theme(axis.title.x = element_blank(),
        # axis.title.x = element_text(size = 16),
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

plot(p)

# 保存
ggsave("./PDF/年金繰下げ受給額.pdf", plot = p, device = cairo_pdf, dpi=300, width=10, height=6)
