kurisage <- function(
    date,
    subject = "kosei", # "kosei" or "kiso"
    start.month = "2026-03-01",
    end.month = "2055-12-31",
    kosei.n = 1907504,
    kiso.n = 737290,
    haigusya.k = 388900){
  ## 繰り下げ月数
  postponement <- length(seq(as.Date("2024/03/01"), as.Date(start.month), "month"))-1
  print(paste0("繰り下げ月数: ",postponement))
  if(subject == "kosei"){
    Amount.paid <- kosei.n
  }else if(subject == "kiso"){
    Amount.paid <- kiso.n
  }else{
    print("subject指定エラー")
  }
  # 老齢基礎・厚生年金
  print(paste0("支給額: ",Amount.paid/12*(postponement*0.007+1)))
  X.date <- paste0("X.date.",postponement)
  assign(X.date,seq(as.POSIXct(start.month), as.POSIXct(end.month), by = "month"))
  Nenkin.date.p <- tibble(X.date = eval(parse(text = paste0("X.date.",postponement)))) %>% 
    mutate(
      !!paste0(subject,".",postponement) := as.integer(Amount.paid/12*(postponement*0.007+1))
    )
  Nenkin.date.p <- 
    Nenkin.date.p %>% 
    mutate(
      !!paste0(subject,".cum.",postponement) := cumsum(!!eval(parse(text = paste0("Nenkin.date.p$",subject,".",postponement))))
    )
  
  if(subject == "kosei"){
    print("老齢厚生年金")
    if(as.Date("2026-06-30") > as.Date(start.month)){
      # 配偶者加給
      print("配偶者加給有り")
      HXL.date <- seq(as.POSIXct(start.month), as.POSIXct(end.month), by = "month")
      Haigu.date <- tibble(X.date = HXL.date)
      HX.date <- seq(as.POSIXct(start.month), as.POSIXct("2026-06-30"), by = "month")
      Haigu.date.p <- tibble(X.date = HX.date) %>% 
        mutate(
          !!paste0("haigu.",postponement) := as.integer(haigusya.k/12)
        ) %>% 
        full_join(Haigu.date) %>% 
        mutate_if(is.numeric, ~replace(., is.na(.), 0))
      Haigu.date.p <- 
        Haigu.date.p %>%  
        mutate(
          !!paste0("haigu.cum.",postponement) := cumsum(!!eval(parse(text = paste0("Haigu.date.p$haigu.",postponement))))
        )
    }else{
      print("配偶者加給無し")
      HX.date <- seq(as.POSIXct("2024-03-01"), as.POSIXct(end.month), by = "month")
      Haigu.date.p <- tibble(X.date = HX.date) %>% 
        mutate(
          !!paste0("haigu.",postponement) := 0,
          !!paste0("haigu.cum.",postponement) := 0
        )
    }
  }
  # 結合
  if(subject == "kosei"){
    Return.date <- 
      date %>% 
      full_join(Nenkin.date.p) %>% 
      full_join(Haigu.date.p) %>% 
      mutate_if(is.numeric, ~replace(., is.na(.), 0))
  }else{
    Return.date <-
      date %>%
      full_join(Nenkin.date.p) %>%
      mutate_if(is.numeric, ~replace(., is.na(.), 0))
  }
  return(Return.date)
}
