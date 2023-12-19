Read.data.MF4 <- function(
    path.excel = "./excel", # エクセルファイルのフォルダーパス
    pattern1 = "〇共通_保守*.+\\.xlsx$",
    pattern2 = "〇共通_周辺機装着情報*.+\\.csv$",
    pattern3 = "〇共通_機器*.+\\.xlsx$",
    save = TRUE,        # tsvファイル保存の有無
    path.tsv = "./tsv_data/", # tsv保存フォルダーパス
    tsv.name.EM = "Metis_MF4.tsv", # 保守EMデータのtsvファイル名
    tsv.name.MIF = "MF4_MIF.tsv",  # MIFデータのtsvファイル名
    manuf.start.month = "2022/12/01" # 製造開始月
){
  # MF4保守データのエクセルファイルを読込み、tsv形式で戻す関数
  # 戻り値はエクセルファイル毎の保守EMデータ、MIF機データの順に並んだリストとなる
  
  # 戻り値初期化
  return.obje <- list()
  # 該当エクセルファイルのパス取得
  files1 <- list.files(path = path.excel, pattern = pattern1, full.names = T)
  print(files1)
  files2 <- list.files(path = path.excel, pattern = pattern2, full.names = T)
  print(files2)
  files3 <- list.files(path = path.excel, pattern = pattern3, full.names = T)
  print(files3)
  
  # ファイル読込
  print("●共通ファイル読込...")
  Metis_master <- 
    read_excel("./excel/●共通_周辺機マスター.xlsx", sheet = 1, skip = 0)
  Metis_standard <- 
    read_excel("./excel/●共通_周辺機標準搭載構成.xlsx", sheet = 1, skip = 0) %>% 
    select_if(~sum(!is.na(.)) > 0)
  
  Metis_sheet1 <- 
    read_excel(files1, sheet = 1, skip = 0)
  Metis_sheet2 <- 
    read.delim(files2, fileEncoding = "cp932")
  Metis_sheet3 <- 
    read_excel(files3, sheet = 1, skip = 0)
  
  # 空白を削除/デバイスコードの後ろに空白が存在するため、周辺機マスタの周辺機機種略と合致しない
  Metis_sheet2$デバイスコード <- 
    gsub("[[:blank:]]", "", Metis_sheet2$デバイスコード)
  # 周辺機装着情報へ周辺機マスタを紐づけて周辺機名列を作成
  Metis_sheet2 <- 
    Metis_sheet2 %>% 
    left_join(Metis_master, by = c("デバイスコード" = "周辺機機種略")) %>% 
    rename( 周辺機名 = 周辺機開発名)
  # 機種略機番列の作成
  Metis_sheet2a <-
    Metis_sheet2 %>%
    mutate(
      機種略機番 = paste(機種略号, 機番, sep="")
    ) %>% 
    distinct_all()
  
  # 初回訪問区分他のtype変更
  print("初回訪問区分他のtype変更...")
  Metis_sheet1 <- 
    Metis_sheet1 %>% 
    mutate(
      初回訪問区分 = as.logical(初回訪問区分),
      ｵﾌﾟｼｮﾝ機種略号= as.logical(ｵﾌﾟｼｮﾝ機種略号),
      ｵﾌﾟｼｮﾝ機番= as.logical(ｵﾌﾟｼｮﾝ機番)
    )
  
  # 機種略機番列の作成
  print("機種略機番列の作成...")
  Metis_sheet1a <- 
    Metis_sheet1 %>% 
    mutate(
      機種略機番 = paste(機種略号, 機番, sep="")
    ) %>% 
    distinct_all()
  
  # 結合
  Metis_sheet1a2join <- left_join(Metis_sheet1a, Metis_sheet2a, by="機種略機番", relationship = "many-to-many")
  Metis_sheet1a2join %>% distinct_all()
  # print(names(Metis_sheet1a2join))
  
  # 列の選定
  print("列の選定...")
  Metis_MF4 <- Metis_sheet1a2join %>% 
    select(機種略号.x,機種略機番,機種ｺｰﾄﾞ,訪問区分,
           年月度,製造年月,納入日.x,稼動月,保守実施日,
           CE作業時間,ｺｰﾙ,EM,現象,現象SC名称,処置場所,周辺機名) %>%
    dplyr::filter(EM == 1) %>% 
    distinct_all()
  
  # 欠損行数の表示
  print(paste0("年月度 欠損行 : ",count(dplyr::filter(Metis_MF4, is.na(年月度)))$n))
  print(paste0("製造年月 欠損行 : ",count(dplyr::filter(Metis_MF4, is.na(製造年月)))$n))
  print(paste0("納入日 欠損行 : ",count(dplyr::filter(Metis_MF4, is.na(納入日.x)))$n))
  print(paste0("稼動月 欠損行 : ",count(dplyr::filter(Metis_MF4, is.na(稼動月)))$n))
  print(paste0("保守実施日 欠損行 : ",count(dplyr::filter(Metis_MF4, is.na(保守実施日)))$n))
  
  # 欠損行削除
  print("欠損行削除...")
  Metis_MF4 <- 
    Metis_MF4 %>% 
    drop_na(年月度,製造年月,納入日.x,稼動月,保守実施日)
  
  # 日付へ変換
  print("年月度変換...")
  Metis_MF4$年月度 <- 
    paste(str_sub(Metis_MF4$年月度, start=1, end=4),
          str_sub(Metis_MF4$年月度, start=5, end=-1),("01"),
          sep="-") %>% 
    as.POSIXct() %>% 
    as.Date(tz = "Asia/Tokyo")
  print("製造年月変換...")
  Metis_MF4$製造年月 <- 
    paste(str_sub(Metis_MF4$製造年月, start=1, end=4),
          str_sub(Metis_MF4$製造年月, start=5, end=-1),("01"),
          sep="-") %>% 
    as.POSIXct() %>% 
    as.Date(tz = "Asia/Tokyo")
  print("納入月変換...")
  Metis_MF4$納入日.x <- 
    paste(str_sub(Metis_MF4$納入日.x, start=1, end=4),
          str_sub(Metis_MF4$納入日.x, start=5, end=6),
          str_sub(Metis_MF4$納入日.x, start=7, end=-1),
          sep="-") %>% 
    as.POSIXct() %>% 
    as.Date(tz = "Asia/Tokyo")
　print("保守実施日変換...")
  Metis_MF4$保守実施日 <- 
    paste(str_sub(Metis_MF4$保守実施日, start=1, end=4),
          str_sub(Metis_MF4$保守実施日, start=5, end=6),
          str_sub(Metis_MF4$保守実施日, start=7, end=-1),
          sep="-") %>% 
    as.POSIXct() %>% 
    as.Date(tz = "Asia/Tokyo")

  # 列名変更
  print("列名変更")
  colnames(Metis_MF4)
  Metis_MF4 <- 
    rename(Metis_MF4, 
           Model_abbreviation = 機種略号.x,
           Machine_numbers = 機種略機番,
           Model_code = 機種ｺｰﾄﾞ,
           Visit_classification = 訪問区分,
           Year_and_month = 年月度,
           Manufacturing_date = 製造年月,
           Due_date = 納入日.x,
           Working_month = 稼動月,
           Maintenance_date = 保守実施日,
           CE_Working_hours = CE作業時間,
           Call = ｺｰﾙ,
           Phenomenon = 現象,
           SC_name = 現象SC名称,
           Treatment_location = 処置場所,
           # Peripheral_machine = ...17,
           Peripheral_name = 周辺機名
    )
  
  # 製造経過月数・稼働月数
  print("製造経過月数・稼働月数")
  Metis_MF4a <- 
    Metis_MF4 %>% 
    rowwise() %>%
    mutate(
      Elapsed_mf_months = 
        (length(seq(as.Date(manuf.start.month), as.Date(Manufacturing_date), "month"))-1),
      Working_months = 
        (length(seq(as.Date(Due_date), as.Date(Maintenance_date), "month"))-1)
    ) %>% 
    ungroup()
  # 作業月の追加
  Metis_MF4.maintenance <- 
    Metis_MF4a %>% 
    mutate(
      Maintenance_month = as.Date(paste(str_sub(Maintenance_date, start=1, end=4),
                                        str_sub(Maintenance_date, start=6, end=7),
                                        ("01"), sep="-"),tz = "Asia/Tokyo")
    ) %>% 
    distinct_all()
  
  # print(names(Metis_MF4.maintenance))
  
  # 機器データ -----
  print("機器データ")
  # 周辺機マスターの情報を周辺機装着情報へ結合
  Metis_MF4.Peripheral_name <- 
    Metis_sheet2a %>%
    select(機種略機番, 周辺機名) %>% 
    left_join(Metis_master, by = c("周辺機名" = "周辺機開発名"), relationship = "many-to-many") %>% 
    select(1:4)
  
  names(Metis_sheet3)
  
  # 機種略機番列の作成と周辺機情報を結合
  Metis_sheet3a <-
    Metis_sheet3 %>%
    mutate(
      機種略機番 = str_c(機種略号コード,機器番号)
    ) %>%
    left_join(Metis_MF4.Peripheral_name) %>% 
    distinct_all()
  
  # 標準搭載機情報を結合
  names(Metis_sheet3a)
  Metis_ADF.standard <-
    Metis_sheet3a %>%
    mutate(
      機番頭文字 = as.double(substr(機器番号, 1, 1))
    ) %>%
    left_join(Metis_standard, by = c("機種略号コード"="本体機種略","機番頭文字")) %>%
    select(機種略機番, 顧客名, ADF) %>% 
    distinct_all()
  
  # ADF標準装備機のADF周辺機名を標準装備ADF名に統一する 
  Metis_sheet3b <<- 
    Metis_sheet3a %>%
    left_join(Metis_ADF.standard) %>% 
    mutate(
      周辺機名 = if_else(
        {周辺機分類 == "ADF" & !is.na(ADF)},
        ADF, 
        周辺機名
      )
    ) %>% 
    distinct_all()
  
  # 機器データの日付変換　NAのある最新訪問日は変換不可
  Metis_MF4.machine <- 
    Date_conversion("Metis_sheet3b", col_list = c("納品年月日","製造年月","納入月","リンク日"))
  
  
  # # 機器データ
  # print("機器データ")
  # Metis_MF4.Peripheral_name <- 
  #   Metis_sheet2a %>%
  #   select(機種略機番, 周辺機名) %>% 
  #   left_join(Metis_master, by = c("周辺機名" = "周辺機開発名"))
  # 
  # Metis_sheet3a <-
  #   Metis_sheet3 %>%
  #   mutate(
  #     機種略機番 = str_c(機種略号コード,機器番号)
  #   ) %>%
  #   left_join(Metis_MF4.Peripheral_name) %>% 
  #   distinct_all()
  # 
  # Metis_sheet3.wider <<- 
  #   Metis_sheet3a %>% 
  #   select(-周辺機商品名, -コール目標値, -EM目標値, -狙いのACV, -周辺機機種略) %>% 
  #   pivot_wider(names_from = 周辺機分類, values_from = 周辺機名)
  
  # Metis_sheet3b <<-
  #   Metis_sheet3a %>%
  #   mutate(
  #     機番頭文字 = as.double(substr(機器番号, 1, 1))
  #   ) %>%
  #   left_join(Metis_standard, by = c("機種略号コード"="本体機種略","機番頭文字")) %>% 
  #   distinct()
  
  # print(names(Metis_sheet3))
  # 
  # Metis_sheet3 <<- 
  #   Metis_sheet3 %>% 
  #   mutate(
  #     機番頭文字 = as.double(substr(機器番号, 1, 1))
  #   ) %>% 
  #   left_join(Metis_standard, by = c("機種略号コード"="本体機種略","機番頭文字"))
  
  # 機器データの日付変換　NAのある最新訪問日は変換不可
  # Metis_MF4.machine <<- 
  #   Date_conversion("Metis_sheet3.wider", col_list = c("納品年月日","製造年月","納入月","リンク日"))
  
  # リスト作成
  print("リスト作成")
  return.obje <- append(return.obje, list(Metis_MF4.maintenance))
  return.obje <- append(return.obje, list(Metis_MF4.machine))
  index <- length(return.obje)
  names(return.obje)[c(1,2)] <- c("Metis_MF4.maintenance","MF4_MIF.machine")
  
  # ファイル保存
  if(save == TRUE){
    print("ファイル保存")
    write_tsv(Metis_MF4.maintenance,  str_c(path.tsv,tsv.name.EM))
    write_tsv(Metis_MF4.machine, str_c(path.tsv,tsv.name.MIF))
  }
  print("終了")
  return(return.obje)
}
