Read.excel.MF4 <- function(
    path.excel = "../EM-analysis/Excel/保守データ", # エクセルファイルのフォルダーパス
    pattern = "周辺機稼働品質Metis-MF4.+\\.xlsx$",
    hosyu = c(4, 1),    # 保守データ c(シート番号, スキップ行数)
    syuhenki = c(5, 0), # 周辺機データ
    kiki = c(6, 0),     # 機器データ 
    save = TRUE,        # tsvファイル保存の有無
    path.tsv = "./tsv_data", # tsv保存フォルダーパス
    tsv.name.EM = "/tmp_Metis_MF4_", # 保守EMデータのtsvファイル名 年月は末尾に自動取得する
    tsv.name.MIF = "/tmp_MF4_MIF_",  # MIFデータのtsvファイル名 年月は末尾に自動取得する
    manuf.start.month = "2022/12/01" # 製造開始月
    ){
  # MF4保守データのエクセルファイルを読込み、tsv形式で戻す関数
  # 戻り値はエクセルファイル毎の保守EMデータ、MIF機データの順に並んだリストとなる
  
  # 戻り値初期化
  return.obje <- list()
  # 該当エクセルファイルのパス取得
  files <- list.files(path = path.excel, pattern = pattern, full.names = T)
  print(files)
  # データ年月の抽出
  ident.year <- stringr::str_sub(files, -21, -20)
  ident.month <- stringr::str_sub(files, -15, -14)
  ident <- str_c(ident.year,ident.month)
  print(ident)
  # エクセル読込
  tbl_1 <- sapply(files, function(x){read_excel(x, sheet = hosyu[1], skip = hosyu[2])}, simplify=FALSE)
  tbl_2 <- sapply(files, function(x){read_excel(x, sheet = syuhenki[1], skip = syuhenki[2])}, simplify=FALSE)
  tbl_3 <- sapply(files, function(x){read_excel(x, sheet = kiki[1], skip = kiki[2])}, simplify=FALSE)
  # データ抽出
  for (n in 1:length(tbl_1)) {
    for (i in 1:3) {
      print(str_c("Metis_sheet",i,"_",ident[n]))
      assign(str_c("Metis_sheet",i,"_",ident[n]), eval(parse(text = str_c("tbl_",i,"[[",n,"]]"))))
      if(i == 3){
        assign(str_c("Metis_sheet",i,"_",ident[n]), eval(parse(text = str_c("tbl_",i,"[[",n,"]]"))), envir = globalenv())
      }
    }
    # 機器データの日付変換　NAのある最新訪問日は変換不可
    print("機器データの日付変換")
    assign(str_c("MF4_MIF_",ident[n]),
           Date_conversion(str_c("Metis_sheet3_",ident[n]),
                           col_list = c("納品年月日","製造年月","納入月","リンク日")))
    # 初回訪問区分他のtype変更
    Metis_sheet1a <-
      eval(parse(text = str_c("Metis_sheet1_",ident[n]))) %>%
      mutate(
        初回訪問区分 = as.logical(初回訪問区分),
        ｵﾌﾟｼｮﾝ機種略号= as.logical(ｵﾌﾟｼｮﾝ機種略号),
        ｵﾌﾟｼｮﾝ機番= as.logical(ｵﾌﾟｼｮﾝ機番),
        機種略機番 = paste(機種略号, 機番, sep="")
      )%>% 
      distinct_all()
    # Metis_sheet2に重複行あり
    Metis_sheet2a <-
      eval(parse(text = str_c("Metis_sheet2_",ident[n]))) %>%
      distinct_all()
      
    # 結合
    Metis_sheet1a2join <- left_join(Metis_sheet1a, Metis_sheet2a, by="機種略機番") # A tibble: 8,226 × 123
    Metis_sheet1a2join %>% distinct_all() # A tibble: 8,226 × 123
    names(Metis_sheet1a2join)
   
    # 列の選定
    Metis_MF4 <- Metis_sheet1a2join %>% 
      select(機種略号.x,機種略機番,機種ｺｰﾄﾞ,訪問区分,
             年月度,製造年月,納入日.x,稼動月,保守実施日,
             # CE作業時間,ｺｰﾙ,EM,現象,現象SC名称,処置場所,...17,周辺機名) %>%
             CE作業時間,ｺｰﾙ,EM,現象,現象SC名称,処置場所,...17,周辺機名,顧客都道府県名,顧客住所) %>%
      dplyr::filter(EM == 1) %>% 
      distinct_all() # A tibble: 3,057 × 19
    
    # 日付へ変換
    Metis_MF4$年月度 <- 
      paste(str_sub(Metis_MF4$年月度, start=1, end=4),
            str_sub(Metis_MF4$年月度, start=5, end=-1),("01"),
            sep="-") %>% 
      as.POSIXct() %>% 
      as.Date(tz = "Asia/Tokyo")
    
    Metis_MF4$製造年月 <- 
      paste(str_sub(Metis_MF4$製造年月, start=1, end=4),
            str_sub(Metis_MF4$製造年月, start=5, end=-1),("01"),
            sep="-") %>% 
      as.POSIXct() %>% 
      as.Date(tz = "Asia/Tokyo")
    
    Metis_MF4$納入日.x <- 
      paste(str_sub(Metis_MF4$納入日.x, start=1, end=4),
            str_sub(Metis_MF4$納入日.x, start=5, end=6),
            str_sub(Metis_MF4$納入日.x, start=7, end=-1),
            sep="-") %>% 
      as.POSIXct() %>% 
      as.Date(tz = "Asia/Tokyo")
    
    Metis_MF4$保守実施日 <- 
      paste(str_sub(Metis_MF4$保守実施日, start=1, end=4),
            str_sub(Metis_MF4$保守実施日, start=5, end=6),
            str_sub(Metis_MF4$保守実施日, start=7, end=-1),
            sep="-") %>% 
      as.POSIXct() %>% 
      as.Date(tz = "Asia/Tokyo")
    
    # 列名変更
    rename <- 
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
             Peripheral_machine = ...17,
             Peripheral_name = 周辺機名,
             Prefectures = 顧客都道府県名,
             Customer_address = 顧客住所
      )
    assign(str_c("Metis_MF4_",ident[n]), rename)
    
    # 製造経過月数・稼働月数
    assign(str_c("Metis_MF4_",ident[n],"a"),
      eval(parse(text = str_c("Metis_MF4_",ident[n]))) %>% 
      rowwise() %>%
      mutate(
        Elapsed_mf_months = 
          (length(seq(as.Date(manuf.start.month), as.Date(Manufacturing_date), "month"))-1),
        Working_months = 
          (length(seq(as.Date(Due_date), as.Date(Maintenance_date), "month"))-1)
      ) %>% 
      ungroup()) # A tibble: 3,057 × 21
    # 作業月の追加
    assign(str_c("Metis_MF4_",ident[n],"b"),
      eval(parse(text = str_c("Metis_MF4_",ident[n],"a"))) %>% 
      mutate(
        Maintenance_month = as.Date(paste(str_sub(Maintenance_date, start=1, end=4),
                                          str_sub(Maintenance_date, start=6, end=7),
                                          ("01"), sep="-"),tz = "Asia/Tokyo")
      ) %>% 
      distinct_all())
    # 重複行の削除/念のため
    assign(str_c("Metis_MF4_",ident[n],"c"),
           eval(parse(text = str_c("Metis_MF4_",ident[n],"b"))) %>%
           distinct_all())
    # リスト作成
    return.obje <- append(return.obje, list(eval(parse(text = str_c("Metis_MF4_",ident[n],"c")))))
    return.obje <- append(return.obje, list(eval(parse(text = str_c("MF4_MIF_",ident[n])))))
    index <- length(return.obje)
    names(return.obje)[c((index-1),index)] <- c(str_c("Metis_MF4_",ident[n]),str_c("MF4_MIF_",ident[n]))
    # ファイル保存
    if(save == TRUE){
      write_tsv(eval(parse(text = str_c("Metis_MF4_",ident[n],"c"))),
                str_c(path.tsv,tsv.name.EM,ident[n],".tsv"))  # A tibble: 3,057 × 22
      write_tsv(eval(parse(text = str_c("MF4_MIF_",ident[n]))),
                str_c(path.tsv,tsv.name.MIF,ident[n],".tsv")) # A tibble: 12,304 × 32
    }
  }
    return(return.obje)
}

  