library(haven);library(data.table);library(magrittr);library(fst);library(openxlsx)

setwd("/home/minhyuk.kim/ShinyApps/R-skku-biohrs/study")
setDTthreads(0)  ## 0: All


## SAS to fst/csv
# for (v in c("bnc", "bnd", "m20", "m30", "m40", "m60", "inst", "g1e_0208", "g1e_0915")){
#   read_sas(file.path("data", paste0("nsc2_", v, "_1000.sas7bdat"))) %>% 
#     write_fst(file.path("data", paste0("nsc2_", v, "_1000.fst")))
#     #fwrite(file.path("data", paste0("nsc2_", v, "_1000.csv")))
# }


## fst
# inst <- read_fst("data/nsc2_inst_1000.fst", as.data.table = T)
# bnc <- read_fst("data/nsc2_bnc_1000.fst", as.data.table = T)
# bnd <- read_fst("data/nsc2_bnd_1000.fst", as.data.table = T)
# t20 <- read_fst("data/nsc2_m20_1000.fst", as.data.table = T)
# t30 <- read_fst("data/nsc2_m30_1000.fst", as.data.table = T)
# t40 <- read_fst("data/nsc2_m40_1000.fst", as.data.table = T)
# t60 <- read_fst("data/nsc2_m60_1000.fst", as.data.table = T)
# g1e_0915 <- read_fst("data/nsc2_g1e_0915_1000.fst", as.data.table = T)

inst <- read_fst("/home/minhyuk.kim/knhis_data/INST.fst", as.data.table = T)
bfc <- read_fst("/home/minhyuk.kim/knhis_data/BFC.fst", as.data.table = T)
bnd <- read_fst("/home/minhyuk.kim/knhis_data/DTH.fst", as.data.table = T)
# t20 <- read_fst("/home/minhyuk.kim/knhis_data/T20.fst", as.data.table = T)
t30 <- read_fst("/home/minhyuk.kim/knhis_data/T30.fst", as.data.table = T)
#t40 <- read_fst("/home/minhyuk.kim/knhis_data/T40.fst", as.data.table = T)
t60 <- read_fst("/home/minhyuk.kim/knhis_data/T60.fst", as.data.table = T)
# g1e_2002 <- read.csv("/home/minhyuk.kim/knhis_data/g1e_obj_2002.csv") %>% as.data.table()
# g1e_2017 <- read.csv("/home/minhyuk.kim/knhis_data/g1e_obj_2017.csv") %>% as.data.table()
g1e_2018 <- read.csv("/home/minhyuk.kim/knhis_data/g1e_rst_2018.csv") %>% as.data.table()

g1e_2017 <- read.csv("/home/minhyuk.kim/knhis_data/g1e_obj_2017.csv") %>% as.data.table()



# depression
code.DEP <- paste(paste0("F", 32:34), collapse = "|")

# exclude diseases
excl.code.disease <- list(
  dementia = c("G20"),
  Parkinson = c("G20"),
  stroke  = c("I60", "I61", "I62", "I63", "I64"),
  cerebral.hemorrhage = c("S06"),
  mental.disease <- paste(paste0("F", setdiff(c(10:19,20:29,30:39,40:48,50:59,60:69,70:79,80:89,90:98,99),c(32:34))), collapse = "|")
)

code.mental.disease <- paste(paste0("F", setdiff(c(10:19,20:29,30:39,40:48,50:59,60:69,70:79,80:89,90:98,99),c(32:34))), collapse = "|")


# PDT, PCA code - include
code.PDT <- c("K4410027", "K4410127", "K4411023", "K4411123", "K4411223", "BI0701OQ", "O1301")
code.PCA <- c("LA201", "LA202", "LA203", "LA204", "LA205", "LA206")

# active ingredient code - include
# midazolam (195201BIJ,195202BIJ, 195203BIJ, 195204BIJ, 195230BIJ, 195231BIJ, 195232BIJ) 및
# remimazolam (694101BIJ, 694102BIJ)처방 유무, 처방 용량 추출 여부 확인 부탁드립니다. 

code.ingredient <- list(
  midazolam = c("195201BIJ","195202BIJ", "195203BIJ", "195204BIJ", "195230BIJ", "195231BIJ", "195232BIJ"),
  remimazolam = c("694101BIJ", "694102BIJ")
)

# operation list
code.surgery = list(
  heart = c(
    "M6551","M6552","M6553","M6554",
    "M6561","M6562","M6563","M6564","M6565","M6566","M6567",
    "M6571","M6572","M6580","M6581","M6582",
    "O1640","O1641","O1642","O1647","O1648","O1649",
    "O1671","O1672","O1680",
    "O1701","O1702","O1703","O1704","O1705",
    "O1710","O1711","O1721","O1723",
    "O1740","O1750","O1760","O1770",
    "O1781","O1782","O1783",
    "O1791","O1792","O1793","O1794","O1795","O1796","O1797","O1798",
    "O1800","O1810","O1821","O1822","O1823","O1824","O1825","O1826",
    "O1840","O1841","O1861","O1873","O1874","O1875","O1878","O1879",
    "O1960",
    "OA640","OA641","OA642","OA647","OA648","OA649"
  ),
  stomach = c(
    "Q0251","Q0252","Q0253","Q0254","Q0255","Q0256","Q0257","Q0258",
    "Q2533","Q2534","Q2536","Q2537","Q2594","Q2598"
  ),
  colon = c(
    "Q1261","Q1262",
    "Q2671","Q2672","Q2673","Q2679",
    "Q2921","Q2922","Q2923","Q2924","Q2925","Q2926","Q2927",
    "QA671","QA672","QA673","QA679",
    "QA921","QA922","QA923","QA924","QA925","QA926"
  ),
  prostate = c("R3950","R3960","RZ512")
)

code.heart <- code.surgery$heart
code.other.major <- c(code.surgery$stomach, code.surgery$colon, code.surgery$prostate)


# dementia 리스트 확인
code.dementia <- paste0(c("F00", "F01", "F02", "F03", "F051", "G30", "G311"), collapse = "|")

# 치매 약물
code.dementia.drug <- list(
  rivastigmine = c("224501ACH", "224503ACH", "224504ACH", "224505ACH", "224506CPC", "224507CPC", "224508CPC"),
  galantamine = c("385203ACR", "385203ATR", "385204ACR", "385204ATR", "385205ACR", "385205ATR"),
  memantine  = c("190001ATB", "190003ATD", "190004ATB", "190004ATD", "190005ATB", "190006ATD", "190031ALQ", "738600ATB"),
  donepezil = c("148601APD", "148601ATB", "148601ATD", "148602APD", "148602ATB", "148602ATD", "148603ATB", "148604ATB", "148630ALQ", "148631ALQ", "643403CPC", "643404CPC", "738600ATB")
)

# 진통 마취제
code.opiode <- list(
  oxycodone = c("359001ATR", "359002ATB", "359002ATR", "359003ATB", "359003ATR", "359004ATB", "359007ATR", "359030BIJ", "359031BIJ", "517100ATR", "517200ATR", "564000ATR", "564100ATR", "667600ATR"),
  codeine = c("137703ATB", "144901ATR", "267400ATB", "268000ATB", "313400ACH", "493200ATB", "532500ASY", "532600ASY", "532700ASY", "532800ASY", "677200ASY", "690300ASY"),
  pentazocine = c(),
  buprenorphine = c("120203CPC", "120204CPC", "120205CPC", "120234CPC", "120235CPC", "120236CPC"),
  nalbuphine = c("198930BIJ"),
  butorphanol = c("121130BIJ", "121131BIJ"),
  tapentadol = c("628401ATB", "628401ATR", "628402ATR", "628404ATR"),
  tramadol = c("242301ATR", "242302ATR", "242304ATR", "242305ACH", "242308ATR", "242330BIJ", "242331BIJ", "480600ATB", "513000ATB", "513000ATR", "514100ATR"),
  fentanyl = c("158209CPC", "158210CPC", "158211CPC", "158213CPC", "158305ATC", "158305ATL", "158306ATC", "158306ATL", "158307ATC", "158308ATC", "158313ATC",
               "158314ATC", "158314ATL", "158315ATC", "158316ATC", "158317ATC", "158321ATL", "158322ATL", "158330BIJ", "158331BIJ", "158332BIJ", "158333BIJ", "158334BIJ",
               "158337ATL", "158338ATL", "630901CSI", "630904CSI", "630907CSI", "630910CSI", "630911CSI"),
  morphine = c("197230BIJ", "197231BIJ", "197301ATR", "197302ATR", "197305ATB", "197330BIJ", "197331BIJ", "197332BIJ", "197334BIJ", "197335BIJ", "197336BIJ", "197339BIJ", "197340BIJ"),
  hydromorphone = c("441102ATB", "441130BIJ", "441131BIJ"),
  pethidine = c("211530BIJ", "211531BIJ")
)

# Cardiovascular outcome 
code.cardiovascular <- list(
  acute.myocardial.infarction = c(), #급성 심근경색
  angina.pectoris = c(), #협심증
  CHF = c() #심부전
  )

code.AF <- c("I48", "I480", "I481", "I482", "I483", "I484", "I489")


t20 <- read_fst("/home/minhyuk.kim/ShinyApps/R-skku-biohrs/study/data/m20.fst", as.data.table = T)

## Previous disease: Among all sick code

## 2002년 1월부터 2021년 12월까지 심장수술, 위절제술, 대장절제술, 전립선절제술을 받은 자
# code.surgery.named <- unlist(code.surgery)
# names(code.surgery.named) <- gsub("[0-9]", "", names(code.surgery.named))
# code.surgery.named2 <- names(code.surgery.named) 
# names(code.surgery.named2) <- code.surgery.named
# 
# code.surgery.named <- stack(code.surgery)
# code.surgery.named

code.surgery.named <- with(stack(code.surgery),
                            setNames(ind, values))


# t30.surgery <- t30[MCARE_DIV_CD_ADJ %in% unlist(code.surgery), .(CMN_KEY = as.character(CMN_KEY), MCARE_DIV_CD_ADJ, Type_surgery = code.surgery.named[MCARE_DIV_CD_ADJ])]
t30.surgery <- t30[MCARE_DIV_CD_ADJ %in% unlist(code.surgery), .(CMN_KEY, MCARE_DIV_CD_ADJ, Type_surgery = code.surgery.named[MCARE_DIV_CD_ADJ])]

# a.start <- merge(t30.surgery, t20[, .(CMN_KEY, INDI_DSCM_NO, Surgery_date = as.Date(MDCARE_STRT_DT, format = "%Y%m%d"), SICK_SYM1, SICK_SYM2)], by = "CMN_KEY")
a.start <- merge(t30.surgery, t20[, .(CMN_KEY = as.numeric(CMN_KEY), INDI_DSCM_NO, Surgery_date = as.Date(MDCARE_STRT_DT, format = "%Y%m%d"), surg.SICK_SYM1 = SICK_SYM1, surg.SICK_SYM2 = SICK_SYM1)], by = "CMN_KEY")

attr <- list("Total number of subjects:" = nrow(a.start))

#cat("Total number of subjects:", nrow(a.start))
#rm(t30.surgery)



count_by_code <- a.start[,.(MCARE_DIV_CD_ADJ),][, .N, by = MCARE_DIV_CD_ADJ][order(MCARE_DIV_CD_ADJ)]
cat("각 수술별 환자 수 ")
print(count_by_code)

attr$`각 수술별 환자 수` <- count_by_code

## 우울증 발생
#만약에 진단명으로 샘플수가 부족한 경우 수술 후 1년 이내 우울증 발생한 것을postoperative depression case의 정의로 변경하여 다시 case를 산출해 볼 예정.

# a.dep <- t20[SICK_SYM1 %like% code.DEP, .(INDI_DSCM_NO, Surgery_date = as.Date(MDCARE_STRT_DT, format = "%Y%m%d"), Indexdate = as.Date(MDCARE_STRT_DT, format = "%Y%m%d"))] %>% 
#   .[a.start, on = c("INDI_DSCM_NO", "Surgery_date"), roll = -365]

a.dep <- t20[SICK_SYM1 %like% code.DEP, .(INDI_DSCM_NO, Surgery_date = as.Date(MDCARE_STRT_DT, format = "%Y%m%d"), Indexdate = as.Date(MDCARE_STRT_DT, format = "%Y%m%d"), SICK_SYM1, SICK_SYM2)][
  a.start, on = c("INDI_DSCM_NO", "Surgery_date"), roll = -365] %>% # 일단은 30일, 샘플수 부족시에 365일로
  .[!is.na(Indexdate)] # a.start에서 우울증 있는 사람들만 추출


attr$`Exclusion1 :우울증 환자만` <- a.dep

a <- copy(a.dep)
#rm(a.dep)



## 정신질환자
# a[!(SICK_SYM1 %like% code.mental.disease | SICK_SYM2 %like% code.mental.disease), ] %>% dim 
# excl.mental <- t20[(SICK_SYM1 %like% code.mental.disease | SICK_SYM2 %like% code.mental.disease), .(INDI_DSCM_NO, Mental_date = as.Date(MDCARE_STRT_DT, format = "%Y%m%d"))]

excl.mental <- t20[(SICK_SYM1 %like% code.mental.disease | SICK_SYM2 %like% code.mental.disease), .(Mental_date = min(as.Date(MDCARE_STRT_DT, format = "%Y%m%d"))), by = INDI_DSCM_NO]
#excl.mental2 <- t20[(SICK_SYM1 %like% code.mental.disease | SICK_SYM2 %like% code.mental.disease), .(INDI_DSCM_NO, Mental_date = as.Date(MDCARE_STRT_DT, format = "%Y%m%d"))][order(Mental_date), .SD[1], .SDcols = c("Mental_date"), keyby = "INDI_DSCM_NO"]
#[MDCARE_STRT_DT < Indexdate][order(MDCARE_STRT_DT), .SD[1], .SDcols = c("MDCARE_STRT_DT"), keyby = "INDI_DSCM_NO"]

#mental.before.dep <- excl.mental[a, on = .(INDI_DSCM_NO, Mental_date < Indexdate), nomatch = 0L, .(INDI_DSCM_NO, Mental_date, Indexdate, Surgery_date, CMN_KEY)]


# 로직 다시 한 번 확인 필요
mental.before.dep <- a[excl.mental, on = .(INDI_DSCM_NO, Indexdate > Mental_date), nomatch = 0L,
                       .(INDI_DSCM_NO, Surgery_date, Mental_date, Indexdate, CMN_KEY, MCARE_DIV_CD_ADJ, Type_surgery, surg.SICK_SYM1, surg.SICK_SYM2)][Mental_date < Indexdate]
attr$`이전의 정신질환자 수` <- mental.before.dep
#rm(excl.mental)

# mental.before.dep2 <- excl.mental[a, on = .(INDI_DSCM_NO, Mental_date == Indexdate), nomatch = 0L, .(INDI_DSCM_NO, Mental_date, Indexdate, Surgery_date, CMN_KEY)]
# mental.before.dep[Mental_date != Indexdate] %>% dim
# check code
# a[(SICK_SYM1 %like% code.mental.disease | SICK_SYM2 %like% code.mental.disease)][MDCARE_STRT_DT < Indexdate] %>% dim
# a[(SICK_SYM1 %like% code.mental.disease | SICK_SYM2 %like% code.mental.disease)][MDCARE_STRT_DT >= Indexdate] %>% dim
# a[(SICK_SYM1 %like% code.mental.disease | SICK_SYM2 %like% code.mental.disease), .(SICK_SYM1, SICK_SYM2, MDCARE_STRT_DT, Indexdate)]


# 정신 질환자 exclusion applied


a <- a[!mental.before.dep, on = "INDI_DSCM_NO"][,  `:=` (INDI_DSCM_NO = as.integer(INDI_DSCM_NO))]
attr$`Exclusion2: 정신질환자 제외 후` <- a

a[, Surgery_date_minus5yr := Surgery_date - 365*5]

#rm(mental.before.dep)

a %>% head



# 과거력 5년 제외 (대수술 받기 전에 dementia, Parkinson’s disease (G20), stroke (I60-64), cerebral hemorrhage (S06) 과거력이 5년내 있었던 환자 제외)
past.history <- t20[SICK_SYM1 %in% excl.code.disease | SICK_SYM2 %in% excl.code.disease | SICK_SYM3 %in% excl.code.disease| SICK_SYM4 %in% excl.code.disease| SICK_SYM5 %in% excl.code.disease,
                    .(INDI_DSCM_NO = as.numeric(INDI_DSCM_NO), Disease_date = as.Date(MDCARE_STRT_DT, "%Y%m%d"))]
past.history %>% head 


## 확인 t40으로 만들기 
# past.history.40 <- t40[MCEX_SICK_SYM %in% excl.code.disease] %>% merge(t20[, .(CMN_KEY = as.numeric(CMN_KEY), INDI_DSCM_NO, Disease_date = as.Date(MDCARE_STRT_DT, "%Y%m%d"))], by = "CMN_KEY")
# past.history.40 %>% head
# past.history.40[is.na(MDCARE_STRT_DT)] %>% dim
# past.5yr.40 <- past.history.40[a, on = .(CMN_KEY, Disease_date < Surgery_date, Disease_date > Surgery_date_minus5yr), nomatch = 0L]
# past.5yr.40 %>% dim


past.5yr <- past.history[a, on = .(INDI_DSCM_NO, Disease_date < Surgery_date, Disease_date > Surgery_date_minus5yr), nomatch = 0L]
past.5yr %>% head


a <- a[!past.5yr, on = "INDI_DSCM_NO"]
attr$`Exclusion3: 과거력 5년 제외 후` <- a

a[, `:=` (Surgery_after_1y = Surgery_date + 365, Surgery_after_5y = Surgery_date+ 365*5)]


## 성별과 사망일 추가

setkey(bfc, INDI_DSCM_NO)
setkey(bnd, INDI_DSCM_NO)
t <- a %>% merge(bfc[, .(SEX_TYPE = SEX_TYPE[1], BYEAR = BYEAR[1]), by = "INDI_DSCM_NO"], by = "INDI_DSCM_NO", all.x = T) %>%
  .[, `:=` (Age = year(Indexdate)-as.integer(BYEAR))] %>% 
  merge(bnd[, .(INDI_DSCM_NO, DTH_ASSMD_DT = as.Date(DTH_ASSMD_DT, "%Y%m%d"))], by = "INDI_DSCM_NO", all.x=T)

rm(bfc)
rm(bnd)


# t <- a %>% merge(bfc[, .(SEX_TYPE = SEX_TYPE[1], BYEAR = BYEAR[1]), keyby = "INDI_DSCM_NO"], by = "INDI_DSCM_NO", all.x = T) %>%
#   .[, `:=` (Age = year(Indexdate)-as.integer(BYEAR))] %>% 
#   merge(bnd[, .(INDI_DSCM_NO, DTH_ASSMD_DT = as.Date(DTH_ASSMD_DT, "%Y%m%d"))], by = "INDI_DSCM_NO", all.x=T)


a.heart <- a[Type_surgery == "heart",]
a.other.major <- a[Type_surgery != "heart",]

attr$`Heart surgery` <- a.heart
attr$`Heart surgery N: ` <- nrow(a.heart)
attr$`Other major surgery` <- a.other.major
attr$`Other major surgery N: ` <- nrow( a.other.major)
### data preprocessing done ###



# 치매

#수술 후 1년 이후부터 5년 이내 supplementary table 2의 dementia진단 코드를 주 진단명 또는 부 진단명으로 가지고 있으며 
# Supplementary table 3의 dementia 약제 (Rivastigmine, Galantamine, Memantine, Donepezil)을 2회 이상 처방받은 환자
dimentia <- t20[SICK_SYM1 %like% code.dementia | SICK_SYM2 %like% code.dementia, .(CMN_KEY, INDI_DSCM_NO, dimen.SICK_SYM1 = SICK_SYM1, dimen.SICK_SYM2 = SICK_SYM2, Dimentia_Date = MDCARE_STRT_DT)]

code.dimentia.drug.named <- with(stack(code.dementia.drug),
                           setNames(ind, values))

t30.dimentia <- t30[MCARE_DIV_CD_ADJ %in% unlist(code.dementia.drug), .(CMN_KEY = as.character(CMN_KEY), MCARE_DIV_CD_ADJ, Type_drug = code.dimentia.drug.named[MCARE_DIV_CD_ADJ])]
t60.dimentia <- t60[MCARE_DIV_CD_ADJ %in% unlist(code.dementia.drug), .(CMN_KEY = as.character(CMN_KEY), MCARE_DIV_CD_ADJ, Type_drug = code.dimentia.drug.named[MCARE_DIV_CD_ADJ])]

t.combined.dimentia <- rbind(t30.dimentia, t60.dimentia)
t.combined.dimentia

excl.dimentia <- t.combined.dimentia[, N := .N, by = .(CMN_KEY, MCARE_DIV_CD_ADJ)][N>=2]
t.combined.dimentia


dimentia.filtered <- dimentia[!excl.dimentia, on = "CMN_KEY"]











t60[MCARE_DIV_CD_ADJ %in% unlist(code.opiode), MCARE_DIV_CD_ADJ] %>% unique 


#m20[SICK_SYM1 %like% "F" | SICK_SYM2 %like% "F" | SICK_SYM3 %like% "F"| SICK_SYM4 %like% "F"| SICK_SYM5 %like% "F"]
# write_fst(m20[SICK_SYM1 %like% "F" | SICK_SYM2 %like% "F" | SICK_SYM3 %like% "F"| SICK_SYM4 %like% "F"| SICK_SYM5 %like% "F"], file.path("study/data", "m20.fst"))



#m20[SICK_SYM1 %like% code.DEP | SICK_SYM2 %like% code.DEP, SICK_SYM3 %like% code.DEP | SICK_SYM4 %like% code.DEP, SICK_SYM5 %like% code.DEP] %>% dim
m20 <- m20[SICK_SYM1 %like% code.DEP | SICK_SYM2 %like% code.DEP | SICK_SYM3 %like% code.DEP | SICK_SYM4 %like% code.DEP | SICK_SYM5 %like% code.DEP, ] # drug prescription will be added later
# a$SICK_SYM1 %>% unique %>% sort
# a[SICK_SYM1 %like% code.DEP, SICK_SYM1] %>% unique %>% sort



#data <- read_excel("약제급여목록및급여상한금액표_(2025.11.1.)(21,685)_공개용(개정안)_수정 1부.xlsx") %>% as.data.table()




