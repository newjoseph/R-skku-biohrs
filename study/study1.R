library(haven);library(data.table);library(magrittr);library(fst);library(openxlsx)

setwd("/home/minhyuk.kim/ShinyApps/R-skku-biohrs/study")
setDTthreads(0)  ## 0: All


## SAS to fst/csv
# for (v in c("bnc", "bnd", "m20", "m30", "m40", "m60", "inst", "g1e_0208", "g1e_0915")){
#   read_sas(file.path("data", paste0("nsc2_", v, "_1000.sas7bdat"))) %>% 
#     write_fst(file.path("data", paste0("nsc2_", v, "_1000.fst")))
#     #fwrite(file.path("data", paste0("nsc2_", v, "_1000.csv")))
# }


# ## fst
# # inst <- read_fst("data/nsc2_inst_1000.fst", as.data.table = T)
# # bnc <- read_fst("data/nsc2_bnc_1000.fst", as.data.table = T)
# # bnd <- read_fst("data/nsc2_bnd_1000.fst", as.data.table = T)
# # t20 <- read_fst("data/nsc2_m20_1000.fst", as.data.table = T)
# # t30 <- read_fst("data/nsc2_m30_1000.fst", as.data.table = T)
# # t40 <- read_fst("data/nsc2_m40_1000.fst", as.data.table = T)
# # t60 <- read_fst("data/nsc2_m60_1000.fst", as.data.table = T)
# # g1e_0915 <- read_fst("data/nsc2_g1e_0915_1000.fst", as.data.table = T)
# 
# inst <- read_fst("/home/minhyuk.kim/knhis_data/INST.fst", as.data.table = T)
# bfc <- read_fst("/home/minhyuk.kim/knhis_data/BFC.fst", as.data.table = T)
# bnd <- read_fst("/home/minhyuk.kim/knhis_data/DTH.fst", as.data.table = T)
# # t20 <- read_fst("/home/minhyuk.kim/knhis_data/T20.fst", as.data.table = T)
# t30 <- read_fst("/home/minhyuk.kim/knhis_data/T30.fst", as.data.table = T)
# #t40 <- read_fst("/home/minhyuk.kim/knhis_data/T40.fst", as.data.table = T)
# t60 <- read_fst("/home/minhyuk.kim/knhis_data/T60.fst", as.data.table = T)
# # g1e_2002 <- read.csv("/home/minhyuk.kim/knhis_data/g1e_obj_2002.csv") %>% as.data.table()
# # g1e_2017 <- read.csv("/home/minhyuk.kim/knhis_data/g1e_obj_2017.csv") %>% as.data.table()
# g1e_2018 <- read.csv("/home/minhyuk.kim/knhis_data/g1e_rst_2018.csv") %>% as.data.table()
# 
# g1e_2017 <- read.csv("/home/minhyuk.kim/knhis_data/g1e_obj_2017.csv") %>% as.data.table()

t20 <- t20[substr(t20$MDCARE_STRT_DT,1,4) <= "2013"]
#write_fst(t20, file.path("data", "t20.fst"))
t20 <- read_fst("data/t20.fst", as.data.table = T)

# cmn_key <- t20[,CMN_KEY]
# 
# # write_fst(t20[,.(INDI_DSCM_NO, CMN_KEY)], file.path("data", "CMN_KEY.fst"))
# t20[CMN_KEY]
# 
# t30 <- unique(t30)
# # t30 <- t30[as.numeric(CMN_KEY) %in% as.numeric(cmn_key),]
# write_fst(t30, file.path("data", "t30.fst"))
# 
# t60 <- t60[CMN_KEY %in% CMN_KEY,]
# write_fst(t60, file.path("data", "t60.fst"))


# depression
code.DEP <- paste(c("F32.0", "F32.1", "F32.2", "F32.3", "F32.8", "F32.9", "F33.0", "F33.1", "F33.2", "F33.3", "F33.8", "F33.9"), collapse = "|")

## Supp 2 확인필요 교수님 정의 필요. 아래 항목은 무관
# code.dep.list <- list(
#   depression = paste0("F", 32:34)
#   # Attention = paste0(c("F90.0", "Z43", "Z48.0"), collapse = "|"),
#   # Memory = c("G20"),
#   # Visuoconstruction  = c("I60", "I61", "I62", "I63", "I64"),
#   # VerbalFfluency = c("S06"),
#   # Processing_Speed <- paste(paste0("F", setdiff(c(10:19,20:29,30:39,40:48,50:59,60:69,70:79,80:89,90:98,99),c(32:34))), collapse = "|"),
#   # Executive_Function = c("G20"),
#   # Fine_Motor_Speed  = c("I60", "I61", "I62", "I63", "I64")
# )

# 교수님 확인후 추가 필요 
code.dep.drug <- list(
  # 예시일 뿐 
  escitalopram = c( #'제품코드
    "651903320","643904070","653006400","642404280","657203640",
    "645305630","642706520","642505820","642004140","651602520",
    "694001740","653404070","645405120","657807270","657307740",
    "647204870","628901550","073001140","647804660","668000010",
    "671705600","643305000","642802330","645406340","651905310",
    "643904030","651902560","642402040","653004430","657203650",
    "643802930","640004880","649804370","642505810","694001360",
    "645304650","652606140","645405130","649505850","651602530",
    "642704470","653404060","642001670","693901750","657307730",
    "671702330","689000540","670701290","669805540","669501970",
    "661904390","657807280","649103080","649002010","647204860",
    "646002900","628901560","073001420","055800190","668000020",
    "647802910","642801900","643504370","647301590","648102570",
    "643305040","645406320","651905320","649508040","651903550",
    "643904060","653404650","642403290","645305060","657203630",
    "645405140","653005320","642705770","652606090","671703430",
    "642003450","694001370","657307720","669501940","628901570",
    "073001430","647804500","668000050","645406330","651905330",
    "651903850","643904050","653006860","642005310","657203220",
    "668000070"
  ),
  sertraline = c(
    "653006560","651902160","642404370","668100100","642005160",
    "657203270","073400310","643501290","649807070","651903750",
    "653006640","668101160","642404380","642005150","657203280",
    "073400320","653006630","657204130","642404390","642005140",
    "651905250"
  )
)

# exclude diseases
excl.code.disease <- list(
  dementia =c("F00", "F01", "F02", "F03", "F051", "G30", "G311"),
  Parkinson = c("G20"),
  stroke  = c("I60", "I60.0", "I60.1", "I60.2", "I60.3", "I60.4", "I60.5", "I60.6", "I60.7", "I60.8", "I60.9",
            "I61", "I61.0", "I61.1", "I61.2", "I61.3", "I61.4", "I61.5", "I61.6", "I61.8", "I61.9",
            "I62", "I62.0", "I62.1", "I62.9",
            "I63", "I63.0", "I63.1", "I63.2", "I63.3", "I63.4", "I63.5", "I63.6", "I63.8", "I63.9",
            "I64"),
  cerebral.hemorrhage = c("S06", "S06.0", "S06.1", "S06.2", "S06.3", "S06.4", "S06.5", "S06.6", "S06.7", "S06.8", "S06.9"),
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
    "O1640", "O1641", "O1647", "O1648", "O1649", # CABG On-pump
    "OA640", "OA641", "OA647", "OA648", "OA649", # CABG Off-pump
    "O2031", "O2032", "O2033", # Thoracic aorta surgery
    "O1671", "O1672", # PDA
    "O1701", "O1702", # Cardiovascular septum closure
    "O1703", "O1704", # Pulmonary artery ligation
    "O1705", "O1710", # ASD closure
    "O1711", # Minimal invasive ASD closure
    "O1721", # VSD closure
    "O1723", # Minimal invasive VSD closure
    "O1800", # Tetraology of Fallot
    "O1730", "O1782", "O1792", "O1795", # Mitral valve
    "O1740", "O1783", "O1793", "O1796", "O1799", # Aortic valve
    "O1750", "O1770", "O1810", "O1797", "O1798", # Pulmonary valve
    "O1760", "O1781", "O1791", "O1794" # Tricuspid valve
  ),
  stomach = c( 
    "Q0251","Q0252","Q0253","Q0254","Q0255","Q0256","Q0257","Q0258",
    "Q2533","Q2534","Q2536","Q2537","Q2594","Q2598" #upper GI
  ),
  colon = c(
    "Q1261","Q1262",
    "Q2671","Q2672","Q2673","Q2679",
    "Q2921","Q2922","Q2923","Q2924","Q2925","Q2926","Q2927",
    "QA671","QA672","QA673","QA679",
    "QA921","QA922","QA923","QA924","QA925","QA926" #lower GI
  ),
  
  ## Robotic prostatectomy 추가 필요 
  prostate = c("R3950","R3960","RZ512")
)

code.heart <- code.surgery$heart
code.other.major <- c(code.surgery$stomach, code.surgery$colon, code.surgery$prostate)


# dementia 리스트 확인
code.dementia <- paste0(c("F00", "F01", "F02", "F03", "F051", "G30", "G311"), collapse = "|")

# 치매 약물
code.dementia.drug.base <- list(
  rivastigmine = c("224501ACH", "224502ALQ", "224503ACH", "224504ACH", "224505ACH", "224506CPC", "224507CPC", "224508CPC"),
  galantamine = c("385201ATB", "385202ATB", "385203ACR", "385203ATR", "385204ACR", "385204ATR", "385205ACR", "385205ATR"),
  memantine  = c("190001ALQ", "190001ATB", "190001BIJ", "190002ASY", "190003ATD", "190004ATB", "190004ATD", "190005ATB", "190006ATD", "190030ASY", "190031ALQ", "190032ALQ"),
  donepezil = c("148601APD", "148601ATB", "148601ATD", "148602APD", "148602ATB", "148602ATD", "148603ATB", "148604ATB", "148630ALQ", "148631ALQ")
)

# 성분 코드별 상품(약제) 코드 리스트
code.dementia.drug.product <- list(
  `224501ACH` = c("651904210", "650303980", "694001710", "657807210", "652607050", "646004540", "643803220", "647303110", "653005870", "640903370", "662503930", "653600770"),
  `224503ACH` = c("651904220", "694001730", "657805970", "652607070", "650303970", "649103170", "646004550", "643803210", "647303050", "653005860", "653600780", "640903340", "662503530"),
  `224504ACH` = c("651904390", "653006280", "694002080", "652607040", "643803240", "647303150", "653600790"),
  `224505ACH` = c("651904370", "694002090", "652607060", "643803230", "653006290", "647303160", "653600800"),
  `224506CPC` = c("651904040", "657202580", "694001790", "653804850", "645404110", "642203270", "643802221", "644704140", "653601330", "649806350"),
  `224507CPC` = c("657202570", "694001780", "665506670", "651904030", "645404120", "642203260", "642003280", "643802210", "644704150", "653601340", "649806360", "653804860"),
  `224508CPC` = c("694001800", "651905201", "645406220", "643802790", "644704160", "653602450"),
  `385203ACR` = c("651903340", "642802340", "646902530"),
  `385203ATR` = c("642001120"),
  `385204ACR` = c("642802140", "651903230", "646902550"),
  `385204ATR` = c("642001920"),
  `385205ACR` = c("651903350", "642802350", "646902540"),
  `385205ATR` = c("642001930"),
  `190001ATB` = c("669605580", "653404660", "644301130", "642400130", "657500710", "684501580", "654300860", "657202740", "656702040", "670607840", "642700420", "651902310", "643802980", "663607660", "698505000", "698003070", "694001310", "693901500", "693201440", "690300930", "689001260", "678601630", "665500570", "660703260", "658502010", "658204740", "658107480", "657806070", "656004700", "655404460", "652604780", "650300340", "649507310", "649405290", "649102970", "649002310", "647204940", "646204060", "646004930", "645906690", "645405100", "645210250", "643309670", "642307010", "628902430", "625800740", "625202290", "057600540", "055800320", "642003420", "653001550", "642801020", "668000040", "642900500", "661900170", "648603220", "649805440", "647303910"),
  `190003ATD` = c("657203190", "651905010"),
  `190004ATB` = c("698004750", "656005660", "651904960", "642004160", "657204270", "653006960", "642803410"),
  `190004ATD` = c("657203180", "651905000"),
  `190005ATB` = c("643309810", "079700020", "642707740", "653405580", "698004740", "693903980", "693202590", "656005670", "654305590", "649508570", "628902460", "622805450", "055800340", "642803480", "642004950", "694004520", "670609200", "668000160", "658204760", "657204180", "653006970", "652607400", "651905350", "645210920"),
  `190006ATD` = c("657204170", "651905300"),
  `190031ALQ` = c("668000031"),
  `148601APD` = c("642004861"),
  `148601ATB` = c("622600450", "058200620", "694205860", "645208760", "669604810", "684501320", "651901550", "052401240", "649001540", "656702060", "055800160", "643803000", "642002750", "679601250", "644805600", "654005570", "644502490", "663300910", "653702270", "658203210", "658106340", "657201230", "648301280", "658501970", "657804470", "052701300", "653403620", "642800220", "645906850", "653805400", "649804280", "670607850", "640002940", "653004350", "625500530", "678600660", "662503360", "649501580", "645302290", "651602540", "642401720", "698003130", "693201260", "690301040", "689000710", "679801740", "671804130", "670501960", "670402270", "670103740", "669906610", "669805060", "669502560", "665507160", "665002470", "662600800", "661904160", "657503390", "654701650", "653103600", "652904640", "651503790", "651205500", "649701640", "648104430", "647204220", "646203910", "644603480", "644309440", "644004020", "642307170", "641502510", "629700530", "628900640", "628801180", "625800400", "623005590", "058800570", "057600470", "054800220", "053300500", "698500440", "660702410", "671701150", "644902420", "664900330", "060500200", "642902840", "647800230", "670300150", "694001100", "642100910", "642500860", "652105770", "648501020", "647303420", "645402660", "658600090", "643500330", "642700210", "646001340", "648603210", "674401270", "663607210", "657307250", "656001230", "651300810", "649404980", "649102530", "643300230", "643202800", "641802210", "625200560", "622802260", "621802980", "059000080", "053600750"),
  `148601ATD` = c("653005790", "651903480", "694001290", "649104460", "647804420", "644913040", "640005900", "652105790", "641904600", "643505540", "644701510", "621100130", "622600060"),
  `148602APD` = c("642004851"),
  `148602ATB` = c("622600460", "058200560", "669604700", "052401230", "684501300", "694205870", "656701960", "645208750", "649001550", "055800150", "644805590", "642002760", "658203190", "651902490", "679601240", "654005580", "653805410", "663300920", "653702280", "658106330", "643803010", "671701160", "664901930", "657201240", "644502500", "645303400", "052701310", "658501820", "657804460", "653403610", "649804270", "644603510", "662503350", "647800240", "651602550", "642800230", "645906840", "653000830", "660701430", "649601040", "646001350", "060500190", "644000170", "641802220", "625500540", "652904630", "670300140", "648504600", "642704270", "621801580", "665507170", "658600100", "656001240", "649404970", "670607860", "640004320", "054800210", "642100900", "690302240", "669805050", "698003140", "693201270", "689000700", "679801730", "670501950", "670402280", "670103750", "669906730", "669502570", "665002520", "662600810", "661904170", "657503620", "654701640", "653103590", "651503780", "649701650", "649501570", "648301270", "648104420", "647204230", "646203900", "644309450", "642307160", "641502530", "629700520", "628900650", "628801170", "625800390", "623005580", "058800560", "057600460", "053300910", "642903080", "645403430", "678601310", "694001110", "642505280", "642401950", "652105760", "647303430", "698503280", "644913790", "643500340", "648603200", "671800660", "674401300", "663607220", "657307260", "651300800", "649102570", "643304830", "643202810", "625200550", "622802340", "059000090", "053600740"),
  `148602ATD` = c("651903540", "640005410", "653006140", "694001280", "649104450", "647804430", "644913390", "622600070", "652105780", "641901550", "643505550", "621100120", "644702830"),
  `148603ATB` = c("642003510", "651904330", "694001570", "647804540", "657202810", "663300980", "660703550", "657807540", "646005330", "645306610", "622804570", "652105800", "645405360"),
  `148604ATB` = c("651905360", "642005050"),
  `148630ALQ` = c("694003841", "671707111", "644915091", "643803541"),
  `148631ALQ` = c("694003851", "671707121", "644915101", "643803551"),
  `643403CPC` = c("674402440", "623800140"),
  `643404CPC` = c("674402450", "623800130"),
  `738600ATB` = c("657204220", "642204820", "642405280", "642005320", "649106080", "643309630", "642906910", "642803530")
)

# 성분 코드 + 상품 코드를 모두 포함하도록 리스트 확장
code.dementia.drug <- lapply(
  code.dementia.drug.base,
  function(codes) unique(c(unlist(code.dementia.drug.product[codes], use.names = FALSE)))
)


code.depression.drug.named <- with(
  stack(code.dep.drug),
  setNames(ind, values)
)

# 진통 마취제
code.opiode.base <- list(
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



# 성분 코드별 상품(약제) 코드 리스트
code.opiode.product <- list(
  `359001ATR` = c("657805210", "678000110"),
  `359002ATB` = c("649506120", "657805890"),
  `359002ATR` = c("657805140", "678000090"),
  `359003ATB` = "649508050",
  `359003ATR` = c("657805150", "678000100"),
  `359004ATB` = c("649501370", "657801500"),
  `359007ATR` = "678000160",
  `359030BIJ` = c("657806061", "678000171"),
  `359031BIJ` = c("657806031", "678000201"),
  `517100ATR` = "678000220",
  `517200ATR` = "678000210",
  `564000ATR` = "678000180",
  `564100ATR` = "678000190",
  `667600ATR` = "678000480",
  `137703ATB` = c("649801470", "653100790", "657802830", "664900030", "669800390"),
  `144901ATR` = "657800350",
  `267400ATB` = "641900090",
  `268000ATB` = c(
    "642102300", "643303510", "645701150", "671802920"
  ),
  `313400ACH` = c("649802260", "657801830", "664900120", "669802710"),
  `493200ATB` = "645900990",
  `532500ASY` = c("642003611", "642102282", "642404021", "644102563", "645702661",
                  "648103481", "655403441", "670303712", "670500581", "671704212",
                  "671805622" ),
  `532600ASY` = c("642404023", "645702663", "648103482", "671805621"),
  `532700ASY` = "645701142",
  `532800ASY` = "645701141",
  `677200ASY` = c("642105881", "670304331"),
  `690300ASY` = "671807171",
  `120203CPC` = "678000020",
  `120204CPC` = "678000030",
  `120205CPC` = "678000010",
  `120234CPC` = "649807440",
  `120235CPC` = "649807420",
  `120236CPC` = "649807430",
  `198930BIJ` = c("649800051", "657802531"),
  `121130BIJ` = "649801911",
  `121131BIJ` = "649801921",
  `628401ATB` = "653103870",
  `628401ATR` = "653103840",
  `628402ATR` = "653103850",
  `628404ATR` = "653103860",
  `242301ATR` = "642102480",
  `242302ATR` = "678000140",
  `242304ATR` = "678000150",
  `242305ACH` = c("642102520", "654001730"),
  `242308ATR` = "678000230",
  `242330BIJ` = c(
    "625201731", "629701211", "640903941", "642102501", "642300341",
    "645102391", "645902851", "648100181", "648201961", "648502283",
    "649405001", "650500061", "654001721", "655602984", "657502161",
    "657804971", "665002081", "665500671", "669804851", "683100411",
    "698503291"
  ),
  `242331BIJ` = c(
    "642102511", "642300331", "645601831", "648603961", "657804981"
  ),
  `480600ATB` = c(
    "052400500", "052700520", "053300240", "054800230", "055800250",
    "057600560", "057700430", "059000960", "059600100", "060500290",
    "621803710", "622600240", "622802150", "625200770", "628800730",
    "628900230", "640004590", "640903990", "641500910", "641606960",
    "641801210", "641904550", "642001070", "642307790", "642505240",
    "642703040", "642801070", "642902720", "643102330", "643202380",
    "643303210", "643504230", "643702570", "644003090", "644306140",
    "645208780", "645301170", "645404160", "645604410", "645902810",
    "646002780", "646202460", "646902640", "647204090", "647303460",
    "647803860", "648201830", "648503730", "648602570", "649000140",
    "649104730", "649403050", "649504980", "649701800", "649804140",
    "650303170", "651502520", "651600790", "652105100", "652604190",
    "652902170", "653101970", "653401480", "653802130", "654000740",
    "654701660", "655404250", "655604520", "656000840", "656203590",
    "656702320", "657307170", "657802010", "658106370", "658603290",
    "662502870", "663602840", "664102830", "664900690", "665000210",
    "665508230", "669501740", "669603630", "669700200", "669804370",
    "669904800", "670103660", "670300980", "670401240", "670604310",
    "671702220", "674400710", "678601500", "684501030", "689001350",
    "690300920", "693201160", "693900270", "694000830", "696600200",
    "697100080", "698004080", "698500760"
  ),
  `513000ATB` = c(
    "052700180", "053300230", "053600230", "054800240", "057000740",
    "057600550", "059000950", "059600180", "060500320", "621802220",
    "622600230", "622801730", "623005320", "625200220", "628800140",
    "628900250", "629700710", "640004600", "640901370", "641500880",
    "641606950", "641801220", "641904560", "642001080", "642304450",
    "642401930", "642505230", "642703050", "642801080", "642902710",
    "643200540", "643303220", "643504220", "643702580", "644002040",
    "644102010", "644306010", "644603270", "644803360", "645203440",
    "645301180", "645404170", "645600990", "645902820", "646002790",
    "646202450", "646902330", "647201840", "647301350", "647802880",
    "648102530", "648201840", "648503740", "648602600", "649000150",
    "649101510", "649403040", "649504940", "649602830", "649702040",
    "649803220", "650301710", "651202540", "651501570", "651600800",
    "652105070", "652604280", "652902240", "653004470", "653101980",
    "653401490", "653802140", "654000750", "654303650", "654701110",
    "655604530", "656000850", "656203580", "656701470", "657303100",
    "657802020", "658106360", "658201830", "658601980", "660702150",
    "661902090", "662502010", "663602810", "664102820", "664900670",
    "665000220", "665505400", "669501730", "669603640", "669700190",
    "669803290", "670102290", "670300990", "670400890", "670604320",
    "671702200", "674400670", "678600770", "684500660", "689000630",
    "693200450", "693900300", "694000500", "696600210", "697100040",
    "698500770"
  ),
  `513000ATR` = c(
    "053300900", "057600610", "073000350", "621803590", "641607040",
    "642507270", "644102740", "645305470", "645404050", "645905900",
    "646902660", "647803810", "649001890", "649102860", "649806300",
    "650206150", "652105230", "652903810", "653404300", "655403810",
    "657203080", "658502210", "658604170", "661904710", "669502590",
    "669906380", "671704810", "671805560", "694002930"
  ),
  `514100ATR` = c(
    "057600600", "073000340", "621803580", "641607030", "642507230",
    "644102690", "645305140", "645403830", "645905800", "646902650",
    "647803730", "649102670", "649806190", "650206160", "652105220",
    "652903680", "653404290", "654004680", "657203070", "658502200",
    "658603280", "669502600", "669906400", "671704620", "671805340",
    "694002940"
  ),
  `158209CPC` = c(
    "645304800", "646900090", "649806240", "659901030"
  ),
  `158210CPC` = c(
    "645304790", "646900100", "649806250", "659901020"
  ),
  `158211CPC` = c(
    "645304830", "646900110", "649806230", "659901010"
  ),
  `158213CPC` = c(
    "645305180", "646901010", "659900990"
  ),
  `158305ATC` = "642000770",
  `158305ATL` = c("653103370", "674900490"),
  `158306ATC` = "642000780",
  `158306ATL` = "674900510",
  `158307ATC` = "642000790",
  `158308ATC` = "642000800",
  `158313ATC` = "626900400",
  `158314ATC` = "626900410",
  `158314ATL` = "659901100",
  `158315ATC` = "626900420",
  `158316ATC` = "626900430",
  `158317ATC` = "626900390",
  `158321ATL` = c("653103320", "674900480"),
  `158322ATL` = c("653103380", "674900500"),
  `158330BIJ` = c(
    "645303281", "649804431", "657803211"
  ),
  `158331BIJ` = c(
    "645103351", "645302781", "645302783", "649800801", "649805131",
    "653100391", "653102471", "657802271", "657802273", "669800033",
    "669804761"
  ),
  `158332BIJ` = c(
    "645102991", "645302791", "645302794", "645304081", "649803851",
    "649805091", "653100411", "653102381", "657802281", "657802283",
    "669800043", "669804771"
  ),
  `158333BIJ` = c(
    "645103001", "645302801", "645304091", "645304093", "649803861",
    "649805101", "653102391", "657802294", "669800051"
  ),
  `158334BIJ` = c("653103051", "657805291"),
  `158337ATL` = "659901120",
  `158338ATL` = "659901090",
  `630901CSI` = "641605801",
  `630904CSI` = "641605781",
  `630907CSI` = "641605791",
  `630910CSI` = "674900551",
  `630911CSI` = "674900561",
  `197230BIJ` = "650500441",
  `197231BIJ` = c("657801481", "650500451", "649801294"),
  `197301ATR` = "657801450",
  `197302ATR` = "657801460",
  `197305ATB` = c("664900410", "657802960"),
  `197330BIJ` = "653103641",
  `197331BIJ` = c("657805711", "653101951"),
  `197332BIJ` = c("657803031", "653103651", "649803891", "645302943"),
  `197334BIJ` = "653101941",
  `197335BIJ` = c("653101681", "649802161"),
  `197336BIJ` = c("657804871", "653102591"),
  `197339BIJ` = "649803881",
  `197340BIJ` = "653103101",
  `441102ATB` = "657800360",
  `441130BIJ` = "657800401",
  `441131BIJ` = "657800411",
  `211530BIJ` = c("657802741", "650500481"),
  `211531BIJ` = c("657802751", "653100771", "649801381", "650500491")
)


# 성분 코드 + 상품 코드를 모두 포함하도록 리스트 확장
code.opiode.drug <- lapply(
  code.opiode.base,
  function(codes) unique(unlist(code.opiode.product[codes], use.names = FALSE))
)

code.opiode.drug.named <- with(
  stack(code.opiode.drug),
  setNames(ind, values)
)


## 확인 필요 
# Cardiovascular outcome 
code.cardiovascular <- list(
  acute.myocardial.infarction = c("I21", "I22", "I23", "I24"),
  angina.pectoris = c("I20"),
  congestive.heart.failure = c("I09.9", "I11.0", "I13.2", "I25.5", "I42", "I42.0", "I42.5", "I42.6", "I42.7", "I42.8", "I42.9", "I43", "I43.0", "I43.1", "I43.2", "I43.8", "I43.9", "I50", "I50.0", "I50.1", "I50.9", "P29.0")
  )


code.AF <- c("I48", "I48.0", "I48.1", "I48.2", "I48.3", "I48.4", "I48.9")

code.hospitalized <- c("2", "4", "7", "10", "12")

code.chemotherapy <- c("Z08.2", "Z09.2", "Z29.2", "Z51.1", "Z51.2", "Z54.2")
code.radiotherapy <- c("Y84.2", "Z08.1", "Z09.1", "Z51.0", "Z54.1")

# chemotherapy
# NO	분류코드	분류항목명
# 1	Z08.2	Follow-up examination after chemotherapy for malignant neoplasm
# 2	Z09.2	Follow-up examination after chemotherapy for other conditions
# 3	Z29.2	Other prophylactic chemotherapy
# 4	Z51.1	Chemotherapy session for neoplasm
# 5	Z51.2	Other chemotherapy
# 6	Z54.2	Convalescence following chemotherapy
# radiotherapy
# NO	분류코드	분류항목명
# 1	Y84.2	Radiological procedure and radiotherapy
# 2	Z08.1	Follow-up examination after radiotherapy for malignant neoplasm
# 3	Z09.1	Follow-up examination after radiotherapy for other conditions
# 4	Z51.0	Radiotherapy session
# 5	Z54.1	Convalescence following radiotherapy



# t20 <- read_fst("/home/minhyuk.kim/ShinyApps/R-skku-biohrs/study/data/m20.fst", as.data.table = T)

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

# --- Robotic Prostatectomy Logic Start ---
# Robotic prostatectomy definition: QZ961 + L1211 (General Anesthesia) + Pathology Code + NO Standard Prostatectomy Codes
code.robot <- "QZ961"
code.anesthesia <- "L1211"
code.pathology <- c("C5500", "C5501", "C5502", "C5503", "C5504", "C5505", "C5506", "C5507", "C5508", "C5509", 
                    "C5911", "C5912", "C5913", "C5914", "C5915", "C5916", "C5917", "C5918", "C5919")
code.prostate.std <- code.surgery$prostate

# Filter for relevant codes first for performance
t30.subset <- t30[MCARE_DIV_CD_ADJ %in% c(code.robot, code.anesthesia, code.pathology, code.prostate.std)]
#write_fst(t30.subset, file.path("data", "t30_subset.fst"))


# Check conditions by CMN_KEY
robotic.candidates <- t30.subset[, .(
  has_robot = any(MCARE_DIV_CD_ADJ == code.robot),
  has_anesthesia = any(MCARE_DIV_CD_ADJ == code.anesthesia),
  has_pathology = any(MCARE_DIV_CD_ADJ %in% code.pathology),
  has_std_prostate = any(MCARE_DIV_CD_ADJ %in% code.prostate.std)
), by = CMN_KEY]

write_fst(robotic.candidates, file.path("data", "robotic.candidates.fst"))

# Condition: Robot AND Anesthesia AND Pathology AND NOT Standard Prostatectomy
robotic.keys <- robotic.candidates[has_robot & has_anesthesia & has_pathology & !has_std_prostate, CMN_KEY]

# Extract robotic surgery cases
t30.robotic <- t30[CMN_KEY %in% robotic.keys & MCARE_DIV_CD_ADJ == code.robot, 
                   .(CMN_KEY, MCARE_DIV_CD_ADJ, Type_surgery = "prostate")]
write_fst(t30.robotic, file.path("data", "t30_robotic.fst"))

# --- Robotic Prostatectomy Logic End ---

# t30.surgery <- t30[MCARE_DIV_CD_ADJ %in% unlist(code.surgery), .(CMN_KEY = as.character(CMN_KEY), MCARE_DIV_CD_ADJ, Type_surgery = code.surgery.named[MCARE_DIV_CD_ADJ])]
t30.surgery.std <- t30[MCARE_DIV_CD_ADJ %in% unlist(code.surgery), .(CMN_KEY, MCARE_DIV_CD_ADJ, Type_surgery = code.surgery.named[MCARE_DIV_CD_ADJ])]
write_fst(t30.surgery.std, file.path("data", "t30_surgery_std.fst"))


# Merge standard surgery and robotic surgery
t30.surgery <- rbind(t30.surgery.std, t30.robotic)

#write_fst(t30.surgery, file.path("data", "t30_surgery.fst"))
t30.surgery <- read_fst("data/t30_surgery.fst", as.data.table = T)

# a.start <- merge(t30.surgery, t20[, .(CMN_KEY, INDI_DSCM_NO, Surgery_date = as.Date(MDCARE_STRT_DT, format = "%Y%m%d"), SICK_SYM1, SICK_SYM2)], by = "CMN_KEY")
a.start <- merge(t30.surgery, t20[, .(CMN_KEY = as.numeric(CMN_KEY), INDI_DSCM_NO, Surgery_date = as.Date(MDCARE_STRT_DT, format = "%Y%m%d"),
                                      surg.SICK_SYM1 = SICK_SYM1, surg.SICK_SYM2 = SICK_SYM2, VSHSP_DD_CNT, MDCARE_DD_CNT)], by = "CMN_KEY")

#write_fst(a.start, file.path("data", "a_start.fst"))
a.start <- read_fst("data/a_start.fst", as.data.table = T)

attr <- list("Total number of subjects:" = nrow(a.start))




#cat("Total number of subjects:", nrow(a.start))
#rm(t30.surgery)



count_by_code <- a.start[,.(MCARE_DIV_CD_ADJ),][, .N, by = MCARE_DIV_CD_ADJ][order(MCARE_DIV_CD_ADJ)]
cat("각 수술별 환자 수 ")
print(count_by_code)

attr$`각 수술별 환자 수` <- count_by_code

## 우울증 발생
#만약에 진단명으로 샘플수가 부족한 경우 수술 후 1년 이내 우울증 발생한 것을postoperative depression case의 정의로 변경하여 다시 case를 산출해 볼 예정.

# a.dep <- t20[SICK_SYM1 %like% code.DEP, .(INDI_DSCM_NO, CMN_KEY, Surgery_date = as.Date(MDCARE_STRT_DT, format = "%Y%m%d"), Indexdate = as.Date(MDCARE_STRT_DT, format = "%Y%m%d"))] %>% 
#   .[a.start, on = c("INDI_DSCM_NO", "Surgery_date"), roll = -365]


# t30.dep <- t30[MCARE_DIV_CD_ADJ %in% unlist(code.dep.drug), .(CMN_KEY = as.character(CMN_KEY), Drug_date = MDCARE_STRT_DT, MCARE_DIV_CD_ADJ, Type_drug = code.depression.drug.named[MCARE_DIV_CD_ADJ])]
# t60.dep <- t60[MCARE_DIV_CD_ADJ %in% unlist(code.dep.drug), .(CMN_KEY = as.character(CMN_KEY), Drug_date = MDCARE_STRT_DT, MCARE_DIV_CD_ADJ, Type_drug = code.depression.drug.named[MCARE_DIV_CD_ADJ])]
t30.dep <- t30[MCARE_DIV_CD_ADJ %in% unlist(code.dep.drug), .(CMN_KEY = as.character(CMN_KEY), MCARE_DIV_CD_ADJ, Type_drug = code.depression.drug.named[MCARE_DIV_CD_ADJ])]
t60.dep <- t60[MCARE_DIV_CD_ADJ %in% unlist(code.dep.drug), .(CMN_KEY = as.character(CMN_KEY), MCARE_DIV_CD_ADJ, Type_drug = code.depression.drug.named[MCARE_DIV_CD_ADJ])]


t.combined.dep <- rbind(t30.dep, t60.dep)
t.combined.dep
#write_fst(t.combined.dep, file.path("data", "t_combined_dep.fst"))

t.combined.dep <- read_fst("data/t_combined_dep.fst", as.data.table = T)
t.combined.dep[, CMN_KEY := as.numeric(CMN_KEY)]
setkey(t.combined.dep, CMN_KEY)

    
# a.dep <- t20[SICK_SYM1 %like% code.DEP, .(INDI_DSCM_NO, Surgery_date = as.Date(MDCARE_STRT_DT, format = "%Y%m%d"), Indexdate = as.Date(MDCARE_STRT_DT, format = "%Y%m%d"), SICK_SYM1, SICK_SYM2)][
#   a.start, on = c("INDI_DSCM_NO", "Surgery_date"), roll = -365] %>% # 일단은 365일로 (실제 데이터에서는 30일로 해보고 부족하면 다시 365일로)
#   # .[!is.na(Indexdate)] # a.start에서 우울증 있는 사람들만 추출


a.dep.diag <- t20[SICK_SYM1 %like% code.DEP | SICK_SYM2 %like% code.DEP, .(INDI_DSCM_NO, CMN_KEY_dep = CMN_KEY, Surgery_date = as.Date(MDCARE_STRT_DT, format = "%Y%m%d"), Indexdate = as.Date(MDCARE_STRT_DT, format = "%Y%m%d"), SICK_SYM1, SICK_SYM2)][
  a.start, on = c("INDI_DSCM_NO", "Surgery_date"), roll = -365] # 일단은 365일로 (실제 데이터에서는 30일로 해보고 부족하면 다시 365일로)

#write_fst(a.dep.diag, file.path("data", "a_dep_diag.fst"))
a.dep.diag <- read_fst("data/a_dep_diag.fst", as.data.table = T)  

# dep.id <- a.dep.diag$CMN_KEY %>% unique %>% sort
# drug.dep.id <- t.combined.dep$CMN_KEY %>% unique %>% sort
# dep.id[dep.id %in% drug.dep.id]


## 이전 정의 
####################################################################


# #a.dep.drug <- t20[SICK_SYM1 %like% code.DEP | SICK_SYM2 %like% code.DEP, .(INDI_DSCM_NO, CMN_KEY, Drug_date = as.Date(MDCARE_STRT_DT, format = "%Y%m%d"), Indexdate = as.Date(MDCARE_STRT_DT, format = "%Y%m%d"), SICK_SYM1, SICK_SYM2)]
# setkey(t.combined.dep, CMN_KEY)
# 
# t20_sub <- t20[, .(INDI_DSCM_NO, CMN_KEY = as.numeric(CMN_KEY), Drug_date = as.Date(MDCARE_STRT_DT, format = "%Y%m%d"), drug.SICK_SYM1 = SICK_SYM1, drug.SICK_SYM2 = SICK_SYM1)]
# rm(t20)
# setkey(t20_sub, CMN_KEY)
# 
# 
# 
# a.dep.drug <- merge(t.combined.dep, t20_sub, by = "CMN_KEY")
# # write_fst(a.dep.drug, file.path("data", "a_dep_drug.fst"))
# a.dep.drug <- read_fst("data/a_dep_drug.fst", as.data.table = T)
# a.dep.drug[, `:=` (CMN_KEY_drug = CMN_KEY, drug_MCARE_DIV_CD_ADJ = MCARE_DIV_CD_ADJ, CMN_KEY = NULL, MCARE_DIV_CD_ADJ=NULL)]
# 
# a.dep.drug[, drug_cutoff := Drug_date - 365]        # Drug_date - 365일
# setkey(a.dep.diag, INDI_DSCM_NO, Surgery_date)
# setkey(a.dep.drug, INDI_DSCM_NO, drug_cutoff)
# 
# 
# a.dep.final <- a.dep.drug[
#   a.dep.diag,
#   on = .(
#     INDI_DSCM_NO,
#     drug_cutoff >= Surgery_date   # ≡ Surgery_date + 365 <= Drug_date
#   ),
#   nomatch = NA,                   # 기본값; 진단만 있는 행도 유지
#   allow.cartesian = TRUE
# ]
# # write_fst(a.dep.final, file.path("data", "a_dep_final.fst"))
# a.dep.final <- read_fst("data/a_dep_final.fst", as.data.table = T)

####################################################################



a <- copy(a.dep.diag)
a$Depression <- 0
a[!is.na(Indexdate), Depression := 1]
a <- unique(a)


# .[!is.na(Indexdate)] # a.start에서 우울증 있는 사람들만 추출
#write_fst(a.dep, file.path("data", "a_dep.fst"))
# a.dep <- read_fst("data/a_dep.fst", as.data.table = T)






## 정신질환자
# a[!(SICK_SYM1 %like% code.mental.disease | SICK_SYM2 %like% code.mental.disease), ] %>% dim 
# excl.mental <- t20[(SICK_SYM1 %like% code.mental.disease | SICK_SYM2 %like% code.mental.disease), .(INDI_DSCM_NO, Mental_date = as.Date(MDCARE_STRT_DT, format = "%Y%m%d"))]

excl.mental <- t20[(SICK_SYM1 %like% code.mental.disease | SICK_SYM2 %like% code.mental.disease), .(Mental_date = min(as.Date(MDCARE_STRT_DT, format = "%Y%m%d"))), by = INDI_DSCM_NO]
#excl.mental2 <- t20[(SICK_SYM1 %like% code.mental.disease | SICK_SYM2 %like% code.mental.disease), .(INDI_DSCM_NO, Mental_date = as.Date(MDCARE_STRT_DT, format = "%Y%m%d"))][order(Mental_date), .SD[1], .SDcols = c("Mental_date"), keyby = "INDI_DSCM_NO"]
#[MDCARE_STRT_DT < Indexdate][order(MDCARE_STRT_DT), .SD[1], .SDcols = c("MDCARE_STRT_DT"), keyby = "INDI_DSCM_NO"]

#write_fst(excl.mental, file.path("data", "excl_mental.fst"))
excl.mental <- read_fst("data/excl_mental.fst", as.data.table = T)

#mental.before.dep <- excl.mental[a, on = .(INDI_DSCM_NO, Mental_date < Indexdate), nomatch = 0L, .(INDI_DSCM_NO, Mental_date, Indexdate, Surgery_date, CMN_KEY)]


# 로직 다시 한 번 확인 필요
mental.before.dep <- a[excl.mental, on = .(INDI_DSCM_NO), nomatch = 0L,
                       .(INDI_DSCM_NO, Surgery_date, Mental_date, Indexdate, CMN_KEY, MCARE_DIV_CD_ADJ, Type_surgery, surg.SICK_SYM1, surg.SICK_SYM2)][Mental_date < Indexdate]
attr$`이전의 정신질환자 수` = nrow(mental.before.dep)



#write_fst(mental.before.dep, file.path("data", "mental_before_dep.fst"))
mental.before.dep <- read_fst("data/mental_before_dep.fst", as.data.table = T)
#rm(excl.mental)



# 정신 질환자 exclusion applied

a <- a[!mental.before.dep, on = "INDI_DSCM_NO"][,  `:=` (INDI_DSCM_NO = as.integer(INDI_DSCM_NO))]
attr$`Exclusion2: 정신질환자 제외 후` <- nrow(a)

a[, Surgery_date_minus5yr := Surgery_date - 365*5]
#rm(mental.before.dep)


a %>% head

# past.history <- t20_sub[drug.SICK_SYM1 %in% excl.code.disease | drug.SICK_SYM2 %in% excl.code.disease, .(INDI_DSCM_NO, Disease_date=Drug_date)]

# 과거력 5년 제외 (대수술 받기 전에 dementia, Parkinson’s disease (G20), stroke (I60-64), cerebral hemorrhage (S06) 과거력이 5년내 있었던 환자 제외)
past.history <- t20[SICK_SYM1 %in% excl.code.disease | SICK_SYM2 %in% excl.code.disease | SICK_SYM3 %in% excl.code.disease| SICK_SYM4 %in% excl.code.disease| SICK_SYM5 %in% excl.code.disease,
                    .(INDI_DSCM_NO = as.numeric(INDI_DSCM_NO), Disease_date = as.Date(MDCARE_STRT_DT, "%Y%m%d"))]
# write_fst(past.history, file.path("data", "past_history.fst"))
past.history <- read_fst("data/past_history.fst", as.data.table = T)
past.history[, INDI_DSCM_NO := as.numeric(INDI_DSCM_NO)]
past.history %>% head 


## 확인 t40으로 만들기 
# past.history.40 <- t40[MCEX_SICK_SYM %in% excl.code.disease] %>% merge(t20[, .(CMN_KEY = as.numeric(CMN_KEY), INDI_DSCM_NO, Disease_date = as.Date(MDCARE_STRT_DT, "%Y%m%d"))], by = "CMN_KEY")
# past.history.40 %>% head
# past.history.40[is.na(MDCARE_STRT_DT)] %>% dim
# past.5yr.40 <- past.history.40[a, on = .(CMN_KEY, Disease_date < Surgery_date, Disease_date > Surgery_date_minus5yr), nomatch = 0L]
# past.5yr.40 %>% dim


past.5yr <- past.history[a, on = .(INDI_DSCM_NO, Disease_date < Surgery_date, Disease_date > Surgery_date_minus5yr), nomatch = 0L]
# write_fst(past.5yr, file.path("data", "past_5yr_history.fst"))
past.5yr <- read_fst("data/past_history.fst", as.data.table = T)
past.5yr[, INDI_DSCM_NO:= as.numeric(INDI_DSCM_NO)]
past.5yr %>% head


a <- a[!past.5yr, on = "INDI_DSCM_NO"]
attr$`Exclusion3: 과거력 5년 제외 후` <- nrow(a)

a[, `:=` (Surgery_after_1y = Surgery_date + 365, Surgery_after_5y = Surgery_date+ 365*5)]

# write_fst(a, file.path("data", "a_final_before_bfc_bnd.fst"))
a <- read_fst("data/a_final_before_bfc_bnd.fst", as.data.table = T)

## 성별과 사망일 추가

setkey(bfc, INDI_DSCM_NO)
setkey(bnd, INDI_DSCM_NO)


## 여기에서 Age는 일단 수술일로 한다. (기간이 1년이라면 다른 방식이 필요함)
## Age = year(Indexdate)-as.integer(BYEAR) 이게 정석인데 불가능
## Age = year(Surgery_date)-as.integer(BYEAR) 임시 방편 
t <- a %>% merge(bfc[, .(SEX_TYPE = SEX_TYPE[1], BYEAR = BYEAR[1]), by = "INDI_DSCM_NO"], by = "INDI_DSCM_NO", all.x = T) %>%
  .[, `:=` (Age = year(Surgery_date)-as.integer(BYEAR))] %>%  
  merge(bnd[, .(INDI_DSCM_NO, DTH_ASSMD_DT = as.Date(DTH_ASSMD_DT, "%Y%m%d"))], by = "INDI_DSCM_NO", all.x=T)

rm(bfc)
rm(bnd)











## 여기에 merged data


#write_fst(t, file.path("data", "merged_surg_indexdate.fst"))
t <- read_fst("data/merged_surg_indexdate.fst", as.data.table = T)
# t <- read_fst("data/merged.fst", as.data.table = T)
# t <- t[, `:=` (Surgery_after_1y = Surgery_date + 365, Surgery_after_5y = Surgery_date+ 365*5)]
t <- t[Age>=18, ]
attr$`Inclusion 1: Age >=18 :` <-  nrow(t)
#a <- read_fst("data/merged.fst", as.data.table = T)


######################################################################
a <- read_fst("data/merged_surg_indexdate.fst", as.data.table = T)
a <- a[Age>=18, ]
attr$`Inclusion 1: Age >=18 :` <-  nrow(a)
attr$`Heart surgery N: ` <- nrow(a[Type_surgery == "heart",])
attr$`Other surgery (not heart) N: ` <- nrow(a[Type_surgery != "heart",])
attr$`Colon surgery N: ` <- nrow(a[Type_surgery == "colon",])
attr$`prostate surgery N: ` <- nrow(a[Type_surgery == "prostate",])
attr$`stomach surgery N: ` <- nrow(a[Type_surgery == "stomach",])

# a.heart <- a[Type_surgery == "heart",]
# a.other.major <- a[Type_surgery != "heart",]
# attr$`Heart surgery` <- a.heart
# attr$`Other major surgery` <- a.other.major
# attr$`Other major surgery N: ` <- nrow( a.other.major)

### data preprocessing done ###



# 치매

#수술 후 1년 이후부터 5년 이내 supplementary table 2의 dementia진단 코드를 주 진단명 또는 부 진단명으로 가지고 있으며 
# Supplementary table 3의 dementia 약제 (Rivastigmine, Galantamine, Memantine, Donepezil)을 2회 이상 처방받은 환자
dimentia <- t20[
  SICK_SYM1 %like% code.dementia | SICK_SYM2 %like% code.dementia,
  .(
    CMN_KEY = as.character(CMN_KEY),
    INDI_DSCM_NO = as.integer(INDI_DSCM_NO),
    dimen.SICK_SYM1 = SICK_SYM1,
    dimen.SICK_SYM2 = SICK_SYM2,
    Dimentia_Date = as.Date(MDCARE_STRT_DT, "%Y%m%d")
  )
]

# write_fst(dimentia, file.path("data", "t20_dimentia.fst"))
dimentia <- read_fst("data/t20_dimentia.fst", as.data.table = T)


code.dimentia.drug.named <- with(
  stack(code.dementia.drug),
  setNames(ind, values)
)


t30.dimentia <- t30[MCARE_DIV_CD_ADJ %in% unlist(code.dementia.drug), .(CMN_KEY = as.character(CMN_KEY), MCARE_DIV_CD_ADJ, Type_drug = code.dimentia.drug.named[MCARE_DIV_CD_ADJ])]
# write_fst(t30.dimentia, file.path("data", "t30_dimentia.fst"))
t30.dimentia<- read_fst("data/t30_dimentia.fst", as.data.table = T)


t60.dimentia <- t60[MCARE_DIV_CD_ADJ %in% unlist(code.dementia.drug), .(CMN_KEY = as.character(CMN_KEY), MCARE_DIV_CD_ADJ, Type_drug = code.dimentia.drug.named[MCARE_DIV_CD_ADJ])]
#write_fst(t60.dimentia, file.path("data", "t60_dimentia.fst"))
t30.dimentia<- read_fst("data/t60_dimentia.fst", as.data.table = T)


t.combined.dimentia <- rbind(t30.dimentia, t60.dimentia)
t.combined.dimentia
#write_fst(t.combined.dimentia, file.path("data", "t30_60_dimentia.fst"))


#write_fst(dimentia, file.path("data", "t20_dimentia.fst"))
dimentia <- read_fst("data/t20_dimentia.fst", as.data.table = T)[
  ,
  `:=`(
    CMN_KEY = as.character(CMN_KEY),
    INDI_DSCM_NO = as.integer(INDI_DSCM_NO),
    Dimentia_Date = as.Date(Dimentia_Date, "%Y%m%d")
  )
]


#write_fst(t.combined.dimentia, file.path("data", "t30_60_dimentia.fst"))
t.combined.dimentia <- read_fst("data/t30_60_dimentia.fst", as.data.table = T)


# t.combined.dimentia[, `:=`(
#   CMN_KEY = as.character(CMN_KEY),
#   MCARE_DIV_CD_ADJ = as.character(MCARE_DIV_CD_ADJ),
#   Type_drug = as.character(Type_drug)
# )]


dementia.med <- t.combined.dimentia[
  dimentia[
    ,
    .(
      CMN_KEY = as.character(CMN_KEY),
      INDI_DSCM_NO = as.integer(INDI_DSCM_NO),
      Drug_Date = as.Date(Dimentia_Date, "%Y%m%d")
    )
  ],
  on = "CMN_KEY",
  nomatch = 0L
] %>%   # 우울증 진단 받은 사람들과 우울증약 처방받은 사람 합치기.
  .[
  a[, .(INDI_DSCM_NO, Surgery_after_1y, Surgery_after_5y)],
  on = .(INDI_DSCM_NO),
  nomatch = 0L] %>% # 기존에 대수술 받은 사람들이랑 합치기 
  .[Drug_Date >= Surgery_after_1y & Drug_Date <= Surgery_after_5y] # 수술후에 1년 이후로 필터링. (& Drug_Date <= Surgery_after_5y  5년 사이 필터링 

#write_fst(dementia.med, file.path("data", "dimentia_final.fst"))
dementia.med<- read_fst("data/dimentia_final.fst", as.data.table = T)


dementia.med.unique <- unique(dementia.med, by = c("INDI_DSCM_NO", "CMN_KEY", "Drug_Date"))
dementia.med.unique[, N := .N, by = INDI_DSCM_NO][N >= 2]

dementia.med.count <- dementia.med.unique[, .N, by = INDI_DSCM_NO][N >= 2]


####

# tt1 <- dimentia[
#   a[, .(INDI_DSCM_NO, Surgery_date, Surgery_after_1y, Surgery_after_5y)],
#   on = .(INDI_DSCM_NO),
#   nomatch = 0L
# ]  %>% unique()
# 
# tt2 <- tt1[]
# 
# 
# tt2 <- dimentia[
#   a[, .(INDI_DSCM_NO, Surgery_date, Surgery_after_1y, Surgery_after_5y)],
#   on = .(INDI_DSCM_NO),
#   nomatch = 0L
# ][Dimentia_Date < Surgery_date] %>% unique()



dimentia.before.surg <- dimentia[
  a[, .(INDI_DSCM_NO, Surgery_date, Surgery_after_1y, Surgery_after_5y)],
  on = .(INDI_DSCM_NO),
  nomatch = 0L
][Dimentia_Date < Surgery_date] %>% unique()


###
# Q. 수술전에 우울증이 있던 환자를 모두 제외하고, 수술후 1~5년 안에 새롭게 우울증이 발병한 환자만 선택

####

dementia.dx <- dimentia[
  a[, .(INDI_DSCM_NO, Surgery_date, Surgery_after_1y, Surgery_after_5y)],
  on = .(INDI_DSCM_NO),
  nomatch = 0L
][Dimentia_Date >= Surgery_date][!INDI_DSCM_NO %in% dimentia.before.surg$INDI_DSCM_NO] %>% unique

# 
# dementia.dx <- dimentia[
#   a[, .(INDI_DSCM_NO, Surgery_date, Surgery_after_1y, Surgery_after_5y)],
#   on = .(INDI_DSCM_NO),
#   nomatch = 0L
# ][Dimentia_Date >= Surgery_after_1y] # & Dimentia_Date <= Surgery_after_5y
####


# a$Dimentia = 0
# a <- dimentia[a, on = .(INDI_DSCM_NO), nomatch = 0L ][Dimentia_Date >= Surgery_after_1y & Dimentia_Date <= Surgery_after_5y, Dimentia := 1]


dementia.cohort <- dementia.dx[dementia.med.count, on = "INDI_DSCM_NO", nomatch = 0L]

#write_fst(dementia.cohort, file.path("data", "dimentia_cohort.fst"))
dementia.cohort<- read_fst("data/dimentia_cohort.fst", as.data.table = T)




# Exclude Alzheimer disease diagnoses occurring before or within 1 year after depression diagnosis.
code.alzheimer <- paste0(c("F00", "G30"), collapse = "|")
alzheimer <- t20[
  SICK_SYM1 %like% code.alzheimer | SICK_SYM2 %like% code.alzheimer,
  .(
    INDI_DSCM_NO = as.integer(INDI_DSCM_NO),
    CMN_KEY,
    AD_Date = as.Date(MDCARE_STRT_DT, "%Y%m%d")
    )
][!is.na(AD_Date)]

#write_fst(alzheimer, file.path("data", "alzheimer.fst"))
alzheimer <- read_fst("data/alzheimer.fst", as.data.table = T)

alzheimer[
  a[, .(INDI_DSCM_NO, Indexdate)],
  on = .(INDI_DSCM_NO),
  nomatch = 0L
] 


alzheimer_before_dep <- alzheimer[
  a[, .(INDI_DSCM_NO, Indexdate)],
  on = .(INDI_DSCM_NO),
  nomatch = 0L
][AD_Date < Indexdate, .(INDI_DSCM_NO)] %>% unique

alzheimer_within_1y <- alzheimer[
  a[, .(INDI_DSCM_NO, Indexdate)],
  on = .(INDI_DSCM_NO),
  nomatch = 0L
][AD_Date >= Indexdate & AD_Date <= Indexdate + 365, .(INDI_DSCM_NO)]

alzheimer_exclude <- unique(rbindlist(list(alzheimer_before_dep, alzheimer_within_1y)))

dementia.cohort.final <- dementia.cohort[!alzheimer_exclude, on = "INDI_DSCM_NO"]

#write_fst(dementia.cohort.final, file.path("data", "dimentia_cohort_final.fst"))
dementia.cohort.final<- read_fst("data/dimentia_cohort_final.fst", as.data.table = T)

#attr$`Dementia cohort` <- dementia.cohort.final
attr$`Dementia cohort N` <- nrow(dementia.cohort.final)


## Cardiovascular

cardiovascular <- t20[
  SICK_SYM1 %in% unlist(code.cardiovascular) | SICK_SYM2 %in% unlist(code.cardiovascular),
  .(
    INDI_DSCM_NO = as.integer(INDI_DSCM_NO),
    CMN_KEY,
    Cardio_Date = as.Date(MDCARE_STRT_DT, "%Y%m%d")
  )
][!is.na(Cardio_Date)]

#write_fst(cardiovascular, file.path("data", "cardiovascular.fst"))
cardiovascular <- read_fst("data/cardiovascular.fst", as.data.table = T)

# Exclude patients diagnosed before surgery or within 1 year after surgery
exclude_patients_cardio <- cardiovascular[
  a[, .(INDI_DSCM_NO, Indexdate)], 
  on = "INDI_DSCM_NO", 
  nomatch = 0L 
][
  Cardio_Date <= Indexdate + 365, 
  .(INDI_DSCM_NO)
] %>% unique()

# Define cohort: Diagnoses between 1 year and 6 years post-surgery (5-year observation)
cardiovascular.cohort <- cardiovascular[
  !exclude_patients_cardio, on = "INDI_DSCM_NO"
][
  a[, .(INDI_DSCM_NO, Indexdate)],
  on = "INDI_DSCM_NO",
  nomatch = 0L
][
  Cardio_Date > Indexdate + 365 & Cardio_Date <= Indexdate + (365 * 6)
] %>% unique(by = "INDI_DSCM_NO")


#write_fst(cardiovascular.cohort, file.path("data", "cardiovascular_cohort.fst"))
cardiovascular.cohort <- read_fst("data/cardiovascular_cohort.fst", as.data.table = T)


## Atrial fibrillation 

AF <- t20[
  SICK_SYM1 %in% unlist(code.AF) | SICK_SYM2 %in% unlist(code.AF),
  .(
    INDI_DSCM_NO = as.integer(INDI_DSCM_NO),
    CMN_KEY,
    AF_Date = as.Date(MDCARE_STRT_DT, "%Y%m%d")
  )
][!is.na(AF_Date)]

#write_fst(AF, file.path("data", "AF.fst"))
AF <- read_fst("data/AF.fst", as.data.table = T)


exclude_patients_AF <- AF[
  a[, .(INDI_DSCM_NO, Indexdate)], 
  on = "INDI_DSCM_NO", 
  nomatch = 0L 
][
  AF_Date <= Indexdate + 365, 
  .(INDI_DSCM_NO)
] %>% unique()

#write_fst(exclude_patients_AF, file.path("data", "exclude_patients_AF.fst"))
exclude_patients_AF<- read_fst("data/exclude_patients_AF.fst", as.data.table = T)


# Define cohort: Diagnoses between 1 year and 6 years post-surgery (5-year observation)
AF.cohort <- AF[
  !exclude_patients_AF, on = "INDI_DSCM_NO"
][
  a[, .(INDI_DSCM_NO, Indexdate)],
  on = "INDI_DSCM_NO",
  nomatch = 0L
][
  AF_Date > Indexdate + 365 & AF_Date <= Indexdate + (365 * 6)
] %>% unique(by = "INDI_DSCM_NO")


#write_fst(AF.cohort, file.path("data", "AF_cohort.fst"))
AF.cohort <- read_fst("data/AF_cohort.fst", as.data.table = T)




## Readmission & Length of stay 
setkey(a.start, INDI_DSCM_NO)
setkey(t20, INDI_DSCM_NO)

a.start[, Surgery_date_after_1m :=  Surgery_date +30]


readmit <- a.start[  t20[FORM_CD %in% code.hospitalized, .(CMN_KEY = as.numeric(CMN_KEY), INDI_DSCM_NO, Readmit_date = as.Date(MDCARE_STRT_DT, format = "%Y%m%d"),
                                                           readmit.SICK_SYM1 = SICK_SYM1, length_of_stay = VSHSP_DD_CNT, FORM_CD)], 
                   on = .(INDI_DSCM_NO, Surgery_date < Readmit_date, Surgery_date_after_1m >= Readmit_date, surg.SICK_SYM1 = readmit.SICK_SYM1), nomatch = 0L]

#write_fst(readmit, file.path("data", "readmit.fst"))
readmit<- read_fst("data/readmit.fst", as.data.table = T)


## Opioid consumption 
t30.opi <- t30[MCARE_DIV_CD_ADJ %in% unlist(code.opiode.drug), .(CMN_KEY = as.character(CMN_KEY), MCARE_DIV_CD_ADJ, Type_drug = code.opiode.drug.named[MCARE_DIV_CD_ADJ])]
#write_fst(t30.opi, file.path("data", "t30_opi.fst"))
t30.opi <- read_fst("data/t30_opi.fst", as.data.table = T)


t60.opi <- t60[MCARE_DIV_CD_ADJ %in% unlist(code.opiode.drug), .(CMN_KEY = as.character(CMN_KEY), MCARE_DIV_CD_ADJ, Type_drug = code.opiode.drug.named[MCARE_DIV_CD_ADJ])]
#write_fst(t60.opi, file.path("data", "t60_opi.fst"))
t60.opi<- read_fst("data/t60_opi.fst", as.data.table = T)

t30_60_opi <- rbind(t30.opi, t60.opi)
#write_fst(t30_60_opi, file.path("data", "t30_60_opi.fst"))
t30_60_opi<- read_fst("data/t30_60_opi.fst", as.data.table = T)

a.opioid <- merge(t30_60_opi, 
                 t20[, .(CMN_KEY, INDI_DSCM_NO, Opioid_date = as.Date(MDCARE_STRT_DT, format = "%Y%m%d"))],
                 by = "CMN_KEY")

#write_fst(a.opioid, file.path("data", "a_opioid.fst"))
a.opioid<- read_fst("data/a_opioid.fst", as.data.table = T)




############################################################
# Final dataset
############################################################

a <- read_fst("data/merged_surg_indexdate.fst", as.data.table = T)
dementia.cohort.final<- read_fst("data/dimentia_cohort_final.fst", as.data.table = T)
a.opioid<- read_fst("data/a_opioid.fst", as.data.table = T)
readmit<- read_fst("data/readmit.fst", as.data.table = T)
AF.cohort <- read_fst("data/AF_cohort.fst", as.data.table = T)
cardiovascular.cohort <- read_fst("data/cardiovascular_cohort.fst", as.data.table = T)

a$death = 0
a[!is.na(DTH_ASSMD_DT), death:=1]

a$dementia <- 0
a[INDI_DSCM_NO %in% dementia.cohort.final$INDI_DSCM_NO, dementia:=1]


a$cardiovascular <- 0
a[INDI_DSCM_NO %in% cardiovascular.cohort$INDI_DSCM_NO, cardiovascular:=1]

a$new_AF <- 0
a[INDI_DSCM_NO %in% AF.cohort$INDI_DSCM_NO, new_AF:=1]

a$readmission <- 0
a[INDI_DSCM_NO %in% readmit$INDI_DSCM_NO, readmission:=1]

a$opioid <- 0
a[INDI_DSCM_NO %in% a.opioid$INDI_DSCM_NO, opioid:=1]

a$yr1_death <- NA

