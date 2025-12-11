library(data.table);library(magrittr);library(fst);library(openxlsx);library(parallel);library(haven);

setwd("/home/minhyuk.kim/ShinyApps/R-skku-biohrs")
setDTthreads(0)  ## 0: All
options(scipen = 999)


############# Code Names #############  

# 대수술 코드

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

# --- Robotic Prostatectomy Logic Start ---
# Robotic prostatectomy definition: QZ961 + L1211 (General Anesthesia) + Pathology Code + NO Standard Prostatectomy Codes
code.robot <- "QZ961"
code.anesthesia <- "L1211" # 전신마취 
code.pathology <- c("C5500", "C5501", "C5502", "C5503", "C5504", "C5505", "C5506", "C5507", "C5508", "C5509", 
                    "C5911", "C5912", "C5913", "C5914", "C5915", "C5916", "C5917", "C5918", "C5919")
code.prostate.std <- code.surgery$prostate


code.heart <- code.surgery$heart
code.other.major <- c(code.surgery$stomach, code.surgery$colon, code.surgery$prostate)


# depression
code.DEP <- paste(c("F32.0", "F32.1", "F32.2", "F32.3", "F32.8", "F32.9", "F33.0", "F33.1", "F33.2", "F33.3", "F33.8", "F33.9"), collapse = "|")

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
# code.excl.disease <- list(
#   dementia = c("F00", "F01", "F02", "F03", "F051", "G30", "G311"),
#   Parkinson = c("G20"),
#   stroke  = c("I60", "I60.0", "I60.1", "I60.2", "I60.3", "I60.4", "I60.5", "I60.6", "I60.7", "I60.8", "I60.9",
#             "I61", "I61.0", "I61.1", "I61.2", "I61.3", "I61.4", "I61.5", "I61.6", "I61.8", "I61.9",
#             "I62", "I62.0", "I62.1", "I62.9",
#             "I63", "I63.0", "I63.1", "I63.2", "I63.3", "I63.4", "I63.5", "I63.6", "I63.8", "I63.9",
#             "I64"),
#   cerebral.hemorrhage = c("S06", "S06.0", "S06.1", "S06.2", "S06.3", "S06.4", "S06.5", "S06.6", "S06.7", "S06.8", "S06.9"), 
#   mental.disease = paste0("F", setdiff(c(10:19,20:29,30:39,40:48,50:59,60:69,70:79,80:89,90:98,99),c(32:34)))
# )

code.excl.disease <- list(
  dementia = c("F00", "F01", "F02", "F03", "F051", "G30", "G311"),
  Parkinson = c("G20"),
  stroke  = c("I60", "I61", "I62", "I63", "I64"),
  cerebral.hemorrhage = c("S06"), 
  mental.disease = paste0("F", setdiff(c(10:19,20:29,30:39,40:48,50:59,60:69,70:79,80:89,90:98,99),c(32:34)))
)

code.mental.disease <- paste0("F", setdiff(c(10:19,20:29,30:39,40:48,50:59,60:69,70:79,80:89,90:98,99),c(32:34)))


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
  acute.myocardial.infarction = paste0(c("I21", "I22", "I23", "I24"),collapse="|"),
  angina.pectoris = c("I20"),
  congestive.heart.failure = paste0(c("I09.9", "I11.0", "I13.2", "I25.5", "I42", "I42.0", "I42.5", "I42.6", "I42.7", "I42.8", "I42.9", "I43", "I43.0", "I43.1", "I43.2", "I43.8", "I43.9", "I50", "I50.0", "I50.1", "I50.9", "P29.0"), collapse="|")
)


code.AF <- paste0(c("I48", "I48.0", "I48.1", "I48.2", "I48.3", "I48.4", "I48.9"), collapse ="|")

code.hospitalized <- c("2", "4", "7", "10", "12")

code.chemotherapy <- c( # Chemotherapy codes (planned chemotherapy for readmission exclusion)
  "Z08.2", # Follow-up examination after chemotherapy for malignant neoplasm
  "Z09.2", # Follow-up examination after chemotherapy for other conditions
  "Z29.2", # Other prophylactic chemotherapy
  "Z51.1", # Chemotherapy session for neoplasm
  "Z51.2", # Other chemotherapy
  "Z54.2", # Convalescence following chemotherapy
  "Z92.6"  # Personal history of chemotherapy for neoplastic disease
)
code.radiotherapy <- c( # Radiotherapy codes (planned radiotherapy for readmission exclusion)
  "Y84.2", # Radiological procedure and radiotherapy
  "Z08.1", # Follow-up examination after radiotherapy for malignant neoplasm
  "Z09.1", # Follow-up examination after radiotherapy for other conditions
  "Z51.0", # Radiotherapy session
  "Z54.1"  # Convalescence following radiotherapy
)

code.surgery.named <- with(stack(code.surgery),
                           setNames(ind, values))


# --- Robotic Prostatectomy Logic Start ---
# Robotic prostatectomy definition: QZ961 + L1211 (General Anesthesia) + Pathology Code + NO Standard Prostatectomy Codes
code.robot <- "QZ961"
code.anesthesia <- "L1211"
code.pathology <- c("C5500", "C5501", "C5502", "C5503", "C5504", "C5505", "C5506", "C5507", "C5508", "C5509", 
                    "C5911", "C5912", "C5913", "C5914", "C5915", "C5916", "C5917", "C5918", "C5919")
code.prostate.std <- code.surgery$prostate







############# Subject #############  

t30 <- read_fst("/home/minhyuk.kim/knhis_data/T30.fst", as.data.table = T)

setkey(t30, CMN_KEY)

# Robotic prostatectomy 케이스 구하기 
# robotic.prostate.candidates <- t30[MCARE_DIV_CD_ADJ %in% c(code.robot, code.anesthesia, code.pathology, code.prostate.std), 
#                         .(CMN_KEY,
#                           has_robot = any(MCARE_DIV_CD_ADJ %in% code.robot),
#                           has_anesthesia = any(MCARE_DIV_CD_ADJ %in% code.anesthesia),
#                           has_pathology = any(MCARE_DIV_CD_ADJ %in% code.pathology),
#                           has_prostate = any(MCARE_DIV_CD_ADJ %in% code.prostate.std) ), by = CMN_KEY]
robotic.prostate.candidates <- t30[MCARE_DIV_CD_ADJ %like% paste(c(code.robot, code.anesthesia, code.pathology, code.prostate.std), collapse="|"), 
                                   .(CMN_KEY,
                                     has_robot = any(MCARE_DIV_CD_ADJ %like% paste(code.robot, collapse="|")),
                                     has_anesthesia = any(MCARE_DIV_CD_ADJ %like% paste(code.robot, collapse="|")),
                                     has_pathology = any(MCARE_DIV_CD_ADJ %like% paste(code.pathology, collapse="|")),
                                     has_prostate = any(MCARE_DIV_CD_ADJ %like% paste(code.prostate.std, collapse="|"))), by = CMN_KEY]
# write_fst(robotic.prostate.candidates, "data/robotic_candidate.fst")
# robotic.prostate.candidates <- read_fst("data/robotic_candidate.fst")


# tt <-  t30[MCARE_DIV_CD_ADJ %like% paste(c(code.robot, code.anesthesia, code.pathology, code.prostate.std), collapse="|"),]
# tt.code <- tt$MCARE_DIV_CD_ADJ %>% unique %>% sort()
# saveRDS(tt.code, "MCARE_code.rds")




# Condition: Robot AND Anesthesia AND Pathology AND NOT Standard Prostatectomy
robotic.keys <- robotic.prostate.candidates[has_robot & has_anesthesia & has_pathology & !has_prostate, CMN_KEY]


t30_robotic <- t30[CMN_KEY %in% robotic.keys & MCARE_DIV_CD_ADJ %in% code.robot, ]
# write_fst(t30_robotic, "data/t30_robotic.fst")
# t30_robotic <- read_fst("data/t30_robotic.fst")

# t30_all <- rbind( t30[MCARE_DIV_CD_ADJ %in% unlist(code.surgery), ], t30_robotic)
t30_all <- rbind( t30[MCARE_DIV_CD_ADJ %like% paste0(unlist(code.surgery), collapse="|"), ], t30_robotic)
# write_fst(t30_all, "data/t30_all.fst")
# t30_all <- read_fst("data/t30_all.fst")

target_key <- t30_all$CMN_KEY %>% unique
saveRDS(target_key, "data/target_key.rds")




############# Target #############  

t20 <- read_fst("study/data/t20.fst", as.data.table = T)
options(scipen = 999)
target_key <- readRDS("data/target_key.rds")


t20_target <- t20[CMN_KEY %in% target_key]#[, Indexdate := min(MDCARE_STRT_DT), keyby="INDI_DSCM_NO"]
t20_target_raw <- copy(t20_target)
# write_fst(t20_target, "data/t20_target_raw.fst")
t20_target <- read_fst("data/t20_target.fst", as.data.table = T)

## exclusion criteria

attr = list("대수술 받은 사람:" = nrow(t20_target))

#exclusion1: 대수술 받기 전에 dementia, Parkinson’s disease (G20), stroke (I60-64), cerebral hemorrhage (S06) 과거력이 5년내 있었던 환자 제외

t20_excl_disease_whole <- t20[SICK_SYM1 %like% paste0(unlist(code.excl.disease), collapse="|")
                              | SICK_SYM2 %like% paste0(unlist(code.excl.disease),collapse="|"),
                              .(INDI_DSCM_NO, Disease_date= MDCARE_STRT_DT)] #CMN_KEY
# write_fst(t20_excl_disease_whole, "data/t20_excl_disease_whole.fst")
# t20_excl_disease_whole <- read_fst("data/t20_excl_disease_whole.fst")


# merged_data <- merge(t20_target, t20_excl_disease_whole, by="INDI_DSCM_NO")
# t <- merged_data[difftime(as.Date(MDCARE_STRT_DT, format="%Y%m%d"), as.Date(Disease_date, format="%Y%m%d"), units="days") <= 365.25*5, ]


## 5년내 환자들만 확인
t20_past_5yr_disease_whole <- merge(t20_target, t20_excl_disease_whole, by="INDI_DSCM_NO") %>% 
  .[difftime(as.Date(MDCARE_STRT_DT, format="%Y%m%d"), as.Date(Disease_date, format="%Y%m%d"), units="days") <= 365.25*5, ]

# write_fst(t20_past_5yr_disease_whole, "data/t20_past_5yr_disease_whole.fst")
# t20_past_5yr_disease_whole <- read_fst("data/t20_past_5yr_disease_whole.fst")


excl_patients_id <- t20_past_5yr_disease_whole$INDI_DSCM_NO %>% unique
# saveRDS(excl_patients_id, "data/excl_patients_id.rds")
options(scipen = 999)
# excl_patients_id <- readRDS("data/excl_patients_id.rds")


t20_target <- t20_target[!INDI_DSCM_NO %in% excl_patients_id,] %>% .[, Indexdate := MDCARE_STRT_DT]

# t20_target <- t20_target[!INDI_DSCM_NO %in% excl_patients_id,][, Indexdate := as.Date(as.character(MDCARE_STRT_DT), "%Y%m%d")]
# t20_target[, MDCARE_STRT_DT := as.Date(as.character(MDCARE_STRT_DT), "%Y%m%d")]

attr$`Exclusion1: 대수술 받기 전 5년내 과거력 환자 제외 후: ` <- nrow(t20_target)







############# CCI 계산 #############  

code.cci <- list(
  MI = c("I21", "I22", "I25"),
  CHF = c(paste0("I", c("099", 110, 130, 132, 255, 420, 425:429, 43, 50)), "P290"),
  Peripheral_VD = c(paste0("I", c(70, 71, 731, 738, 739, 771, 790, 792)), paste0("K", c(551, 558, 559)), "Z958", "Z959"),
  Cerebro_VD = c("G45", "G46", "H340", paste0("I", 60:69)),
  Dementia = c(paste0("F0", c(0:3, 51)), "G30", "G311"),
  Chronic_pulmonary_dz = c("I278", "I279", paste0("J", c(40:47, 60:67, 684, 701, 703))),
  Rheumatologic_dz = paste0("M", c("05", "06", 315, 32:34, 351, 353, 360)),
  Peptic_ulcer_dz = paste0("K", 25:28),
  # Mild_liver_dz에 B19 추가
  Mild_liver_dz = c("B18", "B19", paste0("K", c(700:703, 709, 713:715, 717, 73, 74, 760, 762:764, 768, 769)), "Z944"),
  DM_no_complication = paste0("E", c(100, 101, 106, 108:111, 116, 118:121, 126, 128:131, 136, 138:141, 146, 148, 149)),
  DM_complication = paste0("E", c(102:105, 107, 112:115, 117, 122:125, 127, 132:135, 137, 142:145, 147)),
  Hemi_paraplegia = paste0("G", c("041", 114, 801, 802, 81, 82, 830:834, 839)),
  # Renal_dz N17 추가
  Renal_dz = c("I120", "I131", paste0("N", c("032", "033", "034", "035", "036", "037",
                                             17, 18, 19, 250)), paste0("Z", c(490:492, 940, 992))),
  # C73 (갑상선암) 제외, C76은 Metastatic_solid_tumor로 이동
  Malig_with_Leuk_lymphoma = paste0("C", c(paste0("0", 0:9), 10:26, 30:34, 37:41, 43, 45:58, 60:72, 74:75, 81:85, 88, 90, 97)),
  Moderate_severe_liver_dz = c(paste0("I", c(85, 859, 864, 982)), paste0("K", c(704, 711, 721, 729, 765:767))),
  # C76-C80 범위로 수정
  Metastatic_solid_tumor = paste0("C", 76:80),
  # B20-B24 전체 포함
  AIDS_HIV = paste0("B", c(20:24))
)
cciscore <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 6, 6)
names(cciscore) <- names(code.cci)

cci_res <- mclapply(names(code.cci), function(x){
  
  
  ## past history
  dt1 <- t20[SICK_SYM1 %like% paste0(code.cci[[x]], collapse = "|") | SICK_SYM2 %like% paste0(code.cci[[x]], collapse = "|"),] %>% 
    .[, .(INDI_DSCM_NO, MDCARE_STRT_DT = MDCARE_STRT_DT, Incident_Date = MDCARE_STRT_DT)] %>% 
    .[, .SD[1], keyby=c("INDI_DSCM_NO", "MDCARE_STRT_DT")]
    

  # dt <- t20[SICK_SYM1 %like% paste0(code.cci[[x]], collapse = "|") | SICK_SYM2 %like% paste0(code.cci[[x]], collapse = "|"),][, .(INDI_DSCM_NO, MDCARE_STRT_DT = as.Date(as.character(MDCARE_STRT_DT), "%Y%m%d"), Incident_Date = as.Date(as.character(MDCARE_STRT_DT), "%Y%m%d"))]
  # saveRDS(dt, "data/dt.rds")
  # dt <- readRDS("data/dt.rds")
  
  
  #t <- dt1[t20_target, on=c("INDI_DSCM_NO", "MDCARE_STRT_DT"), roll = 365*5]
  ## CCI calculation
  out1 <- dt1[t20_target, on=c("INDI_DSCM_NO", "MDCARE_STRT_DT"), roll = 365*5] %>%
    .[, cci_event := as.integer(!is.na(Incident_Date))  ] %>% 
    .[,cci_event] * cciscore[x]
  
  return(out1)
              
}, mc.cores = 8) %>% do.call(cbind, .) %>% rowSums(.)

saveRDS(cci_res, "data/cci_res.rds")
cci_res <- readRDS("data/cci_res.rds") 

t20_target[, CCI := cci_res]
t20_target[,CCI_group := as.factor(ifelse(CCI==0,"0",
                                ifelse(CCI==1, "1",
                                       ifelse(CCI==2, "2", ">= 3"))))]




############# other variables ############# 

# Death
# dth <- read_fst("/home/minhyuk.kim/knhis_data/DTH.fst", as.data.table = T)
# dth[, INDI_DSCM_NO := as.character(INDI_DSCM_NO)]
# # dth <- dth[as.numeric(substr(DTH_ASSMD_DT, 1,4))<=2013, ]
# # write_fst(dth, "data/dth.fst")
dth <- read_fst("data/dth.fst", as.data.table = T)

t20_target <- merge(t20_target, dth[ , .(INDI_DSCM_NO , death_date = DTH_ASSMD_DT)], by="INDI_DSCM_NO", all.x = T)
t20_target[, Death := fifelse(is.na(death_date),0,1)]


### Need to change the cutoff day (2013-12-31 to 2021-12-31)
t20_target[, FU_days := as.integer(pmin(as.Date(as.character(death_date), format="%Y%m%d"), as.Date("2013-12-31"), na.rm=T) - as.Date(Indexdate, format="%Y%m%d"))]


## Age & Income 

# bfc <- read_fst("/home/minhyuk.kim/knhis_data/BFC.fst", as.data.table = T)
# bfc <- bfc[STD_YYYY %in% c(2002:2013)]
# write_fst(bfc, "data/bfc.fst")

bfc <- read_fst("data/bfc.fst", as.data.table = T)


t20_target <- merge(t20_target[, STD_YYYY:= substr(Indexdate, 1,4)], unique(bfc[, .(STD_YYYY, SEX = SEX_TYPE, BYEAR,  disablity_type = MAIN_DSB_TYPE,
                                       insurance_quantile = CALC_CTRB_VTILE_FD), by=INDI_DSCM_NO]), by=c("INDI_DSCM_NO", "STD_YYYY"), all.x=T)  # insurance_fee = CALC_CTRB_FD, disablity_code := CMPR_DSB_GRADE
rm(bfc)


# 보험료 순위: 0~6, 7~13, 14~20
t20_target[, income_tertile := ifelse(insurance_quantile %in% c(0:6), 1,
                                  ifelse(insurance_quantile %in% c(7:13), 2, 3))]

# Age
t20_target[, Age := as.numeric(substr(Indexdate,1,4)) - as.numeric(substr(BYEAR,1,4))]
t20_target <- t20_target[Age>=18, ]
attr$`Exclusion2: Age >=18` = nrow(t20_target)


t20_target[, Age_group := ifelse(Age<65, "18-64", "65+")]

ob_02 <- read.csv("/home/minhyuk.kim/knhis_data/g1e_obj_2002.csv") %>% as.data.table()
g1eq <- read.csv("/home/minhyuk.kim/knhis_data/g1eq_0217.csv") %>% as.data.table()
g1eq <- g1eq[EXMD_BZ_YYYY %in% c(2002:2013), ]

aa <- g1eq[, .(INDI_DSCM_NO,
               STD_YYYY = as.character(EXMD_BZ_YYYY),
               BMI = G1E_BMI, 
               drink_freq = Q_DRK_FRQ_V0108, 
               drink_amount = Q_DRK_AMT_V0108, 
               smoke=Q_SMK_YN,
               exercise_freq = Q_PA_FRQ,
               #exercise_per_time = Q_PA_DRT,
               exercise_freq_v = Q_PA_VD,
               exercise_freq_m = Q_PA_MD,
               exercise_freq_walk = Q_PA_WALK
               ) ]


t20_target <- merge(t20_target, aa, by=c("INDI_DSCM_NO", "STD_YYYY"))





# Surgery Type
# t20_target[, Surgery_Type := code.surgery.named[MCARE_DIV_CD_ADJ]]



# Sex
# BMI
# income
# education
# CCI
# disability type 
# Alcohol 
# Regular exercise 
# Smoking 


## outcome variables  