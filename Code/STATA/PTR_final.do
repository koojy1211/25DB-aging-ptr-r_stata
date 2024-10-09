** 2024 ACE
** 구재영 권민주



clear
cap drop _all
cap graph drop _all

import delimited "DATA_new.csv"





** hexpen: 개인 의료비 지출, gdpg: 실질 GDP 성장률, aging: 고령화지수, saving: 저축률
** expen: 국가 총 지출, lexp: 기대수명, emp: 고용률, labor: 노동생산성





/* Panel Data Setting */

* 문자형으로 저장된 데이터를 숫자형으로 변환
destring hexpen gdpg aging dsave saving expen lexp emp labor, replace force

* 기초통계량 확인
describe
summarize

* 문자열 변수 country를 숫자형 변수로 인코딩 (패널 데이터 세팅을 위해)
encode country, gen(country_id)

* 패널 데이터로 설정
xtset country_id year, yearly





/* Balanced Panel Setting */

* 결측치 확인
misstable summarize

* 결측치가 있는 모든 관측치 제거
drop if missing(hexpen) | missing(gdpg) | missing(saving) | missing(expen) | missing(emp)

* 결측치가 잘 제거되었는지 확인
misstable summarize

xtdescribe

* 균형 패널 데이터만 남기기
bysort country_id: gen T = _N  // 국가별 관측된 연도 수 계산
xtdescribe
keep if T == 17
drop T

* 최종 국가 리스트 확인
levelsof country, local(countries)





/* Panel Unit Root Test */

* EMP -> D.EMP
xtunitroot ips emp		// p-value: 0.5131
xtunitroot ips D.emp	// p-value: 0.0000

gen D_emp = D.emp


* LABOR -> D.LABOR
xtunitroot ips labor	// p-value: 1.0000
xtunitroot ips D.labor	// p-value: 0.0000

gen D_labor = D.labor


* HEXPEN -> D.HEXPEN
xtunitroot ips hexpen	// p-value: 1.0000
xtunitroot ips D.hexpen	// p-value: 0.0000

gen D_hexpen = D.hexpen


* GDPg (STATIONARY)
xtunitroot ips gdpg		// p-value: 0.0000


* AGING
xtunitroot llc aging	// p-value: 0.0000
	// AGING 변수는 오직 LLC 방법을 이용해 패널 단위근 검정을 진행했을 때에만 정상성을 띤다고 출력됨


* LEXP (STATIONARY)
xtunitroot ips lexp		// p-value: 0.0116


* SAVING
xtunitroot ips saving	// p-value: 0.3088
xtunitroot ips d.saving	// p-value: 0.0000

gen D_saving = D.saving


* EXPEN (STATIONARY)
xtunitroot ips expen	// p-value: 0.0070





/* Normalization */

* 각 변수에서 평균을 빼야 함
* 개체 ID인 country별로 변수들의 평균을 계산하고 변수에서 그 평균을 뺌

bysort country: egen mean_gdpg = mean(gdpg)
gen gdpg_demean = gdpg - mean_gdpg

bysort country: egen mean_D_hexpen = mean(D_hexpen)
gen D_hexpen_demean = D_hexpen - mean_D_hexpen

bysort country: egen mean_D_saving = mean(D_saving)
gen D_saving_demean = D_saving - mean_D_saving

bysort country: egen mean_expen = mean(expen)
gen expen_demean = expen - mean_expen

bysort country: egen mean_D_emp = mean(D_emp)
gen D_emp_demean = D_emp - mean_D_emp

bysort country: egen mean_D_labor = mean(D_labor)
gen D_labor_demean = D_labor - mean_D_labor

bysort country: egen mean_aging = mean(aging)
gen aging_demean = aging - mean_aging

bysort country: egen mean_lexp = mean(lexp)
gen lexp_demean = lexp - mean_lexp





/* Panel Threshold Regression */


*** CASE (1): 임계 변수 LEXP

* 전체 데이터에서 LEXP 변수의 평균 구하기
summarize lexp
local mean_lexp = r(mean)

* 로컬 매크로의 값을 숫자 변수로 변환
scalar mean_lexp_val = `mean_lexp'


* Single Threshold
xthreg gdpg_demean, rx(D_hexpen_demean D_saving_demean expen_demean D_emp_demean D_labor_demean) qx(lexp_demean) thnum(1) grid(400) trim(0.05) bs(300)
	// 문턱 효과 p-value: 0.0133
	// R-squred:
		// Within  = 0.4275
		// Between = 0.0518
		// Overall = 0.4242

* 문턱 모수 복원
di "Single Threshold Actual Value: " 0.0819 + mean_lexp_val		// 76.976589
di "Single Threshold Lower Bound: " 0.0239 + mean_lexp_val		// 76.918589
di "Single Threshold Upper Bound: " 0.0845 + mean_lexp_val		// 76.979189


* Double Threshold
xthreg gdpg_demean, rx(D_hexpen_demean D_saving_demean expen_demean D_emp_demean D_labor_demean) qx(lexp_demean) thnum(2) grid(100) trim(0.05 0.05) bs(300 300)
	// Single 문턱 효과 p-value: 0.0133
	// Double 문턱 효과 p-value: 0.0267
	// R-squred:
		// Within  = 0.4600
		// Between = 0.0331
		// Overall = 0.4516

* 문턱 모수 복원
di "Double Threshold 1 Actual Value: " 0.0773 + mean_lexp_val
di "Double Threshold 1 Lower Bound: " 0.0165 + mean_lexp_val
di "Double Threshold 1 Upper Bound: " 0.1062 + mean_lexp_val

di "Double Threshold 2 Actual Value: " -0.7492 + mean_lexp_val
di "Double Threshold 2 Lower Bound: " -0.8240 + mean_lexp_val
di "Double Threshold 2 Upper Bound: " -0.7204 + mean_lexp_val


* Triple Threshold
xthreg gdpg_demean, rx(D_hexpen_demean D_saving_demean expen_demean D_emp_demean D_labor_demean) qx(lexp_demean) thnum(3) grid(100) trim(0.05 0.05 0.05) bs(300 300 300)
	// Single 문턱 효과 p-value: 0.0100
	// Double 문턱 효과 p-value: 0.0200
	// Double 문턱 효과 p-value: 0.9833




*** CASE (2): 임계 변수 AGING

* 전체 데이터에서 AGING 변수의 평균 구하기
summarize aging
local mean_aging = r(mean)

* 로컬 매크로의 값을 숫자 변수로 변환
scalar mean_aging_val = `mean_aging'

* Single Threshold
xthreg gdpg_demean, rx(D_hexpen_demean D_saving_demean expen_demean D_emp_demean D_labor_demean) qx(aging_demean) thnum(1) grid(400) trim(0.05) bs(300)
	// 문턱 효과 p-value: 0.0000
	// R-squred:
		// Within  = 0.4420
		// Between = 0.0000
		// Overall = 0.4348

* 문턱 모수 복원
di "Single Threshold Actual Value: " -0.2776 + mean_aging_val	// 13.454888
di "Single Threshold Lower Bound: " -0.2925 + mean_aging_val	// 13.439988
di "Single Threshold Upper Bound: " -0.2771 + mean_aging_val	// 13.455388


* Double Threshold
xthreg gdpg_demean, rx(D_hexpen_demean D_saving_demean expen_demean D_emp_demean D_labor_demean) qx(aging_demean) thnum(2) grid(100) trim(0.05 0.05) bs(300 300)
	// Single 문턱 효과 p-value: 0.0000
	// Double 문턱 효과 p-value: 0.0000
	// R-squred:
		// Within  = 0.4885
		// Between = 0.0051
		// Overall = 0.4795

* 문턱 모수 복원
di "Double Threshold 1 Actual Value: " -0.2975 + mean_aging_val	// 13.434988
di "Double Threshold 1 Lower Bound: " -0.3220 + mean_aging_val	// 13.410488
di "Double Threshold 1 Upper Bound: " -0.2745 + mean_aging_val	// 13.457988

di "Double Threshold 2 Actual Value: " 1.1670 + mean_aging_val	// 14.899488
di "Double Threshold 2 Lower Bound: " 1.1350 + mean_aging_val	// 14.867488
di "Double Threshold 2 Upper Bound: " 1.2492 + mean_aging_val	// 14.981688


* Triple Threshold
xthreg gdpg_demean, rx(D_hexpen_demean D_saving_demean expen_demean D_emp_demean D_labor_demean) qx(aging_demean) thnum(3) grid(100) trim(0.05 0.05 0.05) bs(300 300 300)
	// Single 문턱 효과 p-value: 0.0000
	// Double 문턱 효과 p-value: 0.0000
	// Double 문턱 효과 p-value: 0.9900





/* Panel Fixed Effect Regression */

xtreg lexp aging D_hexpen D_saving expen D_emp D_labor, fe
xtreg lexp aging, fe



export delimited using "results.csv", replace
