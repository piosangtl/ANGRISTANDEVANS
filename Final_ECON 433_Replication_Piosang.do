log using RP_433_logfile

*		Tristan Piosang			Replication Project 			ECON 433     *

use "H:\usa_00011 (1).dta"

sort serial pernum 


tab nchild 
tab chborn 
   
/*Make sure value num labels are the same. Recode chborn by combining 0 and 1. 
Then use the generated/recoded vars as a restriction to li*/

recode nchild (0=0) (1=1) (2=2) (3=3)(4=4)(5=5)(6=6)(7=7) (8=8) (9=9),gen(nnchild)
recode chborn (0=0) (1=0) (2=1) (3=2)(4=3)(5=4)(6=5)(7=6) (8=7) (9=8) (10/max=9),gen(cchborn)

*Generate mom_age 
gen eligible_mom=0
replace eligible_mom=1 if sex==2 & momloc==0 & cchborn>0
gen mom_age=age if eligible_mom==1


*								TABLE 1										*

**Women aged 21-35
gen women2135=. 
replace women2135=1 if sex==2 & age >=21 & age <=35
replace women2135=0 if women2135==.
tab women2135

*Mean child ever born 
sum cchborn if women2135==1

*Percent with 2 or more children 
gen women2135_2more=.
replace women2135_2more=1 if women2135==1 & cchborn>1
replace women2135_2more=0 if women2135==1 & cchborn <=1
tab women2135_2more

*Percent worked last year
tab workedyr if women2135==1


**Women aged 36-50
gen women3650=. 
replace women3650=1 if sex==2 & age >=36 & age <=50
replace women3650=0 if women3650==.
tab women3650

*Mean child ever born
sum cchborn if women3650==1

*Percent with 2 or more children 
gen women3650_2more=.
replace women3650_2more=1 if women3650==1 & cchborn>1
replace women3650_2more=0 if women3650==1 & cchborn <=1
tab women3650_2more

*Percent worked last year

tab workedyr if women3650==1


**Women aged 21-35 with 2 or more children

*Mean child ever born
sum cchborn if women2135_2more==1

*Percent with 2 or more children 
tab women2135_2more

*Percent worked last year
gen women2135_2wrkyr=. 
replace women2135_2wrkyr=1 if women2135_2more==1 & workedyr==3
replace women2135_2wrkyr=0 if women2135_2more==1 & workedyr==1
tab women2135_2wrkyr


**Married women aged 21-35 with 2 or more children
gen mar2135_2more=. 
replace mar2135_2more=1 if women2135==1 & marst==1 & cchborn >=2
replace mar2135_2more=0 if women2135==1 & marst>2 & cchborn <2
tab mar2135_2more

*Mean child ever born 
sum cchborn if mar2135_2more==1

*Percent with 2 or more children 
tab mar2135_2more

*Percent workedyr
tab  workedyr if mar2135_2more==1



***Match kids and mom

gen age1=age
tab age1, nolabel

*make a distinction between kids and other family members
gen kid=.
by serial: replace kid=1 if pernum>2 & (mom_age-age1)>20

*manipulating age so that sorted child sequence appears in descending order
gen negage=-age1

*generating child sequence from older to younger
sort serial negage
by serial: gen chseq=_n-2 if kid!=.

*generating child id (childid=household id+child sequence)
gen childid = string(serial)+string(chseq) if kid!=.
gen chid = real(childid)
 

forv j=1(1)10 {
	gen kid`j'age=.
	gen kid`j'sex=.
	gen kid`j'pernum=.
	loc k=`j'-1
	
	forv i=1(1)27 {
		display "kid `j' - `i'"
		if `j' != 1 {
			replace kid`j'age=age[_n+`i'] if momloc[_n+`i']==pernum & serial[_n+`i']==serial & kid`j'age==. & pernum[_n+`i']>kid`k'pernum
			replace kid`j'sex=sex[_n+`i'] if momloc[_n+`i']==pernum & serial[_n+`i']==serial & kid`j'sex==. & pernum[_n+`i']>kid`k'pernum
			replace kid`j'pernum=pernum[_n+`i'] if momloc[_n+`i']==pernum & serial[_n+`i']==serial & kid`j'pernum==. & pernum[_n+`i']>kid`k'pernum
		}
		
		if `j' == 1 {
			replace kid`j'age=age[_n+`i'] if momloc[_n+`i']==pernum & serial[_n+`i']==serial & kid`j'age==. 
			replace kid`j'sex=sex[_n+`i'] if momloc[_n+`i']==pernum & serial[_n+`i']==serial & kid`j'sex==. 
			replace kid`j'pernum=pernum[_n+`i'] if momloc[_n+`i']==pernum & serial[_n+`i']==serial & kid`j'pernum==. 
		}	
	}
}


sort serial pernum

**Generate the final subsample (twomore_g1, i.e. age group 1( or g1=21-35 with 2 or more kids and kid1's age<18))

gen twomore_g1=0
replace twomore_g1=1 if pernum==momloc[_n+1] & sex==2 & cchborn==nnchild & cchborn>=2 & momloc==0 & mom_age >=21 & mom_age <=35 & kid2age >=1 & kid1age<18


*Generate mom age groups
gen mom_g1=1 if mom_age >=21 & mom_age <=35 & kid2age >=1
replace mom_g1=0 if mom_g1==. 

gen mom_g2=1 if mom_age >=36 & mom_age <=50
replace mom_g2=0 if mom_g2==. 




 

*									TABLE 2									*


*Child ever born 
sum cchborn if twomore_g1==1 
sum cchborn if twomore_g1==1 & pernum== sploc[_n-1] & serial== serial[_n-1] & marst==1 

*More than 2 children 
gen morethan2=.
replace morethan2=1 if twomore_g1==1 & cchborn>2
replace morethan2=0 if morethan2==. & twomore_g1==1 & cchborn <=2

sum morethan2
sum morethan2 if twomore_g1==1 & pernum== sploc[_n-1] & serial== serial[_n-1] & marst==1 


**generate Boy1st(first child was a boy)
gen boy1=.
replace boy1=1 if kid1sex==1 & twomore_g1==1
replace boy1=0 if boy1==. & twomore_g1==1
sum boy1 if twomore_g1==1
sum boy1 if twomore_g1==1 & pernum== sploc[_n-1] & serial== serial[_n-1] & marst==1 

*Boy2nd (second child was a boy)
gen boy2=. 
replace boy2=1 if kid2sex==1 & twomore_g1==1
replace boy2=0 if boy2==. & twomore_g1==1
sum boy2 if twomore_g1==1 
sum boy2 if twomore_g1==1 & pernum== sploc[_n-1] & serial== serial[_n-1] & marst==1 

*Twoboys (first two children were boys)
gen twoboys=1 if kid1sex==1 & kid2sex==1 & twomore_g1==1
replace twoboys=0 if twoboys==. & twomore_g1==1
sum twoboys if twomore_g1==1 
sum twoboys if twomore_g1==1 & pernum== sploc[_n-1] & serial== serial[_n-1] & marst==1 


*Twogirls (first two children were girls)
gen twogirls=1 if kid1sex==2 & kid2sex==2 & twomore_g1==1
replace twogirls=0 if twogirls==. & twomore_g1==1
sum twogirls if twomore_g1==1 
sum twogirls if twomore_g1==1 & pernum== sploc[_n-1] & serial== serial[_n-1] & marst==1 


*samesex1n2 (first two children were the same sex)
gen samesex1n2=1 if kid1sex==kid2sex & twomore_g1==1
replace samesex1n2=0 if samesex1n2==. & twomore_g1==1
sum samesex1n2 if twomore_g1==1 
sum samesex1n2 if twomore_g1==1 & pernum== sploc[_n-1] & serial== serial[_n-1] & marst==1 

*twins2 (if second birth was a twin)
gen twins2=1 if kid2age==kid3age & twomore_g1==1
replace twins2=0 if twins2==. & twomore_g1==1
sum twins2 if twomore_g1==1 
sum twins2 if twomore_g1==1 & pernum== sploc[_n-1] & serial== serial[_n-1] & marst==1 

*generate age of eldest kid
egen max_kid_age=rowmax(kid*age)

*age
sum mom_age if twomore_g1==1 
sum mom_age if twomore_g1==1 & pernum== sploc[_n-1] & serial== serial[_n-1] & marst==1 

*age at first birth 
gen mom_ageat1st= mom_age-max_kid_age if twomore_g1==1
sum mom_ageat1st if twomore_g1==1
sum mom_ageat1st if twomore_g1==1 & pernum== sploc[_n-1] & serial== serial[_n-1] & marst==1 


*Worked for pay 

gen worked4pay=0
replace worked4pay=1 if uhrswork>0 & incwage>0 & twomore_g1==1
sum worked4pay if twomore_g1==1
sum worked4pay if twomore_g1==1 & pernum== sploc[_n-1] & serial== serial[_n-1] & marst==1 

*Weeks worked last year
sum wkswork1 if twomore_g1==1
sum wkswork1 if twomore_g1==1 & pernum== sploc[_n-1] & serial== serial[_n-1] & marst==1 

*Hours per week 
sum uhrswork if twomore_g1==1
sum uhrswork if twomore_g1==1 & pernum== sploc[_n-1] & serial== serial[_n-1] & marst==1 

*labor/personal income a year prior to census. In 1995 USD assuming 18.72% (~~1.19) total inflation.  
gen incwage_1995usd= incwage*1.19 if twomore_g1==1
sum incwage_1995usd if twomore_g1==1  
sum incwage_1995usd if twomore_g1==1 & pernum== sploc[_n-1] & serial== serial[_n-1] & marst==1 

*family income a year prior to census
gen ftotinc_1995usd= ftotinc*1.19 if twomore_g1==1
sum ftotinc_1995usd if twomore_g1==1 
sum ftotinc_1995usd if twomore_g1==1 & pernum== sploc[_n-1] & serial== serial[_n-1] & marst==1 

*non-wife income(family income minus wife's labour income)
gen nonwife_inc= ftotinc_1995usd-incwage_1995usd if twomore_g1==1 & pernum== sploc[_n-1] & serial== serial[_n-1] & marst==1  
sum nonwife_inc if twomore_g1==1



***Gen father's data and put to mom. First, sort to original. 

sort serial pernum 

gen husage=. 
replace husage= age[_n-1] if twomore_g1==1 & pernum== sploc[_n-1] & serial== serial[_n-1]  & marst==1 

gen huspernum=. 
replace huspernum= pernum[_n-1] if twomore_g1==1 & pernum== sploc[_n-1] & serial== serial[_n-1] & marst==1 

gen husworkedyr=0 if twomore_g1==1 & pernum==sploc[_n-1] & serial==serial[_n-1] & marst==1 
replace husworkedyr=1 if uhrswork[_n-1]>0 & pernum==sploc[_n-1] & serial==serial[_n-1] &twomore_g1==1 & marst==1 

gen huswkswork1=.
replace huswkswork1= wkswork1[_n-1] if twomore_g1==1 & pernum== sploc[_n-1] & serial== serial[_n-1] & marst==1 

gen husuhrswork=.
replace husuhrswork= uhrswork[_n-1] if twomore_g1==1 & pernum== sploc[_n-1] & serial== serial[_n-1] & marst==1 

gen husincwage=incwage[_n-1] if twomore_g1==1 & pernum== sploc[_n-1] & serial== serial[_n-1]
gen husincwage_1995usd= husincwage*1.19 if twomore_g1==1 & pernum== sploc[_n-1] & serial== serial[_n-1] & marst==1 


*husband's age 

sum husage 

*husband's age at first birth

gen husageat1st= husage-kid1age 
sum husageat1st


*husband Worked for pay 
sum husworkedyr

*husband weeks worked last year

sum huswkswork1

*husband Hours per week 

sum husuhrswork

*husband labor/personal income a year prior to census 

sum husincwage_1995usd




*								TABLE 3										*

***All women -sex of first child with one or more children 

**Generate group for all eligible women
gen allwomT3=0
replace allwomT3=1 if eligible_mom==1 & cchborn >=1 & mom_age>=21 & mom_age<=35 & momloc[_n+1]==pernum & cchborn==nchild

*fraction of sample
gen onegirl=0 if allwomT3==1
replace onegirl=1 if allwomT3==1 & kid1sex==2
sum onegirl

gen oneboy=0 if allwomT3==1
replace oneboy=1 if allwomT3==1 & kid1sex==1
sum oneboy

ttest oneboy==onegirl, unp

*fraction that had another child 

gen anochildgirl=0 if onegirl==1
replace anochildgirl=1 if onegirl==1 & cchborn>1
reg anochildgirl onegirl


gen anochildboy=0 if oneboy==1
replace anochildboy=1 if oneboy==1 & cchborn>1
reg anochildboy oneboy

ttest anochildboy==anochildgirl, unp

***Married women- sex of first child with one or more children 

gen marallwomT3=0 
replace marallwomT3=1 if pernum==momloc[_n+1] & sex==2 & cchborn==nnchild & cchborn>=1 & momloc==0 & mom_age >=21 & mom_age <=35 & kid1age >=1 &marst==1


*fraction of sample 
gen onegirlmar=0 if marallwomT3==1
replace onegirlmar=1 if marallwomT3==1 & kid1sex==2
sum onegirlmar

gen oneboymar=0 if marallwomT3==1 
replace oneboymar=1 if marallwomT3==1 & kid1sex==1 
sum oneboymar

*fraction that had another child 
gen mar_anogirl=0 if onegirlmar==1
replace mar_anogirl=1 if onegirlmar==1 & cchborn>1 
reg mar_anogirl onegirlmar


gen mar_anoboy=0 if oneboymar==1 
replace mar_anoboy=1 if oneboymar==1 & cchborn>1
reg mar_anoboy oneboymar

ttest mar_anoboy==mar_anogirl, unp


***Married women-sex of the first TWO children with two or more children 

**All women (twomore_g1)

*One boy, one girl

gen boygirl=0 if twomore_g1==1
replace boygirl=1 if twomore_g1==1 & kid1sex != kid2sex
sum boygirl

gen morechi_bg=0 if twomore_g1==1 & boygirl==1
replace morechi_bg=1 if twomore_g1==1 & boygirl==1 & cchborn>2
reg morechi_bg boygirl

*Two girls

sum twogirls if twomore_g1==1

gen more2girls=0 if twomore_g1==1 & twogirls==1
replace more2girls=1 if twomore_g1==1 & twogirls==1 & cchborn>2
reg more2girls twogirls

*Two boys

sum twoboys if twomore_g1==1

gen more2boys=0 if twomore_g1==1 & twoboys==1
replace more2boys=1 if twomore_g1==1 & twoboys==1 & cchborn>2
reg more2boys twoboys

*both same sex

sum samesex1n2 if twomore_g1==1

gen moresamesex=0 if twomore_g1==1 & samesex1n2==1
replace moresamesex=1 if twomore_g1==1 & samesex1n2==1 & cchborn>2
reg moresamesex samesex1n2

ttest moresamesex==morechi_bg, unp


**Married women 

*One boy, one girl

gen mboygirl=0 if twomore_g1==1 & pernum== sploc[_n-1] & serial== serial[_n-1] & marst==1
replace mboygirl=1 if twomore_g1==1 & kid1sex != kid2sex & pernum== sploc[_n-1] & serial== serial[_n-1] & marst==1
sum mboygirl

gen m_morechi_bg=0 if twomore_g1==1 & mboygirl==1 & pernum== sploc[_n-1] & serial== serial[_n-1] & marst==1
replace m_morechi_bg=1 if twomore_g1==1 & mboygirl==1 & cchborn>2 & pernum== sploc[_n-1] & serial== serial[_n-1] & marst==1
reg m_morechi_bg mboygirl


*Two girls

sum twogirls if twomore_g1==1 & pernum== sploc[_n-1] & serial== serial[_n-1] & marst==1

gen m_more2girls=0 if twomore_g1==1 & twogirls==1 & pernum== sploc[_n-1] & serial== serial[_n-1] & marst==1
replace m_more2girls=1 if twomore_g1==1 & twogirls==1 & cchborn>2 & pernum== sploc[_n-1] & serial== serial[_n-1] & marst==1
reg m_more2girls twogirls 

*Two boys

sum twoboys if twomore_g1==1 & pernum== sploc[_n-1] & serial== serial[_n-1] & marst==1

gen m_more2boys=0 if twomore_g1==1 & twoboys==1 & pernum== sploc[_n-1] & serial== serial[_n-1] & marst==1
replace m_more2boys=1 if twomore_g1==1 & twoboys==1 & cchborn>2 & pernum== sploc[_n-1] & serial== serial[_n-1] & marst==1
reg m_more2boys twoboys

*Both same sex

sum samesex1n2 if twomore_g1==1 & pernum== sploc[_n-1] & serial== serial[_n-1] & marst==1

gen m_moresamesex=0 if twomore_g1==1 & samesex1n2==1 & pernum== sploc[_n-1] & serial==serial[_n-1] & marst==1
replace m_moresamesex=1 if twomore_g1==1 & samesex1n2==1 & cchborn>2 & pernum== sploc[_n-1] & serial== serial[_n-1] & marst==1
reg m_moresamesex samesex1n2

ttest m_moresamesex==m_morechi_bg, unp


****TABLE 4

***Compare the demog characteristics of people who have same-sex 

**Generate race groups+ yrs of education var

gen white=0 
replace white=1 if race==1

gen black=0
replace black=1 if race==2 

gen hispanic=0
replace hispanic=1 if hispan==1 | hispan==2 | hispan==3 | hispan==4

gen other_race=0
replace other_race=1 if race==3 | race==4 | race==5 | race==6 | race==7      

gen yrs_educ=.
replace yrs_educ=0 if educ==0
replace yrs_educ=4 if educ==1
replace yrs_educ=8 if educ==2
replace yrs_educ=9 if educ==3
replace yrs_edu=10 if educ==4
replace yrs_edu=11 if educ==5
replace yrs_edu=12 if educ==6
replace yrs_edu=13 if educ==7
replace yrs_edu=14 if educ==8
replace yrs_edu=16 if educ==9
replace yrs_edu=17 if educ==10
   

foreach y in "ttest" {
    `y' age, by(samesex1n2)
	`y' mom_ageat1st, by(samesex1n2)
	`y' black, by(samesex1n2)
	`y' white, by(samesex1n2)
	`y' hispanic, by(samesex1n2)
	`y' other_race, by(samesex1n2)
	`y' yrs_educ, by(samesex1n2)
}


****TABLE 5

gen lnftotinc= ln(ftotinc_1995usd) if eligible_mom==1

foreach y in "ttest" {
    `y' morethan2, by(samesex1n2)
	`y' cchborn, by(samesex1n2)
	`y' worked4pay, by(samesex1n2)
	`y' wkswork1, by(samesex1n2)
	`y' uhrswork, by(samesex1n2)
	`y' incwage, by(samesex1n2)
	`y' lnftotinc, by(samesex1n2)
}

*Using samesex1n2 as instrument

ivregress 2sls worked4pay (morethan2=samesex1n2)

ivregress 2sls wkswork1 (morethan2=samesex1n2)

ivregress 2sls uhrswork (morethan2=samesex1n2) 

ivregress 2sls incwage (morethan2=samesex1n2)

ivregress 2sls lnftotinc (morethan2=samesex1n2)


*Using number of children as instrument

ivregress 2sls worked4pay (cchborn=samesex1n2)

ivregress 2sls wkswork1 (cchborn=samesex1n2)

ivregress 2sls uhrswork (cchborn=samesex1n2) 

ivregress 2sls incwage (cchborn=samesex1n2)

gen ln_ftotinc= ln(ftotinc_1995usd) if twomore_g1==1 
ivregress 2sls ln_ftotinc (cchborn=samesex1n2)


****TABLE 10 
 
ssc install egenmore
xtile husearn_3= husincwage_1995usd, nq(3)
tab husearn_3

gen bottom3rd= 0
replace bottom3rd=1 if husearn_3==1 

gen middle3rd=0
replace middle3rd=1 if husearn_3==2

gen top3rd=0
replace top3rd=1 if husearn_3==3

gen race_grp=0
replace race_grp=1 if white==1
replace race_grp=2 if black==1
replace race_grp=3 if hispanic==1
replace race_grp=4 if other_race==1


*First stage

foreach y in "reg" {
	`y' morethan2 samesex1n2 mom_age mom_ageat1st i.kid1sex i.kid2sex i.race_grp if pernum== sploc[_n-1] & serial== serial[_n-1] & bottom3rd ==1 & marst==1
	`y' morethan2 samesex1n2 mom_age mom_ageat1st i.kid1sex i.kid2sex i.race_grp if pernum== sploc[_n-1] & serial== serial[_n-1] & middle3rd==1 & marst==1
	`y' morethan2 samesex1n2 mom_age mom_ageat1st i.kid1sex i.kid2sex i.race_grp if pernum== sploc[_n-1] & serial== serial[_n-1] & top3rd==1 & marst==1
	
}


*Mean of dependent variable: worked for pay

sum worked4pay if twomore_g1==1 & pernum== sploc[_n-1] & serial== serial[_n-1] & bottom3rd ==1 & marst==1

sum worked4pay if twomore_g1==1 & pernum== sploc[_n-1] & serial== serial[_n-1] & middle3rd ==1 & marst==1

sum worked4pay if twomore_g1==1 & pernum== sploc[_n-1] & serial== serial[_n-1] & top3rd ==1 & marst==1

*OLS: worked for pay

reg worked4pay morethan2 mom_age mom_ageat1st boy1 boy2 black hispanic other_race  if pernum== sploc[_n-1] & serial== serial[_n-1] & bottom3rd==1 & marst==1

reg worked4pay morethan2 mom_age mom_ageat1st boy1 boy2 black hispanic other_race if pernum== sploc[_n-1] & serial== serial[_n-1] & middle3rd==1 & marst==1

reg worked4pay morethan2 mom_age mom_ageat1st boy1 boy2 black hispanic other_race if pernum== sploc[_n-1] & serial== serial[_n-1] & top3rd==1 & marst==1

*2SLS: worked for pay 

ivregress 2sls worked4pay (morethan2=samesex1n2) mom_age mom_ageat1st black hispanic other_race  if pernum== sploc[_n-1] & serial== serial[_n-1] & bottom3rd==1 & marst==1, robust 

ivregress 2sls worked4pay (morethan2=samesex1n2) mom_age mom_ageat1st black hispanic other_race  if pernum== sploc[_n-1] & serial== serial[_n-1] & middle3rd==1 & marst==1, robust 

ivregress 2sls worked4pay (morethan2=samesex1n2) mom_age mom_ageat1st black hispanic other_race  if pernum== sploc[_n-1] & serial== serial[_n-1] & top3rd==1 & marst==1, robust


*Mean of dependent variable: weeks/year

sum wkswork1 if twomore_g1==1 & pernum== sploc[_n-1] & serial== serial[_n-1] & marst==1 & bottom3rd ==1

sum wkswork1 if twomore_g1==1 & pernum== sploc[_n-1] & serial== serial[_n-1] & marst==1 & middle3rd ==1

sum wkswork1 if twomore_g1==1 & pernum== sploc[_n-1] & serial== serial[_n-1] & marst==1 & top3rd ==1

*OLS: weeks/year

reg wkswork1 morethan2 mom_age mom_ageat1st boy1 boy2 black hispanic other_race  if pernum== sploc[_n-1] & serial== serial[_n-1] & bottom3rd==1 & marst==1

reg wkswork1 morethan2 mom_age mom_ageat1st boy1 boy2 black hispanic other_race if pernum== sploc[_n-1] & serial== serial[_n-1] & middle3rd==1 & marst==1

reg wkswork1 morethan2 mom_age mom_ageat1st boy1 boy2 black hispanic other_race if pernum== sploc[_n-1] & serial== serial[_n-1] & top3rd==1 & marst==1

*2SLS: weeks/year

ivregress 2sls wkswork1 (morethan2=samesex1n2) mom_age mom_ageat1st black hispanic other_race  if pernum== sploc[_n-1] & serial== serial[_n-1] & bottom3rd==1 & marst==1, robust 

ivregress 2sls wkswork1 (morethan2=samesex1n2) mom_age mom_ageat1st black hispanic other_race  if pernum== sploc[_n-1] & serial== serial[_n-1] & middle3rd==1 & marst==1, robust

ivregress 2sls wkswork1 (morethan2=samesex1n2) mom_age mom_ageat1st black hispanic other_race  if pernum== sploc[_n-1] & serial== serial[_n-1] & top3rd==1 & marst==1, robust



log close 



/* Reference List

1. Lines 25 to 92. 

2. Lines 98 to 137. 

3. Lines 14 to 15. 
(Note: vars generated by these lines are used as restrictions in all relevant regressions. Restriction is -if cchborn==nnchild-)

4. Lines 144 to 145. 
(Note: No var was created for the married sample. Restrictions to the all women sample are added to derive the maried samples estimates.)

5. Lines 163 to 280. 

6. Lines 210 to 214.

7. Line 245

8. Lines 260 to 280 

9. Lines 311 to 449

10. Line 341

11. Lines 371 to 449

12. 316 to 339

13. 449

14. 452 to 492

15. 497 to 533

16. 536 to 615

17. 538 to 549 

18. 568 to 590

19. 595 to 615

20. Line 586 (for an example of the set of covariates as per the notes to Table 8.)

*/




