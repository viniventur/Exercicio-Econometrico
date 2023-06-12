/*
Exercício Econométrico - AB1 - Econometria 2
Aluno: Vinícius Ventura
*/


* Criando uma pasta e salvando o arquivo na pasta criada *
*mkdir "C:\Users\Robson Ventura\OneDrive\Trabalhos\Econometria - Stata\Exe - AB2 - Econometria 2"*

* Definindo o diretório onde o stata irá trabalhar *
cd "C:\Users\Robson Ventura\OneDrive\Trabalhos\Econometria - Stata\Exe - AB2 - Econometria 2\Resultados"

*Importando os dados*

use http://www.stata.com/data/jwooldridge/eacsap/fertil2.dta

save exereconometrico.dta, replace

* Manipulando Variáveis*

rename children cria
rename electric elet
rename urban urb

label variable cria "Número de filhos vivos"
label variable age "Idade da mãe em anos"
label variable agesq "Idade da mãe em anos ao quadrado"
label variable educ "Anos de educação"
label variable elet "Possui eletricidade"
label variable urb "Vive em área urbana"

*letra a) regressão*

*Vírgula em vez de ponto*
set dp comma

reg cria age agesq educ elet urb

ssc install tabout

logout, save(regressao) replace noauto excel dec(3): reg cria age agesq educ elet urb

*letra b) matriz de correlação*

correlate cria age agesq educ elet urb
matrix corrmatriz = r(C)

ssc install heatplot, replace
ssc install colrspace, replace
ssc install palettes, replace

heatplot corrmatriz, values(format(%4.2f)) color(hcl diverging, intensity(.7))

graph export "matrizcorrelação.png", replace

reg cria age agesq educ elet urb

logout, save(vif) replace noauto excel dec(3):vif


*corrigindo multicolineariedade - exclusão da variavel agesq*

reg cria age agesq educ elet urb

vif

reg cria age educ elet urb

vif

logout, save(reg-sem-agesq) replace noauto excel dec(3): reg cria age educ elet urb

reg cria age educ elet urb
logout, save(vif-sem-agesq) replace noauto excel dec(3): vif

* testando log para resolver multicolineariedade*
reg cria age agesq educ elet urb
gen lnagesq = ln(agesq)
gen lnage = ln(age)
gen lncria = ln(cria)
gen lneduc = ln(educ)

replace lncria = 0 if cria == 0
replace lneduc = 0 if educ == 0


reg lncria lnage lnagesq lneduc elet urb
vif

*Testando a Heterocedasticidade *
reg cria age educ elet urb
rvfplot, yline(0)
graph export "constuhat.png", replace
imtest, white
hettest

logout, save(whitetest) replace noauto excel dec(3): imtest, white
reg cria age educ elet urb
logout, save(BPteste) replace noauto excel dec(3): hettest

reg cria age educ elet urb, robust

logout, save(regressao-robust) replace noauto excel dec(3): reg cria age educ elet urb, robust

