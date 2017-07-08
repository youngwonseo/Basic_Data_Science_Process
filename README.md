
## 데이터 분석의 5단계
1. 문제정의
2. 데이터탐색
3. 데이터준비
4. 모델 생성
5. 검증
6. 시스템구축

각 과정을 이해하고 R을 이용하여 직접 데이터 분석을 체험해보자!


## 분석 데이터 및 환경
* 언어 : R
* IDE : R Studio
* 데이터 : 포르투칼 은행 캠페인 데이터 셋


다음은 실습을 위한 R 다운로드 링크이다.
* R 설치 : https://cloud.r-project.org/    base 설치
* R Studio 설치 : https://www.rstudio.com/ free버전 설치







## 1.문제정의
##### 1.1 문제정의란?
* 데이터 분석에 앞서 데이터 분석을 통하여 어떠한 문제를 해결할 것인지 정의하는 단계
* **우리는 이러한 데이터를 이용하여 저러한 문제를 풀어보겠다!**
* 결국 데이터 분석을 수행하는 목적을 정의하여 진행 방향을 결정하는 단계
* 


##### 1.2 실습
	bank_0 <- read.csv("bank-additional-full.csv" , stringsAsFactors = F , header = T
                  , na.strings=c("","NA","unknown")
                  , sep = ";")



## 2.데이터탐색
##### 2.1 데이터탐색이란?


##### 2.2 실습

## 3.데이터준비
##### 3.1 데이터준비란?


##### 3.2 실습


## 4.모델 생성

## 5.검증


## 6.시스템구축