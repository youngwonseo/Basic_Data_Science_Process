
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
* 데이터 : 포르투칼 은행 캠페인 데이터 셋 https://archive.ics.uci.edu/ml/datasets/bank+marketing


다음은 실습을 위한 R 다운로드 링크이다.
* R 설치 : https://cloud.r-project.org/    base 설치
* R Studio 설치 : https://www.rstudio.com/ free버전 설치

**안타깝지만 여기서 R 문법에 대한 상세 사항은 다루지 않는다..**





## 1.문제정의
#### 1.1 문제정의란?
* 데이터 분석에 앞서 데이터 분석을 통하여 어떠한 문제를 해결할 것인지 정의하는 단계
* **우리는 이러한 데이터를 이용하여 저러한 문제를 풀어보겠다!**
* 결국 데이터 분석을 수행하는 목적을 정의하여 진행 방향을 결정하는 단계



#### 1.2 실습
* **포르투칼 은행 캠페인 데이터 셋을 이용하여 해결해야 할 문제는?**
* 기존 고객들의 정보와 해당 고객의 예금 가입여부 통해 새로운 고객의 가입여부를 예측해보자!
* 사실 문제정의라는 것은 분석할 사람 마음이지만 실습개념에서 위와 같은 문제로 정의하고 진행한다.
* 데이터를 불러와서 어떻게 생겼는지 보자


	bank_0 <- read.csv("bank-additional-full.csv" , stringsAsFactors = F , header = T
                  , na.strings=c("","NA","unknown")
                  , sep = ";")
	head(bank0)
	str(bank0)


## 2.데이터탐색
#### 2.1 데이터탐색이란?


#### 2.2 실습

## 3.데이터준비
#### 3.1 데이터준비란?


#### 3.2 실습


## 4.모델 생성

## 5.검증


## 6.시스템구축