
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
* 데이터 분석에 앞서 데이터 분석을 통하여 어떠한 문제를 해결할 것인지 정의하는 단계이다.
* **우리는 이러한 데이터를 이용하여 저러한 문제를 풀어보겠다!**
* 결국 데이터 분석을 수행하는 목적을 정의하여 진행 방향을 결정하는 단계이다.



#### 1.2 실습
* **포르투칼 은행 캠페인 데이터 셋을 이용하여 해결해야 할 문제는?**
* 기존 고객들의 정보와 해당 고객의 예금 가입여부 통해 새로운 고객의 가입여부를 예측해보자!
* 사실 문제정의라는 것은 분석할 사람 마음이지만 실습개념에서 위와 같은 문제로 정의하고 진행한다.
* 1단계에서는 데이터를 불러와서 어떻게 생겼는지 보자.
<pre><code>
bank_0 <- read.csv("bank-additional-full.csv" , stringsAsFactors = F , header = T
	  , na.strings=c("","NA","unknown")
	  , sep = ";")
head(bank0)
str(bank0)
</code></pre>



## 2.데이터탐색
#### 2.1 데이터탐색이란?
* 먼저 데이터의 속성들이 가지는 의미를 파악한다. 이것은 당연하고 당연 할 만큼 기본적이면서 중요한 사항이다. 
  * 이를 위해서는 해당 business에 대한 이해도가 필요하다.
  * 이것이 안되면 분석결과가 나와서 좋은지 나쁜지 알 수 없다.
  * 보통 업무 관계자, 특히 해당 업무의 숙련자와의 미팅을 통해 데이터 과학자 또는 개발자 들이 이해하고 분석에 넘어가야한다.
* 데이터의 구조를 통해 데이터의 성질을 파악한다. 여기서 구조란 기본적인 통계기법에 의해 표출되는 통계량, 즉 데이터의 형태 또는 성질을 나타낸다.
* 종속변수와 독립변수 정의한다. 독립변수란 종속변수에 영향을 주는 변수를 나타낸다.


#### 2.2 실습
* **먼저 본 데이터셋의 컬럼이 가지는 속성을 이해하자!**
  *
  *
  *
* **데이터의 구조를 파아가기 위해서 어떤 통계량을 계산해볼것인가?**
  * 데이터 개수
  * Unknown 데이터 개수
  * 최소, 최대값
  * 최빈값, 중위수
  * 4분위수
  * 범위
* **R의 패키지를 사용하면 위와 같은 값의 한번에 계산할 수 있다!**
* 이를 위해 먼저 입력되는 값을 범주와 시키자. 범주화란 입력된 데이터를 기반으로 다른 언어의 ENUM 형식으로 변환하는 것을 의미한다.
<pre><code>
bank_0$job <- as.factor(bank_0$job)
bank_0$marital <- as.factor(bank_0$marital)
bank_0$education <- as.factor(bank_0$education)
bank_0$default <- as.factor(bank_0$default)
bank_0$housing <- as.factor(bank_0$housing)
bank_0$loan<- as.factor(bank_0$loan)
bank_0$contact<- as.factor(bank_0$contact)
bank_0$month<- as.factor(bank_0$month)
bank_0$day_of_week<- as.factor(bank_0$day_of_week)
bank_0$campaign<- as.factor(bank_0$campaign)
bank_0$y<- as.factor(bank_0$y)
bank_0$poutcome <- as.factor(bank_0$poutcome)
</code></pre>

* 다음은 실제 통계량 계산이다.
<pre><code>
stat_fn <- function(x) {
  c(n = length(x), na.count = sum(is.na(x))
    , min = min(x, na.rm = T)
    , qt1st = quantile(x, 0.25,na.rm = T)
    , median = median(x, na.rm = T)
    , mean = mean(x, na.rm = T)
    , qt3st = quantile(x, 0.75,na.rm = T)
    , max = max(x, na.rm = T)
    , range = max(x, na.rm = T) - min(x, na.rm = T))
}
sapply(bank_0[sapply(bank_0, is.numeric)], stat_fn)
</code></pre>

* 나이에 대한 통계량을 파악해보자.
* 다음은 나이의 히스토그램을 나타낸다.

* 


## 3.데이터준비
#### 3.1 데이터준비란?
* 데이터를 정제하여 분석알고리즘이 적용가능하게 만드는 단계이다.
* 실제 관측되는 값은 오류, 노이즈 등이 많이 포함되어 있고 경우에 따라 NA(unknown)값이 존재하는데 이 데이터들을 어떻게 처리하는지에 따라 분석결과의 차이가 많이난다.
* **데이터 분석에서 가장 중요한 과정이다!!!**
* 결국 garbage in은 garbage out, 이 과정에서 garbage가 아닌 gold를 만들어 gold out을 유도하자! 
* 사실 우리는 앞의 두 단계를 통해 이와 같은 작업을 조금은 수행하였다. (범주화가 이에 해당한다)


#### 3.2 실습


## 4.모델 생성

## 5.검증


## 6.시스템구축