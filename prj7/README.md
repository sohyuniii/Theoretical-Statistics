## --- prj 7 : 생명보험해지 ( CRM ) 
1) 아래 모형을 이용하여 보험해지모형 모델링
- ***Logistic Regression***
  - 적절한 link 함수 
![1](https://user-images.githubusercontent.com/41772329/53742651-79e09780-3edc-11e9-9896-14133f71e812.JPG)

- ***GAM***
  - 독립변수 중 정성변수는 GLM / 정량변수는 GAM
![2](https://user-images.githubusercontent.com/41772329/53743340-ec9e4280-3edd-11e9-9b5e-93c914864711.JPG)

- ***Survival Analysis***
![3](https://user-images.githubusercontent.com/41772329/53743326-e14b1700-3edd-11e9-973d-1dfd1ba060cb.JPG)
  - ***Cox PHM*** (Proportional Haward Model)
  ![4](https://user-images.githubusercontent.com/41772329/53743398-122b4c00-3ede-11e9-992f-1e6535967255.JPG)
  

2) stepwise selection / best-subset selection / backward-elimination 등으로 선택된 모형들의 AIC를 비교

3) 위에서 구한 GLM/ GAM/ CoxPHM 확률 및 점수값의 크기순으로 전체 고객을 10개의 구간으로 나눈 후 test data를 이용하여 각 구간별 실제 해지고객의 백분
율 및 Lift 값을 구하여 세 방법을 비교
