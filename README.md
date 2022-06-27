# **2022太陽能發電量預測**
##### 比賽網址: https://aidea-web.tw/topic/09679060-518a-4e6f-94db-53c7d8de8138


## **作法** 
### **編譯環境**
* 使用程式語言: R
* 筆電、GPU

### **問題概述**
本議題透過太陽能發電歷史資料如日射量、裝置容量…等，以機器學習的方法預測各地的太陽能發電量，期望參賽者能掌握影響太陽能發電量的關鍵因素，並對於未來太陽能建置細節擬定能有相當程度的助益。


### **使用資料**
* 主辦方資料
  * train.csv
  * test.csv
  * submission.csv
   

### **方法**
* 資料前處理
  * 缺失值: Temp_m, Irradiance, Temp
    * 使用mice補值
  * 日期mapping成對應的季節
    * 3、4、5月: 春天
    * 6、7、8月: 夏天
    * 9、10、11月: 秋天
    * 12、1、2月: 冬天
  * 經緯度與面向角度
    * 觀察資料後可發現幾乎一組經緯度與另一組面向角度相對應，因此將其經過xgboost的feature_importance後可發現經緯度的重要性大於角度
    * 後由於經度主要與時間有關，緯度與太陽是否直射有關，因此選定緯度 
  * 模組型號對應成相對應之電壓 
  * 篩選資料
    * 季節
    * 模溫計：模板溫度
    * 預測目標 - 發電量(kWh)
    * 日射量(MJ/m²)
    * 裝置容量(kWp)
    * 緯度
    * 日照計：日射量(Wh/m²)
    * 當日平均氣溫(°C)
    * 模組輸出電壓   
* 模型訓練
  * 使用xgboost模型進行  
    * learningrate: 0.001
    * max_depth=3
    * min_child_weight=4
    * nrounds = 6000
  * 預測並計算RMSE

### **結果**
* public Leader board
  * RMSE: 253.76240
  * Rank: 58
* priviate Leader board
  * RMSE:160.09821 
  * Rank: 63

### **心得**
* 以我自己第一次用參加比賽來說，能達到主辦單位設定的目標`RMSE<260`，我就已經很開心ㄌ><
* 這次實作因為時間因素並沒有用到任何外部資料，我覺得有加入外部資料(eg. 空氣品質，晴天陰天)的話，排名應該是可以繼續往前的~~~~~~~~~
* 但查詢了許多資料，至少我也更熟悉一些機器學習或是資料處理ㄉ技巧
