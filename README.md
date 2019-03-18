# A new LoanDefaultPredict Project learned from TianshanZhineng
1.业务理解
涉及5300个银行客户的100万笔的交易，以及700份贷款信息与近900张信用卡的数据。通过分析这份数据可以获取与银行服务相关的业务知识。例如，风险管理的业务人员可以及早发现贷款的潜在损失。
2.数据介绍
业务中的一个人可以拥有多个账户号（account_id），一个账户号（account_id）可以对应有多个顾客（client_id），即多个顾客共享一个账户号（account_id），但是每个帐户号（account_id）的所有者（即最高权限者）只能是一人。账户号（account_id）与客户号（client_id）的对应关系，在表“Disp”中进行列示；表“Loan”为银行提供给账户号（account_id）的服务；表“Credit card”为银行提供给的顾客（client_id）的服务，每个客户可以申请一张信用卡；贷款为基于账户的服务，一个账户（account_id）在一个时点最多只能有一笔贷款。
        
●账户表（Accounts）：每条记录描述了一个账户（account_id）的静态信息，共4500条记录；
  
●顾客信息表（Clients）：每条记录描述了一个客户（client_id）的特征信息，共5369条记录；
  
●权限分配表（Disp）：每条记录描述了顾客（client_id）和账户（account_id）之间的关系，以及客户操作账户的权限，共5369条记录；
  
●支付订单表（Orders）：每条记录代表描述了一个支付命令，共6471条记录；
  
●交易表（Trans）：每条记录代表每个账户（account_id）上的一条交易，共1056320条记录；
   
●贷款表（Loans）：每条记录代表某个账户（account_id）的上的一条贷款信息，共682条记录；
 
●信用卡（Cards）：每条记录描述了一个顾客号的信用卡信息，共892条记录；
 
●人口地区统计表（District）：每条记录描述了一个地区的人口统计学信息，共77条记录。
 

##加载所需包
library(reshape2)
library(VIM)
library(DMwR)
##导入数据并检查存在缺失值的表
myFiles <- list.files(getwd(), pattern = ".csv")
naFile <- NULL
for(file in myFiles)
    {
        perpos <- which(strsplit(file, "")[[1]]==".")
        cleanName <- gsub(" ","",substr(file, 1, perpos-1))
        assign(
          cleanName,
          read.csv(paste(getwd(),file,sep="/"), stringsAsFactors = F))
        if (sum(is.na(get(cleanName[1]))>0) & is.null(naFile))
          {
                naFile <- cleanName
        }
        else if (sum(is.na(get(cleanName[1]))>0))
          {
            naFile <- paste(naFile, cleanName,sep = ",")
        }
    }

ifelse(nchar(naFile)>0, paste("有缺失值的表为:", naFile), "没有缺失值")

 
3.数据整理
3.1导入数据后发现district和trans表有缺失值，处理前先查看具体缺失情况：

aggr(district, prop = FALSE, numberS = TRUE)
district[!complete.cases(district),]

aggr(trans, prop = FALSE, numbers = TRUE)
sum(is.na(trans))
 
 
从图表可以发现district表缺失不是很严重，只有第69个观测中的A12（1995年失业率）和A15（1995年犯罪率）变量各一个值，而trans表相对比较严重，幸运的是account变量在后面分析中并不需要考虑，所以可以忽略不处理。

使用DMwR包中的knnImputation（）函数处理district表的缺失值：

district_new <- knnImputation(district)

3.2 从各个表中提取所需数据，构建建模用宽表，包含被解释变量和解释变量。
#·································································································································
#在贷款（Loans）表中还款状态（status）变量记录了客户的贷款偿还情况，其中A代表合同终止且正常还款，B代表合同终止但是未还款，C代表合同未结束且正常还款，D代表合同未结束但是已经拖欠贷款。我们可以把已经欠款的客户和合同结束但没有欠款的客户（B、D和A）作为被解释变量，建立量化模型，然后对合同还没结束且没有欠款的客户(C)进行预测，看这些客户谁有更大的可能性出现违约。
#·································································································································

##利用loans表创建因变量bad_good

loans <- within(loans, {
                bad_good <- NA
                bad_good[status == 'B'] <- 1
                bad_good[status == 'D'] <- 1
                bad_good[status == 'A'] <- 0
                bad_good[status == 'C'] <- 2
                })

#·································································································································
#一般违约有还款意愿不足和还款能力不足两方面，因此可从客户的表征信息、行为信息和状态信息等维度整理和构建有预测价值的解释变量。
#·································································································································

##表征信息维度：如年龄、性别等人口信息，需要从clients表获取并与loans表信息整合。

loans_P <- merge(loans, disp, by = "account_id", all.x = T)
loans_P <- merge(loans_P, clients, by = "client_id", all.x = T)

##状态信息维度：如居住地失业率等居住地信息，需要从distric表获取并与Population_Inf表信息进一步整合。

loans_P_D <- merge(loans_P, district_new, by.x = "district_id", by.y = "A1", all.x = T)

##行为信息维度：根据客户的帐户变动的行为信息，考察借款人还款能力，如账户平均余额、余额的标准差、变异系数、平均入账和平均支出的比例、贷存比等。这些属于衍生变量，需要通过直接变量计算转换得到

##将贷款表和交易表按照account_id内连接建立新数据框。

loans_P_D_A_T1 <- merge(subset(loans,select = c("account_id","date")),subset(trans, select = c("account_id","type","amount","balance","date")), by = "account_id")
names(loans_P_D_A_T1) <- c("account_id","date","type","amount","balance","t_date")

##两个字符串类型的日期变量转换为日期。

attach(loans_P_D_A_T1)
loans_P_D_A_T1$date <- as.Date(date)
loans_P_D_A_T1$t_date <- as.Date(t_date)
loans_P_D_A_T1 <- loans_P_D_A_T1[order(account_id,t_date),]
detach(loans_P_D_A_T1)

##帐户余额和交易额度这两个为字符变量，有千分位符，需要进行数据清洗，并转化为数值类型。

loans_P_D_A_T1 <- within(loans_P_D_A_T1,{
                              balance2 <- NA
                              amount2 <- NA
                              amount2 <- as.numeric(gsub(",","",gsub("$","",amount,fixed = T),fixed = T))
                              balance2 <- as.numeric(gsub(",","",gsub("$","",balance,fixed = T),fixed = T))
                          })

##实现窗口取数，只保留贷款日期前365天至贷款前1天内的交易数据。

loans_P_D_A_T2 <- subset(loans_P_D_A_T1,t_date <= date & t_date > (date - 365), select = account_id : balance2)

##构建每个贷款帐户贷款前一年的平均帐户余额（代表财富水平）、帐户余额的标准差（代表财富稳定情况）和变异系数（代表财富稳定情况的另一个指标）变量。

attach(loans_P_D_A_T2)
avg_balance <- aggregate(balance2, by = list(account_id), FUN = mean, na.rm = T)
stdev_balance <- aggregate(balance2, by = list(account_id), FUN = sd, na.rm = T)
detach(loans_P_D_A_T2)
names(avg_balance) <- c("account_id","avg_balance")
names(stdev_balance) <- c("account_id","stdev_balance")
loans_P_D_A_T3 <- merge(avg_balance, stdev_balance, by = "account_id")
loans_P_D_A_T3 <- within(loans_P_D_A_T3,{
                          cv_balance <- NA
                          cv_balance <- as.numeric(stdev_balance / avg_balance)
                          })

##基于取数窗口的数据集，按照每个帐户的“借-贷”类型分别汇总交易金额并建立支出收入比变量。

loans_P_D_A_T2 <- within(loans_P_D_A_T2, {
                            typeNew <- NA
                            typeNew[type == '借'] <- "out"
                            typeNew[type == '贷'] <- "income"
                          })

attach(loans_P_D_A_T2)
loans_P_D_A_T4 <- aggregate(loans_P_D_A_T2[7], by = list(account_id=account_id, typeNew=typeNew), FUN = sum, na.rm = T)
detach(loans_P_D_A_T2)

loans_P_D_A_T4 <- dcast(melt(loans_P_D_A_T4, id = c("account_id","typeNew")), account_id ~ typeNew + variable, fill = 0)

names(loans_P_D_A_T4)[c(2,3)] <- c("income","out")
loans_P_D_A_T4$r_out_in <- loans_P_D_A_T4$out / loans_P_D_A_T4$income

##将分别构建的平均帐户余额、帐户余额的标准差、变异系数、平均入账和平均支出的比例等变量与之前的loans_P_D数据合并。

loans_P_D_A <- merge(loans_P_D, loans_P_D_A_T3, by = "account_id", all.x = T)
loans_P_D_A <- merge(loans_P_D_A, loans_P_D_A_T4, by = "account_id", all.x = T)

##建立贷存比、贷收比变量。

loans_P_D_A$r_lb <- loans_P_D_A$amount / loans_P_D_A$avg_balance
loans_P_D_A$r_lincome <- loans_P_D_A$amount / loans_P_D_A$income
	
4.构建模型
数据按照想法准备好后，可以进行建模分析，而预测账户的违约情况可看作是分类,而分类模型中逻辑回归最常用。

##提取状态为C的用于预测。其它样本随机抽样，建立训练集与测试集。

data_model <- subset(loans_P_D_A, status!= "C")
data_for_predict <- subset(loans_P_D_A, status == "C")
set.seed(1235)
data_for_train <- data_model[sample(1:nrow(data_model), 0.7*nrow(data_model)),]
data_for_test <- data_model[!(row.names(data_model) %in% row.names(data_for_train)),]

##用逐步回归法建立预测模型

lg.step <- step(glm(bad_good ~ ., data = data_for_train[candidates], family = binomial()))
summary(lg.step)

查看拟合描述，其中申请贷款前一年的贷存比（r_lb），存款余额稳定情况（cv_balance）回归系数非常显著，且与违约呈正相关，符合预期。
 
构建模型过程中出现警告:glm.fit:拟合几率算出来是数值零或一
	 	
计算建模数据的预测概率并观察发现有一个观测account_id：442拟合概率为1，与世纪违约情况相符，应该算拟合得很好。或者可以看作一个离群值，去除离群值再拟合一次，警告消失，并得到同样的结果：

data_select <- subset(data_for_train[candidates], r_lb > 50)
lg.step2 <- step(glm(bad_good ~ ., data = data_select, family = binomial()))
summary(lg.step2)
 
5.评价模型有效性
通过混淆矩阵评价模型有效性
#评估模型有效性

data_for_test$prob <- predict(lg.step2, data_for_test, type = "response")
data_for_test$pred <- ifelse(data_for_test$prob >= 0.5, 'yes','no')
data_for_test$pred <- factor(data_for_test$pred, levels =c('no','yes'), order = TRUE)
f <- table(data_for_test$bad_good, data_for_test$pred, dnn = c("Actual","Predicted"))
f

      Predicted
Actual	no	yes
  0	73	5
  1	8	15

结果显示，模型的准确率为87.1%【(73+15)/(73+15+5+8)】，效果较好。

6.模型的运用
贷款状态为“C”的帐户是尚没有出现违约的合同未到期客户。这些客户中有些人的违约可能性较高，需要业务人员重点关注。一旦发现问题时，可以及时处理，挽回损失。
#计算C客户的违约概率
data_for_predict$prob <- predict(lg.step2, data_for_predict, type = "response")
attach(data_for_predict)
head(data_for_predict[c("account_id","prob")])
detach(data_for_predict)
需要强调的是，此处的“概率”仅是代表违约可能性的相对值，并不代表其真实违约概率。比如预测“概率”为0.77的违约可能性高于0.46，这已经足够了。
