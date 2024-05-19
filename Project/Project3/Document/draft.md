我们看到了关于Part变量对于highcars变量的影响。β-estimate 是 0.83，这表示当 Part 变量增加一个单位时，因变量发生的对数几率（log-odds）会增加 0.83 个单位。在这种情况下，e^0.83 表示这个事件的几率比例（Odds Ratio）。我们可以得到e^0.83 等于 2.3，那么这意味着因变量的几率在 Part 变量增加一个单位时会增加到 2.3 倍。

通过观察表格1（a），我们发现当Part增加一个单位时，highcars变量会增加到原始几率的2.3倍。

使用wald_test对model 1b 进行测试，结果如表1所示，根据Pr < 0.05、z > 1.96的标准，我们发现part和highcars之间存在显著性

根据odd-ratio可以计算出当因变量（Transit）增加1percent时，因变量会变成原本的0.93倍

1. **McFadden's adjusted pseudo R\textsuperscript{2}（R\textsuperscript{2}McF.adj）**：这个指标用于衡量模型的拟合优度。数值越接近1，说明模型对观测数据的解释能力越好。
2. **AIC（Akaike Information Criterion）**：AIC是一个综合性的指标，结合了模型的拟合优度和复杂度。AIC值越小表示模型在拟合数据时所使用的信息量越少，说明模型更好。
3. **BIC（Bayesian Information Criterion）**：BIC也是一个综合性的指标，类似于AIC，但对于样本量较小的情况更为有效。与AIC类似，BIC值越小表示模型的质量越好。
4. **log-likelihood（对数似然）**: 通常用于评估统计模型的拟合优度，数值越大表示模型拟合数据的能力越好。可以计算两个模型的 log-likelihood 差异，然后观察哪个模型的 log-likelihood 更高。通常情况下，差异越大，log-likelihood 较高的模型更优。

综合考虑这几个指标，可以通过比较它们的数值来判断哪个模型更好。通常情况下，选择具有更高McFadden's adjusted pseudo R\textsuperscript{2}和更小AIC、BIC、log-likelihood的模型作为更好的模型。



我们可以看到 Model 1(c) 的AIC、BIC比 Model 1(b) 的AIC、BIC更小，而R\textsuperscript{2}McF.adj和log-likelihood更高，说明Model 1(c)比Model 1(b)更好。witch indicate that access to a bus stop seems more important .



通过观察table3b我们可以发现 model2b的auc值大于0.9且与aic模型，full模型的auc值相近，那么我们可以认为model2b是一个性能很好的模型，再参考其他性能指标，与2b得到结论的一致。

Sensitivity 变大

McNemar’s P-Value 变小

对比table3a 和table3c后发现，一个有趣的现象是，调整阈值之后，虽然acc变小，但是 Sensitivity 变大，且model2b是Sensitivity 最大的模型，这证明了model2b是最好的。



考虑所有结果，我们选择2b（bic 结果）作为最佳的模型。以下为我们考量的几个方面：

参数量最小

confusion matrix：aic 、full 、2b 的准确率相当，但是model2b的Sensitivity 最高




基于所有结果，我们选择模型2b（bic结果）作为最优模型，选择的几个理由如下：

1. **参数量最小**：模型2b拥有最少的参数数量，往往意味着模型的泛化能力更强，表明这是一个更加简洁的模型，避免了过拟合的同时保持良好的预测性能。
2. **混淆矩阵中的表现**：虽然模型aic、full和2b的准确率相当，都达到了80%以上。
3. **高敏感性**：模型2b的敏感性得分高于其他模型，显示了其准确分类正类的能力。当假阴性的成本很高时，这一点尤其宝贵，因此使得模型2b在比较的模型中成为最可靠的选择。

这些因素共同确立了模型2b作为考虑的模型中的最佳选择，平衡了复杂性、性能以及正确识别正例的能力。

Urban

城市居住在城市地区的人口的比例大表明人口密度较高，公共交通发达，所以与车辆拥有率之间可能存在负相关关系。

公寓数量（Apartments）:

一般来说，一个区域内公寓数量较多表明人口密度较高，且收入较低。在公共交通设施较好的高密度城市区域，居民可能较少依赖私人车辆，因此公寓数量与车辆拥有率之间可能存在负相关关系。

Persperhh



每户的人数越多，意味着更多人共用车辆。这表明每户人数与车辆数量之间可能存在负相关

收入（Income）:

通常，较高的收入水平使得更多家庭有能力并且实际上也拥有更多的车辆。较富裕的个人和家庭往往居住在车辆更为必需且拥有车辆更为可行的地区。这表明收入与车辆数量之间可能存在正相关



通过分析图1中的HIghcars数据的分布特征，HIghcars数据点在Transit为0到65之间分布较为密集，特别是在低Transit（0到25）和中等Transit数量（40到65）处。

根据leverage的特性，通常，数据点距离均值越远，其leverage值越高。

- **0附近的突起**：在图一中，公交车站数量较少（0到25）时，有大量数据点值为高汽车拥有率（y=1）。这些数据点距离均值较远，导致leverage值较高，形成了图二中左侧的突起。
- **65附近的突起**：在图一中，公交车站数量在40到65之间的数据点分布较为密集，同时逐渐过渡到低汽车拥有率（y=0）的情况。这些数据点由于集中分布且逐渐变化，在65之后逐渐没有HIghcars的数据点分布，导致leverage值在65附近再次上升，形成了图二中右侧的突起。
