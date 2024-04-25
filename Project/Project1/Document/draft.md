2e

 Fit a new model using the new categorisation from 2(c) and all the continuous variables，GVIF值由表\ref{2e-1}所示。

GVIF或广义方差膨胀因子是VIF的一种推广，用于处理包含分类预测变量（因子变量）的情形。当模型中包括多级分类变量时，传统的VIF计算可能不适用或不足以提供准确的多重共线性度量。

由于2d中我们采用的都是连续变量，而我们在新模型中引入了2c中得到的NewParts，这是一种多级分类变量，所以 they are now GVIF values。

Builton 是最有问题的，他的GVIF values异常的高。Builton 代表着“Area covered in buldings, roads, etc (= not nature), (hectares / 1000 inhabitants)”，也就是城市的面积和繁华程度。我们认为他出问题的原因是和多个变量存在很强的线性相关性，比如log（Vehicles ）、BRP。因为一个城市很繁华往往意味着有更多的车辆，更高的收入。

Seniors也是有问题的，Seniors代表着65+ year olds (percentage)。Seniors明显和Children 变量（0–14 year olds (percentage)）存在着很强的负相关关系。

删除Builton和Seniors变量，拟合模型model2e并测试GVIF，结果\ref{2e-2}所示。可以看到筛选后的模型各变量的GVIF大小都是可以接受的。





3a

杠杆效应定义： 在线性回归分析中,数据点对参数估计的影响程度概念是很重要的。定义为投影矩阵(也就是帽矩阵)的对角元素,杠杆分数可以看出某个观测值相对于独立变量的中心点有多远。高杠杆点表明这些观测值可能会对回归线产生不成比例的影响,从而可能扭曲模型的预测。识别这些点对模型诊断很重要,因为它们可能代表需要进一步检查的异常值或影响观测值。

通过计算leverage，我们得到如下图所示的leverage-yhat的分布图，添加的参考线为1/n，2*p+1/n。

接着我们对所有数据的leverage值进行排序，我们得到了基于*Model 2(e)* ，leverage最大的六组数据，如\ref{3a}所示。 

为了探究他们的leverage值为什么这么大，我们分别将模型中各变量和log（PM10）作图，根据NewParts的分类分成三小图。在途中高亮显示这些top-leverage的数据并标注了Kommun。

通过观察这些图像，我们发现在“NorrlandandNo”这个part中，Gällivare和Kiruna在每张图中都属于离群值。Sundbyberg在"SvealandandNo"也属于离群值。这些离群值的存在会对回归线产生较大的影响。

Danderyd Solna Lund 这三个数据并没有出现分布异常的现象，但是在log(Higheds)的图中这三个数据点分布很接近，在log(Vehicles)中分布也很接近。 考虑到log(Higheds)和log(Vehicles)这两个自变量存在着多重共线性，可能增加了数据的杠杆值。



3b

Cook距离是一个强大的度量,它结合了杠杆效应和预测误差信息,给出一个观测值影响的综合指标。通过量化某个观测值被排除时整个模型拟合值的变化,Cook距离可以全面评估单个数据点对模型的影响。高Cook距离的观测值表明它可能对回归模型起决定性作用,需要进一步调查是否应该包含在数据集中。

通过计算Cook‘s distance，我们得到如下图\ref{3b}所示的Cook‘s distance-yhat的分布图，添加的参考线为4/n (dashed), F_0.5, p+1, n-(p+1) (solid)

接着我们对所有数据的Cook‘s distance值进行排序，我们得到了基于*Model 2(e)* ，Cook‘s distance最大的六组数据，如\ref{3b}所示。

然后我们调查DFBETAS。DFBETAS代表“系数差异”,指在含有和不含某个观测值的情况下各系数估计的差异。每个DFBETAS值经过标准化处理,提供了观测值对个别回归系数影响的标准化度量。大的DFBETAS值可能表明对应观测值对某个预测变量是异常值,有助于识别和处理影响数据点。

我们plot log-PM10 against the corresponding variable(s), highlighting the influential municipalities,并与 log(PM10) against 各corresponding variable(s)的 β-parameter(s)所对应的DFBETAS值的图像进行对比，如图\ref{3b-plot}。观察图像，我们发现DFBETAS和Cook‘s distance较高的数据往往都是离群值，分布于其他数据较远，有可能是异常数据。

有些municipalities 和 high leverage 的不同了，因为 Leverage评估观测值对参数估计的影响,Cook's distance评估观测值对模型拟合的影响。Cook's distance考虑的范围更广,可以识别对模型拟合影响更大的异常点。但Leverage也是重要指标,可以识别可能是异常点的观测值。



3c

标准化残差是回归分析中用于识别异常值的残差形式。与简单残差的差值不同,标准化残差根据估计的标准差进行缩放,不同观测值的标准差不同。这种标准化过程使所有残差可以用一个阈值来识别异常值。绝对值大于3或-3的标准化残差通常被视为异常,需要进一步检查其是否有杠杆效应或对模型产生影响。

通过计算Studentized residuals，我们得到如下图\ref{3c}所示的Studentized residuals-yhat的分布图，添加的参考线为4/n (dashed), F_0.5, p+1, n-(p+1) (solid)

接着我们对top Cook's distanced数据的Studentized residuals值进行高亮。他们的residual，如Table\ref{3c}所示。

然后我们得到了所有*|**r**i* *∗* *|* *>* 3的municipalities。如表\ref{3c-2}所示，其中Kalix、Karlshamn和Mönsterås没有很高的Cook's distance值。

同样的，我们Plot *|**r**i* *∗* *|* against the linear predictor，如图\ref{3c-2}所示，参考线为y = sqrt(0.75 quantile of normal), sqrt(2), sqrt(3)"。方差看起来是恒定的。



3d

根据所提供给我们的资料我们了解到PM10的主要来源是国内运输，占瑞典排放量的40%，这也就解释了为什么车辆的数量和PM10的相关性很高。瑞典PM10排放的第二大来源是工业，占瑞典排放量的32%。这一项在数据的特征中中没有体现，而且我们更关注的是人均PM10排放，而不太关心一个小市镇是否恰好有一家大型PM10排放企业。所以我们要删除一部分数据。根据表格和Plot sqrt(|r*|) vs refitted values 筛选数据，以此删除以下Kommun："0481 Oxelösund", "1082 Karlshamn", "0861 Mönsterås",
                           "2523 Gällivare", "2514 Kalix", "2584 Kiruna",
                           "1480 Göteborg", "1761 Hammarö", "1484 Lysekil",
                           "1494 Lidköping", "1882 Askersund", "2284 Örnsköldsvik",
                           "0319 Älvkarleby", "1460 Bengtsfors",  "1781 Kristinehamn", 
                           "2262 Timrå",  "0980 Gotland", "1272 Bromölla",  "1885 Lindesberg","1764 Grums"，得到refit模型model_3d。Plot sqrt(|r*|) vs refitted values 如图\ref{3d}所示。

将model_3d（参数如表\ref{model_3d}所示，测试指标如表\ref{model_3d}所示）与model_2e（参数如表\ref{model_2e}所示，测试指标如表\ref{model_2e}所示） compare how well the assumptions of normality and constant variance of the residuals。

通过观察Q-QPlot和residual-fitted values Plot,如图\ref{3d-compare}， model_3d的残差基本符合正态分布，model_2e存在偏移，不符合正态分布。



小组分工：

张亦非负责问题分析，代码撰写，report part3撰写

许嘉璐负责问题分析，代码撰写和审核，report part 1&2 撰写







首先，我想对你们的报告表示衷心的赞扬。在阅读你们的报告时，我感受到了你们的专业素养和团队合作精神。

你们对问题的深入分析展现了高水平的思考能力和对课题的透彻理解。你们提出的问题准确、清晰，为后续问题研究提供了坚实的基础。

报告的结构清晰、条理分明，使读者能够轻松理解整个研究过程。每个部分都紧密联系，呈现出了完整的思路。

你们运用了多种方法和工具对数据进行了全面分析，展示了对数据的充分挖掘和利用。分析结果详实、可信，为后续的结论提供了有力支撑。

报告中的图表设计精美，图文并茂，生动直观地展现了研究结果。这些视觉效果不仅美观，而且有助于读者更好地理解和消化信息。

你们团队之间的默契配合和高效沟通在报告中得到了充分体现，每个人都发挥了自己的优势，共同完成了这份出色的工作。

但是针对报告的内容，我还要提出几点建议。在第三部分的部分图像中，可以将城市名标注在图像上，以便更好的观察这些特殊的数据。



Here's a template for a report evaluation that includes praise, tailored to meet all the points outlined in the guide you provided:

---

**Report Evaluation Template**

**Overall Impression:**
Your report is a testament to meticulous research and comprehensive analysis. It is evident that a great deal of effort and thought has been invested in your work.

**Highlights of the Report:**
- **Content Organization:** The way you structured the sections – from introduction to conclusion – made the report exceptionally reader-friendly. The logical flow allowed me to follow your arguments and evidence seamlessly.
- **Quality of Analysis:** The depth of your analysis is commendable. You did not merely scratch the surface but delved into the intricacies of the subject matter, showcasing a well-rounded understanding.
- **Presentation and Visuals:** The graphical representations were not only informative but also aesthetically pleasing, adding significant value to the content.
- **Methodology:** The methods you selected for your research were both robust and appropriate, reflecting your ability to tailor your approach to the needs of the study.

**Specific Comments:**
- **Clarity and Precision:** Each point you made was articulated clearly and supported by concrete evidence. Your precision in this regard is noteworthy.
- **Constructive Questions:** The questions you posed throughout the report were thought-provoking and added to the critical evaluation of your research.
- **Data Interpretation:** You demonstrated a high level of competency in interpreting the data and drawing relevant conclusions that are well-supported by the evidence.
  

**Discussion and Conclusion:**
- **Insightful Discussion:** The discussion section is rich with insights and shows a strong connection between your findings and the broader context of the field.
- **Sound Conclusion:** Your conclusions are drawn logically from your data and discussion, providing a satisfying closure to the report.

**Suggested Improvements:** 
While your report is strong in many areas, I would encourage you to consider the following for further enhancement:
- **City Labeling in Graphs:** Labeling the cities directly on the graphs could facilitate an even easier interpretation of the data.
- **Exploration of Variances:** A deeper exploration of the variances within your data could enrich the analysis.

**整体布局与结构：**

- 你们的报告拥有一个明确的**标题页**，上面包含了报告的标题、作者姓名、日期以及页码，这为读者提供了良好的第一印象。
- 报告中的**简介、结果和结论部分**结构完善，信息充实，展现了研究工作的完整性。
- 显而易见，报告已经过了详尽的**校对**，语言和拼写错误已经得到了修正，展现了你们对细节的关注。
- 所有的**图表**都编号规范，配有合适的标题，并在文中得到了恰当的引用。这些图表不仅为数据提供了清晰的视觉表现，而且增强了报告的可读性。

**内容组织：**

- 报告内容条理清晰，逻辑流畅。通过使用明确合适的**章节标题**，报告的结构被划分得井井有条，每一段都聚焦于核心议题，有效地引导读者深入理解。
- 即使没有项目描述，报告的表述也足够清晰，易于理解。这说明了你们在确保报告自洽方面做得非常出色。

**内容完整性与相关性：**

- 你们完整地完成了所有的任务，并且所有的**图表内容**都与主题紧密相关，为读者提供了有价值的信息。
- 所有使用的**符号和术语**都有适当的引入和解释，确保了读者对材料的准确理解。
- 你们对**模型**的介绍清晰明确，详细地展示了其作用和重要性，这对理解整个分析框架至关重要。





Overall Layout and Structure:

Your report boasts a clearly defined title page, featuring the report's title, authors' names, date, and page numbers, providing readers with a positive first impression.
The introduction, results, and conclusion sections of the report are well-structured and informative, displaying the completeness of your research efforts.
It is evident that the report has undergone thorough proofreading, with language and spelling errors having been corrected, demonstrating your attention to detail.
All figures and tables are properly numbered, equipped with suitable captions, and appropriately referenced in the text. These visual elements not only provide a clear visual representation of the data but also enhance the report's readability.

Content Organization:

The content of the report is well-organized and logical. The use of clear and appropriate section headings divides the structure neatly, with each paragraph focusing on core issues, effectively guiding the reader to a deeper understanding.
The report's narrative is clear and understandable, even without access to the project description, indicating your excellent work in ensuring the report's consistency.

Content Completeness and Relevance:

You have completed all the tasks at hand, and all figures and tables are closely related to the theme, providing readers with valuable information.
All notations and terms used have been properly introduced and/or explained, ensuring readers' accurate understanding of the material.
Your introduction of the model is clear and precise, detailing its function and significance, which is crucial for understanding the entire analytical framework.

