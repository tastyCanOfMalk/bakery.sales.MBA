# EDA and MBA of bakery transaction data
This is my first attempt at analysis of a Kaggle.com dataset.

## Links
The original dataset was found here: https://www.kaggle.com/sulmansarwar/transactions-from-a-bakery

With my Kaggle Kernel located here:
https://www.kaggle.com/tastycanofmalk/bakery-sales-data-exploration

And this link to this repository is here: https://github.com/tastyCanOfMalk/bakery.sales.MBA

## Analysis
The analysis starts with an exploratory data analysis of the transaction data, determining busiest work hours based on line items sold, unique transactions, and finally visualizing transaction density by time of day and week.

We then move on to the market basket analysis: choosing values of support and confidence, viewing our rules, and finally visualizing some of them.

## Conclusions
Based on the data we can make some generalized assumptions about scheduling and number of staff required based on time of day as well as what day of the week it is. This information was probably already well known to the staff.

Looking more closely ar numbers:

{Cake} {Pastry}, and {Sandwich} => {Coffee} all have very high **support**, which means they are frequent transactions.

{Cake, Sandwich} and {Toast} => {Coffee} have high **confidence**, which means any purchase of {Cake, Sandwich} or {Toast} is likely to include {Coffee}.

{Cake, Sandwich} and {Toast} => {Coffee} also have high **lift**. Higher lift values ensure that the relationship is not random and that interaction between the items is happening.

We also know that two-item transactions are the norm. Of course, three-item purchased would be better and increase line items sold. How can we push two-item purchasers to purchase three-items? 

If we can encourage {Cake, Sandwich} purchases we're more likely to get the {Coffee}. One relatively straightforward line of action is to create a lunch combination or special which encourages purchasing {Cake, Sandwich} to increase the likelihood of a {Coffee} purchase. Alternatively offering a {Cake, Sandwich, Coffee} combination would also be an option.

There are many other lines of action that could be implemented to increase sales or increase customer satisfaction. This would require some "on-the-ground"" insight.

## Visualizations
Below are some visualizations produced via the analysis. For a more detailed "follow-along" analysis, please take a look at the Kaggle kernel linked above.

![x1](/images/x1.png?raw=true "x1")

![x2](/images/x2.png?raw=true "x2")

![x3](/images/x3.png?raw=true "x3")

![x4](/images/x4.png?raw=true "x4")

![x5](/images/x5.png?raw=true "x5")

![x6](/images/x6.png?raw=true "x6")

![x7](/images/x7.png?raw=true "x7")

![x8](/images/x8.png?raw=true "x8")

![x9](/images/x9.png?raw=true "x9")

![x10](/images/x10.png?raw=true "x10")

![y1](/images/y1.png?raw=true "y1")

![y2](/images/y2.png?raw=true "y2")

![y3](/images/y3.png?raw=true "y3")

![y4](/images/y4.png?raw=true "y4")

![y5](/images/y5.png?raw=true "y5")

![y6](/images/y6.png?raw=true "y6")

![y7](/images/y7.png?raw=true "y7")
