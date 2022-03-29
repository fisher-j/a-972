## 03/28/2022

* Calculated sh-BA
* put all structural diversity figures together
* began to look at changes dominant species

## 03/27/2022

* Finished importing and cleaning understory cover data
* Fixed Shannon-Weaver index for trees: its back to being non-significant
* I realized that I can't calculate Sh-W for understory: would need counts or cover by species and this was not collected.
* Made basic graphs depicting:
  - change in number of understory species
  - change in percent cover of herbs and shrubs

## 03/25/2022

* Cleaned up species list data


## 03/24/2022

* Switched to using least square means for mean and confidence interval
* Import and begin checking out veg data
* Calculate mean directional index

## 03/23/2022

* extracted a function to calculate DBH differentiation

## 03/21/2022

* Monday meeting tasks:
  * calculate DBH differentiation
  * Look at veg data, assess for Shannon index, or other analysis

## 03/15/2022

* Calculated MDI


## Monday meeting

* Calculated Shannon-Weaver H
* update graphs of SCI
* We talk about:
  * Calculating DBH~sd~, MDI, Hui Dominance Index
  * Calculate means using mixed linear model and emmeans

## 03/13/2022

* Reading Keuhne paper

## 03/11/2022

* I realized there was an error in my calculation of SCI. Unfortunately the new values are half of what I had calculated previously which makes differences between treatments even smaller than before.

* Confirmed with Dr. Zenner about SCI calculation terminology

## 03/09/2022

* Added [summary table](06_summary.html#dominance-figure) of average values associated with the dominant redwood and Douglas-fir
* Made adjustments to mean BAI comparisons bar graph: axes, letters, error bars, legend
* Started working on SCI, graphing trajectories, and simplifying table

## 03/08/2022

* Made BAI model table with AICc, Delta AIC, AIC weights, RMSE, and R^2^
* Fixed figures:
  - summary fig: legend and axis lables
  - dominance fig: legend, scale, and axis labels, adjusted BAI bars
  - BAI predicted means: legend and axis labels, rescaled inset (to try it out) 

## 03/04/2022

* added visualization of SCI to website
* Added more emmeans output for BAI model
* created bar chart showing compact letter display of significantly different estimates

## 03/03/2022

* Most of today was spent considering if average initial conditions varied significantly among treatments.
* I conducted anova followed by Tukey post-hoc tests as well as Kruskal-Wallis followed by both Dunn and Wilcox post-hoc tests.
* Here again, I'm wondering what the conceptual difference is between comparing averages between plots (n=4) and aggregating trees in treatments (n=number of trees). Statistical tests are difficult with n=4, but is it appropriate to aggregate trees from different plots?
* Unless we can use a more powerful test (all trees in a treatment?) only heights seem to have marginal significance, and this may not be entirely important to us, given the already questionable height data.


## 03/02/2022

* Added updated regression tables for ht and ba increment to shared document
* Added estimated marginal means graph with CR * year interaction
* Tried to specify the model form in the caption for the regression table
* Calculated SCI_dbh_ for each treatment/year

## 03/01/0222

* Worked on figuring out how to graphically plot SCI
* Read some of Zenner's dissertation on the relevance of SCI:
  - it is correlated with other stand metrics: BA, density
  - it is not correlated with horizontal stem distribution: clumped or even

## 02/28/2022

* Presented new BA increment model in meeting
* Change proportion dominant figures to include initial conditions
* Fixed [non parametric test function](06_summary.html#species-dominance-significance-test) for comparing change in redwood dominance among treatments and included output.
* created composite plot: [dbh and cr](05_analyzing_ba_increment.html#estimated-marginal-means-and-regression-table-of-selected-model)
* changed regression output tables to:
  * include: SE, P-val
  * remove: ICC
  * did this for [ht increment model regression table](04_pred_ht.html#best-model-scrapping-above) as well.

## 02/27/2022

Started work journal

* Finished initial BA increment model analysis
  * Produced emmeans comparisons
  * emmeans predicted growth
  * regression outputs
  * [model validation](05_analyzing_ba_increment.html#model-validation)
* Updated website with new content
  * required various bug fixes
