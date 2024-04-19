---
title: "Developing Measures of Public Perceptions of Values in Science"
author: 
    - name: Daniel J. Hicks
      orcid: 0000-0001-7945-4416
      email: dhicks4@ucmerced.edu
      affiliations:
          - ref: ucm
      attributes:
          corresponding: true
    - name: Emilio Lobato
      orcid: 0000-0002-3066-2932
      email: elobato@ucmerced.edu
      affiliations: 
        - ref: ucm

affiliations: 
    - id: ucm
      name: University of California, Merced
      address: 5200 N Lake Road
      city: Merced
      state: CA
      postal-code: 95343
    
date: Typeset \today
abstract: |
	abstract

<!--bibliography: ../bibfile.bib-->
---

<!--
PUS submission guidelines: <https://journals.sagepub.com/author-instructions/PUS>
Research Article (max. 8,000 words and 5 figures or tables)
Research Note (max. 4,000 words and 5 figures or tables)

OA agreement: library covers first $1k, 20% off remaining APC
-->


# Introduction #

Historians, philosophers, and sociologists of science who study public scientific controversies often put forward explanations for these controversies that refer to views of publics on the relationship (actual or normative) between science, values, and policy.  *[examples]*

However, in the citations above, there is little empirical evidence to support key claims about the views of publics.  Historians are carefully tracking the activities of scientists or industry actors, and assume, for example, that public acceptance of climate science or policy depends on perceptions of a scientific consensus *[Oreskes]*.  The sociologists cited above are providing rich accounts of particular cases, rooted in ethnographic fieldwork.  But these cases don't obviously generalize to other cases or the general public.  And, at best, philosophers are candid about offering speculation and empirical research questions.  *[notable exceptions are Weisberg et al. and Elliott et al.]*

The aim of the current study is to begin to develop a Values in Science Scale (VISS) that could be used to measure public views on the relationship between science, values, and policy.  While some intriguing patterns are identified, we also find evidence that more work needs to be done to refine the wording of the prompts used.  


# Materials and Methods #

## Values in Science Scale Items ##

Draft items for the Values in Science Scale (VISS items) were prepared by [author 1], a philosopher of science specializing in these issues.  [author 1] reviewed the instrument used by *[Weisberg et al.]* along with the text of *[Kovaka]* to create an initial list of items.  These items were grouped into "areas," corresponding to views and ideas discussed in the philosophy of science literature.  Each area was given 3 items.  Additional areas were added, with items written by [author 1], to cover a wide range of views and ideas from the philosophical literature.  Importantly, we did not hypothesize that responses within each area would be correlated.  On the contrary, we expected that the views of the general public would not line up with the positions defended or critiqued by philosophers.  

Each item was a descriptive sentence of approximately 15-20 words, and participants were asked to agree or disagree with each item on a 5-point Likert scale. *[do better words]*

A total of 36 items, for 12 areas, were used in the current study.  These items, their areas, and short labels used in reporting results, are shown in table *[x]*.  *[could walk through the areas here, but let's see how much space we have first]*


## Participants ##

Data was collected concurrently with a closely related project [redacted <!-- @HicksValuesDisclosuresTrust2022 -->].  Participants were recruited using the online survey platform Prolific, and the survey was administered in a web browser using Qualtrics.  Prolific has an option to draw samples that are balanced to be representative by age, binary gender, and a 5-category race variable (taking values Asian, Black, Mixed, Other, and White) for US adults [@RepresentativeSamplesFAQ2022].  A recent analysis finds that Prolific produces substantially higher quality data than Amazon Mechanical Turk for online survey studies, though three of the five authors are affiliated with Prolific [@PeerDataQualityPlatforms2021].  


<!-- Text below was copied and pasted from the other paper -->
<!-- 
Participants were recruited using the online survey platform Prolific, and the survey was administered in a web browser using Qualtrics.  Prolific has an option to draw samples that are balanced to be representative by age, binary gender, and a 5-category race variable (taking values Asian, Black, Mixed, Other, and White) for US adults [@RepresentativeSamplesFAQ2022].  A recent analysis finds that Prolific produces substantially higher quality data than Amazon Mechanical Turk for online survey studies, though three of the five authors are affiliated with Prolific [@PeerDataQualityPlatforms2021].  Preliminary power analysis recommended a sample of approximately 1,000 participants to reliably detect non-interaction effects (H1-4).  

After excluding participants who declined consent after opening the survey or did not complete the survey, we had 988 participants in the full analysis sample ($M_{age}$ = 44-years-old, $SD_{age}$ = 16-years, Woman/Female = 498, Man/Male = 458, White = 712, Black = 124, Asian or Pacific Islander = 63, Hispanic = 33, American Indian or Alaskan Native = 5, Mixed or Other = 51).  Participants were randomly assigned to condition, with 163 assigned to the No Disclosure + Causes Harm condition, 165 assigned to the No Disclosure + Does Not Cause Harm condition, 165 assigned to the Economic Growth + Causes Harm condition, 165 assigned to the Economic Growth + Does Not Cause Harm condition, 168 assigned to the Public Health + Causes Harm condition, and 162 assigned to the Public Health + Does Not Cause Harm condition (@tbl-condition). Due to researcher error a question about participants' values was not included in the original survey. Of the full 988 participants, 844 participants (85%) responded to the followup question about their own values (participant prioritizes economic growth or public health).  Consequently, subsamples for hypotheses 4 and 5 were substantially smaller than the full analysis sample.  

```{r}
#| label: tbl-condition
#| tbl-cap: "Assignment of participants to conditions"
readRDS(here(out_dir, '03_condition_tbl.Rds')) |> 
	print_tbl()
```

--> 

The study was approved by the UC Merced IRB on August 17, 2021, and data collection ran October 18-20, 2021.  


## Software and reproducibility ##

<!--
Data cleaning and analysis was conducted in R version 4.1.2 [@RCoreTeamLanguageEnvironmentStatistical2021], with extensive use of the `tidyverse` suite of packages version 1.3.1 [@WickhamWelcomeTidyverse2019].  Regression tables were generated using the packages `gt` version 0.5.0 [@IannoneGtEasilyCreate2022] and `gtsummary` version 1.6.0 [@SjobergReproducibleSummaryTables2021]. 

Anonymized original data and reproducible code are available at <https://github.com/dhicks/transparency>.  Instructions in that repository explain how to automatically reproduce our analysis. 
 -->

## Methods ##


# Results #

Despite the efforts undertaken by Prolific to provide a representative sample, exploratory data analysis indicated that our data are unlikely to be representative by education level and political ideology.  In 2021, about 9% of US adults 25 or older had a less than high school education, and 38% had a Bachelor's degree or higher [@CPSHistoricalTime2022 fig. 2].  Only 1% of our participants reported a less than high school education, and 57% reported a Bachelor's degree or higher.  For political ideology, the General Social Survey has consistently found over several decades that about 30% of US adults identify as liberal, about 30% identify as conservative, and about 40% as moderate [@GSSDataExplorer2022].  Among our participants, liberals (574) heavily outnumber conservatives (248; @fig-part-values).  Both overrepresentation of college graduates and underrepresentation of conservatives (especially conservatives with strong anti-institutional views) are known issues in public opinion polling [@KennedyEvaluation2016Election2018].  

In particular, because political partisanship plays a significant role in many prominent public scientific controversies [though not all\; @FunkAmericansPoliticsScience2015], we believe it will be important to validate a VISS across the political spectrum before using it to understand the dynamics of a controversy.  



## Validation of Values in Science Scale ##

To examine the underlying dimensionality of the Values in Science scale, we split the sample in half for the purposes of carrying out exploratory factor analysis (EFA) to see what underlying factor structure may be present based on participants' response and confirmatory factor analysis (CFA) to compare models that could be abstracted from the EFA. We created a dummy variable that randomly assigned participants to one of two groups, resulting in two sub-samples of 439 and 467 response sets, respectively. Before conducting the EFA, we checked whether the assumptions necessary for a valid EFA held. Bartlett's test of sphericity was significant, $$\chi^2$$ (`r bartlett$parameter`) = `r bartlett$statistic`, p = `r bartlett$p.value(round = 3)`. The Kaiser-Meyer-Olkin measure of sampling adequacy was `r kmo$MSA(round =2)`. The determinant of the correlation matrix was `r det(round = 5)`, indicating no multicollinearity issues.

We conducted our factor analysis on the first sub-sample using the psych package (version #whatever) in R. Parallel analysis indicated we should retain six factors for an exploratory factor analysis, although only three factors had eigenvalues greater than 1.0. As such, we computed solutions for a three and a six factor solution using varimax rotation, deciding to retain items that had at least |.3| factor loading on only one of the resulting factors. The six-factor solution explained 33% of the variance and was preferred because the resulting factor structure was more easily interpretable than the three factor solution, which only explained 25% of the variance.

We retained 29 out of the original 36 items. The retained items, factor loadings, and communalities are all shown in Table #. The first identified factor contains six items that cohere around scientistic perceptions of science, elevating the status of science as superior to other knowledge-production efforts. The second factor identified contains three items that emphasize the role of science within society, focusing on societal consequences of scientific endeavors. The third factor contains eight items that, together, appear to represent participants' level of cynicism regarding the credibility of scientists and the scientific community. The fourth factor contains three items that tap into participants' perceptions of the social power dynamics within the scientific community. The fifth factor identified contains five items that reflect perceptions of science as constrained to a relatively narrow set of methods and practices in order to be counted as science, i.e. a mythological "the" scientific method. The final factor identified contains four items that appear to tap into perceptions of science as capable of pure objectivity.

For cross-validation purposes, we ran a CFA on the second sub-sample using the factor structure extracted from the EFA. We carried out the CFA with maximum likelihood estimation using the lavaan package (version #whatever) in R. Fit indices, unfortunately, revealed the model was insufficient to adequately fit the data, $$\chi^2$$(308) = 981.10, p < .001, CFI = .622, AGFI = .817, RMSEA = .071, and SRMR = .086. Typically, values exceeding .90 for CFI and AGFI indicate acceptable fit and values below .05 for RMSEA and SRMR indicate acceptable fit (Hu & Bentler, 1999).

Despite results from the CFA, the strong theoretical coherence of the factor structure suggests ways in which future iterations of a scale to tap into peoples' perceived values about science could improve upon the current Values in Science scale. More precise item wording may be necessary to better capture elements of the aforementioned constructs. Similarly, additional items may be needed to capture more aspects of each the proposed latent constructs. Lastly, there may be additional relevant values and perceptions about science that are not captured at all in the present iteration of the Values in Science scale. Future work should consider each of these suggestions to build upon the preliminary work we report here on the development of a psychometrically valid instrument to capture peoples' values in science. Excepting the comparative fit index (CFI), the fit indices were close to thresholds indicative of acceptable model fit. As such, we have decided to continue with our subsequent analyses using the six-factor structure as a means of examining whether participant responses to the Values in Science scale has any potential predictive validity for participant responses to the perceived credibility of scientists who acknowledge the role of values in their scientific research. Results of any such tests must be interpreted with caution, however, in light of the CFA results.

