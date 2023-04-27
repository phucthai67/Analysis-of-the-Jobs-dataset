# Analysis-of-the-Jobs-dataset

This analysis is part of the three-part assignment in STATS7022 (Data Science PG) of Uni Adelaide. Code, dataset and report for each part are provided in this repository.
The original dataset is retrieved from Kaggle: https://www.kaggle.com/datasets/PromptCloudHQ/us-jobs-on-monstercom

PART 1

This part involves cleaning the dataset. The dataset is quite messy, some major issues are: job_type column contains many levels that are the same but typed differently,  location column has values belong to the organization columns and vice versa, salary is provided as a described salary and contains many missing values. There are more issues which is described and dealt with in the provided pdf report, the pdf already includes the code (code draft, Rmarkdown file, pdf report and an uncleaned dataset is provided)

PART 2

Involves preprocessing data and prep it for model building. Although no response or predictors are defined in this step, so the preprocessing step is only done to a limited extent. All that was done was extracting or estimating annual salary from what was given, extracting state from the location column and simplifying sector column so it only has 20 most common levels, while other levels are lumped together as "Other". Code draft, Rmarkdown file, pdf report including the code and a cleaned dataset is included in this repositories. 

PART 3

The third and final part is model building using annual salary estimated from part 2 as the response (so only about more than 2000 out of the 22000 rows of the data was usable for this part. For the purpose of the assignment, we only used job_type, state and sector as the predictors. The model that I was asked to built was random forest. Code draft, Rmarkdown file, pdf report including the code and a cleaned dataset is included in this repositories. With a relatively messy data, a response that involves estimation, the data used to build the model was not great. Consequently, the model was not great as well. Code draft, Rmarkdown file, pdf report including the code and a cleaned dataset is included in this repositories. 

Extension
In an attempt to improve the model, I tried to incorporate job description and job title. I don't have any natural language processing knowledge, and the code used for processing job description and title was given to me as part of the course, but the course did not teach the rationale behind that processing.
