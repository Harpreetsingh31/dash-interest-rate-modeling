# dash-interest-rate-modeling

## About this app:
This R app lets you perform various data analytical operations on the historical interest rate datasets.

## We have three main .R files:
- fcts.R (helper functions for callbacks)
- app.R (mainly app layout)
- init.R (required libaries)

## App Layout:
- First, we have a tab to choose a currency (default USD).
- Next, we have Table, 3D, 2D, and Pie chart to give overall picture of the dataset. 
- Then, we have Dash_core_components: Date slider, Dropdown (for tenures/columns), Input (differencing), and a dropdown selector.
- Finally, we have four modules of operations for processing, fitting, pca and vasicek simulation.

## Overall:
![animated](screenshot/screencaptured.gif)

## Dash_Components: Slider/dropdown
![animated](screenshot/dash_components.gif)

## Distribution Fitting
![animated](screenshot/probs.gif)

## PCA_Vasicek:
![animated](screenshot/pca_vas.gif)
