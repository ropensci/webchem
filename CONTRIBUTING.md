# CONTRIBUTING #

Thank you for your interest in contributing to the project! The goal of this guide is to help you with your endevour. There are many ways to contribute and we have outlines some which might be interesting to you. If you have another idea, feel free to contact the maintainter with it.

### Fill out the survey

The ```webchem``` survey allows us to learn who you are, how you would like to use the package, and how you think the project should develop to become more useful to you. This is extremely valuable information for us and guides our development efforts. We really appreciate if you share your thoughts with us.

You can fill out the survey [here](<link here>) (under development).

### Share a use case

Write us an e-mail and show us a full example about how you use ```webchem``` in your data analysis! This would again help us in developing the package further, and would also help us create better vignettes that help others get started.

Please send your e-mails to <webchem@ropensci.com>.

### Raise an issue

If you found a bug either in the code or the documentation, or a data source you would like us to integrate into ```webchem```, maybe you dreamed up a new functionality that would be nice to implement, raise an issue and let's discuss it! Even if you don't have the time or the coding background to resolve the issue yourself, maybe others do, and so just by giving a good problem you might help others who are looking for interesting issues to solve. 

You can raise an issue [here](https://github.com/ropensci/webchem/issues)

Thank you The goal of this guide is to help those who want to contribute to the project.
There are multiple ways to contribute, not only code contribution. This gui

Check if packages are updated.
Separate branch for each contribution.

### Code contributions?

* **Fork** this repo to your Github account.
* **Clone** your version on your account down to your machine from your account, e.g,. `git clone https://github.com/<yourgithubusername>/webchem.git`.
* Make sure to **track upstream** progress (i.e., on our version of `webchem` at `ropensci/webchem`) by doing `git remote add upstream https://github.com/ropensci/webchem.git`. Before making changes make sure to pull changes in from upstream by doing either `git fetch upstream` then merge later or `git pull upstream` to fetch and merge in one step
* Make your **changes** (bonus points for making changes on a new branch).
* **Push** up to your account.
* Submit a **pull request** to home base at `ropensci/webchem`.

### Code guidelines

We do not have strong guidelines for code contributions and are happy to help.

1. We follow the [tidyverse](https://tidyverse.org) style. You can find the style guide [here](https://style.tidyverse.org/). Before committing your code, we encourage you to use ```lintr::lint_file()``` to check for nonconformances.

2. We use ```roxygen2``` for documentation. Please make sure you update the package to the latest version before you update the documentation with ```devtools::document()```.

We want to keep dependencies to a minimum:

3. Please use the [`xml2`](https://github.com/hadley/xml2) package instead of the `XML` package. The maintainance of xml2 is much better.

4. Please use the lightweight [`jsonlite`](https://github.com/jeroenooms/jsonlite) package for handling JSON.

5. Use utilities in utils.R when possible to keep function style consistent across the package.

6. Be nice to the resources! Use appropriate timeouts.

7. Tests go into a separate tests branch and not in the master branch.

Some consistency guidelines:

8. Functions that query a database for one or more database specific identifiers should follow the naming convention ```get_*```, e.g. the function that queries ChEBI IDs is called ```get_chebiid()```.

9. The naming of functions that query a database for chemical information should start with the name of the database, followed by the functionality, e.g. ```pc_synonyms()``` searches for synonyms in PubChem.

### Data Sources

You might think all webscraping is perfectly legal but it is unfortunately not that simple. Some services allow you to browse their website but do not allwo you to scrape it, for various reasons. Therefore, we always have to check the Terms & Conditons and any legal documents that might restrict programmable access. ```webchem``` only provides access to databases where programmable access is clearly approved by the database provider. A provider might create a publicly accessible API, and if they do not have a restrcitive T&C, this indicates their implicit approval for programmatically accessing their data. In all other cases explicit approval is required, i.e. either the T&C has to state that sraping is allowed, or we have to acquire written consent from the database provider before scraping their website.

### Questions? 

If you have any questions, get in touch with us at <webchem@ropensci.com>.

### Thanks for contributing!
