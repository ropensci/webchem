# CONTRIBUTING #

Thank you for your interest in contributing to the project! The goal of this guide is to help you with your endevour. There are many ways to contribute and we have outlined some opportunities which might be interesting to you. If you have any questions or suggestions, feel free to contact us at <webchem@ropensci.org>.

### Fill out the survey

You can fill out our survey at https://forms.gle/V7dfGGn73dkesn5L6.

The `webchem` survey allows us to learn which databases you use and how you interact with chemical data. This is extremely valuable information for us and guides our development efforts. The survey takes about 5 minutes to fill out.

### Share a use case

Write us an e-mail and show us a full example of how you use or how you would like to use `webchem` in your data analysis! This would give us ideas about new features and also help us create better vignettes that help others get started. Please send your e-mails to <webchem@ropensci.org>.

### Raise a new issue or join discussion on an existing issue

If you found a bug either in the code or the documentation, or a data source you would like us to integrate into ```webchem```, maybe you dreamed up a new functionality that would be nice to implement, raise an issue and let's discuss it! Even if you don't have the time or the coding background to resolve the issue yourself, maybe others do, and so just by giving a good problem you might help others who are looking for interesting problems to solve. You can raise an issue [here](https://github.com/ropensci/webchem/issues). Feel free to join discussions on existing issues as well!

### Code contributions

If you know some coding, you can also add code contributions.

1. **Fork** this repo to your Github account.
2. **Clone** your version on your account down to your machine from your account, e.g,. `git clone https://github.com/<yourgithubusername>/webchem.git`.
3. Make sure to **track upstream** progress (i.e., on our version of `webchem` at `ropensci/webchem`) by doing `git remote add upstream https://github.com/ropensci/webchem.git`. Before making changes make sure to pull changes in from upstream by doing either `git fetch upstream` then merge later or `git pull upstream` to fetch and merge in one step
4. Make your **changes**. Bonus points for making changes on a new branch.

Creating new branches is good practice. This is because if you finish with a topic, open a pull request and start working on another topic without starting a new branch, any further commits you push to your account will be automatically added to your pull request as well, making it much harder for us to evaluate your request. To aboid this, open a new branch for each new topic.

5. **Push** up to your account.
6. Submit a **pull request** to home base at `ropensci/webchem`.

### Guidelines for code contributions

You can find the rOpenSci developer guide at https://devguide.ropensci.org/

We are happy to help at any point in your work.

1. We follow the [tidyverse](https://tidyverse.org) style. You can find the style guide [here](https://style.tidyverse.org/). Before committing your code, we encourage you to use ```lintr::lint_file()``` to check for nonconformances.

2. We use [`roxygen2`](https://cran.r-project.org/web/packages/roxygen2/index.html) for documentation. Please make sure you update the package to the latest version before you update the documentation with `devtools::document()`. Use `@noRd` for non exported functions.

3. Please use the [`xml2`](https://cran.r-project.org/web/packages/xml2/index.html) package instead of the `XML` package. The maintainance of xml2 is much better.

4. Please use the lightweight [`jsonlite`](https://cran.r-project.org/web/packages/jsonlite/index.html) package for handling JSON.

5. Use utilities in `webchem::utils.R` when possible to keep function style consistent across the package.

6. Be nice to the resources! Minimise interaction with the servers. Use appropriate timeouts.

7. Within test files always include a check whether the webservice is running and skip all tests when it is not. See `R/ping.R` for more details.  

Some consistency guidelines:

8. Functions that query a database for one or more database specific identifiers should follow the naming convention `get_*`, e.g. the function that queries ChEBI IDs is called `get_chebiid()`. These functions should take a vector of queries and return a single [tibble](https://cran.r-project.org/web/packages/tibble/index.html). Whenever possible these functions should have arguments `query`, `from`, `match`, `verbose` and `...`. The first column of the tibble should contain the ID-s and the last should contain the queries. Invalid queries should return a row of NA-s (apart from the last element of the row which should be the query itself).

9. The naming of functions that query a database for chemical information should start with the name of the database, followed by the functionality, e.g. `pc_synonyms()` searches for synonyms in PubChem. These functions should take a vector of queries and return a list of responses. Invalid queries should return `NA`.

10. Functions should always validate their input when appropriate. Use `match.arg()` for input validation.

11. Make sure `NA` is not confused with sodium.

12. Functions that retrieve images should follow the naming convention `*_img`, e.g. the function that retrieves images from ChemSpider is called `cs_img()`. These functions should take a vector of arguments and download images into a user defined directory. They should not keep images in memory, should not implement image processing functionality, and should not return anything to the console. Functions should include arguments `dir`, `overwrite = TRUE` and `verbose = TRUE`. `dir` should not have a default value.

### Data Sources

You might think all webscraping is perfectly legal but it is unfortunately not that simple.

Some services allow you to browse their website but do not allow you programmable access, for various reasons. Therefore, we always have to check the Terms & Conditons and any other legal documents that might restrict programmable access. `webchem` only provides access to databases where programmable access is clearly approved by the database provider. A provider might create a publicly accessible API, and if they do not have a restrictive T&C, this indicates their implicit approval for programmatically accessing their data. In all other cases explicit approval is required, i.e. either the T&C has to state that scraping is allowed, or we have to acquire written consent from the database provider before developing functions that scrape their website.

And there is a big difference between scraping and crawling. `webchem` does provide some scraping functionality but it does not provide crawling functionality.

### Thanks for contributing!