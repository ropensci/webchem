# CONTRIBUTING #

### Please contribute!
We love collaboration.

### Found a Bug?

* Submit an issue on our Issues page [here](https://github.com/ropensci/webchem/issues).

### Code contributions?

* **Fork** this repo to your Github account.
* **Clone** your version on your account down to your machine from your account, e.g,. `git clone https://github.com/<yourgithubusername>/taxize.git`.
* Make sure to **track upstream** progress (i.e., on our version of `webchem` at `ropensci/webchem`) by doing `git remote add upstream https://github.com/ropensci/webchem.git`. Before making changes make sure to pull changes in from upstream by doing either `git fetch upstream` then merge later or `git pull upstream` to fetch and merge in one step
* Make your **changes** (bonus points for making changes on a new branch).
* **Push** up to your account.
* Submit a **pull request** to home base at `ropensci/webchem`.

### New databases (IMPORTANT!)

If your code accesses a database not previously accessed by `webchem`, please check that the database service is OK with being accessed by third a third party.  If your code accesses the database through an API this is *usually* OK.  For web scraping, please check the site's robots.txt (we recommend using the `polite` package for web scraping, which does this automatically) **and** the site's About or FAQ page for any information on permission for web scraping or indexing.  If you are uncertain, please create a database request issue first so the package maintainers can look into it.

### Code guidelines

We do not have strong guideline for our contributions and are happy to help.

1. Please follow [this](http://adv-r.had.co.nz/Style.html) styleguide for your contributions.

2. For web scraping, we recommend the use of the [polite](https://dmi3kno.github.io/polite/) package.

We want to keep dependencies to a minimum:

2. Please use the [`xml2`](https://github.com/hadley/xml2) package instead of the `XML` package. The maintainance of xml2 is much better.

3. Please use the lightweight [`jsonlite`](https://github.com/jeroenooms/jsonlite) package for handling JSON.

4. Use utilities in utils.R when possible to keep function style consistent across the package.

5. Be nice to the resources! Use appropriate timeouts.

6. Tests go into a separate tests branch and not in the master branch.




### Questions? 

Get in touch: [eduardszoecs@gmail.com](mailto:eduardszoecs@gmail.com)

### Thanks for contributing!
