# webchem 1.3.1

* `urlchecker::url_check()` returned the following error for \url{https://cfpub.epa.gov/ecotox/}: Error: SSL connect error [cfpub.epa.gov]: TLS connect error: error:0A000152:SSL routines::unsafe legacy renegotiation disabled. My understanding is that the issue is with the webservice and I have no way to resolve this error. This URL is used in the documentation of some example data sets and we would like to keep it, is possible.

# webchem 1.3.0

* This minor relese defuncts two functions, `ci_query()` and `pan_query()` because the webservices they were interacting with are no longer available.
* Submission was rejected on 2023-05-04 due to the following check log entry: "Apparent methods for exported generics not registered: cas.ci_query inchikey.ci_query smiles.ci_query". This has been fixed.

# webchem 1.1.3

* Found the following (possibly) invalid URLs: https://www.gsbl.de (moved to 
https://www.chemicalieninfo.de): I cannot check the impact of this because there is an SSL certificate problem that affects all ETOX functions. Once the certificate is restored we will update any functions that need to be fixed.

# webchem 1.1.2

* https://developer.rsc.org/, SSL certificate problem: unable to get local issuer certificate: The URLs work and including these URLs in the package is important.