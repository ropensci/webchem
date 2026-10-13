# webchem 2.0.0

* At the time of submission a ChEMBL URL timed out (Error: man/chembl_query.Rd:79:17 500: Internal Server Error
\item \url{https://www.ebi.ac.uk/chembl/api/data/docs}). One of the new features in webchem v2.0.0 is support for offline access, which directly addresses this issue: When used in offline mode the functions query a local copy of the ChEMBL database which is robust to temporary issues in webservice availability. We would like to keep the URL in the package.

# webchem 1.3.0

* This minor relese defuncts two functions, `ci_query()` and `pan_query()` because the webservices they were interacting with are no longer available.
* Submission was rejected on 2023-05-04 due to the following check log entry: "Apparent methods for exported generics not registered: cas.ci_query inchikey.ci_query smiles.ci_query". This has been fixed.

# webchem 1.1.3

* Found the following (possibly) invalid URLs: https://www.gsbl.de (moved to 
https://www.chemicalieninfo.de): I cannot check the impact of this because there is an SSL certificate problem that affects all ETOX functions. Once the certificate is restored we will update any functions that need to be fixed.

# webchem 1.1.2

* https://developer.rsc.org/, SSL certificate problem: unable to get local issuer certificate: The URLs work and including these URLs in the package is important.