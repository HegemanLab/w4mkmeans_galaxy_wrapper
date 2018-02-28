# w4mkmeans

w4mkmeans is a planemo-based galaxy-tool-wrapper to wrap the `stats::kmeans` R package for clustering the W4M dataMatrix.

## Roles of files here:
- `run_test.sh` - set up and run `planemo test`
- `run_serve.sh` - set up and run `planemo serve`
- `run_wrapper.sh` - invoke `w4mkmeans_wrapper.R`
- `w4mkmeans.xml` - presentation layer
- `w4mkmeans_wrapper.R` - Galaxy-specific code
- `w4mkmeans_routines.R` - Galaxy-independent code to invoke `stats::kmeans`
- `w4m_general_purpose_routines.R` - some reusable utilities specific to the W4M / XCMS data formats

# Tool usage details

**Author** - Arthur Eschenlauer \(University of Minnesota, [esch0041@umn.edu](mailto:esch0041@umn.edu)\)

---

**Source** - The source code for the w4mkmeans tool is available \(from the Hegeman lab github repository\) at [https://github.com/HegemanLab/w4mkmeans\_galaxy\_wrapper](https://github.com/HegemanLab/w4mkmeans_galaxy_wrapper)

**R code used** - The R code invoked by this wrapper is the R [`stats::kmeans`](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/kmeans.html) package

---

**Tool updates**

See the **NEWS** section at the bottom of this page

---

# K-means for W4M data matrix

## Description

Calculate K-means for sample-clusters \(or feature-clusters, or both\) using W4M dataMatrix \(i.e., XCMS-preprocessed data files\) as input.

_Please note that XCMS refers to features as 'variables'. This documentation generally refers to them as features._

## Workflow Position

| Tool category | Upstream tool category | Downstream tool categories |
| :--- | :--- | :--- |
| Statistical Analysis | Preprocessing | Statistical Analysis |

## Motivation

This tool clusters samples, features \(variables\), or both from the W4M dataMatrix and writes the results to new columns in sampleMetadata, variableMetadata, or both, respectively.

> * If several, comma-separated K's are supplied, then one column is added for each K.
> * This clustering is **not** hierarchical; each member of a cluster is not a member of any other cluster.
> * For feature-clustering, each feature is assigned to a cluster such that the feature's response for all samples is closer to the mean of all features for that cluster than to the mean for any other cluster.
> * For sample-clustering, each sample is assigned to a cluster such that the sample's response for all features is closer to the mean of all samples for that cluster than to the mean for any other cluster.

## Input files

| File | Format |
| :--- | :--- |
| Data matrix | tabular |
| Sample metadata | tabular |
| Variable \(i.e., feature\) metadata | tabular |

## Parameters

**Data matrix** - input-file dataset

> XCMS variable x sample 'dataMatrix' \(tabular separated values\) file of the numeric data matrix, with . as decimal, and NA for missing values; the table must not contain metadata apart from row and column names; the row and column names must be identical to the rownames of the sample and feature metadata, respectively \(see below\)

**Sample metadata** - input-file dataset

> XCMS sample x metadata 'sampleMetadata' \(tabular separated values\) file of the numeric and/or character sample metadata, with . as decimal and NA for missing values

**Feature metadata** - input-file dataset

> XCMS variable x metadata 'variableMetadata' \(tabular separated values\) file of the numeric and/or character feature metadata, with . as decimal and NA for missing values

**prefix for cluster names** - character(s) to add as prefix to category number (default = 'c')

> Some tools require non-numeric values to discern categorical data; e.g., enter 'c' here to prepend 'c' to cluster numbers in the output; default 'c'.

**ksamples** - K or K-range for samples \(default = 0\)

> integer or comma-separated integers ; zero \(the default\) or less will result in no calculation.

**kfeatures** - K or K's for features \(default = 0\)

> integer or comma-separated integers ; zero \(the default\) or less will result in no calculation.

**iter\_max** - maximum\_iterations \(default = 20\)

> maximum number of iterations per calculation \(see
> [`stats::kmeans` documentation](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/kmeans.html)
> \).

**nstart** - how many random sets should be chosen \(default = 20\)

> number of random sets of centers to start calculation \(see
> [`stats::kmeans` documentation](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/kmeans.html)
> \).

**algorithm** - algorithm for clustering \(default = 20\)

> K-means clustering algorithm, default 'Hartigan-Wong'; alternatives 'Lloyd', 'MacQueen'; 'Forgy' is a synonym for 'Lloyd' \(see
> [`stats::kmeans` documentation](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/kmeans.html)
> \).



## Output files

**XCMS sampleMetadata** - \(tabular separated values\) file identical to the Sample metadata file given as an input argument, excepting one column added for each K

> * **k\#** - cluster number for clustering samples with K = \#

**XCMS variableMetadata** - \(tabular separated values\) file identical to the Feature metadata file given as an input argument, excepting one column added for each K

> * **k\#** - cluster number for clustering features with K = \#

**scores** - \(tabular separated values\) file with one line for each K.

> * **clusterOn** - what was clustered - either 'sample' or 'feature'
> * **k** - the chosen K for clustering
> * **totalSS** - total \( *between-treatements* plus total of *within-treatements* \) sum of squares
> * **betweenSS** - *between-treatements* sum of squares
> * **proportion** - betweenSS / totalSS

## Working example

**Input files**

| Input File | Download from URL |
| :--- | :--- |
| Data matrix | [https://raw.githubusercontent.com/HegemanLab/w4mkmeans\_galaxy\_wrapper/master/test-data/input\_dataMatrix.tsv](https://raw.githubusercontent.com/HegemanLab/w4mkmeans_galaxy_wrapper/master/test-data/input_dataMatrix.tsv) |
| Sample metadata | [https://raw.githubusercontent.com/HegemanLab/w4mkmeans\_galaxy\_wrapper/master/test-data/input\_sampleMetadata.tsv](https://raw.githubusercontent.com/HegemanLab/w4mkmeans_galaxy_wrapper/master/test-data/input_sampleMetadata.tsv) |
| Feature metadata | [https://raw.githubusercontent.com/HegemanLab/w4mkmeans\_galaxy\_wrapper/master/test-data/input\_variableMetadata.tsv](https://raw.githubusercontent.com/HegemanLab/w4mkmeans_galaxy_wrapper/master/test-data/input_variableMetadata.tsv) |

**Other input parameters**

| Input Parameter | Value |
| :--- | :--- |
categorical_prefix - character(s) to add as prefix to category number (default = 'k')

some tools treat only non-numeric data as categorical; this prefix ('k' by default) ensures that clusters data will be treated as categorical; an empty string is permitted here if desired (and succeeding tools accept integers as categorical data).
| ksamples | 3,4 |
| kfeatures | 5,6,7 |
| iter\_max | 10 |
| nstart | 1 |
| algorithm | Hartigan-Wong |

## NEWS

August 2017, Version 0.98.4 - Maintenance release

  - Update bioconda r-base dependency to v3.4.1
  - Add dependency on conda packages libssh2 and krb5 needed by makePSOCKcluster on some platforms
  - Make tool fail when no results are produced
  - Changed parameter defaults for iterations and random sets to improve convergence of results.
 
August 2017, Version 0.98.3 - Feature-tuning release
 
  - Add (optional) prefix to category numbers for downstream tools that treat only non-numeric data as categorical.
  - Accept as possible K only unique numbers convertible to integer; discard others without failing.
  - Published to the Galaxy toolshed [https://toolshed.g2.bx.psu.edu/view/eschen42/w4mkmeans/02cafb660b72](https://toolshed.g2.bx.psu.edu/view/eschen42/w4mkmeans/02cafb660b72)

August 2017, Version 0.98.1 - First release

  - Published to the Galaxy toolshed [https://toolshed.g2.bx.psu.edu/view/eschen42/w4mkmeans/6ccbe18131a6](https://toolshed.g2.bx.psu.edu/view/eschen42/w4mkmeans/6ccbe18131a6)

## Citations

R Core Team \(2017\). _stats::kmeans - K-Means Clustering_, R Foundation for Statistical Computing.\[[Link](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/kmeans.html)\]

Forgy, E. \(1965\). Cluster Analysis of Multivariate Data: Efficiency versus Interpretability of Classification. In _Biometrics, 21 \(3\), pp. 768-769._

Guitton, Yann and Tremblay-Franco, Marie and Le Corguillé, Gildas and Martin, Jean-François and Pétéra, Mélanie and Roger-Mele, Pierrick and Delabrière, Alexis and Goulitquer, Sophie and Monsoor, Misharl and Duperier, Christophe and et al. \(2017\). Create, run, share, publish, and reference your LC–MS, FIA–MS, GC–MS, and NMR data analysis workflows with the Workflow4Metabolomics 3.0 Galaxy online infrastructure for metabolomics. In _The International Journal of Biochemistry & Cell Biology,_ \[[doi:10.1016/j.biocel.2017.07.002](http://dx.doi.org/10.1016/j.biocel.2017.07.002)\]

Giacomoni, F. and Le Corguille, G. and Monsoor, M. and Landi, M. and Pericard, P. and Petera, M. and Duperier, C. and Tremblay-Franco, M. and Martin, J.-F. and Jacob, D. and et al. \(2014\). Workflow4Metabolomics: a collaborative research infrastructure for computational metabolomics. In _Bioinformatics, 31 \(9\), pp. 1493–1495._ \[[doi:10.1093/bioinformatics/btu813](http://dx.doi.org/10.1093/bioinformatics/btu813)\]

Hartigan, J. and Wong, M. \(1979\). Algorithm AS136: A k-means clustering algorithm. In _Applied Statistics, 28, pp. 100-108._

Lloyd, S. \(1982\). Least squares quantization in PCM. In _IEEE Transactions on Information Theory, 28 \(2\), pp. 129–137._ \[[doi:10.1109/tit.1982.1056489](http://dx.doi.org/10.1109/tit.1982.1056489)\]

MacQueen, J. B. \(1967\). Some Methods for Classification and Analysis of MultiVariate Observations. In _Proc. of the fifth Berkeley Symposium on Mathematical Statistics and Probability, pp. 281-297._
