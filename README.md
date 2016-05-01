CheckSolr
==========
Monitoring plugins for [Solr](http://lucene.apache.org/solr/).

Requirements
============
CheckSolr is written in Haskell with [GHC](http://www.haskell.org/ghc/).
All required Haskell libraries are listed in [check_solr.cabal](check_solr.cabal).
Use [cabal-install](http://www.haskell.org/haskellwiki/Cabal-Install)
to fetch and build all pre-requisites automatically.

Installation
============
    $ git clone https://github.com/zalora/check-solr.git
    $ cd check-solr
    $ cabal install

Usage
=====
`check_solr_query` fetches http://solr.example.net:1234/solr/CORE/select?q=QUERY&rows=0

```
Usage:
  check_solr_query [options]

Options:
  -u, --uri=URI           Solr URI [default: http://localhost:8983/solr/CORE/]

  -q, --query=QUERY       Solr query [default: *:*]
  -w, --warn=WARN         Warning threshold for low number of results
  -c, --crit=CRIT         Critical threshold for low number of results

  -h, --help              Show this message
```

`check_solr_stat` fetches http://solr.example.net:1234/solr/CORE/admin/mbeans?stats=true&wt=json,
extracts numeric statitics, compares one of the parameters with thresholds.

```
Usage:
  check_solr_stats [options]

Options:
  -u, --uri=URI           Solr URI [default: http://localhost:8983/solr/CORE/]

  -g, --category=CAT      Category name [default: CORE]
  -k, --key=KEY           Object key [default: searcher]
  -s, --stat=STAT         Parameter to check [default: numDocs]

  -w, --low-warn=LOWW     Warning threshold for low value
  -c, --low-crit=LOWC     Critical threshold for low value

  -W, --high-warn=HIGHW   Warning threshold for high value
  -C, --high-crit=HIGHC   Critical threshold for high value

  -h, --help              Show this message
```

Examples
========
```
$ check_solr_query -u http://solr.example.net:10151/solr/CORE/ -c 100000
CRITICAL: numFound = 86207, query: *:* | QTime=0ms;;;0; numFound=86207c;;100000;0;

$ check_solr_query -u http://solr.example.net:10151/solr/SGFAS/ -q brand:zalora
OK: numFound = 4458, query: brand:zalora | QTime=0ms;;;0; numFound=4458c;;;0;
```

```
$ check_solr_stats -u http://solr.example.net/solr/CORE/
OK: numDocs = 93349 for CORE:searcher [ index searcher ] | QTime=0ms;;;0; maxDoc=93349;;;0; numDocs=93349;;;0; warmupTime=0ms;;;0; indexVersion=80420;;;0; deletedDocs=0;;;0;

$ check_solr_stats -C 12 -u http://solr.example.net/solr/CORE/
CRITICAL: numDocs = 92443 for CORE:searcher [ index searcher ] | QTime=0ms;;;0; maxDoc=92443;;;0; numDocs=92443;;;0; warmupTime=0ms;;;0; indexVersion=78775;;;0; deletedDocs=0;;;0;

$ check_solr_stats -g QUERYHANDLER -k /update -s 99thPcRequestTime -u http://solr.example.net/solr/CORE/
OK: 99thPcRequestTime = 297ms for QUERYHANDLER:/update [ Add documents using XML (with XSLT), CSV, JSON, or javabin ] | QTime=0ms;;;0; 95thPcRequestTime=46ms;;;0; 999thPcRequestTime=11919ms;;;0; avgRequestsPerSecond=1.4677610600683635;;;0.0; 99thPcRequestTime=297ms;;;0; medianRequestTime=0ms;;;0; totalTime=7743480ms;;;0; 5minRateReqsPerSecond=0.4679651049093596;;;0.0; requests=189130;;;0; timeouts=0;;;0; handlerStart=1461832954290;;;0; errors=0;;;0; avgTimePerRequest=40ms;;;0; 15minRateReqsPerSecond=0.6707583959132495;;;0.0; 75thPcRequestTime=1ms;;;0;

$ check_solr_stats -g QUERYHANDLER -k /update -s 99thPcRequestTime -u http://example.foo:1151/solr/CORE/
CRITICAL: FailedConnectionException2 "example.foo" 1151 False getAddrInfo: does not exist (Name or service not known)

$ check_solr_stats -g QERYHANDLER -k /update -s 99thPcRequestTime -u http://solr.example.net:10151/solr/BAR/
CRITICAL: CheckSolrStatNotFound "/update"

$ check_solr_stats -g QUERYHANDLER -k /update -s 99thPcReuestTime -u http://solr.example.net/solr/CORE/
WARNING: Not found: 99thPcReuestTime for QUERYHANDLER:/update [ Add documents using XML (with XSLT), CSV, JSON, or javabin ] | QTime=0ms;;;0; 95thPcRequestTime=45ms;;;0; 999thPcRequestTime=11919ms;;;0; avgRequestsPerSecond=1.4690229727981707;;;0.0; 99thPcRequestTime=86ms;;;0; medianRequestTime=0ms;;;0; totalTime=7757636ms;;;0; 5minRateReqsPerSecond=1.1300816881844633;;;0.0; requests=189462;;;0; timeouts=0;;;0; handlerStart=1461832954290;;;0; errors=0;;;0; avgTimePerRequest=40ms;;;0; 15minRateReqsPerSecond=0.9254481606300609;;;0.0; 75thPcRequestTime=0ms;;;0;
```

