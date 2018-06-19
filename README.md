## Introduction

steemr is an open source R package for playing with Steem data in the R environment. It can download, post process, analyze,  and visualize Steem data on the basis of the powerful statistic power of R. Currently, the version 0.0.0 can:

- obtain the complete post list for a given account,
- obtain the complete account information from steemdb.com,
- organized the 'follower' and 'following' information for a given account,
- obtain the following history of a given account,
- download the vote records of a given post, and
- download the complete data of the latest 100 posts of a given account.

## Quick start

### Installation

```
install.packages('devtools')
devtools::install_github('pzhaonet/steemr')
library('steemr')
```

### Examples 

#### obtain the complete account information

```
id_info(id = 'dapeng')
```

#### Get a name list of an ID's followers and following

```{r}
follower(id = 'dapeng')
```

#### Get an id's following history

```
following(id = 'dapeng')
```

#### Get an ID's complete post list with hyperlinks

```{r}
post_links(id = 'dapeng', post_number = 3)
```

#### Get the complete info of a single given post on steemdb.com

```{r}
post_info(postlink = 'utopian-io/@dapeng/steemg-four-more')
```

#### Get the detailed information of given posts from steemdb.com

```{r}
post_df(c('cn/@dapeng/xuer-sale',
          'utopian-io/@dapeng/steemg-four-more'))
```

#### Get an ID's posts with complete information

```{r}
post_id(id = 'dapeng', post_number = 10)
```

More functions are coming soon. Have fun!

## Updates

    2018-06-12. v0.0.1. A bug of the hyperlinks was fixed.
    2018-06-11. v0.0.0. A preliminary version.

## License

Copyright 2018 Peng Zhao.

Released under the MIT license.
