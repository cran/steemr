#' build markdown files from the posts
#'
#' @param post_df A data frame of the posts.
#' @param dest_path A character string of the destination path for the markdown files.
#' @param post_df_source A character string of the data frame source.
#' @param if_yaml A logical value of whether the markdown files contain yaml headers.
#' @param original_link A character string vector of the orginal links to the posts.
#'
#' @return markdown files.
#' @export
#' @examples
#' \dontrun{
#' bmd()
#' }
#'
bmd <- function(post_df = NA,
                dest_path = 'blog',
                post_df_source = c('appbase_api', 'steemsql.com'),
                if_yaml = FALSE,
                original_link = c('steemit.com',
                                 'cnsteem.com',
                                 'busy.org',
                                 'steemdb.com',
                                 'steemd.com')
){
  if(is.na(post_df)) {
    return(print('Please give a valid data frame.'))
  }
  post_df_source <- match.arg(post_df_source)
  if(!dir.exists(dest_path)){
    dir.create(dest_path)
  }
  # prepare data
  if(post_df_source == 'appbase_api'){
    names(post_df)[names(post_df) == 'datetime'] <- 'created'
    post_df$url <- paste0('/', post_df$category, '/@', post_df$author, '/', post_df$permlink)

  }

  ### create posts
  if (nrow(post_df) > 0) {
    post_df <- post_df[order(post_df$created),]
    post_df$title <- clear_title(post_df$title)
    for (i in 1:nrow(post_df)){
      # create the yaml header for each post. optional.
      if(if_yaml){
        if(post_df_source == 'appbase_api'){
          mytags <- paste(paste0('  - ',
                                 strsplit(post_df$tags[i], ' ')[[1]]),
                          collapse = '\n')
        } else if (post_df_source == 'steemsql.com'){
          mytags <- paste(paste0('  - ',
                                 tag_of_post(post_df$json_metadata[i])),
                          collapse = '\n')
        }
        myyaml <- paste('---',
                      paste0('title: "', post_df$title[i], '"'),
                      paste0('author: ', post_df$author[i]),
                      paste0('date: "', post_df$created[i], '"'),
                      paste0('slug: ', post_df$permlink[i]),
                      paste0('categories: [', post_df$category[i], ']'),
                      paste0('tags: '), mytags,
                      '---',
                      '',
                      sep = '\n'
        )
      } else {
        myyaml <- NULL
      }
      # add the original links to the posts. optional.
      if(is.null(original_link)){
        mylink <- NULL
      }
      else {
        mylink <- paste0('Links: ', paste0('[', original_link, '](https://',
                         original_link, post_df$url[i], ')',
                         collapse = ', '))
      }
      # create the text
      mytext <- paste(myyaml, mylink, '', post_df$body[i], sep = '\n'
      )
      # save the md file.
      newfile <- paste0(post_df$permlink[i],'.md')
      write.table(mytext,
                  paste0(dest_path,'/', newfile), #/content/post
                  row.names = FALSE,
                  col.names = FALSE,
                  quote = FALSE,
                  fileEncoding = 'UTF-8')
    }
  }
}

#' Build a hugo blog site for a steem author
#'
#' @param author author name without @
#' @param initial  if initialize a site
#' @param template the hugo template
#' @param post_df A dataframe with the posts contents retrieved from SteemSQL
#' @param my_github A character string of a github repo
#' @param dest_path A character string
#' @param orginal_link A character string
#' @param post_df_source A character string of the data frame source.
#'
#' @return a blogdown-hugo web site
#' @export
#' @examples bblog()
#'
#'
bblog <- function(author = NA,
                  post_df,
                  dest_path = 'blog',
                  initial = FALSE,
                  template = 'xmin',
                  post_df_source = c('appbase_api', 'steemsql.com'),
                  orginal_link = c('steemit.com',
                                   'cnsteem.com',
                                   'busy.org',
                                   'steemdb.com',
                                   'steemd.com'),
                  my_github = 'your_name/your_repo'){
  if(is.na(author)) {
    return(print('Please give a valid author.'))
  }
  post_df_source <- match.arg(post_df_source)
  # Set up for the first time
  if (initial) {
    template_path <- system.file(paste0('template/blogdown_' , template, '.zip'),
                                 package = 'steemr')
    unzip(template_path, exdir = dest_path)
    # create _index.markdown
    index_demo <- readLines(paste0(dest_path, '/content/_index.markdown'),
                            encoding = 'UTF-8')
    index <- gsub(pattern = 'steemauthor',
                  replacement = author,
                  index_demo)
    writeLines(text = index,
               con = paste0(dest_path, '/content/_index.markdown'),
               useBytes = TRUE)

    ### change config.toml
    config_demo <- readLines(paste0(dest_path, '/config.toml'),
                             encoding = 'UTF-8')
    config <- gsub(pattern = 'steemauthor',
                   replacement = author,
                   config_demo)
    writeLines(text = config,
               con = paste0(dest_path, '/config.toml'))
  }

  bmd(post_df = post_df,
      dest_path = paste0(dest_path, '/content/post'),
      post_df_source = post_df_source,
      if_yaml = TRUE)

  ### create about.md
  knitr:: knit(paste0(dest_path, '/R/about.Rmd'),
               output = paste0(dest_path, '/content/about.md'),
               encoding = 'UTF-8')
  oldwd <- getwd()
  setwd(dest_path)
  blogdown::build_site(local = FALSE)
  setwd(oldwd)
}
