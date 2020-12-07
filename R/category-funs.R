#' Find categories nested within categories
#'
#' @param cats character vector representing set of categories
#' @param tree data frame of category relations. Columns are parent, child, relation name.
#' @param .relation a character vector representing the preference list for breakout relations. Special values "alpha" and "desc" can be used.
#'
#'
#' - `subcats_of` finds all subcategories (inclusive of supplied categories)
#' - `leafcats_of` restricts to leaf categories (those with no breakout relations)
#' - `breakout_of` restricts to a single breakout set of categories
#'
#' Data representing category relationships do not necessarily represent all categories (e.g., if a category has neither parent or child.) For this reason, categories not listed in the tree are leaves and roots.
#'
#' @name findcats
NULL

#'  @export
#'  @rdname findcats
subcats_of <- function(cats, tree, .relation = "alpha") {

  # initial
  n_prev_cats <- 0
  new_cats <- unique(cats) %>% discard(is.na)

  while (n_prev_cats < length(new_cats)) {

    new_cat_df <- tibble(
      cats = new_cats
    ) %>%
      left_join(tree, c("cats" = "parent"))

    n_prev_cats <- length(new_cats)
    new_cats <- unique(c(new_cat_df$cats, new_cat_df$child)) %>% discard(is.na)
  }

  sort(new_cats)
}

# subcats_of(c("AgSoils", "Rice", "Manure", "Enteric", "ForestLand"), cattree)

#'  @export
#'  @rdname findcats
leafcats_of <- function(cats, tree, .relation = "alpha") {

  subcats <- subcats_of(cats, tree)
  res <- subcats[is_leafcat(subcats, tree)]
  res
}


#' @export
#' @rdname findcats
is_leafcat <- function(cats, tree) {

  stemcats <- filter(tree,
                     !is.na(.data$child),
                     !is.na(.data$parent),
                     .data$parent != .data$child) %>%
    pull(.data$parent) %>% unique()

  res <- ! cats %in% stemcats
  res
}



# breakout_of(c("Manure", "Enteric"), cattree, "by-animal")
# breakout_of(c("Manure", "Enteric"), cattree)

#' @export
#' @rdname findcats
breakout_of <- function(cats, tree, .relation = "alpha") {

  tree_for_cats <- tree %>%
    filter(parent %in% cats)

  breakout_tree <- breakout_tree(tree_for_cats, .relation = .relation)

  # initial
  new_cats <- unique(cats) %>% discard(is.na)

  # apply breakout relations
  new_cat_df <- tibble(
    cats = new_cats
  ) %>%
    left_join(breakout_tree, c("cats" = "parent"))

  new_cats <- unique(new_cat_df$child) %>% discard(is.na)

  sort(new_cats)
}

normalize_tree <- function(t, .relation = NA_character_) {
  res <- t %>% {
    if(ncol(.) == 2) {
      mutate(., relation = NA_character_)
      } else .
    } %>%
      set_names(c("parent", "child", "relation")) %>% {
        if(is.na(.relation)) {
          .
        } else if(.relation == "ascending") {
          arrange(., parent, relation, child)
        } else if(.relation == "descending") {
          arrange(., parent, desc(relation), child)
        } else if(is.character(.relation))
          filter(., relation %in% c(.relation, NA_character_)) %>%
          arrange(factor(relation, levels = .relation))
      }
  res
}

#' Calculate a subtree of category relations
#'
#' @param tree dataframe of category relations
#' @param .relation character vector
#' @param depth currently 1
#'
#' special .relation values:
#' "alpha" : order the relations alphabetically and pick first
#' "desc"  : order the relations descending and pick first
#' NA      : must be only one relation per parent
#' right now specials cannot be used as part of string specifications, and restricting to NA relations is not possible
#' @export
breakout_tree <- function(tree, .relation, depth = 1) {

  if (is.na(.relation) & length(.relation == 1)) {
    breakout_rels <- distinct(tree, parent, relation)
    if(! all(count(breakout_rels, parent, relation)[["n"]] == 1)) stop("When using .relation NA, only a single relation must be available in the tree for all cats.")
  } else if (.relation == "all") {
    breakout_rels <- tree
  } else if (.relation == "alpha") {
    breakout_rels <- tree %>%
      distinct(parent, relation) %>%
      arrange(parent, relation) %>%
      group_by(parent) %>%
      slice(1) %>%
      ungroup()
  } else if (.relation == "desc") {
    breakout_rels <- tree %>%
      distinct(parent, relation) %>%
      arrange(parent, desc(relation)) %>%
      group_by(parent) %>%
      slice(1) %>%
      ungroup()
  } else { # case where taken as string ord
    breakout_rels <- tree %>%
      distinct(parent, relation) %>%
      filter(relation %in% .relation) %>%
      mutate(relation = factor(relation, levels = .relation)) %>%
      arrange(parent, relation) %>%
      group_by(parent) %>%
      slice(1) %>%
      ungroup() %>%
      mutate(relation = as.character(relation))
  }

  breakout_tree <- tree %>%
    semi_join(breakout_rels, by = c("parent", "relation"))

  breakout_tree
}


