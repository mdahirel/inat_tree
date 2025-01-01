# TO DO
# allow more arguments to the request
# iconic taxon is a good idea, but would like to allow for "truly all animals" and "invertebrates" settings too
# extract more from the request
# for now only names, but a set up where you extract everything actually would be useful
# for my plan to get a regular offline full backup of my own inat obs (RG or not)

get_inat <- function(
    user_id=NULL,
    project_id=NULL,
    iconic_taxon=NULL
){
  
  assertthat::assert_that(
    assertthat::is.string(user_id)|assertthat::is.string(project_id),
    msg="at least one of user_id or project_id needs to be specified (as a string)"
    )
  
  inat_request <-httr2::request("https://api.inaturalist.org/v1") |> 
    req_url_path_append("observations") |> 
    req_url_query(
      `user_id`= user_id,
      `project_id`= project_id,
      `page`=1,
      `per_page`=200,
      `order`="desc",
      `order_by`="created_at") |> 
    req_throttle(
      rate=30/60  # requests/sec  ## iNat API recommends at most 1req/sec
    ) |> 
    req_perform_iterative(
      iterate_with_offset(
        "page",
        resp_pages = function(resp) ceiling(resp_body_json(resp)$total_resul/resp_body_json(resp)$per_page)
      ),
      max_reqs=50, # iNat recommends maxing out requests using APIs at 10k returned results
    )|> 
    resps_successes()
  
  inat_parsed <- tibble(inat_request) |> 
    mutate(json=map(.x=inat_request,.f=~.x |> resp_body_json())) |> 
    mutate(results=map(.x=json,.f=~.x |> pluck("results"))) |> 
    select(results) |>
    unnest(results)|> 
    mutate(
      name=map(.x=results,.f= ~ .x |> pluck("taxon", "name")),
      id=map(.x=results,.f= ~ .x |> pluck("taxon", "id")),
      iconic_taxon_name=map(.x=results,.f= ~ .x |> pluck("taxon", "iconic_taxon_name")),
      iconic_taxon_id=map(.x=results,.f= ~ .x |> pluck("taxon", "iconic_taxon_id"))
    ) |> 
    select(name,id,iconic_taxon_name,iconic_taxon_id) |> 
    unnest(cols=c("name","id","iconic_taxon_name","iconic_taxon_id"))
  
  return(inat_parsed)
  
}
