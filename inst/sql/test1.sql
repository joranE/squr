#' Query Title
#'
#' @param in_arg IN argument
#' @param field4 a value
#' @return field1 description of field1
#' @return field2 describtion of field2
select
  field1,
  field2,
  @field3
from
  my_table
where
  bar in @in_arg and
  foo = @field4
