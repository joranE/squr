select
  col1,
  col2
from
  my_table
where
  field1 in @in_param1 and
  field2 in @in_param2 and
  field3 = @param1
