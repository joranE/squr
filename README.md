![squr-hex](./squr-hex.png?raw=true "squr logo")
# squr [/'skju:əɹ/]: Structured Query Utility for R

This is a substantially re-written fork of Stefan Milton Bache's `squr` package.

The `squr` (pronounced "skewer") package provides a set of tools
for managing structured query language (SQL) files in R projects.
It promotes the separation of logic and query scripts and eases the process
of reading, parameterizing, and composing SQL queries.

The primary changes are the removal of several features that are unnecessary
for my own purposes (transactions, `INSERT` statements, ignore blocks,
composing SQL snippets with `+`) although I may add them back at a later date.
Additionally, I've made significant internal changes to handle the (specific 
to me) situation of needing to run queries with very long `IN` clauses with
more than 1000 elements.

## Example

In the following example the `sql/query.sql` file is read and
parameterized, and sent. There is no database connectivivty in `squr`,
this is left fully flexible and out of scope. There is a `sq_send` 
function, but this is only meant as a wrapper for invoking some 
actual query function.

```R
# Simple example of a query function wrapper. This part varies depending
# on database, drivers, etc, but needs only setup once.
rodbc <- function(query)
{
  ch <- RODBC::odbcDriverConnect("<connectionstring>")
  on.exit(RODBC::odbcClose(ch))
  RODBC::sqlQuery(ch, query)
}

# Example of reading file, setting a parameter value, and sending the query,
# using the `sq_*` ("skew") functions.
result <- 
  sq_file("sql/query") %>% 
  sq_set(Param = value) %>%
  sq_send(.with = rodbc)
```

The corresponding `query.sql` file could look something like:
```SQL
SELECT *
  FROM TheTable
 WHERE Param = @Param
```

Note that many arguments in `squr` are prefixed with a dot; this is to
avoid name clash with values in `...` arguments.

## Separation of logic and SQL Files
It can be argued that it is good practice to keep query scripts separate
from R code, and `squr` aims at

* cleaner R source code,
* queries that (mostly) work as-is both in R and in your SQL IDE,
* easy composability and reusability of SQL blocks

To use a separate query from R:

```R
sql <- sq_file("path/query")
```

Note: 

* the `.sql` extension can be omitted,
* when packaged, the path is relative to the `inst`allation folder.

For the rare occasion, there is also `sq_text`, which is the 
way to add inline SQL. Both `sq_file` and `sq_text` produces
S3 objects which are lists with an additional class
`sq` to enable a few methods (e.g. `print`).

## Parameterizing IN Clauses
Suppose we have the following SQL:

```SQL
SELECT *
  FROM table
 WHERE column IN @column
```
and we'd like to be able to run this query with various collections of values
inserted for `@column`. In `squr` we can do the following:

```r
result <- sq_text(.query = "select * from table where column in @column") %>%
  sq_set(column = IN(1:10)) %>%
  sq_send(.with = rodbc)
```
Wrapping the vector of values to be bound with `IN()` ensures that they are 
properly formatted and wrapped in parens. This will work for only a single value,
resulting in something like `column IN (1)`. Also, if the vector of values passed
is longer than 1000 `squr` will split it into chunks of size at most 1000 and 
then send multiple queries for each chunk, `rbind`ing the results.

Splitting `IN` clauses like this only works for one `IN` clause per SQL query.

## Replacing SQL Text Inline
There is also a function `sq_replace` that is a wrapper for `gsub` that allows
you to edit SQL text, but only prior to setting parameter values with `sq_set`.

## Dynamic Table and Column Names
Since values are appropriately quoted when they are bound, the default 
replacements will not work for dynamically specifying column and table names.
However, you can use `sq_value` explicitly (this is the function used
internally to prepare a value for SQL):

```SQL
-- The SQL file
SELECT [Date]
     , [CustomerId]
     , [CustomerName]
     , @Feature
  FROM Customers
 WHERE Date BETWEEN @DateFrom AND @DateTo
```

```R
# R
result <- 
  sq_file("customers") %>% 
  sq_set(DateFrom = Sys.Date() - 10, 
         DateTo   = Sys.Date(), 
         Feature  = sq_value("Turnover", quote = "[")) %>% 
  sq_send(.with = rodbc)
```

## A note on SQL injection
The `squr::sq_value` function uses `DBI::sqlInterpolate` to parse values.
However, whenever the values
originate from user input (e.g. in a Shiny/web application, or web services, etc),
approprite precautions should still be taken (type checking, whitelisting, etc.) 

OWASP has some [good guidelines](https://www.owasp.org/index.php/SQL_Injection_Prevention_Cheat_Sheet)
on how to prevent SQL injection in your applications.

## See also
A similar (but different) project for Clojure (with ports for some other languages) by @krisajenkins is [Yesql](https://github.com/krisajenkins/yesql).
