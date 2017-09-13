# squr 0.0.3

* Removed INSERT, transaction, ignored block features; may add them back later

* New list format for `sq` objects

* Delay binding of values to parameters until query is sent via `sq_send()`

* `IN()` flags values for use in an `IN` clause; long arguments (>1k elements)
are split into chunks and when `sq_send()` is called multiple queries are sent
and then conbined.

* Add tests
