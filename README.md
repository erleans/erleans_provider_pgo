Erleans Postgres Persistence Provider
=====

[Erleans](https://github.com/erleans/erleans) Persistence Provider backed by Postgres with library [pgo](https://github.com/erleans/pgo).

# Usage

Configure your Grains to use the Postgres provider and the database to use for
the `pgo` pool that backs the provider in `sys.config`:

```
[{erleans, [{providers, #{pgo => #{module => erleans_provider_pgo,
                                   args => #{pool_size => 10,
                                             host => "127.0.0.1",
                                             database => "test",
                                             user => "test",
                                             password=> "test"}
                                  }}},

            {default_provider, pgo}]},
```

Now you can run a grain and save it to Postgres:

```
> Grain1 = erleans:get_grain(test_grain, <<"grain1">>).
> test_grain:activated_counter(Grain1).
> test_grain:save(Grain1).
```

```
test=# \dt
            List of relations
 Schema |      Name      | Type  | Owner
--------+----------------+-------+-------
 public | erleans_grains | table | test
 test=# select * from erleans_grains;
          grain_id          | grain_type | grain_ref_hash | grain_etag |
                                                      grain_state
                                                |        change_time
----------------------------+------------+----------------+------------+--------
--------------------------------------------------------------------------------
------------------------------------------------+----------------------------
 \x836d00000006677261696e31 | test_grain |       99587447 |   64845372 | \x83740
000000377116163746976617465645f636f756e7465726101771364656163746976617465645f636
f756e7465726101770c63616c6c5f636f756e7465726102 | 2024-11-10 10:25:31.303615
(1 row)
 ```
 
 
