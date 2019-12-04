![build](https://github.com/davlum/two-pc-idr/workflows/test/badge.svg)

# two-pc-idr
Implementation of the [Two-phase commit](https://en.wikipedia.org/wiki/Two-phase_commit_protocol) 
protocol in Idris

To build:

```bash
nix build
```

This should create a binary at `result/bin/twopc`. Let's
bring up a few instances of postgres to run two-phase commit over

```
docker-compose up -d
```

Some example usage:

```bash
$ ./result/bin/twopc connections
Connected to peers.
Input query or END to Stop:
CREATE TABLE test (
    did     integer CHECK (did > 100),
    name    varchar(40)
);

Prepare succeeded. Commiting transaction...
Commit succeeded.
Input query or END to Stop:
insert into test values (101, 'hello'),(202,'goodbye');
...
```

