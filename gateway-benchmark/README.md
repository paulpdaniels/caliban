# Caliban GraphQL Gateways Benchmark adapter

This non-published project runs Caliban's production Quick HTTP path against the pinned
[GraphQL Gateways Benchmark](https://github.com/graphql-hive/graphql-gateways-benchmark). The revision is recorded once in
[`upstream.env`](upstream.env); `prepare-upstream.sh` refuses any other checkout.

The adapter acquires the four authored Federation schemas from the benchmark subgraphs, builds an ordinary
`GatewayInterpreter`, and serves it with `QuickAdapter`. It does not consume the benchmark's serialized supergraph. It does
turn on Caliban's in-flight query deduplication, which is off by default, for each remote source, so overlapping identical
queries share one running downstream call. The tested competitor versions behave the same way. A unique-header control
exists so you can separate that benefit from general execution speed.

## Prepare and run

```sh
git clone https://github.com/graphql-hive/graphql-gateways-benchmark.git /path/to/graphql-gateways-benchmark
. ./gateway-benchmark/upstream.env
git -C /path/to/graphql-gateways-benchmark checkout "$GRAPHQL_GATEWAYS_BENCHMARK_REVISION"
./gateway-benchmark/prepare-upstream.sh /path/to/graphql-gateways-benchmark
./gateway-benchmark/install.sh
```

Start the upstream subgraphs, then run the benchmark through the upstream driver:

```sh
make -C /path/to/graphql-gateways-benchmark run-subgraphs
WAIT_FOR_URL=http://127.0.0.1:4000/health \
  make -C /path/to/graphql-gateways-benchmark test gateway=caliban mode=constant

WAIT_FOR_URL=http://127.0.0.1:4000/health \
  make -C /path/to/graphql-gateways-benchmark test gateway=caliban mode=stress
```

The pinned driver's constant profile runs a 15-second warmup, then 50 virtual users for 60 measured seconds. Its stress
profile keeps the same warmup and ramps from 0 to 50, to 500, and back to 50 users over 60 seconds. Use the same unmodified
profile for every comparator. The recorded runs used k6 0.54.0.

`BENCHMARK_SUBGRAPHS_URL` changes the source base URL and `BENCHMARK_GATEWAY_PORT` changes the adapter port; the defaults
are `http://127.0.0.1:4200` and `4000`. `BENCHMARK_UNIQUE_SOURCE_HEADERS=true` gives every logical source call a unique
harmless header. That exists only for the non-deduplicable control. Do not set it for the primary comparison.

## Measurement contract

Nothing here modifies the pinned benchmark's driver or its tracked gateway configurations. `prepare-upstream.sh` rejects a
checkout with tracked changes, then adds one untracked Caliban adapter directory holding `run.sh` and `target` symlinks.
Results use the upstream driver's operation, response checks, setup, warmup, load profile, metrics, and router defaults
exactly as published at the pinned revision.

The comparison reports the upstream `iterations` rate, which counts measured scenario iterations rather than every request
the driver's setup function makes. The upstream checks still require a `200` response, no GraphQL errors, and the expected
response structure.

## Comparable configurations

Every measured implementation uses the same four upstream endpoints, operation text, response checker, warmup, measured
duration, and virtual-user count, and the native routers keep the pinned upstream defaults. Apollo's pinned adapter turns
query deduplication on explicitly; the tested Cosmo, Grafbase, and Hive versions do the equivalent outbound sharing by
default. Caliban's declared override is therefore comparable. The unique-header control is still necessary, because a
workload built on repeated queries is an unusually favorable case for all of them.

| Implementation | Artifact at the published revision | Input | Telemetry |
| --- | --- | --- | --- |
| Caliban | This directory | Authored SDL acquired from `_service` | None installed |
| Apollo Router 2.6.0 | Upstream `apollo-router` | Upstream supergraph | Upstream run script |
| Hive Router 0.0.8 installer | Upstream `hive-router` | Upstream supergraph | Upstream run script |
| Grafbase Gateway 0.49.0 | Upstream `grafbase` | Upstream federated graph | Upstream run script |
| Cosmo Router 0.247.0 | Upstream `cosmo` | Upstream engine config | Upstream run script |
| Hot Chocolate Fusion | Not present upstream | Fusion archive required | Not available |

The dashboard labels Hive Router 0.0.5, while the installer checked into the corresponding source revision asks for 0.0.8.
Both of those release assets are gone, so the same-host comparison uses 0.0.43 with the published configuration. The pinned
repository has no Hot Chocolate Fusion adapter either. Treat any release ranking drawn from this seed workload alone as
unsupported. The provenance is not there.

## Profiles

Use the upstream monitor for process CPU and resident memory. For JVM profiles, launch Caliban with GC and native-memory tracking:

```sh
JAVA_OPTS='-Xlog:gc*:file=/tmp/caliban-gateway-gc.log:time,uptime,level,tags -XX:NativeMemoryTracking=summary' \
  ./gateway-benchmark/run.sh
```

During the measured interval, capture CPU and allocation flamegraphs with async-profiler and take memory snapshots with `jcmd`:

```sh
asprof -d 30 -e cpu -f /tmp/caliban-gateway-cpu.html PID
asprof -d 30 -e alloc -f /tmp/caliban-gateway-alloc.html PID
jcmd PID GC.heap_info
jcmd PID VM.native_memory summary
```

Profiles are deliberately not checked in. They carry machine-specific symbols and are far too easy to mistake for
comparable results.
