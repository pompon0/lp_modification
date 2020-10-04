load("@bazel_gazelle//:deps.bzl", "go_repository")

def gazelle_generated():
    go_repository(
        name = "org_golang_google_protobuf",
        importpath = "google.golang.org/protobuf",
        sum = "h1:UhZDfRO8JRQru4/+LlLE0BRKGF8L+PICnvYZmx/fEGA=",
        version = "v1.24.0",
    )

    go_repository(
        name = "org_golang_x_sync",
        commit = "e225da77a7e68af35c70ccbf71af2b83e6acac3c",
        importpath = "golang.org/x/sync",
    )

    go_repository(
        name = "org_golang_google_grpc",
        importpath = "google.golang.org/grpc",
        sum = "h1:EC2SB8S04d2r73uptxphDSUG+kTKVgjRPF+N3xpxRB4=",
        version = "v1.29.1",
    )

    go_repository(
        name = "org_golang_x_net",
        importpath = "golang.org/x/net",
        sum = "h1:efeOvDhwQ29Dj3SdAV/MJf8oukgn+8D8WgaCaRMchF8=",
        version = "v0.0.0-20191209160850-c0dbc17a3553",
    )

    go_repository(
        name = "org_golang_x_text",
        importpath = "golang.org/x/text",
        sum = "h1:tW2bmiBqwgJj/UpqtC8EpXEZVYOwU0yG4iWbprSVAcs=",
        version = "v0.3.2",
    )

    go_repository(
        name = "org_golang_x_oauth2",
        importpath = "golang.org/x/oauth2",
        sum = "h1:pE8b58s1HRDMi8RDc79m0HISf9D4TzseP40cEA6IGfs=",
        version = "v0.0.0-20191202225959-858c2ad4c8b6",
    )

    go_repository(
        name = "com_google_cloud_go",
        importpath = "cloud.google.com/go",
        sum = "h1:vtAfVc723K3xKq1BQydk/FyCldnaNFhGhpJxaJzgRMQ=",
        version = "v0.58.0",
    )

    go_repository(
        name = "org_golang_google_api",
        importpath = "google.golang.org/api",
        sum = "h1:VJZ8h6E8ip82FRpQl848c5vAadxlTXrUh8RzQzSRm08=",
        version = "v0.26.0",
    )

    go_repository(
        name = "io_opencensus_go",
        importpath = "go.opencensus.io",
        sum = "h1:8sGtKOrtQqkN1bp2AtX+misvLIlOmsEsNd+9NIcPEm8=",
        version = "v0.22.3",
    )

    go_repository(
        name = "com_github_googleapis_gax_go_v2",
        importpath = "github.com/googleapis/gax-go/v2",
        sum = "h1:sjZBwGj9Jlw33ImPtvFviGYvseOtDM7hkSKB7+Tv3SM=",
        version = "v2.0.5",
    )

    go_repository(
        name = "com_github_golang_groupcache",
        importpath = "github.com/golang/groupcache",
        sum = "h1:1r7pUrabqp18hOBcwBwiTsbnFeTZHV9eER/QT5JVZxY=",
        version = "v0.0.0-20200121045136-8c9f03a8e57e",
    )


