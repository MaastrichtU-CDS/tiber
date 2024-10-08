Package: {{PKG_NAME}}
Type: Package
Version: 0.1.0
Title: {{PKG_NAME}}
Description: This package can be used with the vantage6 federated
    learning infrastructure (see https://github.com/IKNL/vantage6).
Encoding: UTF-8
LazyData: true
Depends:
    dplyr
Imports:
    glue,
    vtg,
    bnlearn,
    ROCaggregator,
    ROCR,
    pracma,
    caret,
    mice,
    gRain
Remotes:
    iknl/vtg@b198199
RoxygenNote: 7.0.0
