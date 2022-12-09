#!/usr/bin/env bash

# rust-analyzer
curl -L https://github.com/rust-lang/rust-analyzer/releases/latest/download/rust-analyzer-aarch64-apple-darwin.gz | gunzip -c - > ./rust-analyzer
chmod +x rust-analyzer
