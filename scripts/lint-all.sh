#!/usr/bin/env bash

find . -type f -name "*.dhall" -exec dhall lint --inplace {} \;