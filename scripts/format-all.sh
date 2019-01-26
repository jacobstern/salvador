#!/usr/bin/env bash

find . -not -path "./dist*" -type f -name "*.dhall" -exec dhall format --inplace {} \;