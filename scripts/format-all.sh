#!/usr/bin/env bash

find . -type f -name "*.dhall" -exec dhall format --inplace {} \;