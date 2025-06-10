#!/usr/bin/env bash
use_sops() {
    local path=${1:-$PWD/secrets.yaml}
    shift

    read_vars() {
        SOPS="sops -d --output-type dotenv"
        if [[ $# -eq 0 ]]; then
            $SOPS "$path"
        else
            for x in "$@"; do
                $SOPS --extract "['$x']" "$path"
            done
        fi | direnv dotenv bash /dev/stdin
    }

    eval "$(read_vars "$@")"
    watch_file "$path"
}
