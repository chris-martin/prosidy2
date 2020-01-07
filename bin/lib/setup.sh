#!/bin/bash
set -euo pipefail
cd "$(git rev-parse --show-toplevel)"
msg ()  { printf -- '\e[1m----> %s\e[m\n' "$*" >&2; }
warn () { printf -- '\e[33;1m----! %s\e[m\n' "$*" >&2; }
err ()  { printf -- '\e[31;1m----! %s\e[m\n' "$*" >&2; exit 1; }

