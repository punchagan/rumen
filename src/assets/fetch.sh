#!/usr/bin/env bash

set -euo pipefail

cd "$(dirname "$0")"

if [ -z "${GITHUB_ACTIONS:-}" ]; then
    echo "This script can be set up to run periodically using cron."
    echo "This is useful, since running the fetch on GitHub Actions fails for"
    echo "some sites due to their anti-bot measures."
    echo
    echo "To run this script as a cron job, add something like this to your crontab :"
    echo "0 * * * * /path/to/fetch.sh >> /path/to/fetch.log 2>&1"
    echo "This will run the script every hour and log output to fetch.log."
fi

# Update the local repository if not running in a GitHub Action
if [ -z "${GITHUB_ACTIONS:-}" ]; then
    git pull --rebase origin
fi

# Check if readability-cli is installed
command -v readable >/dev/null 2>&1 || {
    echo "readability-cli is not installed. Please install it with:"
    echo "  npm install -g readability-cli"
    exit 1
}

# Download the latest rumen-cli executable if it does not exist
if ! [ -x ./rumen-cli.exe ]; then
    echo "Downloading rumen-cli..."
    curl -L -o rumen-cli.exe https://punchagan.github.io/rumen/scripts/rumen-cli.exe
    chmod +x rumen-cli.exe
fi

./rumen-cli.exe fetch

if [ -n "$(git status --porcelain -- ./articles/content)" ]; then
    git add articles/
    # Set committer information if running in a GitHub Action
    if [ -n "${GITHUB_ACTIONS:-}" ]; then
        git config user.email "rumen.fetch@example.com"
        git config user.name "Rumen CLI Fetch"
    fi
    git commit -m "Fetch article content"
    git push
fi
