#!/bin/sh

# Decrypt the file
mkdir -p $HOME/secrets
# --batch to prevent interactive command
# --yes to assume "yes" for questions
gpg --quiet --batch --yes --decrypt --passphrase="$CLIENT_CONFIG_PASSPHRASE" \
--output $HOME/secrets/client.config.js client.config.js.gpg
