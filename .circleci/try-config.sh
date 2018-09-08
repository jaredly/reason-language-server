#!/usr/bin/env bash
curl --user `cat ~/.ssh/circle-token`: \
    --request POST \
    --form revision=c1bd97a7e136cd54f6ec00048776f5badaa361a6\
    --form config=@config.yml \
    --form notify=false \
        https://circleci.com/api/v1.1/project/github/jaredly/reason-language-server/tree/master

