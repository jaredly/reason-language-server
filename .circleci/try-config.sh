#!/usr/bin/env bash
curl --user `cat ~/.ssh/circle-token`: \
    --request POST \
    --form revision=5a18cb68ae32de30e0504d7b8398fc8fa088092a\
    --form config=@config.yml \
    --form notify=false \
        https://circleci.com/api/v1.1/project/github/jaredly/reason-language-server/tree/master

