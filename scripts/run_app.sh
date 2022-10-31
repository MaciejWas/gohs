#!/bin/bash
set -e


if [ ! $(redis-cli ping) ] ;
	echo "run_app > starting new redis instance"
	then redis-server $PROJECT_ROOT/microservices/redis/redis.conf;
fi


