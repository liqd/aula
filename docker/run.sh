#!/bin/sh

docker run -it --rm -p 8080:8080 -v `pwd`:/root/aula -v `pwd`/../thentos:/root/thentos fisx/aula /bin/bash

