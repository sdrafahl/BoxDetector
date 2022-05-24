#!/bin/bash

grid=$(cat $1 | tr '\n' 'T')
java -jar bounding-box $grid




