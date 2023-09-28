#!/bin/bash
top -bn1 | awk '/Cpu/ { print $2}'
