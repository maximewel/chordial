#!/bin/bash

kill $(ps aux | grep 'node' | awk '{print $2}')