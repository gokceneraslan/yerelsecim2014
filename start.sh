#!/bin/bash

R -e "shiny::runApp('.', port=8080, host='0.0.0.0')"
