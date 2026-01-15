#!/bin/bash

if [ ! -d $HOME/.hewg/bootstrap/crow.lexible ]; then
  mkdir -p $HOME/.hewg/bootstrap/crow.lexible
fi

cp include/lexible.hh $HOME/.hewg/bootstrap/crow.lexible/
