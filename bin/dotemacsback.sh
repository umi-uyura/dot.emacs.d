#!/bin/bash

rsync -a --delete --exclude=".DS_Store" ~/.emacs.d/ ~/dot.emacs.d_${HOSTNAME}/
