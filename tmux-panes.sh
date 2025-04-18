#!/usr/bin/env bash
tmux new-session -d
tmux split-window -v
tmux select-pane -t 0
tmux split-window -h
tmux select-pane -t 2
tmux split-window -h
tmux select-pane -t 0
tmux attach-session
