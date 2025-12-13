import argparse
from os import path
import sys

from wrapper.guix import guix_home_pull, guix_home_reconfigure, guix_system_pull, guix_system_reconfigure
from wrapper.utils import is_config_worktree_dirty, print_error

def cli():
    parser = argparse.ArgumentParser(
        prog="guix-reconfiguration-wrapper",
        description="Handles Guix Reconfiguration Logic for afistfullofash's guix configuration",
    )
    parser.add_argument("target", choices=["system", "home"])
    parser.add_argument("system", choices=["desktop", "laptop"])
    parser.add_argument("speed", choices=["full", "quick"])

    args = parser.parse_args()

    config_dir = path.expanduser("~/src/guix-config/")

    if args.target == "home":
        if args.speed == "quick":
            guix_home_reconfigure(args.system)
        elif args.speed == "full":
            if is_config_worktree_dirty(config_dir):
                print_error("Ensure there is a clean git worktree before guix pull")
                sys.exit(-1)
            guix_home_pull()
            guix_home_reconfigure(args.system)

    elif args.target == "system":
        if args.speed == "quick":
            guix_system_reconfigure(args.system)
        elif args.speed == "full":
            if is_config_worktree_dirty(config_dir):
                print_error("Ensure there is a clean git worktree before guix pull")
                sys.exit(-1)
            guix_system_pull()
            guix_system_reconfigure(args.system)
