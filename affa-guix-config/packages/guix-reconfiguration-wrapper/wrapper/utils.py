import sys
from subprocess import Popen, PIPE, CalledProcessError

import rich
from git import Repo

def execute(cmd):
    """Executes a command and prints the output to stdout as it is running"""
    with Popen(cmd, stdout=PIPE, bufsize=1, universal_newlines=True) as process:
        for line in process.stdout:
            if not line:
                break
            print(line, end="")  # Outputs the line immediately
        if process.returncode != 0 and process.returncode != None:
            raise CalledProcessError(process.returncode, cmd)


def is_config_worktree_dirty(config_dir):
    """
    Have we been a dirty dirty girl?

    This does not trigger for any untracked files it will only trigger for tracked ones.
    That should probably be changed but this stops a lot of sillyness which we have been dealing with
    """
    repo = Repo(config_dir)
    return repo.is_dirty()


def print_heading(msg, extra=False):
    """Prints Headings with lines"""
    print()
    print()
    rich.print(f"[bold magenta]{msg}[/bold magenta]")
    if extra:
        rich.print(f"[bold]{extra}[/bold]")
    print()
    print()


def print_error(msg):
    """Prints Error Messages in Bold Red Text"""
    rich.print(f"[bold red]{msg}[/bold red]")
