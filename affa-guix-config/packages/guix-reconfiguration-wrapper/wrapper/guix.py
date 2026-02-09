from os import path, getenv
import sys

from dotenv import load_dotenv

from wrapper.utils import print_heading
from wrapper.utils import execute

def guix_home_pull():
    """Pulls a new version of the users Guix"""
    print_heading("Running Guix Home Pull")
    execute(["guix", "pull"])


def guix_home_reconfigure(system):
    """Reconfigures the Users Home"""
    print_heading("Running Guix Home Reconfigure", system)

    restic_secrets_file = path.expanduser("~/.pass/restic.env")
    if not load_dotenv(dotenv_path=restic_secrets_file):
        print_error(f"Cannot open {restic_secrets_file}")
        sys.exit(-1)

    execute(
        [
            "guix",
            "home",
            "reconfigure",
            "-L",
            path.expanduser("~/src/guix-config/"),
            "-L",
            path.expanduser("~/src/afistfullofash/"),
            "-e",
            f"(use-modules (affa-guix-config home {system})) {system}-home-environment"
        ]
    )


def guix_system_pull():
    """Pulls a new version of guix for the system"""
    print_heading("Running Guix System Pull")
    execute(["sudo", "guix", "pull"])


def guix_system_reconfigure(system):
    """Runs a System Reconfiguration"""
    print_heading("Running Guix System Reconfigure", system)
    execute(
        [
            "sudo",
            "guix",
            "system",
            "reconfigure",
            "-L",
            path.expanduser("~/src/afistfullofash/"),
            "-L",
            path.expanduser("~/src/guix-config/"),
            "-e",
            f"(use-modules (affa-guix-config systems {system})) {system}-operating-system"
        ]
    )
