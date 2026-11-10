#!/usr/bin/env python3
"""Run a command in a pseudo-terminal of a fixed size and return its exit status.

    python3 test/hel-test-tty.py ROWS COLUMNS TIMEOUT COMMAND...

`make test-display' uses it to run Emacs in a terminal frame, where Emacs
does real redisplay, unlike a batch Emacs.  The terminal output is thrown
away; the Emacs side writes its results to a file.  When TIMEOUT seconds
pass, the command is killed and the status is 124, as with `timeout'.
"""

import os
import pty
import select
import signal
import struct
import sys
import termios
import fcntl
import time


def main():
    rows, columns, timeout = (int(arg) for arg in sys.argv[1:4])
    command = sys.argv[4:]
    pid, fd = pty.fork()
    if pid == 0:
        os.environ["TERM"] = "xterm-256color"
        os.execvp(command[0], command)
    fcntl.ioctl(fd, termios.TIOCSWINSZ, struct.pack("HHHH", rows, columns, 0, 0))
    deadline = time.monotonic() + timeout
    while time.monotonic() < deadline:
        ready, _, _ = select.select([fd], [], [], 0.5)
        if ready:
            try:
                os.read(fd, 65536)
            except OSError:
                pass  # The terminal closes when the command exits.
        done, status = os.waitpid(pid, os.WNOHANG)
        if done:
            return os.waitstatus_to_exitcode(status)
    os.kill(pid, signal.SIGKILL)
    os.waitpid(pid, 0)
    print(f"hel-test-tty: killed after {timeout} seconds", file=sys.stderr)
    return 124


if __name__ == "__main__":
    sys.exit(main())
