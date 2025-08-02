# !/usr/local/bin/python
# -*- coding: utf-8 -*-

"""
- **Project:** mcpscr-lite
- **Version:** 0.0.1
- **Author:** SammygoodTunes
"""

from mcpscr import javaparser
from mcpscr.core import MCPSCR
import tkinter as tk
from tkinter import filedialog


def main() -> None:
    """
    Main
    :return:
    """
    root = tk.Tk()
    root.withdraw()
    mcp_folder = filedialog.askdirectory()
    if not mcp_folder:
        Exception("Invalid MCP directory")
    mcpscr = MCPSCR(mcp_folder)
    mcpscr.prepare()
    mcpscr.main_menu()

if __name__ == '__main__':
    main()