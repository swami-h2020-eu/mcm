#! /usr/bin/env python

import matplotlib.pyplot as plt
import numpy as np
import pathlib as pl


def read_header(fname):
    names = {}
    with open(fname) as f:
        for line in filter(lambda l: "#" in l, f):
            name, num = line.strip().split(":")
            name = name.replace("#", "").strip()
            try:
                names[name] = float(num)
            except ValueError:
                if "170" in num:
                    names["UM_run"] = 2002
                elif "70" in num:
                    names["UM_run"] = 2008
                elif "140" in num:
                    names["UM_run"] = 2004
    return names


def plot_profile(ax, fname):
    dat = np.loadtxt(fname)

    h = dat[:, 0]
    var = dat[:, 1]

    names = read_header(fname)
    ax.plot(var, h)

    ax.fill_between([min(var), max(var)], 109, 140, color="k", alpha=0.1)

    ax.set_title(" | ".join(f"{k}={v}" for k, v in names.items()))

    ax.set_ylabel("Altitude / km")
    if "dens" in str(fname):
        ax.set_xlabel("Density / g/cm³")
        ax.set_xscale("log")
    elif "temp" in str(fname):
        ax.set_xlabel("Temperature / K")

    ax.grid(True)

    return ax


if __name__ == "__main__":
    import sys

    try:
        p = sys.argv[1]
    except IndexError:
        p = "altitude_profile"

    p = pl.Path(p)
    for f in p.glob("*.dat"):

        print(f.parent, f.name)

        fig, ax = plt.subplots(1, 1)

        ax = plot_profile(ax, f)
        fig.savefig(f"{f.parent}/{f.name}.png")

        plt.close('all')
