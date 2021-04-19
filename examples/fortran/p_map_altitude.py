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
            num = num.strip().split()
            names[name] = np.asfarray(num)
    return names


def plot_map(fname):
    header = read_header(fname)
    lat = header["latitudes"]
    lt = header["local_times"]

    dat = np.loadtxt(fname)
    fig, ax = plt.subplots(1, 1)

    lt_, lat_, = np.meshgrid(lt, lat)

    c = ax.contourf(lt_, lat_, dat)
    fig.colorbar(c, ax=ax)

    ax.set_ylabel("Latitude / deg")
    ax.set_xlabel("Local time / h")

    title = {k: v for k, v in header.items() if k in ["f10.7", "f10.7m", "kp", "doy", "altitude"]}
    title = " | ".join(f"{k}={v}" for k, v in title.items())
    var = "Density / g/cm3" if "dens" in str(fname) else "Temperature / K"
    title = f"{var}\n{title}"

    ax.set_title(title)

    return fig


if __name__ == "__main__":
    import sys

    try:
        p = sys.argv[1]
    except IndexError:
        p = "map_altitude"

    p = pl.Path(p)
    for f in p.glob("*.dat"):

        print(f.parent, f.name)

        

        fig = plot_map(f)

        fig.savefig(f"{f.parent}/{f.name}.png")

        plt.close('all')
