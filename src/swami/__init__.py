# ---------------------------------------------------------------------
# – Project : SWAMI
# – Customer : N/A
# ---------------------------------------------------------------------
# – Author : Daniel Lubián Arenillas
# – Issue : 1.0
# – Date : 2021-03-31
# – Purpose : Python functions for SWAMI
# - Component : swami python library
# ---------------------------------------------------------------------
# – © Copyright Deimos Space SLU, 2021
# – All rights reserved
# ---------------------------------------------------------------------

import os
import subprocess
import tempfile
from pathlib import Path
from typing import Dict, NamedTuple

__version__ = "swami-1.0.rc"

_PWD = Path(__file__).absolute().parent
_NONE = -999999

_PATH_DEFAULT_EXEC = _PWD / "swami.x"
_PATH_DEFAULT_DATA = _PWD / "data"


class MCMOutput(NamedTuple):
    # output
    dens: float
    temp: float
    wmm: float
    d_H: float
    d_He: float
    d_O: float
    d_N2: float
    d_O2: float
    d_N: float
    tinf: float
    dens_unc: float
    dens_std: float
    temp_std: float
    xwind: float
    ywind: float
    xwind_std: float
    ywind_std: float
    # input
    alti: float
    lati: float
    longi: float
    loct: float
    doy: float
    f107: float
    f107m: float
    kp1: float
    kp2: float


class MCM:
    """MCM Model wrapper.

    Args:
        exec_swami (os.PathLike, optional): Path to the executable. Defaults to the one included.
        path_to_data (os.PathLike, optional): Path to the data. Defaults to the included package.
    """

    path_to_bin: Path = _PATH_DEFAULT_EXEC
    path_to_data: Path = _PATH_DEFAULT_DATA

    def __init__(
        self,
        exec_swami: Path | str | None = None,
        path_to_data: Path | str | None = None,
    ):
        """Initialiser

        Args:
            exec_swami (os.PathLike, optional): Path to the executable. Defaults to the one included.
            path_to_data (os.PathLike, optional): Path to the data. Defaults to the included package.
        """

        if exec_swami is not None:
            self.path_to_bin = Path(exec_swami)
        if path_to_data is not None:
            self.path_to_data = Path(path_to_data)

    @staticmethod
    def _generate_nml_from_dict(
        d: Dict[str, int | float | bool | str], name: str = "input"
    ):
        """Generate a namelist file from a dictionary

        Args:
            d (dict): Dictionary
            name (str, optional): Name of the namelist. Defaults to "input".
        """

        def logical(b: bool):
            return ".true." if b else ".false."

        with tempfile.NamedTemporaryFile(
            prefix="swami_", delete=False, suffix=".inp", mode="r+"
        ) as nml:
            _ = nml.write(f"&{name}\n")
            for k, v in d.items():
                # Booleans
                if isinstance(v, bool):
                    _ = nml.write(f"{k} = {logical(v):s}\n")
                # Strings
                elif isinstance(v, str):
                    _ = nml.write(f"{k} = '{v:s}'\n")
                # Floats
                elif isinstance(v, float):
                    _ = nml.write(f"{k} = {v:23.16e}\n")
                # Integers
                elif isinstance(v, int):  # pyright: ignore[reportUnnecessaryIsInstance]
                    _ = nml.write(f"{k} = {v:d}\n")
                # Others
                else:
                    _ = nml.write(f"{k} = {v}\n")
            # nml.write("\\")
            _ = nml.write("&end\n")
            nml.close()

            return nml.name

    @staticmethod
    def _read_output_file(outfile: Path | str):
        """Read output file from swami.x

        Args:
            outfile (os.PathLike): Path to output file

        Returns:
            dict: Dictionary with values per model
        """
        with open(outfile, mode="r") as f:
            res = {}
            for line in f:
                var, val = line.split("=")
                val = float(val)
                val = None if val == _NONE else val
                res[var.strip()] = val

            return MCMOutput(**res)

    def run(
        self,
        altitude: float,
        day_of_year: float,
        local_time: float,
        latitude: float,
        longitude: float,
        f107: float,
        f107m: float,
        kp1: float,
        kp2: float,
        get_uncertainty: bool = False,
        get_winds: bool = False,
    ) -> MCMOutput:
        """Run the model

        Returns a MCMOutput object with the results as attributes.

        Args:
            altitude (float): Altitude in km
            day_of_year (float): Day of the year [0-366]
            local_time (float): Local time, h [0-24]
            latitude (float): Latitude, deg [-90 to 90]
            longitude (float): Longitude, deg [0-360]
            f107 (float): F10.7, instantaneous flux at (t - 24hr)
            f107m (float): F10.7, average of the last 81 days
            kp1 (float): Kp, delayed by 3 hours
            kp2 (float): Kp, mean of previous 24 hours
            get_uncertainty (bool, optional): Uncertainties will be returned. Defaults to False.
            get_winds (bool, optional): Winds will be returned. Defaults to False.

        Returns:
            MCMOutput: NamedTuple with the results
        """

        # Make temporary output file
        output_file = tempfile.NamedTemporaryFile(
            delete=False, suffix=".out", prefix="swami_", mode="r+"
        )

        # Sanitize paths
        data_dtm = str(self.path_to_data)
        data_dtm = data_dtm + "/" if data_dtm[-1] != "/" else data_dtm
        data_um = str(os.path.join(self.path_to_data, "um"))
        data_um = data_um + "/" if data_um[-1] != "/" else data_um

        # Create dictionary with input parameters
        input_dict = {
            "altitude": float(altitude),
            "day_of_year": float(day_of_year),
            "local_time": float(local_time),
            "latitude": float(latitude),
            "longitude": float(longitude),
            "f107": float(f107),
            "f107m": float(f107m),
            "kp1": float(kp1),
            "kp2": float(kp2),
            "b_unc_std": bool(get_uncertainty),
            "b_winds": bool(get_winds),
            "data_dtm": data_dtm,
            "data_um": data_um,
            "output_file": str(output_file.name),
        }

        # Generate input file
        input_file = self._generate_nml_from_dict(input_dict)

        # Run command
        cmd = [str(self.path_to_bin), input_file]
        _ = subprocess.run(
            cmd,
            check=True,
            cwd=self.path_to_bin.parent,
            env={"LD_LIBRARY_PATH": str(self.path_to_bin.parent)}
        )

        # Read output file
        out = self._read_output_file(output_file.name)

        # Delete temporary files
        os.unlink(input_file)
        os.unlink(output_file.name)

        return out
