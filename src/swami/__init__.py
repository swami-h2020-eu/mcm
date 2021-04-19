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
from enum import Enum
from pathlib import Path

__version__ = "swami-1.0.rc"

_PWD = Path(__file__).absolute().parent

_PATH_DEFAULT_EXEC = _PWD / "swami.x"
_PATH_DEFAULT_DATA = _PWD / "data"


class _AtmModel(Enum):
    """Enumerator on the three atmospheric models

    - AtmModel.MCM : MCM
    - AtmModel.DTM2020 : DTM2020
    - AtmModel.UM: UM
    """

    MCM = 1
    DTM2020 = 2
    UM = 3


class SwamiModel:
    """SWAMI Model wrapper. Includes MCM, DTM2020 and UM

    Args:
        model (str, optional): Model to use. Defaults to MCM. Valid values are: MCM, DTM2020, UM
        exec_swami (os.PathLike, optional): Path to the executable. Defaults to the one included.
        path_to_data (os.PathLike, optional): Path to the data. Defaults to the included package.
    """

    model = _AtmModel.MCM
    path_to_bin = _PATH_DEFAULT_EXEC
    path_to_data = _PATH_DEFAULT_DATA

    def __init__(self, model: str = None, exec_swami: os.PathLike = None, path_to_data: os.PathLike = None):
        """Initialiser

        Args:
            model (str, optional): Model to use. Defaults to MCM. Valid values are: MCM, DTM2020, UM
            exec_swami (os.PathLike, optional): Path to the executable. Defaults to the one included.
            path_to_data (os.PathLike, optional): Path to the data. Defaults to the included package.
        """

        if model is not None:
            self.model = _AtmModel[model]
        if exec_swami is not None:
            self.path_to_bin = exec_swami
        if path_to_data is not None:
            self.path_to_data = path_to_data

    @staticmethod
    def _generate_nml_from_dict(d: dict, name: str = "input"):
        """Generate a namelist file from a dictionary

        Args:
            d (dict): Dictionary
            name (str, optional): Name of the namelist. Defaults to "input".
        """

        def logical(b: bool):
            return ".true." if b else ".false."

        with tempfile.NamedTemporaryFile(prefix="swami_", delete=False, suffix=".inp", mode="r+") as nml:
            nml.write(f"&{name}\n")
            for k, v in d.items():
                if isinstance(v, bool):
                    nml.write(f"{k} = {logical(v):s}\n")
                elif isinstance(v, str):
                    nml.write(f"{k} = '{v:s}'\n")
                elif isinstance(v, float):
                    nml.write(f"{k} = {v:23.16e}\n")
                elif isinstance(v, int):
                    nml.write(f"{k} = {v:d}\n")
                else:
                    nml.write(f"{k} = {v}\n")
            # nml.write("\\")
            nml.write("&end\n")
            nml.close()

            return nml.name

    @staticmethod
    def _read_output_file(outfile: os.PathLike):
        """Read output file from swami.x

        Args:
            outfile (os.PathLike): Path to output file

        Returns:
            dict: Dictionary with values per model
        """
        with open(outfile, mode="r") as f:
            res = {}
            for line in f:
                model, var, val = line.split(":")
                model = model.strip()
                if model not in res:
                    res[model] = {}
                res[model][var] = float(val)

            return res

    def run(self,
            altitude: float,
            day_of_year: float,
            local_time: float,
            latitude: float,
            longitude: float,
            f107: float,
            f107m: float,
            kp1: float,
            kp2: float,
            get_uncertainty: bool = False
            ):
        """Run the model

        Returns a dictionary with keys 'MCM', 'DTM2020' or 'UM',
        which is another dictionary with 'temp', 'dens', etc. 
        Also, '_input' is another subdictionary 
        including the input parameters.

        Args:
            altitude (float): Altitude in km
            day_of_year (float): Day of the year [0-366]
            local_time (float): Local time, h [0-24]
            latitude (float): Latitude, deg [-90 to 90]
            longitude (float): Longitude, deg [0-360]
            f107 (float): F10.7
            f107m (float): F10.7 averaged
            kp1 (float): Kp
            kp2 (float): Kp
            get_uncertainty (bool, optional): It uncertainties should be returned. Defaults to False.

        Returns:
            dict: Dictionary with values per model
        """

        output_file = tempfile.NamedTemporaryFile(
            delete=False, suffix=".out", prefix="swami_", mode="r+")

        data_dtm = str(self.path_to_data)
        data_dtm = data_dtm + "/" if data_dtm[-1] != "/" else data_dtm
        data_um = str(os.path.join(self.path_to_data, "um"))
        data_um = data_um + "/" if data_um[-1] != "/" else data_um

        is_mcm = True if self.model is _AtmModel.MCM else False
        is_dtm = True if self.model is _AtmModel.DTM2020 else False
        is_um = True if self.model is _AtmModel.UM else False

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
            "bMCM": is_mcm,
            "bDTM": is_dtm,
            "bUM": is_um,
            "bUMstd": bool(get_uncertainty), # and is_um,
            "bDTMunc": bool(get_uncertainty), # and is_dtm,
            "data_dtm": data_dtm,
            "data_um": data_um,
            "output_file": str(output_file.name)
        }

        input_file = self._generate_nml_from_dict(input_dict)

        cmd = [str(self.path_to_bin), input_file]

        proc = subprocess.run(cmd, check=True)

        out = self._read_output_file(output_file.name)
        out["_input"] = input_dict

        os.unlink(input_file)
        os.unlink(output_file.name)

        return out
