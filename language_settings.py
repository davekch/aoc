import glob
import logging
import os
from dataclasses import dataclass, field
from itertools import chain
from pathlib import Path
from shutil import copy

logger = logging.getLogger(__name__)


@dataclass
class LanguageSettings:
    name: str
    glob_patterns: list[str] = field(default_factory=lambda: ["*"])

    def hook(self, template_dir: Path, new_dir: Path): ...

    def setup_dir(self, template_dir: Path, new_dir: Path):
        lang_dirs = os.listdir(template_dir)
        if not (template_dir / self.name).is_dir():
            logger.error(
                f"no template files found for {self.name}; available: {', '.join(lang_dirs)}"
            )
            return
        for path in chain(
            *[glob.glob(str(template_dir / self.name / g)) for g in self.glob_patterns]
        ):
            file = Path(path).name
            if (new_dir / file).exists():
                logger.warning(f"{new_dir} already contains {file}, skipping")
            else:
                copy(path, new_dir / file)
        self.hook(template_dir, new_dir)
        logger.info(f"done copying templates to {new_dir}")


class HaskellSettings(LanguageSettings):
    def hook(self, template_dir: Path, new_dir: Path):
        cabal = Path("solvers.cabal")
        stack = Path("stack.yaml")
        if not cabal.exists():
            copy(template_dir / self.name / cabal, cabal)
            logger.info("copied solvers.cabal to root directory")
        if not stack.exists():
            copy(template_dir / self.name / stack, stack)
            logger.info("copied stack.yaml to root directory")

        # add the current day to the cabal executables
        day = new_dir.name
        # check if the stanza is already present
        with open(cabal, "r") as f:
            lines = f.readlines()
            if f"executable {day}\n" in lines:
                logger.warning(
                    f"stanza for {day} already in solvers.cabal, skip adding it"
                )
                return
        with open(cabal, "a") as f:
            f.write(
                f"""
executable {day}
    import: solver-settings
    main-is: solver.hs
    hs-source-dirs: {day}
"""
            )
        logger.info(f"added stanza for {day} to solvers.cabal")


def get_setting(lang: str) -> LanguageSettings:
    settings = {
        "hs": HaskellSettings("hs", ["*.hs"]),
    }
    return settings.get(lang, LanguageSettings(lang))
