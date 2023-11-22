import os
from pathlib import Path
from shutil import copyfile
import requests
from datetime import date
import logging
from argparse import ArgumentParser

logging.basicConfig(
    level=logging.INFO, format="%(levelname)-8s %(funcName)s: %(message)s",
)
logger = logging.getLogger(__name__)


BASE_DIR = Path(__file__).parent
TEMPLATES_DIR = BASE_DIR / "templates"


def setup_dir(day, languages):
    newdir = Path(f"day{day:02d}")
    if not newdir.exists():
        os.mkdir(newdir)
        logger.info(f"created a new directory {newdir}")
    lang_dirs = os.listdir(TEMPLATES_DIR)
    for lang in languages:
        if lang not in lang_dirs:
            logger.error(f"no template files found for {lang}; available: {', '.join(lang_dirs)}")
            continue
        template_dir = TEMPLATES_DIR / lang
        for file in os.listdir(template_dir):
            if file in os.listdir(newdir):
                logger.warning(f"{newdir} already contains {file}, skipping")
            else:
                copyfile(template_dir / file, newdir / file)
    logger.info(f"done copying templates to {newdir}")


def get_input(day, year):
    if not "AOC_SESSION" in os.environ:
        logger.warning("no environment variable 'AOC_SESSION' found! skipping download")
        return
    else:
        cookie = os.environ["AOC_SESSION"]

    url = f"https://adventofcode.com/{year}/day/{day}/input"
    dayfolder = f"day{day:02d}"
    if "input.txt" in os.listdir(dayfolder):
        logger.warning(f"{dayfolder}/input.txt already exists, skip download")
        return

    logger.info(f"download input from {url}... ")
    try:
        response = requests.get(
            url=url,
            cookies={"session": cookie},
            headers={"User-Agent": "https://github.com/davekch/aoc by dave-koch@web.de"},
        )
        if response.ok:
            data = response.text
            f = open(Path(dayfolder) / "input.txt", "w+")
            f.write(data.rstrip("\n"))
            f.close()
            logger.info("... done!")
        else:
            logger.error("server response not ok")
    except:
        logger.error("something went wrong")


if __name__ == "__main__":
    parser = ArgumentParser()
    parser.add_argument("-d", "--day", default=date.today().day, type=int)
    parser.add_argument("-y", "--year", default=date.today().year, type=int)
    parser.add_argument("-l", "--language", default=["py"], nargs="+")
    parser.add_argument("--no-download", dest="skip_download", action="store_true")
    args = parser.parse_args()

    setup_dir(args.day, args.language)
    if not args.skip_download:
        get_input(args.day, args.year)
