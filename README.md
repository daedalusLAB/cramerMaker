
# CramerMaker

## Description

This script is a utility for parsing data. It is designed to process files generated by OpenPose and provides various output options, including saving the parsed data as a Parquet or CSV file.

## Dependencies

- R
- argparse library

## Usage

```bash
Rscript cramerMaker.R [OPTIONS]
```

### Options

- `-f`, `--data.input`: Path to the folders containing files generated by OpenPose.
- `-s`, `--path.save`: Directory where the parsed output should be saved.
- `-p`, `--parquet`: Flag to save the output as a Parquet file.
- `-c`, `--csv`: Flag to save the output as a CSV file.
- `-a`, `--all.in.file`: Flag to specify if all data should be saved in one file.

## Warning

Ensure that the `data.input` is not the folder of a single video but where all the videos to be processed are stored.

- `video/file.json` (Incorrect)
- `videos/video_01/file.json` (Correct)

## Examples

To process files from a specified folder and save them in a different directory:

```bash
Rscript cramerMaker.R -f ./input_folder/ -s ./output/
```

To process files and save them as a Parquet file:

```bash
Rscript cramerMaker.R -f ./input_folder/ -s ./output/ -p
```

To process files and save them as a CSV file:

```bash
Rscript cramerMaker.R -f ./input_folder/ -s ./output/ -c
```
