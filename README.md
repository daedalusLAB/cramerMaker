
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


| :warning: Wrong                                                                                                                                       |
|:-----------------------------------------------------------------------|
|   - <span style="color:red">`2006-01-14_0600_US_KTTV-FOX_Ten_OClock_News_273-275_ID202_back_then/`</span>
  ├── 2006-01-14_0600_US_KTTV-FOX_Ten_OClock_News_273-275_ID202_back_then_000000000000_keypoints.json
  ├── 2006-01-14_0600_US_KTTV-FOX_Ten_OClock_News_273-275_ID202_back_then_000000000001_keypoints.json
  ├── 2006-01-14_0600_US_KTTV-FOX_Ten_OClock_News_273-275_ID202_back_then_000000000002_keypoints.json |


``



`Correct`

- <span style="color:green">`exampleVideos/`</span>
├── 2006-01-14_0600_US_KTTV-FOX_Ten_OClock_News_273-275_ID202_back_then/
├── 2008-04-15_0600_US_KCET_Tavis_Smiley_239-242_far_in_the_future/
├── 2014-10-10_0600_US_KCAL_Entertainment_Tonight_374-380_ID933_from_beginning_to_end/
└── 2015-03-26_2100_US_WEWS_Live_on_5_1221-1225_ID509_later_than/

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
