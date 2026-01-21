# AES

This repository contains code accompanying a manuscript on augmenting rating-scale assessments with LLM-scored free-text data. A preprint version is available at https://arxiv.org/abs/2510.08663
. The manuscript is currently under review, and the codebase may evolve to reflect revisions.

## Overview

Psychological assessment is typically based on rating scales, which constrain respondents to predefined categories and cannot capture the nuance of natural language. This project introduces a framework for incorporating free-text responses into psychometric measurement without relying on labelled training data or expert-defined rubrics. 
The approach uses large language models (LLMs) with simple prompts to generate candidate text-based items from free responses. These items are then co-calibrated with an existing baseline scale, and those with the highest item information are retained. The resulting hybrid instrument improves measurement precision and accuracy, along with convergent validity.
The paper demonstrates the method using depression as a case study, evaluated in an upper-secondary student sample (n = 693) and a matched synthetic dataset (n = 3,000).

## Repository contents

This repository includes:

- Code for generating and scoring LLM-derived items from free-text responses
- Scripts for co-calibration with an existing rating-scale instrument 
-  Analysis code for evaluating measurement precision, accuracy, and validity 
- Tools for generating and working with synthetic datasets used for replication and testing

In line with ethical and data-protection requirements, no real participant data are included. Synthetic data and the scripts used to generate them are provided to support reproducibility.

## Note on code generation

Some portions of the codebase were co-created with large language models (e.g. ChatGPT). All code has been reviewed, edited, and executed by the authors.