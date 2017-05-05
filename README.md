# SpatialHumanSimulationExperiments

## Overview

This project contains materials, data, analysis scripts, and papers/presentations for an extremely large-scale human simulation paradigm experiment investigating how the semantic information that is carried by a word's linguistic contexts is modulated by the kind of nonlinguistic context that word is used in. These experiments were designed and run by [Aaron Steven White](http://aswhite.net) using [Ibex](http://code.google.com/p/webspr/). The experiments were deployed natively on [Mechanical Turk](https://www.mturk.com/mturk/) using a custom modification of Ibex.

Portions of this work were presented at the the [2014 Congress of the International Association for the Study of Child Language](http://www.iascl.net/) and the [2016 CUNY Conference on Human Sentence Processing](https://cuny2016.lin.ufl.edu/). A [manuscript](http://aswhite.net/papers/white_contextual_2016.pdf) describing all the experiments found here is now available on [Aaron Steven White's website](http://aswhite.net) and in the `papers/` directory.

## Contents

### materials/

This directory contains all the materials needed to run the experiment. This includes item generation and Ibex templating scripts (`create_items.py`). The experiments were run natively on Mechanical Turk by extracting the relevant Ibex javascript components. (Thanks to [Pranav Anand](https://people.ucsc.edu/~panand/) who helped with this.)

### data/

This directory contains the raw data file pulled from Mechanical Turk. `results.preprocessed` was generated from the raw results files using `preprocess.py` in the `analysis/` directory.

### analysis/

This directory contains the analysis script for the manuscript found in `papers/` as well as a preprocessing/download script for use on the raw Ibex data.

### papers/

This directory contains a paper and posters based on the data.
