# Mixup Analyser

UNFINISHED

Uses Nash equilibriums to analyse how mixups will go, probabilistically, if both players play unexploitably. Note: unexploitable play is just the lower bound for doing well in a mixup, so strive to do better than this tool says you will!

Tell this program where to look for input data, and what gamestate to start with, by modifying `config.yaml`. Input data is in the form of YAML files structured like the example `in/Simple.yaml`, which contains examples of every possible part of the format. All non-generic functionality is in modules under `score`, `endstate`, `updater`, and `printer`.

To build this, use the Haskell Tool Stack

## Config Specification

To create a new starting point in an input file (such as `in/Simple.yaml`), first create a new element of the root list with a `-` followed by a newline and indentation. Then, give it a human-readable name with `name:` (such as "Simple") and an input path with `path:` (this will be a file in `in`, with ".yaml" added to the end automatically by the program).

Next, add an initial context under `context:`, the same as what will be used in the input data file - it should have a map of context keys to their (integer) values under `set:`, though you could technically also use `add:` as the default starting value is 0. It should also have a mixup destination under `next:`, detailing the `attacker:`, `defender:`, and `mixup:`.

Finally, add the output details under `score:` - this will include all desired output configurations for this starting point and this input file, each of which is li
