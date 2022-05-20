# Datasaurs
Justin Matejka and George Fitzmaurice morphed the [Datasaurus](https://www.autodesk.com/research/publications/same-stats-different-graphs) into various shapes, all of which have essentially the same statistics. Their point was to always visualize data. This project uses persistent homology to differentiate various configurations of points, all of which are indistinguishable using basic statistics. 

The output of this program can be visualized in the following youtube videos: 
+ [Here](https://www.youtube.com/watch?v=CGvwX5lUl74) is a video showing the statistics of of the various point configurations. 
+ [Here](https://www.youtube.com/watch?v=ISpv4kQiAns) is a video showing how they are differentiated using persistent homology. 

# Building
To build this program, first install the haskell [stack](https://docs.haskellstack.org/en/stable/README/) tool. Then, after cloning this repop, do `stack setup && stack build`. To run do `stack 
exec -- ds-exe`, which will produce a file `all.csv`. 

An older version of this also produced videos of the stats and barcodes, shown in the videos above. In 
the new version, the file `all.csv` is run through an R-script to produce html files which are also included in this repo. 
