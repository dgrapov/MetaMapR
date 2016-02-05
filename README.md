
[MetaMapR](http://dgrapov.github.io/MetaMapR/) 
========

### A metabolomic network mapping tool 
![demo](Manual/metamapr_long.gif)

### Demo
Contact me at createdatasol at gmail.com if you would like to use the software on a trial server.

### [Screenshots](https://github.com/dgrapov/MetaMapR/blob/master/screenshots.md)

### Installation
### <a href="http://createdatasol.com/"> www.createdatasol.com</a> 


### Instructions
* [Tutorial](http://ufpr.dl.sourceforge.net/project/metamapr/Metmapr%20v1.2.1%20tutorial%20v1.doc.pdf)
* [Examples](http://dgrapov.github.io/MetaMapR/)

### Information
Version: 1.4.0 (1/18/2015)

News: [See the newest features.](https://github.com/dgrapov/MetaMapR/blob/master/NEWS.md)

License: GNU General Public License (v3), [read more](https://github.com/dgrapov/MetaMapR/blob/master/LICENSE)

### Citation
You can read the a pre-print of the [full manuscript](https://github.com/dgrapov/MetaMapR/blob/dev/Manual/MetaMapR%20manuscript.pdf). If you find this tool useful please cite the [published manuscript](http://bioinformatics.oxfordjournals.org/content/early/2015/04/03/bioinformatics.btv194.short?rss=1) in Oxford Bioinformatics.

### TODO
* add partial correlations
* add cytoscape.js networks
* enable network mapping

### FAQ
1) **How are multiple edge types handled in the network output?**

The default setting for returning all edge types or only unique edges can be found under the network tab using the unique edges checkbox. If the option is selected then edges are returned based on a hierarchy Biochemical > Structural > Mass Spectral > Correlation, otherwise all edges are returned.

2) **How do I map between the numeric node ids in the calculated edge list and the data I uploaded?**

By default the numeric node IDs are assigned based on the order (row) of the uploaded data. For example 1 corresponds to the analyte in the first row. The node attributes tab also shows the mapping between the identifier used for the network and the assigned numeric node ID. A custom node ID can also be used during the network calculation process under network > more options > edge index. The custom ID should be unique.
