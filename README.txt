IRONSIDES compilation instructions

SPARK (http://www.altran-praxis.com/spark.aspx) is a programming language that uses formal methods to prove software properties.  There are two separate compilation processes:

1) Use the SPARK toolset (http://libre.adacore.com/tools/spark-gpl-edition/) to perform the automatic theorem proving on the code.  (If you haven't modified the distribution, this step is not required as it has already been done by the authors-- you do still have to download SPARK to get the library modules).

2) Use an Ada compiler (e.g. http://libre.adacore.com/tools/gnat-gpl-edition/) to create an executable.

The following command line will create an executable (assuming SPARK is installed to c:\spark\2011).

gnatmake -gnat05 -O3 -gnatp -Ic:\spark\2011\lib\spark -Ic:\spark\2011\lib\spark\current spark_dns_main

