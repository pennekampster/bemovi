# How to build
The project builds with Ant which can be downloaded at http://ant.apache.org/bindownload.cgi
Ant needs to be in your path, once installed type
ant -version
expected output should be:
Apache Ant(TM) version 1.8.2 compiled on June 20 2012

Buy default ant will look for a build.xml

At the root of the project (where build.xml is) type
ant -projecthelp   (this will list all the available targets)
 clean
 compile
 compileSrc
 compileTests
 jar (default)
 run
 runTestNG

 jar is the default target so just ant will run the jar target which will generate the jar file


# How to run
The program requires only two input parameters, the full path or relative location to the input directory and the full path or relative location to the output file including file name
The properties it uses are in the properties file particle.properties
Any properties provied on the command line in the form of -Dprop=value will take precedence

(minimal)
java -Xmx512m  -jar ParticleLinker.jar input-data-folder output-data-file
E.g
java -Xmx512m -Dparticle.kernal.radius=100 -jar ParticleLinker.jar /Users/zola/Development/rest/ParticleLinker/in/check-folder-24-june/input_data /Users/zola/Development/rest/ParticleLinker/src/test/out/24-june-output.txt

(with property overrides)

java -Xmx512m -Dproperty-name=property-value -jar ParticleLinker.jar ParticleLinker.jar input-data-folder output-data-file
E.g
java -Xmx512m -Dparticle.kernal.radius=100 -jar ParticleLinker.jar /Users/zola/Development/rest/ParticleLinker/in/check-folder-24-june/input_data /Users/zola/Development/rest/ParticleLinker/src/test/out/24-june-output.txt