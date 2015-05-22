[Ryacas](http://cran.r-project.org/web/packages/Ryacas/index.html) (google code name ryacas) is an [R](http://www.r-project.org/) package that allows R users to access the [yacas](http://yacas.sourceforge.net/) computer algebra system from within R.  It can be used for computer algebra, exact arithmetic, ASCII pretty printing and R to TeX output.  R, yacas and Ryacas are all free software packages distributed under the GPL.  Samples of Ryacas are in this [sample session](http://code.google.com/p/ryacas/wiki/SampleSession), in this [message](https://stat.ethz.ch/pipermail/r-help/2010-March/232650.html) regarding calculus, in this [message](https://stat.ethz.ch/pipermail/r-help/2010-March/233520.html) which solves a nonlinear equation and in the package [vignette](http://ryacas.googlecode.com/svn/trunk/inst/doc/Ryacas.pdf). Ryacas is available on [CRAN](http://cran.r-project.org/web/packages/Ryacas/index.html) now.

Readers should also note the availability of the [rSymPy](http://rsympy.googlecode.com) CRAN project which interfaces to the [jython](http://www.jython.org) version of the [sympy](http://sympy.googlecode.com) computer algebra system.  Also http://rmathpiper.googlecode.com provides an R interface to MathPiper (an enhanced java version of yacas).

Below on this page are sections on: 
# NEWS #

In addition to the NEWS items below see the [NEWS](http://ryacas.googlecode.com/svn/trunk/inst/NEWS) file that comes with Ryacas.  It is more focused on changes in the package itself than the news listed here.

September 14, 2014.  A new version, Ryacas 0.2.12.1 has been uploaded. It fixes some problems running under R 3.1 .

September 7, 2011. A new version, Ryacas 0.2-11, has been added to the repository and uploaded to CRAN.  It addresses certain changes that occurred in R 2.14.

February 5, 2010.  A new version, Ryacas 0.2-10, has been added to the repository and uploaded to CRAN.  It is a bug fix version.

October 21, 2009.  The [java version of yacas project](http://yacas.sourceforge.net/yacas.jar)  appears to have moved to Mathpiper of the [Mathrider](https://mathrider.dev.java.net/) project. See announcement [here](http://www.freelists.org/post/geogebra-cas/Ted-Kosan-and-MathPiper). The [STATUS](#STATUS.md) section below has been updated accordingly.

October 21, 2009. [YacasCE](http://pocketpc.rextech.de/yacasce/) [(google translation)](http://translate.google.com/translate?hl=en&sl=de&u=http://pocketpc.rextech.de/yacasce/&ei=W8PfSoeJG9my8QbLhbGqAg&sa=X&oi=translate&resnum=1&ct=result&ved=0CA4Q7gEwAA&prev=/search%3Fq%3Dhttp://pocketpc.rextech.de/yacasce/%26hl%3Den%26sa%3DG) for the pocket PC (Windows mobile) is available.

August 23, 2009.  It seems that a new release of the XML package has again broken Ryacas.  Use XML 2.5.3 or XML 1.9.0 with Ryacas instead.

July 13/09.  It may be that the problem between Ryacas 0.2-9 and XML 2.3-0 (mentioned in news item for Mar 12/09) is resolved by simply using the new release of the XML package, XML 2.5-3 instead of the older versions of the XML package.  Will need to check a bit more to be sure.

Mar 12/09. Just found an incompatibility between Ryacas 0.2-9 and XML 2.3-0.  Until this is fixed use Ryacas 0.2-7 and XML 1.96-0.  If you are having difficulty with this try the [rSymPy](http://rsympy.googlecode.com) package instead.

Mar 9/09. [UNIX/Linux Installation](http://code.google.com/p/ryacas/#UNIX/Linux/Mac_Installation) section further down on this page has been rewritten.

Mar 3/09.  Ryacas 0.2-9 is now available on [CRAN](http://cran.r-project.org/web/packages/Ryacas/index.html).  Primary change is to require XML 2.1-0 or higher and some fixes to the documentation to pass R CMD CHECK on R 2.9.0. Note that Ryacas seems to work once again with the latest version the XML package, XML 2.1-0. It also worked with XML 1.96-0 but versions of the XML package after XML 1.96-0 but before XML 2.1-0 did not work with Ryacas.

Dec 21/08.  Added _Troubleshooting_ instructions on how to use XML 1.96-0 with Ryacas even if you have a newer version of XML installed at the same time.

Nov 16/08.  In _Troubleshooting_ section below, Ryacas works with XML package version 1.96-0 but not with XML package version 1.98-1.

May 07/08.  In _Troubleshooting_ section below added a tip about deleting `.yacCon` .

Feb 04/08.  Gregor Gorjanc's blog has an [article](http://ggorjan.blogspot.com/2007/08/computer-algebra-system-cas-in-lyx-via.html) on how to use Ryacas via Sweave in LyX.

Oct 17/07.  Added an item to the troubleshooting section that **Ryacas 0.2-7 has only been tested with yacas 1.0.63.**  Windows users of Ryacas 0.2-7 will automatically get yacas 1.0.63 if they use the Ryacas yacasInstall() command to install yacas but users on Mac and UNIX platforms currently need to be aware of this so they install yacas 1.0.63 and not a different version of yacas.  The `SystemRequirements:` line of the Ryacas DESCRIPTION file indicates that yacas 1.0.63 or greater should be used; however, versions greater than 1.0.63 have not been tested and so might not work with Ryacas 0.2-7.

Oct 15/07.  Reorganized multiple Installation and multiple Troubleshooting sections (end of this page) into  a single section for each with subsections.

Sep 9/07.  Version 0.2-7 of Ryacas uploaded to CRAN. Changes include socket timing improvement in [yacas.R](http://ryacas.googlecode.com/svn/trunk/R/yacas.R) suggested by [Petr Savicky](http://www.cs.cas.cz/~savicky/), a new Encoding: line in [DESCRIPTION](http://ryacas.googlecode.com/svn/trunk/DESCRIPTION) file to handle R 2.6.0 warnings and several other minor changes to handle R 2.6.0 compatability.

Aug 19/07.  `dep=TRUE` is the default for `install.packages` so instructions have been simplified.

May 13/07.  The [install.packages('Ryacas', dep = TRUE)](http://finzi.psych.upenn.edu/R/library/utils/html/update.packages.html) command for installing Ryacas on Mac is
now the same as on UNIX so the installation sections for Mac and UNIX have been
combined into one below.

May 12/07.  Ryacas 0.2-6 now on CRAN.  Bug fix version.  See [NEWS](http://ryacas.googlecode.com/svn/trunk/inst/NEWS).

May 5/07.  In Links section to right added new link to [CRAN](http://cran.r-project.org/src/contrib/Descriptions/Ryacas.html). (Removed links to Omegahat and Ryacas Yahoo group since Ryacas distribution is now via CRAN.)

May 1/07. Ryacas has been uploaded to CRAN. (Previously it was on Omegahat.) Also Ryacas is upgraded to 0.2-5 to accomodate changes in R that were made with R 2.5.0.  Earlier versions of R will not work with this version of Ryacas.  Note that yacas.exe is no longer included with Ryacas but for Windows users only there does exist a yacasInstall() command that can be issued without arguments from within R to download and install yacas.

Jan 17/06.  Added [Wikipedia CAS Comparison](http://en.wikipedia.org/wiki/List_of_computer_algebra_systems) to Links section on right.

Jan  9/06. The [Downloads](http://code.google.com/p/ryacas/downloads/list) tab above contains yacas 1.0.63 source code in a .tar.gz file as well as the Windows yacas 1.0.63 binaries in a .zip file.  yacas.exe and scripts.dat have been removed from the svn repository in preparation for CRAN -- as CRAN packages cannot have binaries in their source :(

Dec 28/06. Added a note for installing Ryacas under Gentoo on this page in the _UNIX TROUBLESHOOTING_ section below.

Dec 23/06. The Sample Session formerly on this page has been moved to the [Wiki](http://code.google.com/p/ryacas/w/list).  See Wiki tab above.

Dec 6/06.  The two vignettes are now combined into one.  Click on [Ryacas - vignette](http://ryacas.googlecode.com/svn/trunk/inst/doc/Ryacas.pdf) in Links section to right.

Nov 19/06. The very latest (development) version (to become 0.2-4) is available via svn from the _Source_ tab at top of this page.  It is expected that this version will become eventually be available from CRAN.  Until then use Ryacas 0.2-3 as in following news items.  Anyone who would like to test the development version out, any feedback (to the developers listed under TEAM) would be appreciated.

Nov 19/06. Ryacas 0.2-3 is now available for download from Omegahat via the install.packages command (or the _Ryacas on Omegahat - Download_ links)

Nov 16/06.  Ryacas 0.2-3 is available from google groups (see _Ryacas on google groups - Download_ link to right).  Note that google groups has the annoying habit of translating dashes in filenames to spaces so after downloading the file you will have to change the name so that any spaces are replaced with dashes.

# TECHNOLOGY #
Ryacas is written in R.  It uses a recursive decent R-to-yacas translator and an XML-based OpenMath yacas-to-R translator.  yacas is run as a second process and communicates via a socket interface with R.  The design allows for the future possibility of running Ryacas on a low powered client connected to a high power remote yacas compute server over the internet.

# SOURCE REPOSITORY #
The [svn repository](http://ryacas.googlecode.com/svn/) on this site (click on _Source_ tab above) contains the source code to Ryacas and generated PDF file for the vignette.

On Windows, yacas.exe and scripts.dat are included in the Ryacas 0.2-3 distribution but not in the Ryacas 0.2-4 or later distributions; however, in those Windows versions not having yacas.exe and scripts.dat there is a yacasInstall() command in Ryacas which allows one to easily fetch and install them from the internet by simply issuing the command `yacasInstall()` from within R.

# STATUS #
_Ryacas._The Ryacas package was developed several years ago and development has largely stopped (although a bug fix version was issued Feb 5, 2010) and now effort goes into the rMathpiper and rSymPy projects.

_Yacas._ Regarding yacas itself (as opposed to Ryacas), the original developer behind yacas appears to have left the yacas project.  Just before he left he rewrote yacas in Java.  Currently [Alberto González Palomo](http://www.matracas.net/index.en.html) continues to make improvements to the C version yacas although there have been no recent releases.

_MathpiperIDE._ The java version of yacas was [adopted](http://www.freelists.org/post/geogebra-cas/Ted-Kosan-and-MathPiper) by the [MathpiperIDE](https://mathrider.dev.java.net/) project in 2008 (which had been called Mathrider at the time and though the name has since  been changed) and [Ted Kosan](http://java.net/blogs/tkosan) has indicated that under the MathpiperIDE/Mathrider project he has forked the java version of yacas and he and [Howard "Sherm" Ostrowsky](http://groups.google.com/groups/profile?enc_user=rcwwbRIAAAD3csaARRUp0gr04WmWCuxi8rhlH0Pnl47z4AZhN98BFg) have made [2,700 svn commits](http://markmail.org/browse/net.java.dev.mathrider.commits) (from March 2008 to October 2009) of which about half were for Mathpiper (and the rest for Mathrider).   The new name [Mathpiper](http://sage.ssu.portsmouth.oh.us/mathpiper/) is used to distinguish it from the apparently unmaintained [yacas.jar](http://yacas.sourceforge.net/yacas.jar) prior to the fork.  (The original version of the java version of yacas is available on the [yacas site](http://yacas.sourceforge.net)).

_Mathpiper._ Mathpiper is located at http://mathpiper.googlecode.com.  The mathpiper.jar file is located in the mathrider/jars directory in the !MathPiperIDE (formerly Mathrider) distribution which can be found at http://mathrider.org.  The javadocs can be found [here](http://206.21.94.60/mathpiper/javadoc/overview-summary.html). One can launch the GUI console like this:
```
java -jar mathpiper.jar
```
One can launch the text console like this:
```
java -cp mathpiper.jar org.mathpiper.ui.text.consoles.Console
```
and one can launch the help application like this:
```
java -cp mathpiper.jar org.mathpiper.ui.gui.help.FunctionTreePanel
```
One significant new feature in Mathpiper is the ability to emit R code using the following command (which puts Mathpiper in a mode such that all code emitted is R):
```
PrettyPrinter("RForm")
```
This new feature was added to Mathpiper version 0.76t .

# R INTERFACE #
There are 8 interfaces.  All except runYacas are built on top of yacas.character:
  1. `yacas.character`.   Pass a raw yacas character string to yacas function. e.g. yacas('x\*x')
  1. `yacas.expression`.  Pass an R expression to yacas function. e.g. yacas(expression(x\*x))
  1. `yacas.function`.  Pass an R function name which defines a one-line    R function.  e.g. f <- function(x) x\*x; yacas(f)
  1. `yacas.formula`.  Pass an R formula object whose right hand side will be regarded as an expression to pass to yacas.expression. e.g. yacas(~ x\*x)
  1. `Sym` object.  These objects are internally yacas strings.  They can be combined with arithmetic operators  The print method calls yacas. e.g. x <- Sym('x'); x\*x
  1. `Expr` object.  These objects are internally R expressions. They can be combined with artithmetic operators.  e.g. x <- Exprq(x); x\*x
  1. `yacmode()` which places the session in a mode where one can directly type in yacas commands.
  1. `runYacas()`.  Shells out to a yacas session.

# SAMPLE SESSION #
Click on [Wiki tab](http://code.google.com/p/ryacas/w/list) above and then click on Sample Session.

For a sample session illustrating calculus computations look [here](https://stat.ethz.ch/pipermail/r-help/2010-March/232650.html) .

Not all the facilities of yacas are built into Ryacas but its easy to add them yourself -- typically with one line of code.  For example, here we add an interface to the Ryacas [EigenValues function](http://tolstoy.newcastle.edu.au/R/e17/help/12/03/7407.html) and run an example.

# MORE INFORMATION #
The R commands
```
   library(Ryacas)
   package?Ryacas
```

will display pointers to the help commands, vignette, home page, demos and THANKS, WISHLIST and NEWS files.  These files, including pdf versions of the vignette, can also be found in the Source repository by clicking on the Source tab above.

# TEAM #

  * Rob Goedman, goedman at mac dot com
  * Gabor Grothendieck, ggrothendieck at gmail dot com
  * [Søren Højsgaard](http://people.math.aau.dk/~sorenh/), sorenh at math.aau.dk
  * Ayal Pinkus, apinkus at xs4all dot nl

Contact Gabor if you need assistance or Rob if its Mac-related.

# INSTALLATION #

## Choosing a Platform ##
Ryacas 0.2-4 and later on Windows contains a command, yacasInstall(), that is issued without arguments that will automatically download and install yacas for you, so Ryacas is particularly easy to install on Windows.  Also there are a few extra commands available on Windows: yacasFile and yacasCheck that are not available on Ryacas in other operating systems. Thus if you have a choice of platforms you may wish to use Windows.

The development of Ryacas was primarily done on Windows and Mac so next best choice would be Mac.

We have had reports of successful use on UNIX too; however, because there has not been recent development on that platform some troubleshooting of the installation may be necessary on UNIX.  There is a UNIX troubleshooting section below just in case.

## Windows Installation ##

Issue these three commands in R.  They will install Ryacas from CRAN, load it and download yacas itself from the internet into `system.file(package = "Ryacas", "yacdir")` :
```
   install.packages('Ryacas') # omit this line if you already installed it
   library(Ryacas) # load Ryacas into R workspace
   yacasInstall() # install yacas itself - this works only on Windows
```

Note that you should _not_ install Ryacas into the `C:\Program Files` tree but rather should install it into your personal R library.  Normally the first time you use `install.packages` in a fresh R installation it asks you if you want to allow it to create such a library and you should do so.  If you have been installing your downloaded packages into `C:\Program Files` re-install a fresh version of R (do not install it over the old version).  See `?.libPaths` for more info.

(Note that this advice is not specific to Ryacas as there are quite a few other packages that do not work well if installed in the `C:\Program Files` tree, as well.)

**Old**: This is no longer needed but at one time it was necessary to ensure that a certain version of the XML package was used.

Replace the commented out library command with something like:
```
library(XML, lib = file.path(Sys.getenv("R_LIBS_USER"), "../2.7"))
```
or possibly just this:
```
library(XML, lib = "~/R/win-library/2.7")
```
if you don't have XML 1.96-0 installed under your current version of R but do have it under an installation of R 2.7 on the same machine.

## UNIX/Linux/Mac Installation ##


  1. Install GSL
  1. Download "http://ryacas.googlecode.com/files/yacas-1.0.63.tar.gz" and extract.
```
wget http://ryacas.googlecode.com/files/yacas-1.0.63.tar.gz
tar zvxf yacas-1.0.63.tar.gz
```
  1. Follow the instructions in section C in yacas-1.0.63/INSTALL, where in the confuguration step, be sure to include the option "--enable-server":
```
./configure --enable-server
```
  1. Install Ryacas.  i.e. from within R issue this command:
```
install.packages("Ryacas")
```
> or, alternately, from the shell enter:
```
R CMD INSTALL Ryacas_1.0.63.tar.gz
```

Alicia Ellis reported that the following worked for her on the Mac:

```
1.  Install GCC on Mac if you don't have it (its not automatically installed on Mac OS X)
    a. go to https://developer.apple.com/downloads/index.action?regToken=Wi3movjn9r%252F26M7M1gmZn2csBrIUSID7AvtgQTGEiWaUR4erWRECzdTmFEg3oXlYSky9oXwhR2qA%250D%250AD6fREf7nfUO1d76e0G7%252BfybIPf%252FatRk%253D%250D%250A#
    b. click on Command Line Tools for Xcode for your operating system
    c. click on .dmg file and save to Desktop
    d. click on the file to start installation and follow instructions
    e. check to see if works by opening Terminal and typing:  gcc -v
        if working, you should get:
mkyong$ gcc -v
Using built-in specs.
Target: i686-apple-darwin11
Configured with: {ignore long text…}
Thread model: posix
gcc version 4.2.1 (Based on Apple Inc. build 5658) (LLVM build 2336.9.00)

2.  Install yacas
    a. go to:  http://code.google.com/p/ryacas/downloads/detail?name=yacas-1.0.63.tar.gz&can=2&q=
    b. save file to Desktop
    c. Click on file to extract
    d. Open terminal (click on Applications, then the folder called Utilities, Terminal is in the Utilities folder)
    e. type:  cd Desktop/yacas-1.0.63
    f. type:  ./configure --enable-server
       at the prompt type: n  (I have no idea what this is about but it worked for me)
    g. type:  make
    h. type:  sudo make install
    i. If a prompt comes up asking for password, type in your administrator password
    
3.  Install and Use Ryacas
    a. Open R session
    b. Type:  install.packages("Ryacas")
    c. Type:  library(Ryacas)
    d. Start working.  Try a sample at the following to make sure it's working correctly:
       http://code.google.com/p/ryacas/wiki/SampleSession
```
David Winsemius reported that the above also worked for him with yacas 1.3.3 running on a Mac 10.6.8 except the 2f step varied slightly and there were error messages after the make step but it appeared to have succeeded nonetheless.


# TROUBLESHOOTING #

## Introduction ##

Most installation problems are caused by one of these on Windows:

  1. attempting to install yacas to an area where you don't have write permission

Most installation problems are caused by one of these on UNIX/Mac:

  1. using an old version of Ryacas.  Ensure you are using he latest version of Ryacas.  (The old version of Ryacas only works with certain old versions of XML pckage.  XML version 2.5-3, XML version 2.1-0 and XML version 1.96-0 and prior seem to work; however, a number of other versions of the XML package which may include the latest version of the XML package do not work with Ryacas.)
  1. not using yacas 1.0.63 with Ryacas 0.2-9 (Windows users can use the Ryacas `yacasInstall()` command which will automatically find the correct yacas version on the net and install it.  Others can find yacas [here](http://ryacas.googlecode.com/files/yacas-1.0.63.tar.gz).
  1. forgetting to download and install yacas (see last point)
  1. yacas was not built with the server enabled. On UNIX: ./configure --enable-server
  1. yacas is not started in server mode -- this is different from its normal interactive mode
  1. a left over `yacCon` variable in `.RyacasEnv` from a failed attempt can be a problem.  In that case restart R.
  1. yacas is not being intialized to start in OpenMath mode.  This is the mode where XML output is generated.  A yacas command is normally issued automatically to do that but if somehow this command does not get issued the session will fail to work:
```
PrettyPrinter("OMForm");
```

## Windows Troubleshooting ##

1. the user has stored their R library in an area that R cannot write to so `yacasInstall()` is unable to install yacas (e.g. trying to put your R library in `C:\Program Files` or some subfolder of that can cause this problem).  The solution is one of: (a) run R as Administrator while executing `yacasInstall()`.  Note that this only has to be done once, i.e. during the session where you run `yacasInstall()`.  From that point on you don't need to be Adminstrator when running Ryacas.  Be sure to restart R after running `yacasInstall()`. (b) put your library in user space (R does this by default so if its not there then you must have overridden its default location and you may wish to reverse this) or (c) create a folder for `yacas` in an area where you have write permission, e.g.
```
# this creates a directory such as C:\Users\JoeDoe\yacdir
yacdir <- file.path(Sys.getenv("R_USER"), "yacdir")
dir.create(yacdir)
```
and then set the following environment variables in your .Rprofile file:
```
# these statements go in your .Rprofile file which goes in the Sys.getenv("R_USER") folder
yacdir <- file.path(Sys.getenv("R_USER"), "yacdir")
Sys.setenv(YACAS_HOME=yacdir)
Sys.setenv(YACAS_INIT=yacdir)
Sys.setenv(YACAS_SCRIPTS=yacdir)
```
and restart R.

2. if its not the above problem then look at the UNIX/Mac points next.

## UNIX/Linux Troubleshooting ##

Here are some of the common problems that UNIX/Linux/Mac users encounger:

1. **Startup Timing Issues**. (_Ryacas 0.2-7 has fixed this but we have left this point in just case._) The first time you issue the yacas command in each session Ryacas is supposed to start the yacas process; however, there can be timing problems on some systems so if it does not start up just issue another yacas command and it will likely start.

2. **Telnet problems**.  If you are unable to get a connection at all try to connect to yacas manually using

```
telnet localhost 9734
```

making sure that yacas is indeed running since you cannot telnet to a process that is not there!  Also read the link 'Ryacas & Telnet' in the links section of this page. If you think you have telnet problems that you cannot resolve then you can run Ryacas without telnet but that will result in no memory from one command to the next.  To run without telnet use the method = 'system' argument on yacas or set the yacas.method option to 'system'.

```
options(yacas.method = 'system')
x <- Sym('x')
x*x
```

3. **Startup Configuration Problems**. The following should normally not be needed but if you are having problems even after the above, try entering this into R to configure your system.  One Debian user indicated that this solved his problem in getting Ryacas to work:

```
cat("PrettyPrinter('OMForm');\n", file = "~/.yacasrc")
Sys.setenv(YACAS_INVOKE_STRING = 'yacas -pc --server 9734')
```

The cat line above only has to be done once while the second line needs to be done before each Ryacas session.  Please contact one of the developers if you find this is necessary so we can keep track of this.

4. **FC and Others**. [HowTo: Ryacas Installation on FC](https://www.stat.math.ethz.ch/pipermail/r-help/2006-November/117802.html) discusses how to get Ryacas working on Fedora systems.  Some of the information here may apply to other UNIX systems too.  Warning: Some of the info in that link may be outdated now.

5. **Gentoo**. One Gentoo user of Ryacas found that Gentoo does have Yacas in its Portage package repository, but it installs without the '--enable-server' option so install Yacas from source and configure with the '--enable-server' option as Marc Schwartz did on Fedora Core (see previous point, #4).   Then change the Use statement in R.ys to an absolute path so that R.ys looks like this:

```
// Start with the default initialization, R-specific initialization to follow 
Use('/usr/local/share/yacas/yacasinit.ys'); 
PrettyPrinter('OMForm'); 
```

6. One user was finding he could only launch `yacas` when logged in as `root`.   After deleting the `.yacCon` variable -- `rm(.yacCon)` -- that had been left over from a failed attempt this rectified itself.  This remark refers to an older version of Ryacas.  `.yacCon` has since been replaced with the `yacCon` variable in the `.RyacasEnv` environment.  It can be examined by looking at `.RyacasEnv$yacCon' .

7. Note that on UNIX you may need to detach the Ryacas package prior to exiting R:
```
detach() # assuming Ryacas was last one loaded
```
or
```
stopYacas()
```

8. Make sure that yacas is in the PATH environment variable. This is usually guaranteed by the "make install" step of the compilation of Yacas. However, you   may easily verify it. Typing "yacas" to the command line should start an interactive Yacas session.

9.  When installing yacas be sure to use
```
./configure --enable-server
```