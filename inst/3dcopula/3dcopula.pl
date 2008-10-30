#!/usr/bin/perl -w
use strict;

use File::Copy;
use Getopt::Long;
my %OPTS = ();
my @options = qw(clean! file=s help nsimcop=i n4lcomom=i noshadow nolatex);
GetOptions(\%OPTS, @options);

&Help(), exit if($OPTS{'help'});

if(exists $OPTS{'clean'}) {
  if($OPTS{'clean'}) {
    print "STATUS: starting and exiting now with cleanup of TMP*.* files.\n";
    cleanup();
    exit;
  }
  else {
    print "STATUS: starting without cleanup of TMP*.* files.\n";
  }
}
else {
  print "STATUS: starting with cleanup of TMP*.* files.\n";
  cleanup();
}

# These options will be used to set particular variables in the R
# code prior to forking off to R for copula computations.
$OPTS{'n4lcomom'} = (exists $OPTS{'n4lcomom'}) ? $OPTS{'n4lcomom'} : 0;
$OPTS{'nsimcop'}  = (exists $OPTS{'nsimcop'})  ? $OPTS{'nsimcop'}  : 500;
$OPTS{'noshadow'} = (exists $OPTS{'noshadow'}) ? "TRUE" : "FALSE"; 

# BEGIN CORE OF PROGRAM
my $Rlibraries = "library(copBasic);\n";
my $moreRlibraries = "";

my ($cop,$para) = qw(NULL NULL);
my $caption = "";

my $conf = shift(@ARGV);
if(defined $conf and -e $conf) {
  open(FH,"<$conf") or die "Configuration file $conf not opened because $!.\n";
  while(<FH>) {
    next if(/^#/);
    last if(/^CAPTION/);
    chomp($_);

    if(/^THECOPULA=/) {
      (undef,$cop) = split(/=/,$_);
      next;
    }
    if(/^moreRLIBRARIES=/) {
      (undef,$moreRlibraries) = split(/=/,$_);
      next;
    }
    $para = "" if($para eq "NULL"); # need to reset it
    $para .= $_;
  }
  while(<FH>) {
    next if(/^#/);
    chomp($_);
    $caption .= $_;
  }
  close(FH);
} else {
  die "The configuration file for the copula was not provided.\n"
}

print "===========================================\n";
print "STATUS: copula <- $cop\n";
print "STATUS: para <- $para\n";
print "STATUS: caption is \n $caption\n";
print "===========================================\n";

my $Rfile = "TMPoff2R.R";
open(FH,">$Rfile") or die "$Rfile not opened because $!\n";
  Rcode_Preamble();
  Rcode_Variable($cop,$para);
  Rcode_Core();
close(FH);

my $fromperl = "3dcopula_fromperl.tex";
open(FH,">$fromperl") or
     die "$fromperl not opened because $!\n";
  print FH '\renewcommand{\mycaption}{'.$caption."}\n";
close(FH);

print "STATUS: system forking to R... ";
system("R --vanilla < $Rfile");
print "STATUS: R is done.\n";

print "STATUS: system forking to LaTeX (not pdflatex)...\n";
if(! exists $OPTS{'nolatex'}) { 
  system("latex 3dcopula.tex; latex 3dcopula.tex; dvipdf 3dcopula.dvi");
}
print "STATUS: latex is done processing\n";

if($OPTS{file}) {
  print "STATUS: renaming 3dcopula.pdf to $OPTS{file}\n";
  copy("3dcopula.pdf",$OPTS{file});
}


if(exists $OPTS{'clean'}) {
  if($OPTS{'clean'}) {
    print "STATUS: exiting with cleanup of TMP*.* files.\n";
    cleanup();
    exit;
  }
  else {
    print "STATUS: exiting without cleanup of TMP*.* files.\n";
    exit;
  }
}
else {
  print "STATUS: exiting with cleanup of TMP*.* files.\n";
  cleanup();
}


#########################################################
sub cleanup {
  my @tmpfiles = <TMP*.*>;
  unlink(@tmpfiles);
  unlink("3dcopula.dvi");
  unlink("3dcopula.aux");
  unlink("3dcopula.log");
  unlink("3dcopula_ancillary.aux");
  unlink("3dcopula_fromperl.aux");
}

sub Rcode_Preamble {
  print FH "$Rlibraries";
  print FH "$moreRlibraries\n";
}

sub Rcode_Variable {
  my ($cop, $para) = @_;
  if($cop eq "NULL") {
     die "DIED: The specified copula in \$cop remains NULL, exiting.\n";
  }
  print FH "mycop <- $cop;\n";
  print FH "para <- $para;\n";
}

sub Rcode_Core {
print FH << "HERE";
mydelt   <- 0.005;
noshadow <- $OPTS{'noshadow'}
n4lcomom <- $OPTS{'n4lcomom'};
nsimcop  <- $OPTS{'nsimcop'};
HERE
;


print FH << 'HERE';

for(t in c(seq(0.1, 0.9, by=0.1))) {
  z <- level.curvesCOP(cop=mycop, para=para, getlevel=t, ploton=FALSE, lines=FALSE)
  x <- data.frame(U=z$U, V=z$V, Z=rep(t,length(z$V)))
  file <- paste(c("TMP",t,".txt"), collapse="")
  write.table(x, file=file, quote=FALSE, row.names=FALSE, col.names=FALSE)

  z <- sectionCOP(t, cop=mycop, para=para, ploton=FALSE, lines=FALSE, delt=mydelt)
  x <- data.frame(U=rep(z$fvalue,length(z$t)), V=z$t, Z=z$seccop)
  file <- paste(c("TMPsecU",t,".txt"), collapse="")
  write.table(x, file=file, quote=FALSE, row.names=FALSE, col.names=FALSE)

  z <- sectionCOP(t, cop=mycop, para=para, wrtV=TRUE, ploton=FALSE, lines=FALSE, delt=mydelt)
  x <- data.frame(U=z$t, V=rep(z$fvalue,length(z$t)), Z=z$seccop)
  file <- paste(c("TMPsecV",t,".txt"),collapse="")
  write.table(x, file=file, quote=FALSE, row.names=FALSE, col.names=FALSE)

  z <- sectionCOP(t, cop=mycop, para=para, ploton=FALSE, lines=FALSE, dercop=TRUE, delt=mydelt)
  x <- data.frame(U=rep(z$fvalue,length(z$t)), V=z$t, Z=z$seccop)
  file <- paste(c("TMPderU",t,".txt"), collapse="")
  write.table(x, file=file, quote=FALSE, row.names=FALSE, col.names=FALSE)

  z <- sectionCOP(t, cop=mycop, para=para, wrtV=TRUE, ploton=FALSE, lines=FALSE, dercop=TRUE, delt=mydelt)
  x <- data.frame(U=z$t, V=rep(z$fvalue,length(z$t)), Z=z$seccop)
  file <- paste(c("TMPderV",t,".txt"),collapse="")
  write.table(x, file=file, quote=FALSE, row.names=FALSE, col.names=FALSE)


}

z <- diagCOP(cop=mycop, para=para, ploton=FALSE, lines=FALSE)
x <- data.frame(U=z$t, V=z$t, Z=z$diagcop)
file <- paste(c("TMPdiag.txt"), collapse="")
write.table(x, file=file, quote=FALSE, row.names=FALSE, col.names=FALSE)

for(t in c(0,1))  {
  z <- sectionCOP(t, cop=mycop, para=para, ploton=FALSE, lines=FALSE, delt=mydelt)
  x <- data.frame(U=rep(z$fvalue,length(z$t)), V=z$t, Z=z$seccop)
  file <- paste(c("TMPsecU",t,".txt"), collapse="")
  write.table(x, file=file, quote=FALSE, row.names=FALSE, col.names=FALSE)

  z <- sectionCOP(t, cop=mycop, para=para, wrtV=TRUE, ploton=FALSE, lines=FALSE, delt=mydelt)
  x <- data.frame(U=z$t, V=rep(z$fvalue,length(z$t)), Z=z$seccop)
  file <- paste(c("TMPsecV",t,".txt"), collapse="")
  write.table(x, file=file, quote=FALSE, row.names=FALSE, col.names=FALSE)

  z <- sectionCOP(t, cop=mycop, para=para, ploton=FALSE, lines=FALSE, dercop=TRUE, delt=mydelt)
  x <- data.frame(U=rep(z$fvalue,length(z$t)), V=z$t, Z=z$seccop)
  file <- paste(c("TMPderU",t,".txt"), collapse="")
  write.table(x, file=file, quote=FALSE, row.names=FALSE, col.names=FALSE)

  z <- sectionCOP(t, cop=mycop, para=para, wrtV=TRUE, ploton=FALSE, lines=FALSE, dercop=TRUE, delt=mydelt)
  x <- data.frame(U=z$t, V=rep(z$fvalue,length(z$t)), Z=z$seccop)
  file <- paste(c("TMPderV",t,".txt"), collapse="")
  write.table(x, file=file, quote=FALSE, row.names=FALSE, col.names=FALSE)


}


D  <- simCOP(n=nsimcop, cop=mycop, para=para, ploton=FALSE, points=FALSE)
if(noshadow) {
  xo <- NULL
} else {
  xo <- data.frame(X=D$U, Y=D$V, Z=rep(0,length(D$V)))
}
z <- sapply(1:length(D$U), function(i) { return(mycop(D$U[i], D$V[i], para=para)) })
xz <- data.frame(X=D$U, Y=D$V, Z=z)
write.table(xo,file="TMPsimxo.txt", quote=FALSE, row.names=FALSE, col.names=FALSE)
write.table(xz,file="TMPsimxz.txt", quote=FALSE, row.names=FALSE, col.names=FALSE)

D   <- simCOP(n=n4lcomom, cop=mycop, para=para, ploton=FALSE, points=FALSE)
LMR <- lcomoms2(D, nmom=4)
sink(file="TMPlcomoment.txt")
  cat(c("Sample size for L-comoment simulation:",n4lcomom,"\n"))
  print(LMR)
sink()

q()

HERE
;

}








sub Help {
  print << "HERE";

PROGRAM $0
by William H. Asquith, Fall 2008

USAGE:
  $0 [options] 3dcopula_configuration_file.txt

OPTIONS:

  Basic options

  --help   
            This help page and exit.

  --file=<string>
            If present the PDF output file 3dcopula.pdf is copied to
            a file having the name <string>.

  --clean
            Cleanup TMP*.* files in current directory along with
            ancillary LaTeX files---not including the *.tex files!
            This option has no other side effect as an exit is made.

  --noclean
            Do not cleanup TMP*.* files during a processing run.


  Advanced options

  --n4lcomom=<integer>
            Override the default of n=0 for the simulation run to
            compute the L-comoments of the copula, which are outputted
            to the file TMPlcomoment.txt. The default of zero makes the
            $0 run much faster---thus, the output in
            TMPlcomoment.txt is bogus.

  --nolatex
            Do not actually fork to the operating system the LaTeX
            rendering command sequence:
             latex 3dcopula.tex; latex 3dcopula.tex; dvipdf 3dcopula.dvi

  --nsimcop=<integer>
            Override the default of n=500 for the simulation run to
            compute x,y,z coordinates of the copula to layout into files
            TMPsimxo.txt (no vertical dimension) and TMPsimxz.txt (vertical
            dimension). The default of 500 should be sufficient for many
            "singularities" of the copula to be made manifest.

   --noshadow
            If present, then the projection of the simulated copula on the
            xy plane is set to NULL to TMPsimxo.txt becomes an empty file
            and hence so shadow of the copula onto the xy plane is visible.

EXAMPLES:
  $0 3dcopula_config_example.txt
       This configuration file sets up a negative association Plackett
       copula composited with the PSP copula. The file shows most of the
       structure of the configuration.  The processing run, if successful,
       will create a 3dcopula.pdf file containing a 3D representation of
       the level curves, sections, and diagonal of the copula.

  $0 --clean
       In case of crash and you want to quickly deleted unnecessary files
       from the current directory and exit.

  $0 --nsimcop=1000 -n4lcomom=5000 3dcopula_config_example.txt
       You want to see 1000 random samples of the copula in the PDF and
       want a reliable numerical approximation to the L-comoments of the
       copula to be available for inspection in file TMPlcomoment.txt.

FURTHER CONFIGURATION:
  The file 3dcopula_ancillary.tex, if exists, is included in the LaTeX
  run on 3dcopula.tex just before the \\begin{document} and users can 
  place arbitrary LaTeX code as needed.

HOW IT WORKS:
  The file 3dcopula.tex is the core of the 3D rendering. This LaTeX file
  uses packages of the PSTRICKS bundle and processing to dvi file is
  require. Your latex command must not point to pdflatex as PSTRICKS uses
  the postscript language. A dvi file is created and the dvipdf command is
  used to convert the dvi to pdf.

  The program $0 creates numerous temporary files TMP*.txt, which
  contain coordinate information to be read in by 3dcopula.tex during the
  LaTeX processing or TMPoff2R.R for a dispatch to R for the copula
  computations. The file TMPlcomoment.txt contains the L-comoments based
  on a simulation size provided by --n4lcomom=<integer> or 5000.
    

HERE
;

}
