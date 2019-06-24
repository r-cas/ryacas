#!/bin/sh

rm -rf ../inst/yacas/
mkdir ../inst/yacas
cp -R ./new-inst-yacas/* ../inst/yacas/

rm -rf ../src/yacas/
mkdir ../src/yacas
cp -R ./new-src-yacas/* ../src/yacas/

