#!/bin/sh

rm -rf new-src-yacas
rm -rf new-inst-yacas

mkdir new-src-yacas
mkdir new-inst-yacas


cp -R ./yacas/cyacas/libyacas/src ./new-src-yacas/
cp -R ./yacas/cyacas/libyacas/include ./new-src-yacas/
cp -R ./yacas/AUTHORS ./new-src-yacas/
cp -R ./yacas/COPYING ./new-src-yacas/

cp -R ./yacas/cyacas/libyacas_mp/include/yacas/ ./new-src-yacas/include/
cp -R ./yacas/cyacas/libyacas_mp/src/ ./new-src-yacas/

cp ./yacas/build/cyacas/libyacas/config/yacas/yacas_version.h ./new-src-yacas/include/yacas/

####

cp -R ./yacas/scripts/* ./new-inst-yacas/


