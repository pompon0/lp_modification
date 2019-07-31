DIR=/tmp/leancop
rm -rf $DIR
mkdir $DIR
cd $DIR
wget https://storage.googleapis.com/tptp/leancop.tgz
tar xf leancop.tgz
cd bmtp
make
tar -zcvf $DIR/leancop_bin.tgz -C $DIR/bmtp bmtp
gcloud auto login
gsutil cp $DIR/leancop_bin.tgz gs://tptp/
