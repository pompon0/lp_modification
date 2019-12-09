for f in `find | grep -o ".*+.*p" | sort | uniq`; do
  
  mkdir -p "/tmp/out/$(dirname "$f")";
  timeout 3s ./Scripts/tptp4X -x -umachine $f -ftptp | sed '/^%/ d' > /tmp/out/$f;
  echo "$f = $?"
done;

