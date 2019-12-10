for f in `find | grep -o ".*+.*p" | sort | uniq`; do
  mkdir -p "/tmp/out/$(dirname "$f")";
  timeout 3s ./Scripts/tptp4X -x -umachine $f -ftptp | sed '/^%/ d' > /tmp/out/$f;
  RES=$?
  echo "$f = $RES"
  if [ $RES -ne 0 ]; then rm /tmp/out/$f; fi;
done;

