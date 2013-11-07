#!/bin/bash
if [ "$1" != ""  ];
 then
 if [ "$2" != "--no-display-old-contents" ]
  then
   cat $1
   echo "Backing up file to .$1.bk"
   cp  $1 .$1.bk
  else echo "";
 fi
 else echo "No file to load...";
fi
echo "Paste your program here and press Control-D to build."
cat > program.elm
if [ "$1" != "" ];
 then
  echo "Saving code to $1..."
  cp program.elm $1;
 else
  echo "No filename provided, won't save to separate file.";
fi
elm program.elm
chromium build/program.html
bash builder.sh $1 --no-display-old-contents
