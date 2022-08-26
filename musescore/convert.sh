#!/bin/bash

files_dir="./files"

for file in "${files_dir}"/*.mscz; do
	name=$(basename "$file" .mscz)
	echo ${name}
	
	mscx-manager export -e xml "$file"
	python xml2abc.py "${files_dir}/${name}.xml" > "${files_dir}/${name}.abc"
	rm "${files_dir}/${name}.xml"
done
