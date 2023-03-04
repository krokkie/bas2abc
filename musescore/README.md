# Musescore to ABC

_The Musescore source files are manually created by F. Kruger based on the Geneefse melodies of the psalms._

## Dependency setup

_This instruction is created for Linux enviornment_

First install the dependencies needed to convert Musescore to XML to ABC.

### Musescore to XML

Download the Musescore client from [https://musescore.org/en/download]. It's advisable to download the AppImage.

Make sure the client is runnable on the command line, e.g.:
```
./MuseScore-3.6.2.548021370-x86_64.AppImage --version
```

Now create a symbolic link so our converter can use the Musescore client:
```
sudo ln -s MuseScore-3.6.2.548021370-x86_64.AppImage /usr/bin/mscore
```

Download and install the [Musescore to XML](https://pypi.org/project/mscxyz/) converter using pip:
```
pip install mscxyz
```

And check it's installation:
```
mscx-manager --version
```

### XML to ABC

> This converter is packed with this repository and the following step should not be necessary

Download and extract this converter from this website [https://wim.vree.org/svgParse/xml2abc.html].

Check the file by running it with pyton:
```
python xml2abc.py --version
```

Now all dependencies are ready to use, make sure to copy the `xml2abc.py` converter into the same folder as the `convert.sh` script (and this README file). 

## File setup

Edit the `convert.sh` script so `files_dir` points to the correct directory on your system. 

## Run

Run `convert.sh` (`./convert.sh`) and the output ABC files should be created in the `files_dir` where the Musescore files are located. Note that this may take a while (minutes).

