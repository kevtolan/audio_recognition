# synthesized from:
# https://github.com/kahst/BirdNET-Analyzer#setup-macos
# https://vesper.readthedocs.io/en/latest/
  
conda create -n vesper-0.4.14 python=3.10 #create environment

conda activate vesper-0.4.14 #activate environment

xcode-select --install #install xcode

curl https://repo.anaconda.com/miniconda/Miniconda3-latest-MacOSX-arm64.sh -o ~/Downloads/Miniconda3-latest-MacOSX-arm64.sh
bash ~/Downloads/Miniconda3-latest-MacOSX-arm64.sh -b -p $HOME/miniconda
# The installer prompts “Do you wish the installer to initialize Miniconda3 by running conda init?” We recommend “yes”.

conda install -c apple tensorflow-deps #install necessary software ("dependencies")

python -m pip install tensorflow-macos tensorflow-metal

conda install -c conda-forge librosa resampy -y

pip install vesper #install vesper

vesper_admin createsuperuser #create log in

  
  
  ----
  
  
conda activate vesper-0.4.14 #activate environment

cd "/Users/" 

vesper_admin runserver

localhost:8000
  
