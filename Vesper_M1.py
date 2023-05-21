# synthesized from:
# https://github.com/kahst/BirdNET-Analyzer#setup-macos
# https://vesper.readthedocs.io/en/latest/
  
conda create -n vesper-0.4.14 python=3.10

conda activate vesper-0.4.14

xcode-select --install

curl https://repo.anaconda.com/miniconda/Miniconda3-latest-MacOSX-arm64.sh -o ~/Downloads/Miniconda3-latest-MacOSX-arm64.sh
bash ~/Downloads/Miniconda3-latest-MacOSX-arm64.sh -b -p $HOME/miniconda

conda install -c apple tensorflow-deps

python -m pip install tensorflow-macos tensorflow-metal

conda install -c conda-forge librosa resampy -y

pip install vesper


vesper_admin createsuperuser

  
  
  ----
  
  
conda activate vesper-0.4.14

cd "/Users/"

vesper_admin runserver

localhost:8000
  
