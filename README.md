---
title: Personal sysadmin
---

<!-- pandoc README.md -t html -s -o README.html --toc -->


This repository contains the files I need on most (all?) of my computers, whether they are for work or personal usage.
I provide these files without any warranty, more for me (as a backup), yet they can be useful to others.
As such, some files have a license (GPL) whereas others are simply in the public domain.


# List of free softwares I find useful

- text editor / éditeur de texte: [Emacs](https://en.wikipedia.org/wiki/Emacs)
  - with [packages](https://www.gnu.org/software/emacs/manual/html_node/emacs/Packages.html) auctex, auto-complete, elpy, ess, markdown-mode, polymode, xclip
  - useful tips: `emacs -nw`, `emacs --daemon`, `emacsclient -t`, `emacsclient -e "(kill-emacs)"`
  - with [Elpy](https://elpy.readthedocs.io/en/latest/index.html):
    - open python terminal in a new buffer: `C-c C-z`
    - send current line from script buffer to terminal buffer: `C-c C-y e`
- version control / gestion de versions: [Git](https://en.wikipedia.org/wiki/Git)
- internet browser / navigateur internet: [Firefox](https://en.wikipedia.org/wiki/Firefox)
  - with [extensions](https://addons.mozilla.org/en-US/firefox/) uBlock Origin, Open Tabs Next to Current, DuckDuckGo Privacy Essentials
  - with proxy INRA `http://revelec.inra.fr/revelec.pac` (Préférences > Paramètres réseau >  Adresse de configuration automatique du proxy)
  - take whole-page screenshot: Settings > Web development > Development tools > Tool options > Take a screenshot of the whole page
- office suite / suite bureautique: [LibreOffice](https://en.wikipedia.org/wiki/LibreOffice)
  - in Calc, turn off `AutoInput` in `Tools`
- PDF editor / éditeur de PDF:
  - [qpdf](https://en.wikipedia.org/wiki/QPDF)
    - extract pages: `qpdf --pages input.pdf 2-7 -- input.pdf output.pdf`
    - concatenate whole files: `qpdf --empty output.pdf --pages file1.pdf file2.pdf file3.pdf --`
  - [PDFtk](https://en.wikipedia.org/wiki/PDFtk)
    - extract pages: `pdftk input.pdf cat 2-7 output output.pdf`
  - [ghostscript](https://ghostscript.com/)
    - compress file: `gs -sDEVICE=pdfwrite -dCompatibilityLevel=1.4 -dPDFSETTINGS=/screen -dNOPAUSE -dQUIET -dBATCH -sOutputFile=output.pdf input.pdf`
  - [Xournal++](https://xournalpp.github.io/)
- media metadata editor / éditeur de méta-données de médias: [ExifTool](https://en.wikipedia.org/wiki/ExifTool)
  - rename photos: `exiftool '-filename<CreateDate' -d %Y-%m-%d_%H%M%S%%-c.%%le -r -ext jpg *`
- markup converter / convertisseur de documents: [Pandoc](https://en.wikipedia.org/wiki/Pandoc)
  - to convert this README to HTML: `pandoc README.md -f commonmark -t html -s -o README.html`
- typesetting / composition: [TeX Live](https://en.wikipedia.org/wiki/TeX_Live)
- backup / synchronisation: [Rsync](https://en.wikipedia.org/wiki/Rsync)
  - see my own '[time_machine](https://github.com/timflutre/perso/blob/master/time_machine)'
- password manager / gestionnaire de mots de passe: [KeePassXC](https://en.wikipedia.org/wiki/KeePassXC)
- reference manager / gestionnaire de références: [Zotero](https://en.wikipedia.org/wiki/Zotero)
  - with [plugins](https://www.zotero.org/support/plugins) Better Bibtex
  - Edition > Paramètres
    - Général:
      - Renommage des fichiers: décocher le renommage automatique
      - Cliquer sur "Personnaliser le format" et copier-coller le modèle suivant:
      ```
      {{ if {{ authorsCount > 1 }} }}
      {{ authors name="family-given" initialize="given" initialize-with="" name-part-separator="" suffix="et_al _" }}
      {{ else }}
      {{ authors name="family-given" initialize="given" initialize-with="" name-part-separator="" suffix="_" }}
      {{ year suffix="_" }}
      {{ title replaceFrom=" " replaceTo="_" regexOpts="g" truncate="80" }}
      ```
    - Avancé:
      - Répertoire de base pour les pièces jointes liées = ~/Documents/travail/biblio_zotero
      - Emplacement du répertoire de données = ~/Zotero
  - **pour ajouter un article**: ajouter son DOI ce qui crée un nouveau document dans la bibliothèque de Zotero; sauver le pdf dans le répertoire de base; cliquer droit sur l'item ajouté, choisir "Ajouter une pièce jointe" puis "Fichier lié" et sélectionner le pdf en question; cliquer droit sur l'item pièce jointe dans Zotero et choisir "Renomme le fichier" ce qui renomme le fichier dans Zotero mais aussi dans le répertoire de base
- statistical computing: [R](https://en.wikipedia.org/wiki/R_(programming_language))
  - with [packages](https://cran.r-project.org/web/packages/) Matrix, data.table, Rcpp, devtools, testthat, among others
- computing: [Julia](https://en.wikipedia.org/wiki/Julia_(programming_language))
- hypervisor / hyperviseur: [VirtualBox](https://en.wikipedia.org/wiki/VirtualBox)
- screenshot / capture d'écran: [Shutter](http://shutter-project.org/)
- image editor / éditeur d'images:
  - [GIMP](https://en.wikipedia.org/wiki/GIMP)
  - [Inkscape](https://en.wikipedia.org/wiki/Inkscape)
  - [ImageMagick](https://imagemagick.org/)
    - reduce file size: `mogrify -resize 50% *.jpg`
- photo manager / gestionnaire de photos: [Tropy](https://tropy.org/)
  - Install: `sudo tar xjf tropy-1.17.2-x64.tar.bz2 -C /opt/tropy`
  - Then, add the path to PATH in `.bash_profile`, and read their INSTALL file
- audio player / lecteur audio:
  - [Rhythmbox](https://en.wikipedia.org/wiki/Rhythmbox)
  - [Quod Libet](https://en.wikipedia.org/wiki/Quod_Libet_(software))
- audio tagging / éditeur de balises musicales: [Picard](https://en.wikipedia.org/wiki/MusicBrainz_Picard)
- CD ripping / extracteur de musique: [Sound Juicer](https://wiki.gnome.org/Apps/SoundJuicer)
- CD burning / graveur de CD: [Xfburn](http://goodies.xfce.org/projects/applications/xfburn)
- video player / lecteur de vidéos: [VLC](https://en.wikipedia.org/wiki/VLC_media_player)
- instant messaging / messagerie instantanée: [Empathy](https://wiki.gnome.org/Apps/Empathy)
- podcast: [gPodder](http://gpodder.org/)
- genealogy / généalogie: [Gramps](https://en.wikipedia.org/wiki/Gramps)
- encryption / cryptage: [GnuPG](https://en.wikipedia.org/wiki/GNU_Privacy_Guard)
- monitoring / contrôle:
  - processes and networks: [Glances](https://pypi.python.org/pypi/Glances)
  - disks: [smartmontools](https://www.smartmontools.org/) and its GUI GSmartControl
- émulateur de terminal sur Android: [Termux](https://termux.com/)
- clavier d'ordi sur Android: [Hacker's keyboard](https://github.com/klausw/hackerskeyboard)


# Install procedure working for me

1. download an [ISO image](https://en.wikipedia.org/wiki/ISO_image) of a given GNU/Linux distribution (ex.: [Ubuntu](https://en.wikipedia.org/wiki/Ubuntu_(operating_system)))
2. create a [bootable USB](https://en.wikipedia.org/wiki/Boot_disk) stick (use [GParted](https://en.wikipedia.org/wiki/GParted) to first format the USB as [FAT32](https://en.wikipedia.org/wiki/File_Allocation_Table#FAT32), then use [usb-creator](https://en.wikipedia.org/wiki/Startup_Disk_Creator))
3. boot on the USB and install the distribution
4. install `KeePassXC`, set up `.ssh` (copy pub/priv keys and config) and `.gnupg` (export and import pub/priv keys, followed by `shred` when using temporary files)
5. update `Firefox` and install its extensions
6. update all packages, and install new ones for `vim`, `emacs` and `git` (set up `.gitconfig`)
7. clone my git repositories from GitHub (starting with [perso](https://github.com/timflutre/perso))
8. set up `.bashrc`, `.bash_profile`, `.bash_aliases`, `.emacs`
9. install the Dropbox client
10. install the latest `R` version, and set up `.Renviron` and `.Rprofile`
  - on old computer: `x <- list.files(.libPaths()[1]); save(x, file="~/Rpkgs.RData")`
  - on new computer: `load("~/Rpkgs.RData"); for(i in seq_along(x)){message(paste(i,x[i])); if(! x[i] %in% installed.packages())install.packages(x[i])}`
  - for Bioconductor: ``w <- warnings(); pkgs <- sapply(substring(sapply(strsplit(names(y), " "), `[`, 2), 2), function(tmp){substring(tmp, 1, nchar(tmp)-1)}); BiocManager::install(pkgs)``
  - deal with the other packages manually (e.g., INLA)
11. copy all my files from a backup (e.g., `mkdir src; cp -rvT /media/tflutre/tflutre-backup/backup_agap-flutre_latest/src ~/src`)


# Tips

* get info about distribution: `cat /etc/*-release`
* get info about operating system: `uname -a`
* get info about memory: `free -ht`
* get info about file system: `df -T`, then `sudo dumpe2fs /dev/sda6 | less`
* get info about sound card: `alsamixer`
* sort files per size: `du -sk * | sort -rn`
* restart audio: `pulseaudio -k`
* check network interfaces: `ifconfig -a`
* scan wifi networks: `sudo iwlist wlan0 scan`
* diagnose wireless connection: see [this script](https://github.com/UbuntuForums/wireless-info)
* set up SSH:
  * on all computers: `cat .ssh/config` (which returns `ForwardAgent yes`)
  * copy public key: `scp ~/.ssh/id_rsa.pub login@hostname:~/.ssh`
  * use the agent: `` eval `ssh-agent` `` and then `ssh-add`
* install latest `emacs`: see this [PPA](https://launchpad.net/%7Ekelleyk/+archive/ubuntu/emacs)
* install latest `R`: see `install_custom_R.sh`, also see [this](https://cran.r-project.org/bin/linux/ubuntu/README)
* install latest `Julia`: see [these](https://ferrolho.github.io/blog/2019-01-26/how-to-install-julia-on-ubuntu) commands
  1. `mkdir -p ~/src_ext; cd ~/src_ext`
  2. `wget https://julialang-s3.julialang.org/bin/linux/x64/1.5/julia-1.5.2-linux-x86_64.tar.gz`
  3. `tar -xvzf julia-1.5.2-linux-x86_64.tar.gz`
  4. `sudo cp -r julia-1.5.2 /opt/` (see [why](https://askubuntu.com/questions/34880/use-of-opt-and-usr-local-directories-in-the-context-of-a-pc/34922#34922))
  5. `sudo ln -s /opt/julia-1.5.2/bin/julia /usr/local/bin/julia`
* turn off `gnome-keyring` ([source](https://stackoverflow.com/a/25465155/597069)): `sudo chmod -x /usr/bin/gnome-keyring-daemon`
* install latest LibreOffice: [download](http://www.libreoffice.org/download), `tar -xzvf`, `sudo dpkg -i *.deb` (also see [this](https://doc.ubuntu-fr.org/libreoffice))
* set up a virtual machine with VirtualBox:
  1. `sudo apt-get install virtualbox`
  2. `mkdir -p ~/vima` (allow to exclude it easily from backup via `rsync`)
  3. `vboxmanage setproperty machinefolder /home/tflutre/vima/`
  4. `cd ~/vima; ln -s ~/src_ext/windows/mw10.iso .`
  5. follow the [tuto](https://itsfoss.com/install-windows-10-virtualbox-linux/)
  6. `mkdir -p ~/vima/shared_dir`
  7. install extension pack
* [cheatsheet](https://www.computerhope.com/issues/chusedos.htm) to use Windows command line (DOS)
* limit the recharging of the battery: to the interval 50-60%
  - `sudo apt-get install smbios-utils`
  - `sudo smbios-battery-ctl --get-charging-cfg`
  - `sudo smbios-battery-ctl --set-custom-charge-interval 50 60`
  - `sudo smbios-battery-ctl --set-charging-mode=custom`
* in Microsoft OWA, get all emails between two dates: `received:1/1/2015..12/31/2015`


# Useful tutorials

## LaTeX

* https://karthinks.com/software/latex-input-for-impatient-scholars/

## Git

* https://git-scm.com/book/en/v2

* https://githowto.com/

## Emacs

* https://www.gnu.org/software/emacs/tour/

* https://www.tuteurs.ens.fr/unix/editeurs/emacs.html

* https://polymode.github.io/
