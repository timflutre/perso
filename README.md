<!-- pandoc README.md -f commonmark -t html -s -o README.html -->

This repository contains the files I need on most (all?) of my computers, whether they are for work or personal usage.
I provide these files without any warranty, more for me (as a backup), yet they can be useful to others.
As such, some files have a license (GPL) whereas others are simply in the public domain.


# List of free softwares I find useful

- text editor / éditeur de texte: [Emacs](https://en.wikipedia.org/wiki/Emacs)
  - with [packages](https://www.gnu.org/software/emacs/manual/html_node/emacs/Packages.html) auctex, auto-complete, ess, markdown-mode and polymode
- version control / gestion de versions: [Git](https://en.wikipedia.org/wiki/Git)
- internet browser / navigateur internet: [Firefox](https://en.wikipedia.org/wiki/Firefox)
  - with [extensions](https://addons.mozilla.org/en-US/firefox/) uBlock Origin, Open Tabs Next to Current, DuckDuckGo Privacy Essentials
  - with proxy INRA `http://revelec.inra.fr/revelec.pac` (Préférences > Paramètres réseau >  Adresse de configuration automatique du proxy)
  - take whole-page screenshot: Settings > Web development > Development tools > Tool options > Take a screenshot of the whole page
- office suite / suite bureautique: [LibreOffice](https://en.wikipedia.org/wiki/LibreOffice)
  - in Calc, turn off `AutoInput` in `Tools`
- PDF editor / éditeur de PDF: [PDFtk](https://en.wikipedia.org/wiki/PDFtk)
  - extract pages: `pdftk input.pdf cat 2-7 output output.pdf`
- media metadata editor / éditeur de méta-données de médias: [ExifTool](https://en.wikipedia.org/wiki/ExifTool)
  - rename photos: `exiftool '-filename<CreateDate' -d %Y-%m-%d_%H%M%S%%-c.%%le -r -ext jpg *`
- markup converter / convertisseur de documents: [Pandoc](https://en.wikipedia.org/wiki/Pandoc)
  - to convert this README to HTML: `pandoc README.md -f commonmark -t html -s -o README.html`
- typesetting / composition: [TeX Live](https://en.wikipedia.org/wiki/TeX_Live)
- backup / synchronisation: [Rsync](https://en.wikipedia.org/wiki/Rsync)
  - see my own '[time_machine](https://github.com/timflutre/perso/blob/master/time_machine)'
- password manager / gestionnaire de mots de passe: [KeePassXC](https://en.wikipedia.org/wiki/KeePassXC)
- reference manager / gestionnaire de références: [Zotero](https://en.wikipedia.org/wiki/Zotero)
  - with [plugins](https://www.zotero.org/support/plugins) Zotfile and Better Bibtex
  - Preferences > Advanced > Files and Folders:
    - Base directory = ~/Documents/travail/biblio_zotero
    - Data directory location = Use profile directory
  - Zotfile preferences > General Settings
    - Source folder for attaching new files: select "Use Firefox download folder"
    - unselect "Watch for new files in source folder"
    - Location of files: Custom location = same as Zotero base directory
  - Zotfile preferences > Renaming Rules
    - unselect "Use Zotero to rename"
    - format = {%F_}{%y_}{%t}
    - unselect "Change to lower case"
    - select "Replace blanks"
    - select "Truncate title"
    - max length = 80
    - max nb authors = 1
    - select "Add suffix when authors are omitted" = " et al"
  - Zotfile preferences > Advanced settings
    - Automatically rename new attachments = "Always rename"
    - unselect everything
  - Preferences > Sync: enter id and password, then click on Sync with Zotero Server
- statistical computing: [R](https://en.wikipedia.org/wiki/R_(programming_language))
  - with [packages](https://cran.r-project.org/web/packages/) Matrix, data.table, Rcpp, devtools, testthat, among others
- hypervisor / hyperviseur: [VirtualBox](https://en.wikipedia.org/wiki/VirtualBox)
- screenshot / capture d'écran: [Shutter](http://shutter-project.org/)
- image editor / éditeur d'images: [GIMP](https://en.wikipedia.org/wiki/GIMP)
- image editor / éditeur d'images: [Inkscape](https://en.wikipedia.org/wiki/Inkscape)
- audio player / lecteur audio: [Rhythmbox](https://en.wikipedia.org/wiki/Rhythmbox)
- audio player / lecteur audio: [Quod Libet](https://en.wikipedia.org/wiki/Quod_Libet_(software))
- audio tagging / éditeur de balises musicales: [Picard](https://en.wikipedia.org/wiki/MusicBrainz_Picard)
- CD ripping / extracteur de musique: [Sound Juicer](https://wiki.gnome.org/Apps/SoundJuicer)
- CD burning / graveur de CD: [Xfburn](http://goodies.xfce.org/projects/applications/xfburn)
- video player / lecteur de vidéos: [VLC](https://en.wikipedia.org/wiki/VLC_media_player)
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
4. install `KeePassXC`, set up `.ssh` (copy pub/priv keys and config) and `.gnupg` (import pub/priv keys)
5. update `Firefox` and install its extensions
6. update all packages, and install new ones for `vim`, `emacs` and `git` (set up `.gitconfig`)
7. clone my git repositories from GitHub (starting with [perso](https://github.com/timflutre/perso))
8. set up `.bashrc`, `.bash_profile`, `.emacs`
9. install the Dropbox client
10. install the latest `R` version ([help](https://cran.r-project.org/bin/linux/ubuntu/README)), and set up `.Renviron` and `.Rprofile`


# Tips

* get info about distribution: `cat /etc/*-release`
* get info about operating system: `uname -a`
* get info about memory: `free -ht`
* get info about file system: `df -T`, then `sudo dumpe2fs /dev/sda6 | less`
* check network interfaces: `ifconfig -a`
* scan wifi networks: `sudo iwlist wlan0 scan`
* diagnose wireless connection: see [this script](https://github.com/UbuntuForums/wireless-info)
* install latest `emacs`: see this [PPA](https://launchpad.net/%7Ekelleyk/+archive/ubuntu/emacs)
* install latest `R`: see [here](https://cran.r-project.org/bin/linux/ubuntu/README)
* turn off `gnome-keyring` ([source](https://stackoverflow.com/a/25465155/597069)): `sudo chmod -x /usr/bin/gnome-keyring-daemon`
* install latest LibreOffice: [download](http://www.libreoffice.org/download), `tar -xzvf`, `sudo dpkg -i *.deb`
* set up a virtual machine with VirtualBox:
  1. `sudo apt-get install virtualbox`
  2. `mkdir -p ~/vima` (allow to exclude it easily from backup via `rsync`)
  3. `vboxmanage setproperty machinefolder /home/tflutre/vima/`
  4. `cd ~/vima; ln -s ~/src_ext/windows/mw10.iso .`
  5. follow the [tuto](https://itsfoss.com/install-windows-10-virtualbox-linux/)
  6. `mkdir -p ~/vima/shared_dir`
  7. install extension pack
* [cheatsheet](https://www.computerhope.com/issues/chusedos.htm) to use Windows command line (DOS)
