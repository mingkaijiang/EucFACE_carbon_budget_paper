# The fate of carbon in a mature forest under carbon dioxide enrichment

[Mingkai Jiang](https://www.westernsydney.edu.au/hie/people/postdoctoral_fellows/doctor_mingkai_jiang), 
[Belinda E. Medlyn](https://www.westernsydney.edu.au/hie/people/researchers/professor_belinda_medlyn), 
[John E. Drake](https://www.esf.edu/faculty/drake/), 
[Remko A. Duursma](http://www.remkoduursma.com/), 
[Ian C. Anderson](https://www.westernsydney.edu.au/hie/people/researchers/professor_ian_anderson), 
[Craig V.M. Barton](https://www.westernsydney.edu.au/hie/people/admin_and_technical_staff/dr_craig_barton), 
[Matthias M. Boer](https://www.westernsydney.edu.au/hie/people/researchers/assoc_prof_matthias_boer), 
[Yolima Carrillo](https://www.westernsydney.edu.au/hie/people/researchers/doctor_yolima_carrillo), 
[Laura Castañeda-Gómez](https://www.westernsydney.edu.au/hie/people/postgraduate_students/current_postgraduate_students/laura_castaneda_gomez), 
Luke Collins, 
[Kristine Y. Crous](https://www.westernsydney.edu.au/hie/people/researchers/doctor_kristine_crous), 
[Martin G. De Kauwe](https://mdekauwe.github.io/), 
Bruna M. dos Santos, 
[Kathryn M. Emmerson](https://soe.environment.gov.au/authors/dr-kathryn-emmerson), 
[Sarah L. Facey](https://www.westernsydney.edu.au/hie/people/postgraduate_students/graduates/sarah_facey), 
[Andrew N. Gherlenda](https://www.westernsydney.edu.au/hie/people/admin_and_technical_staff/doctor_andrew_gherlenda), 
[Teresa E. Gimeno](https://www.ikerbasque.net/en/teresa-gimeno), 
[Shun Hasegawa](https://scholar.google.com.au/citations?user=CxhD1vsAAAAJ&hl=en), 
[Scott N. Johnson](https://www.westernsydney.edu.au/hie/people/researchers/assoc_prof_scott_johnson), 
[Catriona A. Macdonald](https://www.westernsydney.edu.au/hie/people/researchers/doctor_catriona_macdonald), 
[Kashif Mahmud](https://eri.iu.edu/who-we-are/researchers/kashif-mahmud.html), 
[Astrid Kännaste](https://www.etis.ee/CV/Astrid_K%C3%A4nnaste/est), 
[Ben D. Moore](https://www.westernsydney.edu.au/hie/people/researchers/doctor_benjamin_moore), 
Loic Nazaries, 
E.H.J. Neilson, 
[Uffe N. Nielsen](https://www.westernsydney.edu.au/hie/people/researchers/assoc_prof_uffe_nielsen), 
[Ülo Niinemets](https://niinemetslab.wordpress.com/), 
[Nam Jin Noh](https://www.westernsydney.edu.au/hie/people/postdoctoral_fellows/doctor_nam_jin_noh), 
[Raul Ochoa-Hueso](https://rochoahueso.wordpress.com/), 
[Varsha S. Pathare](https://www.westernsydney.edu.au/hie/people/postgraduate_students/graduates/varsha_pathare), 
[Elise Pendall](https://www.westernsydney.edu.au/hie/people/researchers/professor_elise_pendall), 
[Johanna Pihlblad](https://www.westernsydney.edu.au/hie/people/postgraduate_students/current_postgraduate_students/johanna_pihlblad), 
[Juan Pineiro](https://www.westernsydney.edu.au/hie/people/postgraduate_students/graduates/juan_pineiro_nevado), 
[Jeff R. Powell](https://www.westernsydney.edu.au/hie/people/researchers/assoc_prof_jeff_powell), 
[Sally A. Power](https://www.westernsydney.edu.au/hie/people/researchers/professor_sally_power), 
[Peter B. Reich](https://www.westernsydney.edu.au/hie/people/researchers/professor_peter_reich), 
[Alexis A. Renchon](https://www.westernsydney.edu.au/hie/people/postgraduate_students/graduates/alexis_renchon), 
[Markus Riegler](https://www.westernsydney.edu.au/hie/people/researchers/associate_professor_markus_riegler), 
[Riikka Rinnan](https://www1.bio.ku.dk/staff/rinnan/), 
[Paul Rymer](https://www.westernsydney.edu.au/hie/people/researchers/doctor_paul_rymer), 
Roberto L. Salomón, 
[Brajesh K. Singh](https://www.westernsydney.edu.au/hie/people/researchers/professor_brajesh_singh), 
[Ben Smith](https://www.westernsydney.edu.au/hie/people/researchers/professor_ben_smith_director_of_research), 
[Mark G. Tjoelker](https://www.westernsydney.edu.au/hie/people/researchers/professor_mark_tjoelker), 
Jennifer K.M. Walker, 
Agnieszka Wujeska-Klause, 
[Jinyan Yang](https://sites.google.com/site/jinyanjimyang), 
[Sönke Zaehle](https://www.bgc-jena.mpg.de/bgi/index.php/People/SoenkeZaehle), 
[David S. Ellsworth](https://www.westernsydney.edu.au/hie/people/researchers/professor_david_ellsworth)
*Nature*, 2020, 580, 227-231.

## Overview ##

Repository containing all the code (where possible) and associated plotting scripts required to reproduce results presented in our EucFACE carbon budget paper.

Data available at: 
DOI: 10.6084/m9.figshare.11634315



## General instructions

1. If source data from HIEv, the internal data server at the Hawkesbury Institute for the Environment, you need you place your HIEv token in a file called 'tokenfile.txt', and place it in the directory for this project. Note that only internal user from HIE who has access to HIEv has a token file. 
2. If source data from the data repository, you need to create a folder named "data" in the main directory, and place all the downloaded data, in their existing structure, into the folder. 
3. The `run.R` script is the master script, where the entire repo is processed. Detailed instructions and comments are available within the script. 
4. Codes for the budget are organized in subdirectories of the folder "module". 
5. Each of those folders has a function definition in it, which is named make_<<module>>.R, for example 'make_leaf_pool.R'. Many folders also contain a separate script to download the data, and some additional scripts to process the data and generate statistics. 
6. Constants / hardwired parameters are placed in folder "definitions".
7. All packages and essential pre-setting work are pre-loaded in `R/prepare.R`.
8. Data assimilation codes are stored in folder "DA_scripts".
9. Data assimilation results are stored in folder "DA_output".
10. The folder 'R" synthesized all the individual scripts and compute the budget related variables and statistics. 
11. All key output of the budget are stored in folder "output".


## Contacts
- Mingkai Jiang: m.jiang at westernsydney.edu.au




