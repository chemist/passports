# passports
Simple util for fast search expairing passports in Russia.

## install and prepare db
```bash
> git clone https://github.com/chemist/passports.git
> cd passports
> cabal sandbox init
> cabal install
```

```bash
> wget http://www.fms.gov.ru/upload/expired-passports/list_of_expired_passports.csv.bz2
> bunzip2 list_of_expired_passports.csv.bz2
> .cabal-sandbox/bin/passports_db list_of_expired_passports.csv
```
## usage
```bash
> .cabal-sandbox/bin/passports_check 0000000000
> 1 
```
found in db, bad passport

```bash
> .cabal-sandbox/bin/passports_check 0000000005
> 0
```
its good passport

Attention!!!

Folder with files: ./passports/{0..9} 

must be accesible in relative path ./passports/{0..9} or simple passports/{0-9}
