# Changelog

`fedora-dists` uses [PVP Versioning](https://pvp.haskell.org).

## 2.1.0 (2022-05-18)
- add epel-next to Dist and Branch (juhp/fbrnch#29)
- remove obsolete deps on bytestring and time

## 2.0.0 (2022-01-17)
- Distribution.Fedora: new IO-based Dist API
  without Fedora version hardcoding
- caches current releases from
  https://pdc.fedoraproject.org/rest_api/v1/product-versions/
  under ~/.fedora/
- Distribution.Fedora.Branch provides Branch type taken from juhp/fbrnch

## 1.1.2 (2020-01-24)
- mockConfig rawhide version
- drop F29 and EPEL6

## 1.1.1 (2019-12-15)
- fix read and show for RHEL

## 1.1.0 (2019-10-29)
- update for Fedora 31
- haddock documentation
- move distTag, distTarget and hackageRelease to fedora-haskell-tools

## 1.0.1
- add mockConfig and distContainer
- Fedora 30 branched from Rawhide

## 1.0.0.2
- add haddock strings
- base >= 4.6 (ghc-7.6)

## 1.0.0
- initial release (project created with summoner)
- based Dists from fedora-haskell-tools
- Dist is now a datatype
