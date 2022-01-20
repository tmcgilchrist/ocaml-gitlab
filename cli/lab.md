Lab CLI
==========

lab - makes git easier with GitLab

lab api - Low-level GitLab API request interface.

<!-- $MDX non-deterministic=command -->
```sh
$ dune install lab
...
[1]
```

<!-- $MDX non-deterministic=command -->
```sh
$ lab api "https://gitlab.com/api/v4/users?username=tmcgilchrist"
[
  {
    "id": 490393,
    "username": "tmcgilchrist",
    "name": "Tim McGilchrist",
    "state": "active",
    "avatar_url":
      "https://secure.gravatar.com/avatar/67afd2b4c98c9befd18c19f0ee9d94dc?s=80&d=identicon",
    "web_url": "https://gitlab.com/tmcgilchrist"
  }
]
```

<!-- $MDX non-deterministic=command -->
```sh
$ lab user-name --owner-name tmcgilchrist
tmcgilchrist:490393
```

List user projects

<!-- $MDX non-deterministic=command -->
```sh
$ lab user-projects  --owner 490393
tezos
ocaml-changes
ocaml-gitlab
freer
```

<!-- $MDX non-deterministic=command -->
```sh
$ lab --help
LAB(1)                            Lab Manual                            LAB(1)



NNAAMMEE
       lab - make git easier with GitLab

SSYYNNOOPPSSIISS
       llaabb _C_O_M_M_A_N_D ...

DDEESSCCRRIIPPTTIIOONN
       Lab is a tool that wraps git in order to extend it with extra
       functionality that makes it better when working with GitLab.

CCOOMMMMAANNDDSS
       aappii -- LLooww--lleevveell GGiittLLaabb AAPPII rreeqquueesstt iinntteerrffaaccee


       bbrraanncchh -- LLiisstt bbrraanncchheess ffoorr aa pprroojjeecctt


       ccii--ssttaattuuss -- LLiisstt bbuuiilldd ssttaattuuss ooff aa ccoommmmiitt


       mmeerrggee--rreeqquueessttss -- LLiisstt uusseerr''ss mmeerrggee rreeqquueessttss


       pprroojjeecctt--ccrreeaattee -- CCrreeaatteess aa nneeww pprroojjeecctt oowwnneedd bbyy tthhee aauutthheennttiiccaatteedd uusseerr..


       sseett--ccii--ssttaattuuss -- SSeett oorr uuppddaattee tthhee bbuuiilldd ssttaattuuss ooff aa ccoommmmiitt


       ssttaattuuss--cchheecckkss -- LLiisstt eexxtteerrnnaall ssttaattuuss cchheecckkss


       uusseerr--eevveennttss -- LLiisstt aallll uusseerr eevveennttss


       uusseerr--lliisstt -- DDiissppllaayy uusseerr nnaammee aanndd iidd


       uusseerr--nnaammee -- DDiissppllaayy uusseerrss bbyy nnaammee aanndd iidd


       uusseerr--pprroojjeeccttss -- LLiisstt ppuubblliicc pprroojjeeccttss oowwnneedd bbyy tthhee uusseerr


OOPPTTIIOONNSS
       ----hheellpp[=_F_M_T] (default=auto)
           Show this help in format _F_M_T. The value _F_M_T must be one of `auto',
           `pager', `groff' or `plain'. With `auto', the format is `pager` or
           `plain' whenever the TTEERRMM env var is `dumb' or undefined.

       ----vveerrssiioonn
           Show version information.

BBUUGGSS
       <https://github.com/tmcgilchrist/ocaml-gitlab/issues>

AAUUTTHHOORRSS
       <https://github.com/tmcgilchrist/ocaml-gitlab>



Lab 0.1                                                                 LAB(1)
```
