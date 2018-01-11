# Hanzi data in OCaml

This project provides an OCaml interface to the data available in
<https://github.com/skishore/makemeahanzi>. This data contains information
about Hanzi – often referred to as Chinese characters – such as:

- Unicode point,
- meaning,
- pronuncitation,
- components,
- stroke order,
- etc.


## Make Me a Hanzi

Details about that project is available on
<https://www.skishore.me/makemeahanzi/>.


## Features

This OCaml project provides

- a native datastructure to represent the data, and
- look-up and other such functionality over the data structure.

The project also privodes a printer for `Uchar.t` compatible with the
toplevel.

Further features, which may or may not be developped, include:

- rendering,
- flash-cards,
- etc.

