# A tool convert gfwlist.txt to x

## Current support converters:

* dnsmasq

### Install:

```shell
git clone https://github.com/jimmyhuco/gfwlist2x.git
cd gfwlist2x
stack setup
stack build
```

Wait a moment.

### dnsmasq usage:

`stack exec gfwlist2x -- -d 8.8.8.8#53 -i gfwlist -p dnsmasq_list.conf`

It will generate `dnsmasq_list.conf` in your current directory.

BTW: public dns servers are not safe enough.

### NOTES:

I write it for learning Haskell. Although I can use simple regular expression, but I try to use `parsec` to write my gfwlist parser.

Of course, it's absolute free.
