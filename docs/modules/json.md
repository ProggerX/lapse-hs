# JSON

### Description
Module that allows you to encode and decode JSON.

### Import
```
(import "json")
```

### Usage
```
(print $ decode $ encode 5)
```

### Reference
- `(encode <value:data>)` -- encodes `data` to JSON (Of course, it cannot store functions and macroses) and returns string.
- `(decode <string:json-data>)` -- decodes JSON and returns value (All numbers become floats!)

### Source
[Source](https://github.com/ProggerX/lapse-hs/blob/master/src/Lapse/Modules/Json.hs)
