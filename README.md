# c4c

c4c is really simple template language for creating other config files

## Usage
`c4c <config.conf> <filename.ext.c4c>` creates `filename.ext` file.
```yaml
# config.conf
FOO lorem ipsum
BAR 42
```
```yaml
# filename.ext.c4c
field1: hard coded value
field2: #{FOO}
field3: #{BAR}
```
```yaml
# filename.ext
field1: hard coded value
field2: lorem ipsum
field3: 42
```
