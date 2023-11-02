# Encrypted config management

This is a simple tool to manage encrypted config files. Encryption uses AES-256-CBC.

## Installation

```
> npm install -g enccfg
```

## Usage

### Generate a new encryption key

```
> enccfg key-gen
PoF/Q1lscsCuvB0ANyR9Ci//KGqNsYbW633sluUtAf8=
```

### Encrypt a single value

```
> enccfg encrypt -k PoF/Q1lscsCuvB0ANyR9Ci//KGqNsYbW633sluUtAf8= -v "will be encrypted"
j5/DEwNMEcqW2KgGd73VXw==:VCu9meRUHs60ilz4WJWOekzsYPplHVkle3/WV1O15Ec=
```

### Decrypt a single value

```
> enccfg decrypt -k PoF/Q1lscsCuvB0ANyR9Ci//KGqNsYbW633sluUtAf8= -v j5/DEwNMEcqW2KgGd73VXw==:VCu9meRUHs60ilz4WJWOekzsYPplHVkle3/WV1O15Ec=
will be encrypted
```

### Insert a variable into an encrypted config file

```
> enccfg insert -k PoF/Q1lscsCuvB0ANyR9Ci//KGqNsYbW633sluUtAf8= -f config.enc -n VAR_NAME -v foo
> cat config.enc

VAR_NAME lqUOBQ+4bOA95MFWhO1yuQ==:hqa0F6eJ6nnX/dqS37H/Xw==
```

### Read all variables from an encrypted config file to stdout

```
> enccfg read -k PoF/Q1lscsCuvB0ANyR9Ci//KGqNsYbW633sluUtAf8= -i config.enc

VAR_NAME="foo"
```

### Read all variables from an encrypted config file into a decrypted target file

Note: target file will be created if it does not exist, and truncated prior to write.

```
> enccfg read -k PoF/Q1lscsCuvB0ANyR9Ci//KGqNsYbW633sluUtAf8= -i config.enc -o config.dec
> cat config.dec

VAR_NAME="foo"
```
