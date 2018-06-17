# metar

A METAR report is a loose international semi-standard used by airports for reporting information about wind speeds, humidity, and weather conditions. This program parses a subset of these reports from a stream and keeps some running aggregates.

## Format

```txt
<ICAO Code> <Timestamp> <Wind Info>
```

Which breaks down into this:

### ICAO Code

This is a string in the ASCII range of upper-case letters. It is at least one such character. It is terminated by a space after the final character.

Examples:

- YYZ
- A
- LAX
- BIRK

We’re not concerned about verifying the validity of these codes in this exercise. Parsing them is enough.

### Timestamp

This is a string in the format of:

```txt
<day of month><hours><minutes>Z
```

Where:

- day of month: 2 digits, the parsed number is in the range of 1-31 inclusive
- hours: 2 digits, the parsed number is in the range of 0-23 inclusive
- minutes: 2 digits, the parsed number in the range of 0-59

### Wind Info

This one is a little tricky. The METAR format specifies wind speeds in two different units: _knots_ or _meters per second_. To complicate matters there is an optional _gusts_ value.

Eg:

```txt
18027KT
180120MPS
01323G30MPS
```

The components of the format can be parsed as follows:

- _direction_: 3 digits
- _speed_: 2-3 digits, minimum 00
- _gusts?_: 2 digits, optional. When it appears, parsed as `G23`
- _unit_: Either `KT` or `MPS`

## Example Reports

```txt
YYZ 122201Z 12023MPS
LAX 022355Z 09332G78KT
FR 110232Z 001100G12MPS
```

## Features

- parses a METAR format record as described above
  - the returned data structure has speeds normalized to MPS
  - a speed value in KT is normalized by dividing the value by 2

- Read a stream of records, one per line
  - and keeps a running average wind speed per airport seen
  - and keeps the current wind speed of each airport seen

## Compiling and running

- Get [`stack`](https://docs.haskellstack.org/en/stable/install_and_upgrade/)

- Compile
  ```bash
  $ stack build --pedantic
  ```

- Test
  ```bash
  $ stack test --pedantic
  ```

- Run
  ```bash
  $ stack exec -- metar [ARGS]
  ```

## Usage

- Find available options and flags
  ```bash
  $ stack exec -- metar --help
  ```
- Seed `/tmp/test.txt` with randomly generated 1234567890 records
  ```bash
  $ stack exec -- metar --out /tmp/test.txt --limit 1234567890
  ```
- Parse `/tmp/test.txt` to find average wind speeds
  ```bash
  $ stack exec -- metar --in /tmp/test.txt
  ```
