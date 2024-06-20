# Surfer

This is the SSRF detection tool from our work `SSRF vs. Developers: A Study of SSRF-Defenses in PHP Applications` at USENIX Security '24.
It consumes a PHP bytecode CPG.

## Cite Us
If you are using this tool, cite us via:

```
@inproceedings{USENIX:Wessels:2024,
  title={SSRF vs. Developers: A Study of SSRF-Defenses in PHP Applications}
  authors={Wessels, Malte and Koch, Simon and Pellegrino, Giancarlo and Johns, Martin},
  venue={USENIX Security},
  year={2024}
}
```
## Usage

We recommend using the docker container which can be built via the `create.sh` script in `resources/docker`. It depends on our other docker containers, see the README in this organisation.


### Dependencies


For local dev only:
- `de.tubs.cs.ias:cpg-slice-util_2.13:0.0.26`
  - run `publishLocal` in that repository
