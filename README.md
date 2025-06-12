# Zcash to Fiat

A simple command-line tool written in Haskell that converts Zcash (ZEC) amounts to various fiat currencies.

## Features

- Convert ZEC to multiple fiat currencies
- Supports popular fiat currencies: USD, EUR, RUB and JPY (More to come)

## Run this program

* Install the latest binary (GNU/Linux only I think)
* After you install, navigate to the directory you installed it to
* Run:
  ```bash
  chmod +x zcash-to-fiat
  ./zcash-to-fiat
  ```

## Usage

`zcash-to-fiat` can run in two modes: interactive and non-interactive.

### Interactive Mode

Run the program with the `--interactive` flag (or `-i`) to get guided prompts for currency conversion.

```bash
./zcash-to-fiat --interactive
# or
./zcash-to-fiat -i
```

You will be asked to select currencies and input amounts step-by-step.

---

### Non-Interactive Mode with Flags

You can run the program with flags to convert currencies directly without prompts.

Example:

```bash
./zcash-to-fiat --amount 10 --from ZEC --to USD
```

This converts 10 ZEC to USD and prints the result immediately.

---

### Available Flags

| Flag            | Shortcut | Description                                   | Required in Non-Interactive Mode | Example Usage         |
|-----------------|----------|-----------------------------------------------|---------------------------------|------------------------|
| `--interactive` | `-i`     | Run program interactively with prompts        | No                              | `./zcash-to-fiat -i`   |
| `--amount`      | `-a`     | Amount to convert (decimal values accepted)   | Yes                             | `--amount 10.5`        |
| `--from`        |          | Currency to convert from (e.g., `ZEC`, `USD`) | Yes                             | `--from ZEC`           |
| `--to`          |          | Currency to convert to (e.g., `USD`, `EUR`)   | Yes                             | `--to USD`             |


---

### Supported Currencies

- ZEC (Zcash)
- USD (US Dollar)
- EUR (Euro)
- RUB (Russian Ruble)
- JPY (Japanese Yen)
- CAD (Canadian Dollar)
- GBP (Great British Pound)

---

## Examples

Convert 5 ZEC to USD in non-interactive mode:

```bash
./zcash-to-fiat --amount 5 --from ZEC --to USD
```

Run the program in interactive mode to use the guided interface:

```bash
./zcash-to-fiat --interactive
```

---

## Notes

- The program fetches live prices from CoinGecko.
- In non-interactive mode, all three flags `--amount`, `--from`, and `--to` are required.
- If required flags are missing, the program shows usage instructions.

---

## Support

For help or to request features, open an issue on the GitHub repo.