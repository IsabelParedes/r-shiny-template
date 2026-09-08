# R Shiny → Lucent (WebAssembly) template

GitHub **template** repository for packaging a Shiny app with
[`lucent-pack`](https://github.com/IsabelParedes/lucent-pack) and deploying it
to **GitHub Pages**.

## Quick start (local)

1. Install `lucent-pack`

```bash
micromamba install lucent-pack
# OR
pip install lucent-pack
```

2. Replace `app/` with any Shiny app (keep an `app.R` or Shiny directory layout).
3. Edit `environment.yaml` to include every R package the app needs.
4. Create the WebAssembly environment.

```bash
micromamba create -f environment.yaml --platform=emscripten-wasm32
```

5. Pack the app and its environment

```bash
lucent build \
  --prefix-dir /path/to/wasm/environment \
  --app ./app --outdir /path/to/output/dir/ \
  --title "My Shiny App" # (optional)
```

6. (Optional) Serve with lucent

```bash
lucent serve /path/to/output/dir
```

Open the URL printed by `lucent serve` (default `http://127.0.0.1:8000/`).

## Deploy with GitHub Pages

1. Use this repository as a **template** (“Use this template”) or fork it.
2. In the new repo: **Settings → Pages → Build and deployment → Source: GitHub Actions**.
3. Update `environment.yaml` and `app/` as needed.
4. Push to `main`. The [pages workflow](.github/workflows/pages.yml) will:
   - create the wasm prefix from `environment.yaml`
   - install `lucent-pack` and build the site
   - deploy the site to GitHub Pages
