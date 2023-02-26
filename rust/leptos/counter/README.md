Run with:

cargo leptos watch
rustup toolchain install nightly && rustup default nightly && rustup target add wasm32-unknown-unknown

---

Run Playwright tests:

cd end2end
npx playwright install
npx playwright test