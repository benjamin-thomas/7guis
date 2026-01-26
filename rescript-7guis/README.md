## Development

Run ReScript in dev mode:

```sh
npm run res:dev
```

In another tab, run the Vite dev server:

```sh
npm run dev
```

## Tips

### Fast Refresh & ReScript

Make sure to create interface files (`.resi`) for each `*.res` file.

Fast Refresh requires you to **only export React components**, and it's easy to unintentionally export other values that will disable Fast Refresh (you will see a message in the browser console whenever this happens).
