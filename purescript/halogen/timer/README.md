Run with:

```
# NOTE: 0.0.0.0 does not work properly. Specifying the host like this breaks localhost too.
npm run serve -- --host=$(hostname -I | awk '{print $1}')
```