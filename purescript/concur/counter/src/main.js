import { main } from "../output/Main/index";

if (import.meta.hot) {
    import.meta.hot.accept(() => {
        console.info('Hot reloaded!');
    });
}

main();