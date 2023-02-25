import type {Component} from 'solid-js';
import {createSignal} from "solid-js";

import styles from './App.module.css';

const App: Component = () => {
    const [celsius, setCelsius] = createSignal("");
    const [fahrenheit, setFahrenheit] = createSignal("");
    const [warnMsg, setWarnMsg] = createSignal("");

    function fahrenheitFromCelsius(s: string) {
        const c = Number(s);
        const f = c * 9 / 5 + 32;
        setFahrenheit(sanitize("celsius", f));
    }

    function sanitize(srcName: string, n: number) {
        if (isNaN(n)) {
            setWarnMsg(`Not valid: ${srcName}`)
            return "";
        } else {
            setWarnMsg("");
            return n.toString();
        }
    }

    function celsiusFromFahrenheit(s: string) {
        const f = Number(s);
        const c = (f - 32) * 5 / 9;

        setCelsius(sanitize("fahrenheit", c));
    }

    return (
        <div class={styles.App}>
            <h1>Temperature Converter</h1>

            <form autocomplete="off">


            <span>
                <input id='celsius' type="text" value={celsius()}
                       onInput={(e) => fahrenheitFromCelsius(e.currentTarget.value)}/>
                <label for="celsius">Celsius</label>
            </span>

                <span>&nbsp;=&nbsp;</span>

                <span>
                <input id='fahrenheit' type="text" value={fahrenheit()}
                       onInput={(e) => celsiusFromFahrenheit(e.currentTarget.value)}/>
                <label for="fahrenheit">Fahrenheit</label>
            </span>
            </form>

            <div>
                {warnMsg()}
            </div>
        </div>
    );
};

export default App;
