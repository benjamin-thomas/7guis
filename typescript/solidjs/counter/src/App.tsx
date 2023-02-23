import type {Component} from 'solid-js';
import {createSignal} from "solid-js";

const App: Component = () => {
    const [counter, setCounter] = createSignal(0);
    return (
        <div>
            <div>
                <h1>Counter example</h1>
            </div>
            <div>
                <span >
                    <input style={{"user-select": "none", "pointer-events": "none"}} type="text" value={counter()} readonly={true} disabled={true}/>
                </span>
                    <button onClick={() => setCounter(counter() + 1)}>counter</button>
            </div>
        </div>
    );
};
export default App;
