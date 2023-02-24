import {useState} from "react";

export function Counter() {
    const [count, setCount] = useState(0);
    return (
        <>
            <input type="text" value={count} disabled={true}/>
            <button onClick={() => setCount(count + 1)}>counter</button>
        </>
    );
}
