use yew::prelude::*;

#[function_component(App)]
pub fn app() -> Html {
    html! {
        <main>
            <h1>{ "Counter example" }</h1>
            <Counter />
        </main>
    }
}

#[function_component]
fn Counter() -> Html {
    let counter = use_state(|| 0);
    let onclick = {
        let counter = counter.clone();
        move |_| {
            let value = *counter + 1;
            counter.set(value);
        }
    };
    html! {
        <div>
        <input value={format!("{}", *counter)} />
        <button {onclick}>{ "counter" }</button>
        // <p>{*counter}</p>
        </div>
    }
}