// The recurring timer is a *source* of messages, modeled as a Flame
// subscription. StartTimer/StopTimer just switch this interval on and off;
// each interval fires a payload-free "tick" event on `document` (where Flame's
// custom-event subscriptions listen), which `subscriptions` in Timer.purs
// turns into a `Ticked 100.0`.

let intervalId = null;

export const startTickingImpl = () => {
  if (intervalId !== null) clearInterval(intervalId);
  intervalId = setInterval(() => {
    document.dispatchEvent(new CustomEvent("tick"));
  }, 100);
};

export const stopTickingImpl = () => {
  if (intervalId !== null) {
    clearInterval(intervalId);
    intervalId = null;
  }
};
