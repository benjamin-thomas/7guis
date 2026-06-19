let intervalId = null;

export function start_timer(onTick) {
  stop_timer();
  intervalId = setInterval(() => {
    onTick();
  }, 100);
}

export function stop_timer() {
  if (intervalId !== null) {
    clearInterval(intervalId);
    intervalId = null;
  }
}

export function notify_server(elapsedMs, onSuccess, onError) {
  setTimeout(() => {
    if (globalThis.__GLEAM_TIMER_FAIL_SERVER__) {
      onError("Network error");
    } else {
      onSuccess();
    }
  }, 500);
}

export function delay(milliseconds, callback) {
  setTimeout(callback, milliseconds);
}

export function console_log(message) {
  console.log(message);
}
