document.addEventListener('DOMContentLoaded', function () {
    const reloadWs = new WebSocket('ws://localhost:4321/ws-reload');

    function showNotification(message) {

        const notification = document.createElement('div');
        {
            const style = notification.style;
            style.position = 'fixed';
            style.top = '15px';
            style.left = '50%';
            style.backgroundColor = 'rgba(0, 0, 0, 0.7)';
            style.color = '#fff';
            style.padding = '6px 12px';
            style.borderRadius = '2px';
            style.zIndex = '1000';
        }
        notification.innerText = message;
        document.body.appendChild(notification);
    }

    function waitTilReady(n) {
        if (n > 20) {
            showNotification("Reload manually: server failed too many times");
            return;
        }
        fetch('/', { method: 'HEAD' })
            .then(response => {
                if (!response.ok)
                    throw new Error("Server seems to be broken...");

                console.log('Server is back online, reloading page...');
                location.reload();
            })
            .catch((err) => {
                console.log("Server not ready yet, will retry...");
                setTimeout(() => waitTilReady(n + 1), 100 + (n * 100));
            });
    }

    reloadWs.onclose = function () {
        console.log('Reload WebSocket closed, reloading page...');
        showNotification('Reloading in a sec...');
        waitTilReady(0);
    };
});