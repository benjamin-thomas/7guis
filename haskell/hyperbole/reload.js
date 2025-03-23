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
        fetch('/', { method: 'HEAD' })
            .then(response => {
                if (!response.ok)
                    throw new Error("Server seems to be broken...");

                console.log('Server is back online, reloading page...');
                location.reload();
            })
            .catch((_) => {
                console.log("Server not ready yet, will retry...", n);
                const ms = (n < 20) ? 50 : 1000;
                setTimeout(() => waitTilReady(n + 1), ms);
            });
    }

    reloadWs.onclose = function () {
        console.log('Reload WebSocket closed, reloading page...');
        showNotification('Reloading in a sec...');
        waitTilReady(0);
    };
});