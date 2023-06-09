const { Speller } = Elm;

function getHardMode() {
    const data = localStorage.getItem("hardMode");
    return data === "true";
}

function getOffset(searchParams) {
    const offset = searchParams.get("offset");

    if (offset) {
        return JSON.parse(offset);
    }

    return 0;
}

const { searchParams } = new URL(window.location);
const app = Speller.init({
    node: document.getElementById('elm'),
    flags: { hardMode: getHardMode(), offset: getOffset(searchParams) }
});

function getAlreadyPlayed(key, searchParams) {
    const alreadyPlayed = searchParams.get("alreadyPlayed");
    if (alreadyPlayed === "true" || alreadyPlayed === "false") {
        return { alreadyPlayed: JSON.parse(alreadyPlayed) }
    }

    const item = localStorage.getItem(key);

    if (item) {
        const data = JSON.parse(item);
        return { ...data, alreadyPlayed: true }
    }

    return { alreadyPlayed: false }
}

app.ports.checkAlreadyPlayed.subscribe(function(message) {
    const alreadyPlayedMessage = getAlreadyPlayed(message, searchParams);
    app.ports.messageReceiver.send(JSON.stringify(alreadyPlayedMessage))
});

app.ports.setAlreadyPlayed.subscribe(function(message) {
    const key = message.key;
    const solvedWords = message.solvedWords;
    const data = JSON.stringify({ score: message.score, solvedWords: solvedWords });

    localStorage.setItem(key, data);
});

app.ports.setHardMode.subscribe(function(value) {
    localStorage.setItem("hardMode", value);
});
