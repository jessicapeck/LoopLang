async function compile() {
    // debug test
    console.log('Compiling code...');

    // get code from the input area
    const editor = document.getElementById('editor');
    const codeInput = editor.value;

    const outputArea = document.getElementById('output-area');

    const response = await fetch('/compile', {
        method: 'POST',
        headers: {
            'Content-Type': 'application/json'
        },
        body: JSON.stringify({ code: codeInput })
    });

    const data = await response.json();

    if (data.error) {
        outputArea.textContent = data.error;
    } else {
        outputArea.textContent = data.result;
    }

}

document.addEventListener('DOMContentLoaded', () => {
    const compileButton = document.getElementById('compile-button');
    compileButton.addEventListener('click', compile);
});