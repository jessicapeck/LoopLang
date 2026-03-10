document.addEventListener('DOMContentLoaded', () => {
    // editor (input textarea)
    editor = document.getElementById('editor');
    editor.addEventListener('keydown', function(e) {
        if (e.key === 'Tab') {
            e.preventDefault();

            const start = this.selectionStart;
            const end = this.selectionEnd;

            this.value = this.value.substring(0, start) + '\t' + this.value.substring(end);

            this.selectionStart = this.selectionEnd = start + 1;
        }
    });

    // compile button
    compileButton = document.getElementById('compile-button');
    compileButton.addEventListener('click', compile);

    // terminal for displaying error / warning / success messages
    terminal = document.getElementById('terminal');

    // output area for displaying compiled results
    outputArea = document.getElementById('output-area');
});


function clearTerminal() {
    // clear the content of the terminal
    terminal.innerHTML = '';
}

function addTerminalLine(content) {
    // add the new line
    const newLine = document.createElement('pre');
    newLine.innerHTML = `<code>${content}</code>`;
    terminal.appendChild(newLine);
}

function displayErrors(errorMessage) {
    // clear the output area
    outputArea.textContent = '';

    // display the error message in the terminal
    const formattedMessage = `<span class="text-error font-bold">ERROR</span> ${errorMessage}`;
    addTerminalLine(formattedMessage);
}

function displayWarnings(warnings) {
    // add all warning messages to the terminal
    warnings.forEach((warning) => {
        const formattedMessage = `<span class="text-warning font-bold">WARNING</span> ${warning}`;
        addTerminalLine(formattedMessage);
    });
}

function displaySuccessMessage() {
    const formattedMessage = `<span class="text-success font-bold">Compilation successful!</span>`;
    addTerminalLine(formattedMessage);
}

async function compile() {
    const codeInput = editor.value;

    const response = await fetch('/compile', {
        method: 'POST',
        headers: {
            'Content-Type': 'application/json'
        },
        body: JSON.stringify({ code: codeInput })
    });

    const data = await response.json();

    // clear the terminal
    clearTerminal();

    // display results
    if (data.error) {
        displayErrors(data.error);
    } else {
        displayWarnings(data.warnings);
        outputArea.textContent = data.result;
        displaySuccessMessage();
    }
}
