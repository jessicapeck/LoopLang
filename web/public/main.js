

const compileButton = document.getElementById('compile-button');
compileButton.addEventListener('click', compile);

async function compile() {
    // get code from the input area
    const editor = document.getElementById('editor');
    const code = editor.value;

    // make POST request to the server with the code
    
    // await response from server

    // handle errors and warnings if there are any

    // display the compiled code in the output area
}