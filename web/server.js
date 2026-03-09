const express = require('express');
const app = express();
const port = 3000;

const { execFile } = require('node:child_process');
const path = require('node:path');
const compilerPath = path.join(__dirname, 'loopycompiler_web');

app.use(express.json());
app.use(express.static('public'));

app.set('view engine', 'ejs');


// render pages
app.get('/', (req, res) => {
  res.render('index', { page: 'compiler' })
})

app.get('/docs', (req, res) => {
  res.render('docs', { page: 'docs' })
})

app.get('/about', (req, res) => {
  res.render('about', { page: 'about' })
})


// compile code
app.post('/compile', (req, res) => {
  const code = req.body.code;

  const child = execFile(compilerPath, [code], (error, stdout, stderr) => {
    // handle errors with running the executable
    if (error) {
      throw error;
    }

    // parse the JSON string
    const result = JSON.parse(stdout);


    // return the JSON to the client
    return res.json(result);
  })
})

// listen on port
app.listen(port, () => {
  console.log(`Server is running on port ${port}`)
})
