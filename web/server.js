const express = require('express');
const app = express();
const port = 3000;

const { execFile } = require('node:child_process');

app.use(express.json());
app.use(express.static('public'));

app.set('view engine', 'ejs');


app.get('/', (req, res) => {
  res.render('index', { page: 'compiler' })
})

app.get('/docs', (req, res) => {
  res.render('docs', { page: 'docs' })
})

app.get('/about', (req, res) => {
  res.render('about', { page: 'about' })
})


app.listen(port, () => {
  console.log(`Server is running on port ${port}`)
})
