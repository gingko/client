const {Elm} = require('./MigrationWorker.js')

const app = Elm.MigrationWorker.init();

console.log('Sending 5')
app.ports.input.send(5)

app.ports.output.subscribe(function(data) {
  console.log(data);
});