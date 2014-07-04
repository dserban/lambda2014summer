import flask
import hashlib

app = flask.Flask(__name__)
app.config['DEBUG'] = True

@app.route('/sha512sum/<int:integer_id>')
def sha512sum(integer_id):
    return hashlib.sha512(str(integer_id)).hexdigest()

if __name__ == '__main__':
    app.run(host='0.0.0.0')

