from flask import Flask
import pickle
app = Flask(__name__)

#model = pickle.load(open('model.pkl','rb'))

@app.route("/")

def input_factors():
    #return "food"
    return 'Hello'


if __name__ == '__main__':
    app.run()
