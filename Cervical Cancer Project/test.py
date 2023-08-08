from flask import Flask
import pickle
app = Flask(__name__)

model = pickle.load(open('model.pkl','rb'))

@app.route("/")

def input_factors():
    #return "food"
    return render_template('inputdata.html')


if __name__ == '__main__':
    app.run()
