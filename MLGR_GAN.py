from tensorflow.keras.datasets import mnist
from tensorflow.keras.layers import Dense, Dropout, Input
from tensorflow.keras.models import Model,Sequential
from tensorflow.keras.layers import LeakyReLU
from tensorflow.keras.optimizers import Adam
from tqdm import tqdm
import numpy as np
import matplotlib
import matplotlib.pyplot as plt
#from google.colab import drive


def load_dataset(filename,n):
	X = list()
	y = list()
	with open(filename, 'r') as f:
		for l in f:
			l = l.strip().replace("'",'').split('\t')
			l1 = [float(i) for i in l[0:len(l)-n]]
			l2 = [int(i) for i in l[len(l)-n:len(l)+1]]
			X.append(l1)
			y.append(l2)
	return (np.array(X),np.array(y))

filename = './dataset/traindata.txt'
X_train, y_train = load_dataset(filename,7)
loc=np.mean(X_train)
scale=np.std(X_train)
#print(loc,scale)
print(X_train.shape)


def build_generator():
    model = Sequential()

    model.add(Dense(units=128, input_dim=366))
    model.add(LeakyReLU(alpha=0.2))

    model.add(Dense(units=256))
    model.add(LeakyReLU(alpha=0.2))

    model.add(Dense(units=512))
    model.add(LeakyReLU(alpha=0.2))

    model.add(Dense(units=366, activation='tanh'))

    model.compile(loss='binary_crossentropy', optimizer=Adam(0.0002, 0.5))
    return model


generator = build_generator()
generator.summary()


def build_discriminator():
    model = Sequential()

    model.add(Dense(units=1024, input_dim=366))
    model.add(LeakyReLU(alpha=0.2))
    model.add(Dropout(0.3))

    model.add(Dense(units=512))
    model.add(LeakyReLU(alpha=0.2))
    model.add(Dropout(0.3))

    model.add(Dense(units=256))
    model.add(LeakyReLU(alpha=0.2))
    model.add(Dropout(0.3))

    model.add(Dense(units=1, activation='sigmoid'))

    model.compile(loss='binary_crossentropy', optimizer=Adam(0.0002, 0.5))
    return model


discriminator = build_discriminator()
discriminator.summary()
def build_GAN(discriminator, generator):
    discriminator.trainable=False
    GAN_input = Input(shape=(366,))
    x = generator(GAN_input)
    GAN_output= discriminator(x)
    GAN = Model(inputs=GAN_input, outputs=GAN_output)
    GAN.compile(loss='binary_crossentropy', optimizer=Adam(0.0002, 0.5))
    return GAN

GAN = build_GAN(discriminator, generator)
GAN.summary()


def draw_images(generator, epoch,batch):

    noise = np.random.normal(0, 1, (3053, 366))
    generated_images = generator.predict(noise)
    np.savetxt('./dataset/New_train_%d_%d.txt' %(epoch, batch),generated_images,delimiter=',')




def train_GAN(epochs=5, batch_size=64):
    # Loading the data
    X_train, y_train = load_dataset(filename, n=7)

    # Creating GAN
    generator = build_generator()
    discriminator = build_discriminator()
    GAN = build_GAN(discriminator, generator)

    for i in range(1, epochs + 1):
        print("Epoch %d" % i)

        for _ in tqdm(range(batch_size)):

            noise = np.random.normal(0,1, (batch_size, 366))
            fake_images = generator.predict(noise)

            real_images = X_train[np.random.randint(0, X_train.shape[0], batch_size)]

            label_fake = np.zeros(batch_size)
            label_real = np.ones(batch_size)

            X = np.concatenate([fake_images, real_images])
            y = np.concatenate([label_fake, label_real])

            discriminator.trainable = True
            discriminator.train_on_batch(X, y)

            discriminator.trainable = False
            GAN.train_on_batch(noise, label_real)

        if i == epochs :
            draw_images(generator, i,batch_size)

train_GAN(epochs=400, batch_size=16)