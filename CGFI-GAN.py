from tensorflow.keras.layers import Dense, Dropout, Input
from tensorflow.keras.models import Model,Sequential
from tensorflow.keras.layers import LeakyReLU
from tensorflow.keras.optimizers import Adam
from tqdm import tqdm
import numpy as np


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
	return (np.array(X), np.array(y))
def load_candi(filename):
	X = list()
	with open(filename, 'r') as f:
		for l in f:
			l = l.strip().replace("'",'').split('\t')
			l1 = [float(i) for i in l[0:len(l)+1]]

			X.append(l1)

	return np.array(X)

filename = ''
X_train, y_train = load_dataset(filename, n=7)
X_test = load_candi('')
print(X_train.shape, y_train.shape)


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
def create_fea(generator, epoch,batch):
    noise = np.random.normal(0, 1, (3053, 366))

    generated_fea = generator.predict(noise)
    np.savetxt('Synthetic_%d_%d_batch.csv' %(epoch, batch),generated_fea,delimiter=',')



def train_GAN(epochs=5, batch_size=64):
    # Loading the data
    X_train, y_train = load_dataset(filename,7)

    # Creating GAN
    generator = build_generator()
    discriminator = build_discriminator()
    GAN = build_GAN(discriminator, generator)

    for i in range(1, epochs + 1):
        print("Epoch %d" % i)

        for _ in tqdm(range(batch_size)):
            noise = np.random.normal(0, 1, (batch_size, 366))
            syn_samples = generator.predict(noise)

            real_samples=X_train[np.random.randint(0, X_train.shape[0], batch_size)]

            label_syn = np.zeros(batch_size)
            label_real = np.ones(batch_size)

            # Concatenate fake and real images
            X = np.concatenate([syn_samples, real_samples])
            y = np.concatenate([label_syn, label_real])

            # Train the discriminator
            discriminator.trainable = True
            discriminator.train_on_batch(X, y)

            # Train the generator/chained GAN model (with frozen weights in discriminator)
            discriminator.trainable = False
            GAN.train_on_batch(noise, label_real)

        if i == epochs :
            create_fea(generator, i,batch_size)



train_GAN()