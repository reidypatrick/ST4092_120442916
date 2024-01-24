import pandas as pd
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import StandardScaler
from tensorflow import keras
from keras import layers
from keras.optimizers import Adam
import openml
import graphviz


# Load Data
data = openml.datasets.get_dataset(41214, download_all_files=True)
df, *_ = data.get_data()

# Select numeric columns and scale them using Min-Max scaling
numeric_cols = df.select_dtypes(include='number').columns
df_scaled = df.copy()
df_scaled[numeric_cols] = MinMaxScaler().fit_transform(df[numeric_cols])

# Extract ClaimNb as integer
df_scaled['ClaimNb'] = df_scaled['ClaimNb'].astype(int)

# Convert VehGas to integer
df_scaled['VehGas'] = (df_scaled['VehGas'] == 'Diesel').astype(int)

# Select relevant columns and join with scaled numeric columns
df = pd.merge(df_scaled[['IDpol', 'ClaimNb', 'VehGas']], df_scaled[numeric_cols], left_index=True, right_index=True)

# Initialize control variables
n_epochs = 20
n_neurons = [16, 32, 64]

# Model Building
# Create Recipe
print(df)

X = df.drop(columns=['ClaimNb_x'])
y = df['ClaimNb_x']

# Split Data
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=42)

# Fit Model
# Construct model
model1 = keras.Sequential()
model1.add(layers.Dense(units=X_train.shape[1], activation='relu', input_shape=(X_train.shape[1],)))
model1.add(layers.Dense(units=32, activation='relu'))
model1.add(layers.Dense(units=32, activation='relu'))
model1.add(layers.Dense(units=1, activation='exponential'))

model1.compile(
    loss='poisson',
    optimizer=Adam(learning_rate=0.001),
    metrics=['mean_squared_error']
)

history = model1.fit(
    x=X_train.values,
    y=y_train.values,
    epochs=n_epochs,
    batch_size=256,
    validation_split=0.2
)

plot_model(model1)
