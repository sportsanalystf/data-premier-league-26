import numpy as np, glob, os
d={os.path.basename(f)[:-4]: np.fromfile(f, dtype=np.float32).reshape(510,1,256) for f in glob.glob('voices/*.bin')}
np.savez('voices.npz', **d); os.rename('voices.npz','voices-v1.0.bin'); print(len(d))
