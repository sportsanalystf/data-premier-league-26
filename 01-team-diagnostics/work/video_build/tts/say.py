import sys, wave, numpy as np
from kokoro_onnx import Kokoro
k = Kokoro("kokoro-q8.onnx", "voices-v1.0.bin")
s, sr = k.create(sys.argv[2], voice=sys.argv[3] if len(sys.argv)>3 else "am_michael", speed=1.0, lang="en-us")
w=wave.open(sys.argv[1],"wb"); w.setnchannels(1); w.setsampwidth(2); w.setframerate(sr); w.writeframes((np.clip(s,-1,1)*32767).astype(np.int16).tobytes()); w.close(); print(sr, len(s)/sr)
