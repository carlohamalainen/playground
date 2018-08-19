import pandas               as pd
import matplotlib.pyplot    as plt
import numpy as             np

df = pd.read_csv('runs.csv')

one_mb = 1024*1024

plt.plot(df['bytes']/one_mb, df['string']/one_mb, label='String')
plt.plot(df['bytes']/one_mb, df['bytestring']/one_mb, label='ByteString')
plt.plot(df['bytes']/one_mb, df['bytestringchar8']/one_mb, label='ByteString Char8')
plt.plot(df['bytes']/one_mb, df['lazybytestring']/one_mb, label='Lazy ByteString')
plt.plot(df['bytes']/one_mb, df['lazybytestringchar8']/one_mb, label='Lazy ByteString Char8')

plt.xlabel('Input file (MBytes)')
plt.ylabel('Total memory allocated (MBytes)')
plt.title('readFile memory usage')
plt.legend()
plt.tight_layout() # otherwise y-axis label is chopped off
plt.savefig('readfile_memory_usage.png')
plt.savefig('readfile_memory_usage.svg')
