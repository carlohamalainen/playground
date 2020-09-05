import atexit
import threading, queue, time
import numpy as np
from multiprocessing import Pool
import seaborn as sns
import matplotlib.pyplot as plt
from datetime import datetime
import pandas as pd

class Task(threading.Thread):

    def __init__(self, name, mean, lock=None):
        threading.Thread.__init__(self)

        self.name       = name
        self.mean       = mean
        self.lock       = lock
        self.slowdown   = 1.0
        self.speedup    = 1.0

        self.debug      = False

        self.input_queue  = queue.Queue()
        self.output_queue = queue.Queue()

        self.rng = np.random.default_rng()

    def log(self, msg):
        if self.debug:
            print(msg)

    def run(self):
        x = self.input_queue.get()

        now = datetime.now().strftime('%H:%M:%S.%f')
        self.log(f'{now} {self.name} starting')

        duration = self.mean # self.rng.poisson(self.mean)
        self.log(f'Task {self.name} working for {duration}ms')

        if self.lock is not None:
            t0 = datetime.now()
            self.log(f'Task {self.name} acquiring the global lock...')
            self.lock.acquire()
            t1 = datetime.now()
            self.log(f'Task {self.name} has the global lock.')
            w = (t1 - t0).total_seconds()
            self.log(f'{self.name} waited {w} seconds for the lock')

        # The real work happens here. We run for 'duration'
        # and optionally have a slowdown factor (units are msecs).
        #
        # slowdown = 1 + s
        #
        # then
        #
        # slowdown * (duration/1000) = duration/1000   +   s*(duration/1000)
        #
        assert self.slowdown >= 1
        assert self.speedup  <= 1

        # We are either doing a virtual slowdown or speedup, not both.
        assert not (self.slowdown != 1.0 and self.speedup != 1.0)

        # The real work that the task does.
        d = self.speedup*duration/1000.0
        self.log(f'task {self.name} running for {d} seconds due to speedup {self.speedup}')
        time.sleep(d)

        # Slowdown - in a real system we would measure the actual duration.
        time.sleep((self.slowdown - 1.0)*duration/1000.0)

        if self.lock is not None:
            self.log(f'Task {self.name} releasing the global lock.')
            self.lock.release()

        self.log(f'Task {self.name} finished')
        self.input_queue.task_done()
        self.output_queue.put(x)

        now = datetime.now().strftime('%H:%M:%S.%f')
        self.log(f'{now} {self.name} done')


# https://www.youtube.com/watch?v=r-TLSBdHe1A

# Ogle image search workflow.


COMPRESSION = 'COMPRESSION' # Blue box with arrows pointing at each other.
FEATURES    = 'FEATURES'    # Red box with dotted list.
SAVE        = 'SAVE'        # Green box with floppy disk icon.
SEARCH      = 'SEARCH'      # Yellow box with magnifying glass.
SEND        = 'SEND'        # Orange box with paper plane.

TASKS = [ COMPRESSION, FEATURES, SAVE, SEARCH, SEND ]

def simulate(means_ms, slowdowns=None, speedups=None, debug=False):
    global_lock = threading.Lock()

    tasks = {}

    tasks[COMPRESSION]  = Task(COMPRESSION, means_ms[COMPRESSION], lock=None)
    tasks[FEATURES]     = Task(FEATURES,    means_ms[FEATURES],    lock=None)
    tasks[SEARCH]       = Task(SEARCH,      means_ms[SEARCH],      lock=global_lock)
    tasks[SEND]         = Task(SEND,        means_ms[SEND],        lock=None)
    tasks[SAVE]         = Task(SAVE,        means_ms[SAVE],        lock=global_lock)

    tasks[SAVE].input_queue   = tasks[COMPRESSION].output_queue
    tasks[SEARCH].input_queue = tasks[FEATURES].output_queue
    tasks[SEND].input_queue   = tasks[SEARCH].output_queue

    for (n, t) in tasks.items():
        t.debug = debug

    assert not (slowdowns is not None and speedups is not None)

    if slowdowns is not None:
        for (t, s) in slowdowns.items():
            tasks[t].slowdown = s

    if speedups is not None:
        for (t, s) in speedups.items():
            tasks[t].speedup = s

    x = 'grumpy_cat_photo'

    tasks[COMPRESSION].input_queue.put(x)
    tasks[FEATURES].input_queue.put(x)

    t0 = time.time()

    tasks[COMPRESSION].start()
    tasks[FEATURES].start()
    tasks[SEARCH].start()
    tasks[SEND].start()
    tasks[SAVE].start()

    tasks[SEND].output_queue.get()
    tasks[SAVE].output_queue.get()

    t1 = time.time()

    elapsed_ms = 1000*(t1 - t0)

    return elapsed_ms

def default_runtimes():
    alpha = 1000

    return { COMPRESSION:   1.2*alpha,
             SAVE:          alpha,
             FEATURES:      alpha,
             SEARCH:        alpha,
             SEND:          alpha,
           }

def run_virtual(tweak):

    task, virtual_speedup = tweak

    means_ms = default_runtimes()

    # First simulate with everything slowed down.
    slowdowns = { t:1.0/virtual_speedup for t in TASKS }

    old = simulate(means_ms, slowdowns=slowdowns)
    assert old > 1e-10

    # Then simulate with virtually fast compression.
    slowdowns[task] = 1.0

    new = simulate(means_ms, slowdowns)

    return old/new

def run_real(tweak):
    """
    Same as run_virtual() but using actual speedup.
    """

    task, speedup = tweak

    means_ms = default_runtimes()

    # First simulate normally.
    old = simulate(means_ms)
    assert old > 1e-10

    # Then simulate with actual speedup.
    new = simulate(means_ms, speedups={task: speedup})

    return old/new

def experiment(speedups, task_name):
    tweaks   = [(task_name, s) for s in speedups]

    p = Pool(len(tweaks))

    virtual_results = p.map(run_virtual, tweaks)
    real_results    = p.map(run_real,    tweaks)

    atexit.register(p.close) # https://github.com/dask/dask/issues/5806

    assert np.max(np.abs(np.array(virtual_results) - np.array(real_results))) < 0.01

    return (real_results, virtual_results)

if True:
    speedups = list(np.linspace(0.3, 0.95)) 

    (compression_real_results, compression_virtual_results) = experiment(speedups, COMPRESSION)
    (save_real_results,        save_virtual_results)        = experiment(speedups, FEATURES)

    df = pd.DataFrame({'task speedup': speedups, 'compression': compression_virtual_results, 'save': save_virtual_results,})
    df = pd.melt(df, 'task speedup')
    df = df.rename(columns={'variable': 'task', 'value': 'overall speedup'})

plt.figure()
plot = sns.lineplot('task speedup', 'overall speedup', hue='task', data=df, marker='o', palette=['blue', 'green'])
# plot.set_title('')
fig = plot.get_figure()
fig.savefig('speedups.svg')
