import nipype.pipeline.engine as pe
from nipype.interfaces.utility import Function


def load_pklz(f):
    import pickle
    import gzip
    return pickle.load(gzip.open(f))

def Hello():
   import os
   from nipype import logging
   iflogger = logging.getLogger('interface')
   message = "Hello "
   file_name =  'hello.txt'
   iflogger.info(message)
   with open(file_name, 'w') as fp:
       fp.write(message)
   return os.path.abspath(file_name)

def World(in_file, some_parameter):
   from nipype import logging
   iflogger = logging.getLogger('interface')
   message = "World! " + 'some_parameter: ' + str(some_parameter)
   iflogger.info(message)
   with open(in_file, 'a') as fp:
       fp.write(message)

hello = pe.Node(name='hello',
               interface=Function(input_names=[],
                                  output_names=['out_file'],
                                  function=Hello))
world = pe.Node(name='world',
               interface=Function(input_names=['in_file', 'some_parameter'],
                                  output_names=[],
                                  function=World))

world.iterables = ('some_parameter', [1.0/3.0, 1.0/3.0 + 10e-15, 2.0])

pipeline = pe.Workflow(name='nipype_demo')
pipeline.connect([(hello, world, [('out_file', 'in_file')])])
pipeline.run()
pipeline.write_graph()

