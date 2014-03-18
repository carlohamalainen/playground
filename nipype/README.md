Nipype encodes parameters (see the ```iterables``` property in helloiterables.py, e.g.

    140318-16:15:43,549 workflow INFO:
         ['check', 'execution', 'logging']
    140318-16:15:43,554 workflow INFO:
         Running serially.
    140318-16:15:43,554 workflow INFO:
         Executing node hello in dir: /tmp/tmpOwrR7O/nipype_demo/hello
    140318-16:15:43,556 interface INFO:
         Hello
    140318-16:15:43,561 workflow INFO:
         Executing node world.aI.a1 in dir: /tmp/tmp6DwvV9/nipype_demo/_some_parameter_0.333333333333/world
    140318-16:15:43,563 interface INFO:
         World! some_parameter: 0.333333333333
    140318-16:15:43,566 workflow INFO:
         Executing node world.aI.a2 in dir: /tmp/tmpBSPtB1/nipype_demo/_some_parameter_2.0/world
    140318-16:15:43,568 interface INFO:
         World! some_parameter: 2.0
    140318-16:15:43,571 workflow INFO:
         Executing node world.aI.a0 in dir: /tmp/tmpp7ufX2/nipype_demo/_some_parameter_0.333333333333/world
    140318-16:15:43,573 interface INFO:
         World! some_parameter: 0.333333333333
    140318-16:15:43,606 workflow INFO:
         Converting dotfile: /home/carlo/work/github/playground/nipype/graph.dot to png format

Each node runs in its own directory, and we can look at the ```_inputs.pklz``` file to get the inputs. Other files give provenance data, e.g.

    _0x263b54f283d63420537940f0355c30d8.json
    _inputs.pklz
    _node.pklz
    _report
    result_world.pklz

To check the inputs we have to manually go through the output directories:

    for d in ['/tmp/tmp6DwvV9/nipype_demo/_some_parameter_0.333333333333/world/_inputs.pklz',
              '/tmp/tmpBSPtB1/nipype_demo/_some_parameter_2.0/world/_inputs.pklz',
              '/tmp/tmpp7ufX2/nipype_demo/_some_parameter_0.333333333333/world/_inputs.pklz']:
        print '%.15g ==> %s' % (load_pklz(d)['some_parameter'], d,)

Output:

    0.333333333333343 ==> /tmp/tmp6DwvV9/nipype_demo/_some_parameter_0.333333333333/world/_inputs.pklz
    2                 ==> /tmp/tmpBSPtB1/nipype_demo/_some_parameter_2.0/world/_inputs.pklz
    0.333333333333333 ==> /tmp/tmpp7ufX2/nipype_demo/_some_parameter_0.333333333333/world/_inputs.pklz

As a result of ```world.iterables = ('some_parameter', [1.0/3.0, 1.0/3.0 + 10e-15, 2.0])``` we get three branches on the workflow graph:

![graph_detailed.dot.png](https://github.com/carlohamalainen/playground/raw/master/nipype/graph_detailed.dot.png)

In comparison, [Laboratin]](https://github.com/lucasdicioccio/laborantin-hs) has a ```find``` command. Example from its README:

    Then find where experiments results are located with: `./my-experiment find`.

    ```
    results/ping/2ead949a-ed36-4523-9a9c-7c7e2c22a1b2 ping (Success) {"packet-size":{"val":1500.0,"type":"num"},"destination":{"val":"example.com","type":"string"}}
    results/ping/81e44c78-4fd8-4ab9-8f97-5494dac646a2 ping (Success) {"packet-size":{"val":1500.0,"type":"num"},"destination":{"val":"probecraft.net","type":"string"}}
    results/ping/866b63e8-d407-442c-bc33-f1fb4e96c2a8 ping (Success) {"packet-size":{"val":50.0,"type":"num"},"destination":{"val":"example.com","type":"string"}}
    results/ping/a34826bc-5160-4e12-95cc-12fb5c02fc7b ping (Success) {"packet-size":{"val":50.0,"type":"num"},"destination":{"val":"probecraft.net","type":"string"}}
