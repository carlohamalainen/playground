<p> Suppose we have two datatypes, <code>OptBool</code> and
<code>OptFile</code> for storing boolean and file path options. Perhaps
this might be for a program that provides an interface to legacy command
line applications.

> {-# LANGUAGE MultiParamTypeClasses  #-}
> {-# LANGUAGE TypeSynonymInstances   #-}
> {-# LANGUAGE FlexibleInstances      #-}
> {-# LANGUAGE FunctionalDependencies #-}
>
> module Fundep where

> data OptBool = OptBool { optBoolDesc   :: String
>                        , optBoolValue  :: Bool
>                        } deriving Show

> data OptFile = OptFile { optFileDesc :: String
>                        , optFileValue :: FilePath
>                        } deriving Show

<p> We'd like to be able to set the value of an option without having to
specify the record name, so instead of </p>

< opt { optBoolValue = True }

<p> we want to write </p>

< setValue opt True

<p> As a first attempt we make a type class <code>Option</code>:, where
we have enabled <code>MultiParamTypeClasses</code> because the type signature
for <code>setValue</code> has to refer to the option, of type <code>a</code>,
and the value of type <code>b</code>. We also enable
<code>TypeSynonymInstances</code> and
<code>FlexibleInstances</code>
since <code>FilePath</code> is a type synonym. </p>

< class Option a b where
<     setDesc   :: a -> String -> a
<     setValue  :: a -> b -> a

<p> Instance declarations: </p>

> instance Option OptBool Bool where
>     setDesc opt d  = opt { optBoolDesc  = d }
>     setValue opt b = opt { optBoolValue = b }
>
> instance Option OptFile FilePath where
>     setDesc opt d  = opt { optFileDesc  = d }
>     setValue opt f = opt { optFileValue = f }

<p> All seems well but the following code doesn't compile: </p>

< opt1' = setDesc (OptBool "bool" True) "boolean option"

<p> with the error message </p>

<pre>
    No instance for (Option OptBool b1) arising from a use of `setDesc'
    The type variable `b1' is ambiguous
    Possible fix: add a type signature that fixes these type variable(s)
    Note: there is a potential instance available:
      instance Option OptBool Bool -- Defined at Fundeps.lhs:40:12
    Possible fix: add an instance declaration for (Option OptBool b1)
    In the expression: setDesc (OptBool "bool" True) "boolean option"
    In an equation for opt1':
        opt1' = setDesc (OptBool "bool" True) "boolean option"
</pre>

<p> The problem is that both <code>a</code> and <code>b</code> in the
class declaration are free variables, but really this is not the case.
The trick is to enable the <code>FunctionalDependencies</code>
language extension, and then specify that the type <code>a</code>
in the class declaration for <code>Option</code> implies the type
<code>b</code>. This makes sense if you think about the type of
<code>setValue</code>. Once we know the type of the first parameter,
we then know the type of the value field (assuming that the instance
declaraion uses <code>OptBoolValue</code> or <code>optFileValue</code>
or whatever). </p>

> class Option a b | a -> b where
>     setDesc   :: a -> String -> a
>     setValue  :: a -> b -> a

<p> Now this is ok: </p>

> opt1' :: OptBool
> opt1' = setDesc (OptBool "bool" True) "boolean option"



<p> As a final note, writing the implication <code>b -> a</code> as below

< class Option a b | b -> a where
<     setDesc   :: a -> String -> a
<     setValue  :: a -> b -> a

<p> restricts us unnecessarily. If we had another type with a boolean value field, </p>

> data OptBool' = OptBool' { optBoolDesc'  :: String
>                          , optBoolValue' :: Bool
>                          } deriving Show

> instance Option OptBool' Bool where
>     setDesc opt d  = opt { optBoolDesc'  = d }
>     setValue opt b = opt { optBoolValue' = b }

<p> then this code would not compile </p>

> opt1'' :: OptBool'
> opt1'' = setDesc (OptBool' "bool" True) "boolean option"

<p> due to </p>

<pre>
    Functional dependencies conflict between instance declarations:
      instance Option OptBool Bool -- Defined at Fundeps.lhs:41:12
      instance Option OptBool' Bool -- Defined at Fundeps.lhs:91:12
</pre>

<p> In contrast the implication <code>a -> b</code> means that, for example,
the type <code>OptBool</code> implies the type <code>Bool</code>. </p>

