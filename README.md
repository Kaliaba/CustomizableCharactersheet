# Install and Run the Project

## Haskell Setup

1. If you haven't already, [install Stack](https://haskell-lang.org/get-started)
	* On POSIX systems, this is usually `curl -sSL https://get.haskellstack.org/ | sh`
2. Install the `yesod` command line tool: `stack install yesod-bin --install-ghc`
3. Build libraries: `stack build`

OR

Terminal 

```
$ stack ghci CustomizableCharactersheet:lib --no-load --work-dir .stack-work-devel

    :l app/DevelMain.hs
    
    DevelMain.update
```
(You can find these instructions in "CustomizableCharactersheet/app/DevelMain.hs")

If you have trouble, refer to the [Yesod Quickstart guide](https://www.yesodweb.com/page/quickstart) for additional detail.

## Development

Start a development server with:

```
stack exec -- yesod devel
```

As your code changes, your site will be automatically recompiled and redeployed to localhost.

## Tests

```
stack test --flag CustomizableCharactersheet:library-only --flag CustomizableCharactersheet:dev
```

(Because `yesod devel` passes the `library-only` and `dev` flags, matching those flags means you don't need to recompile between tests and development, and it disables optimization to speed up your test compile times).

## Expressions to evaluate in GHCI

* In the field page provide expression 5+2*8/(9+2)
* You will be able to see the expression result on sheet page that is 6.454545454545455

## Documentation

* Read the [Yesod Book](https://www.yesodweb.com/book) online for free
* Check [Stackage](http://stackage.org/) for documentation on the packages in your LTS Haskell version, or [search it using Hoogle](https://www.stackage.org/lts/hoogle?q=). Tip: Your LTS version is in your `stack.yaml` file.
* For local documentation, use:
	* `stack haddock --open` to generate Haddock documentation for your dependencies, and open that documentation in a browser
	* `stack hoogle <function, module or type signature>` to generate a Hoogle database and search for your query
* The [Yesod cookbook](https://github.com/yesodweb/yesod-cookbook) has sample code for various needs

## Getting Help

* Ask questions on [Stack Overflow, using the Yesod or Haskell tags](https://stackoverflow.com/questions/tagged/yesod+haskell)
* Ask the [Yesod Google Group](https://groups.google.com/forum/#!forum/yesodweb)
* There are several chatrooms you can ask for help:
	* For IRC, try Freenode#yesod and Freenode#haskell
	* [Functional Programming Slack](https://fpchat-invite.herokuapp.com/), in the #haskell, #haskell-beginners, or #yesod channels.

# Project Structure
This is how the source code of this project is organized:

```
|-- app 	-> to run the local web server
|-- config 	-> Contains any application configuration like routes (routes)
|-- src 	-> Source code containing all .hs files controlling the application
	|--Handler 	-> Each Handler file is handling one page of the website (Example: Roll.hs handels /roll)
|-- templates 	-> Each site of the wepage hase a template describing its HTML (Example: /roll GET -> roll-form.hamlet and /roll POST -> roll-result.hamlet)
|-- CustomizableCharactersheet.cabal -> package management via stack
```

The main work in this project has happend in /src, /config and, /templates
The Original template of Yesod includes all of the source code mentioned in project structure.

# Yesod Original Template resource
 
You can find the all the souce code files of Yesod original template from GITHUB Commits made from the beggining.


	    ```
