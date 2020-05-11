# Customizable Character Sheet
With the customizable character sheet the user will be able to create their own character sheet independent to the Table Top Role Playing (TTRPG) system they use.

The application will offer the user the ability to create fields that contain either numbers, text or the result of a calculation. 
These fields can be nested into each other to create a parent-child relationship to enable the representation of abilities having a description of a numerical value or numbers being recalculated with static numbers and both numbers are shown.

For the calculations the user will be able to write down functions containing references to other numeric fields.
The application will offer the ability to create custom dice rolls and use them with calculations to roll for a specific value of the character.

# Run the Project

## Haskell Setup

1. If you haven't already, [install Stack](https://haskell-lang.org/get-started)
	* On POSIX systems, this is usually `curl -sSL https://get.haskellstack.org/ | sh`
2. Install the `yesod` command line tool: `stack install yesod-bin --install-ghc`
3. Build libraries: `stack build`

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
	
# Design Questions
1. How to save the nested Field structure of a Sheet
	```
		data Field = NumField 
			{ type  :: String
			, name  :: String
			, id    :: String
			, value :: Int   
			}
		    | TextField   
			{ type  :: String
			, name  :: String
			, id    :: String
			, value :: String   
			}

		data Sheet = [Sheet]
			   | [Field]
	    ```

2. Should rolls be displayed in a "Chatbox" -> you can see the roll history OR should rolls be displayed in a box -> the new result replaces the old result, so the roll history is not (immediately ?) visible.
	    
(Optional: What would be a better name for this application?)
