# NovaScript
A language started almost 2 years ago
```
[00:28:12] cross@endeavourOS /home/cross/Documents/projects/novascript [0|1]
> git log | tail
Author: code-forge-reaper <cross.code.forge@gmail.com>
Date:   Thu Feb 13 00:30:24 2025 -0300

    Create NovaScript.js

commit 1e26202e4817ecd088a78eadaccb53532f6d0eeb
Author: code-forge-reaper <cross.code.forge@gmail.com>
Date:   Thu Feb 13 00:29:52 2025 -0300

    Initial commit
```


NovaScript is made for convenience and ease of writting.
By consequence, you don't get conventional syntax out of it.

---

## Files
`nova.py`          - main script file that you run nova scripts with
`novascript.py`    - AST-walking interpreter
`nodes.py`         - the ast representation of the nodes nova uses

`parsel.py`        - a renpy-inspired text adventure lang, self-encapsulated(AKA, not dependant on nova) and made from a copy of nova's tokenizer

`parsel/nova.sublime-syntax` - highlighting for both languages
`parsel/nova.sublime-build` - ctrl+b to run them :)

- Nova and Parsel are hand-rolled AST-walker(nova)/recursive descent(Parsel) interpreters
- even then, somehow this monolith of stuff manages to run fast
200ms to 2 seconds at most from what i can see while running the example.nova

---
sizes if you care about them

```txt
> scc --by-file *.py
───────────────────────────────────────────────────────────────────────────────
Language            Files       Lines    Blanks  Comments       Code Complexity
───────────────────────────────────────────────────────────────────────────────
Python                  4       5,788       603       207      4,978        994
───────────────────────────────────────────────────────────────────────────────
novascript.py                   3,814       422       187      3,205        915
parsel.py                       1,186         0         2      1,184          2
nodes.py                          761       174        16        571         72
nova.py                            27         7         2         18          5
───────────────────────────────────────────────────────────────────────────────
Total                   4       5,788       603       207      4,978        994
───────────────────────────────────────────────────────────────────────────────
> cat examples/example.nova | wc -l
766
```

---

## Requirements
- Python 3.13+

---

## Parsel example
(this isn't a good language example, but it works for an example of what you can do)
```txt
[23:23:04] cross@endeavourOS /home/cross/Documents/projects/novascript
> cd ../sample
[23:23:06] cross@endeavourOS /home/cross/Documents/projects/sample
> ls *.par
► data.par :
data.par


► main.par :
main.par
[23:23:09] cross@endeavourOS /home/cross/Documents/projects/sample
> cat data.par
var player = {
    inventory: [],
    visitedScenes:{}
}

var give = player.inventory.append
func has(itemName) return includes(player.inventory, itemName) end

func reset()
    player.inventory.clear()
    player.visitedScenes = {}
end

func visited(name, checking)
    if player.visitedScenes.get(name, nil)
        return true
    end
    if !checking
        player.visitedScenes[name] = true
    end
    return false
end
[23:23:15] cross@endeavourOS /home/cross/Documents/projects/sample
> cat main.par
scene "start"
    goto "bedroom"
end

scene "bedroom"
    if visited("bedroom", false)
        narrate "The bedroom is still filled with dust"
    else
        narrate "You wake up in a dark bedroom, dust covers the entire room."
        narrate "You see a locked door and a cabinet nearby"
    end
    narrate "You:..."
    options
        "Check cabinet" if !has("key") begin
            narrate "You check the cabinet, and find a key"
            give("key")
        end
        "Check the door" begin
            narrate "You walk up to the door and try to open it, only to find that it is locked"
            if has("key")
                narrate "Fortunately, you have a key"
                goto "escape"
            else
                narrate "If only you had a key..."
            end
        end
    end
end


scene "escape"
    narrate "You place key in the lock, and turn it."
    narrate "When you open it, you are greeted with the moonlit street."
    narrate "You escaped"
    exit
end
[23:23:19] cross@endeavourOS /home/cross/Documents/projects/sample
> cp ../novascript/parsel.py .
[23:23:22] cross@endeavourOS /home/cross/Documents/projects/sample
> python parsel.py
You wake up in a dark bedroom, dust covers the entire room.
You see a locked door and a cabinet nearby
You:...
Choose an option:
1) Check cabinet
2) Check the door
Choice: 2
You walk up to the door and try to open it, only to find that it is locked
If only you had a key...
The bedroom is still filled with dust
You:...
Choose an option:
1) Check cabinet
2) Check the door
Choice: 1
You check the cabinet, and find a key
The bedroom is still filled with dust
You:...
Choose an option:
1) Check the door
Choice: 1
You walk up to the door and try to open it, only to find that it is locked
Fortunately, you have a key
You place key in the lock, and turn it.
When you open it, you are greeted with the moonlit street.
You escaped
[23:23:28] cross@endeavourOS /home/cross/Documents/projects/sample
>
```

## NovaScript Example(examples/pyqt6.nova)
($ is just shorthand for `self.`, e.g: `$_setup()`->`self.setup()`)
```nova
#!/usr/bin/env nova.py
using load("PyQt6.QtWidgets")
using load("PyQt6.QtCore")
using load("PyQt6.QtGui")
using load("PyQt6.QtWebEngineCore")
using load("PyQt6.QtWebEngineWidgets")

class Win inherits QMainWindow
  func init(app) super()
    $app = app
    $setWindowTitle("Sample engine")
    
    // Setup UI components and layout
    $_setupUi()
    $_setupWebEngine()
    $_setupConnections()
    $devToolsView.setVisible(false)
    // Load initial page
    $webView.setUrl(QUrl("https://example.com"))
  end
  
  // --------------------------------------------------------------------
  // Private helper methods
  // --------------------------------------------------------------------
  
  func _setupUi()
    // Main container and layout
    $container = QWidget()
    $mainLayout = QVBoxLayout()
    $container.setLayout($mainLayout)
    
    // Create splitter for web view (left) and dev tools (right)
    $splitter = QSplitter(Qt.Orientation.Horizontal)
    
    // Web view (main browser)
    $webView = QWebEngineView()
    $splitter.addWidget($webView)
    
    // Dev tools view (attached to the web view's page)
    $devToolsView = QWebEngineView()
    $splitter.addWidget($devToolsView)
    
    // Set initial splitter proportions (70% web view, 30% dev tools)
    $splitter.setSizes([700, 300])
    
    // URL input bar at the bottom
    $urlInput = QLineEdit()
    $urlInput.setPlaceholderText("Enter URL and press Enter")
    
    // Assemble main layout
    $mainLayout.addWidget($splitter)     // Takes most of the space
    $mainLayout.addWidget($urlInput)     // Fixed height at bottom
    
    $setCentralWidget($container)
  end
  
  func _setupWebEngine()
    // Enable required web engine features
    var settings = $webView.settings()
    settings.setAttribute(QWebEngineSettings.WebAttribute.JavascriptEnabled, true)
    settings.setAttribute(QWebEngineSettings.WebAttribute.LocalContentCanAccessRemoteUrls, true)
    settings.setAttribute(QWebEngineSettings.WebAttribute.ErrorPageEnabled, true)
    settings.setAttribute(QWebEngineSettings.WebAttribute.PluginsEnabled, true)
    
    // Attach dev tools to the separate view
    $webView.page().setDevToolsPage($devToolsView.page())
  end
  
  func _setupConnections()
    // Update URL bar when navigation changes
    $webView.urlChanged.connect(def(qUrl)
      $urlInput.setText(qUrl.toString())
    end)
    
    // Navigate when Enter is pressed in the URL bar
    $urlInput.returnPressed.connect(def()
      var url = $urlInput.text()
      if !url.startswith("http") && !url.startswith("https")
        url = 'https://' + url
      end
      $webView.setUrl(QUrl(url))
    end)
  end
  
  // --------------------------------------------------------------------
  // Key event handling (F12 toggles dev tools)
  // --------------------------------------------------------------------
  
  func keyPressEvent(event)
    if event.key() == Qt.Key.Key_F12
      $_toggleDevTools()
    else
      super.keyPressEvent(event)   // forward other keys
    end
  end

  func _toggleDevTools()
    var isVisible = $devToolsView.isVisible()
    $devToolsView.setVisible(!isVisible)
    // The splitter will automatically adjust and remember the size
  end
end

// Application entry point
const app = QApplication(Runtime.args)
const win = new Win(app)
win.show()
app.exec()

```

Check out `examples/` for more.

---

## Syntax Highlighting

Sublime Text support is included (`nova.sublime-syntax`).
Just run this and you'll get a up-to-date syntax every time you to `git pull`
```bash
ln -sf $(pwd)/*.subl* ~/.config/sublime-text/Packages/User/
```

---

## License
GPLv3
