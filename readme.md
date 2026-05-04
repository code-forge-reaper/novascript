# NovaScript

NovaScript is made for convenience and ease of writting.
By consequence, you don't get conventional syntax out of it.

---

## Files
`nova.py`          - main script file that you run nova scripts with
`novascript.py`    - main AST-walking interpreter
`nodes.py`         - the ast representation of the nodes
`parsel.py`        - a renpy-inspired text adventure lang, self-encapsulated and made from a copy of nova's tokenizer
`parsel/nova.sublime-syntax` - highlighting for both languages
`nova.sublime-build` - ctrl+b to run nova :)

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
Python                  4       5,837       606       206      5,025        984
───────────────────────────────────────────────────────────────────────────────
novascript.py                   3,782       425       186      3,171        904
parsel.py                       1,268         0         2      1,266          3
nodes.py                          761       174        16        571         72
nova.py                            26         7         2         17          5
───────────────────────────────────────────────────────────────────────────────
Total                   4       5,837       606       206      5,025        984
───────────────────────────────────────────────────────────────────────────────
> cat examples/example.nova | wc -l
562

```

---

## Requirements
- Python 3.8+
- No external dependencies (pure Python-based interpreter)

---

## Parsel example
(this isn't a good language example, but it works for an example of what you can do)
```par
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
            return
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
```

## NovaScript Example(examples/pyqt6.nova)

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
