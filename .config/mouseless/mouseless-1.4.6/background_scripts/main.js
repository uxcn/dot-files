var sessions = {},
    ActiveTabs = {},
    TabHistory = {},
    activePorts = [],
    LastUsedTabs = [];

window.httpRequest = function(request) {
  return new Promise(function(acc, rej) {
    var xhr = new XMLHttpRequest();
    xhr.open('GET', request.url);
    xhr.onload = function() {
      acc(request.json ? JSON.parse(xhr.responseText) : xhr.responseText);
    };
    xhr.onerror = rej.bind(null, xhr);
    xhr.send();
  });
};

function updateTabIndices() {
  if (settings.showtabindices) {
    chrome.tabs.query({currentWindow: true}, function(tabs) {
      tabs.forEach(function(tab) {
        chrome.tabs.sendMessage(tab.id, {
          action: 'displayTabIndices',
          index: tab.index + 1
        });
      });
    });
  }
}

function loadDomainStylesheet(changeInfo, tab) {

  if (changeInfo.status === 'loading') {
    var hostname = getHostname(tab.url)
    // TODO(hbt) ENHANCE issue is with defaultSettings not being applied properly and the settings object not being merged properly from the start. 
    settings.domainStylesheets = settings.domainStylesheets || {}
    
    if (!settings.domainStylesheets.hasOwnProperty(hostname)) {
      return;
    }

    var styleurl = settings.domainStylesheets[hostname]

    // TODO(hbt) ENHANCE by caching the ajax response 

    try {
      
      // workaround error
      // Unchecked runtime.lastError while running tabs.insertCSS: Cannot access contents of url "data:text/html,chromewebdata". Extension manifest must request permission to access this host.
      // caused by chrome error pages like network out of reach or the site can't be reached -- e.g paste this in url "http://%20https//www.npmjs.com/package/fs-finder"

      $.ajax({
        url: styleurl
      }).done(function (data) {
        chrome.tabs.insertCSS(tab.id, {
          code: data,
          runAt: 'document_start',
          allFrames: true
        }, function (res) {
        });

      });
    } catch(e) {
      console.log(e)
    }
    
  }
}

chrome.storage.local.get('sessions', function(e) {
  if (e.sessions === void 0) {
    chrome.storage.local.set({ sessions: {} });
  } else {
    sessions = e.sessions;
  }
});

function getTab(tab, reverse, count, first, last) {
  chrome.tabs.query({windowId: tab.windowId}, function(tabs) {
    if (first) {
      return chrome.tabs.update(tabs[0].id, {active: true});
    } else if (last) {
      return chrome.tabs.update(tabs[tabs.length - 1].id, {active: true});
    } else {
      var index = (reverse ? -1 : 1) * count + tab.index;
      if (count !== -1 && count !== 1)
        index = Math.min(Math.max(0, index), tabs.length - 1);
      else
        index = Utils.trueModulo(index, tabs.length);
      return chrome.tabs.update(tabs[index].id, {active: true});
    }
  });
}

var Listeners = {

  tabs: {
    onUpdated: function(id, changeInfo, tab) {
      updateTabIndices();
      loadDomainStylesheet(changeInfo, tab)
      if (changeInfo.hasOwnProperty('url')) {
        History.shouldRefresh = true;
        if (TabHistory.hasOwnProperty(id)) {
          if (TabHistory[id].links.indexOf(changeInfo.url) === -1) {
            if (TabHistory.state !== void 0 && TabHistory[id].state + 1 !==
                TabHistory[id].length) {
              TabHistory[id].links.splice(TabHistory[id].state);
            }
            TabHistory[id].links.push(changeInfo.url);
            TabHistory[id].state = TabHistory[id].state + 1;
          } else {
            TabHistory[id].state = TabHistory[id].links.indexOf(changeInfo.url);
          }
        } else {
          TabHistory[id] = {};
          TabHistory[id].links = [changeInfo.url];
          TabHistory[id].state = 0;
        }
      }
    },
    onActivated: function(tab) {
      LastUsedTabs.push(tab.tabId);
      if (LastUsedTabs.length > 2) {
        LastUsedTabs.shift();
      }
      if (ActiveTabs[tab.windowId] === void 0) {
        ActiveTabs[tab.windowId] = [];
      }
      ActiveTabs[tab.windowId].push(tab.tabId);
      if (ActiveTabs[tab.windowId].length > 2) {
        ActiveTabs[tab.windowId].shift();
      }
    },
    onRemoved: function(id, removeInfo) {
      updateTabIndices();
      if (ActiveTabs[removeInfo.windowId] !== void 0) {
        ActiveTabs[removeInfo.windowId] = ActiveTabs[removeInfo.windowId]
          .filter(function(e) {
            return e !== id;
          });
      }
      if (TabHistory[id] !== void 0) {
        delete TabHistory[id];
      }
      Frames.remove(id);
    },
    onCreated: updateTabIndices,
    onMoved: updateTabIndices,
  },

  windows: {
    onRemoved: function(windowId) { delete ActiveTabs[windowId]; }
  },

  extension: {
    onConnect: function(port) {
      if (activePorts.indexOf(port) !== -1)
        return;
      var frameId = port.sender.frameId;
      port.postMessage({type: 'hello'});
      port.postMessage({type: 'addFrame', frameId: frameId});
      activePorts.push(port);
      port.onMessage.addListener(function(request) {
        return Actions(request, port.sender, port.postMessage.bind(port), port);
      });
      port.onDisconnect.addListener(function() {
        Frames.removeFrame(frameId);

        for (var i = 0; i < activePorts.length; i++) {
          if (activePorts[i] === port) {
            activePorts.splice(i, 1);
            break;
          }
        }
      });
    }
  },

  runtime: { onMessage: Actions },

  commands: {
    onCommand: function(command) {
      switch (command) {
      case 'togglecVim':
        Popup.toggleEnabled({});
        break;
      case 'toggleBlacklisted':
        Popup.toggleBlacklisted();
        Popup.toggleEnabled({
          request: {
            singleTab: true
          }
        });
        break;
      case 'nextTab':
      case 'previousTab':
        chrome.tabs.query({active: true, currentWindow: true}, function(e) {
          return getTab(e[0], false, (command === 'nextTab' ? 1 : -1),
                        false, false);
        });
        break;
      case 'viewSource':
        chrome.tabs.query({active: true, currentWindow: true}, function(e) {
          chrome.tabs.create({url: 'view-source:' + e[0].url, index: e[0].index + 1});
        });
        break;
      case 'nextCompletionResult':
        chrome.tabs.query({active: true, currentWindow: true}, function(tab) {
          chrome.tabs.sendMessage(tab[0].id, {
            action: 'nextCompletionResult'
          }, function() {
              chrome.windows.create({url: 'chrome://newtab', state: 'maximized'});
          });
        });
        break;
      case 'deleteBackWord':
        chrome.tabs.query({active: true, currentWindow: true}, function(tab) {
          chrome.tabs.sendMessage(tab[0].id, {action: 'deleteBackWord'});
        });
        break;
      case 'closeTab':
        chrome.tabs.query({active: true, currentWindow: true}, function(tab) {
          chrome.tabs.remove(tab[0].id, function() {
            return chrome.runtime.lastError;
          });
        });
        break;
      case 'reloadTab':
        chrome.tabs.query({active: true, currentWindow: true}, function(tab) {
          chrome.tabs.reload(tab[0].id);
        });
        break;
      case 'newTab':
        chrome.tabs.create({url: chrome.runtime.getURL('pages/blank.html')});
        break;
      case 'restartcVim':
        chrome.runtime.reload();
        break;
      default:
        break;
      }
    }
  }

};

(function() {
  for (var api in Listeners) {
    for (var method in Listeners[api]) {
      chrome[api][method].addListener(Listeners[api][method]);
    }
  }
})();
