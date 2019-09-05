'use strict';

// enable installed extensions

var id = 'eggkanocgddhmamlbiijnphhppkpkmkl'
chrome.management.get(id, (extension) => {
  if(extension && !extension.enabled) {
    chrome.management.setEnabled(id, true)
  }
})
