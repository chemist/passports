/* global exports */
"use strict";

// module Main

exports.setValueById = function(tag) {
    return function(newValue) {
        return function() {
            return document.getElementById(tag).value = newValue;
        }
    }
};

