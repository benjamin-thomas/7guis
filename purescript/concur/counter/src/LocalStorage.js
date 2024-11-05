"use strict";

const store = window.localStorage;

export const get = (key) =>
    () => store.getItem(key);

export const set = (key) => (val) =>
    () => store.setItem(key, val);
