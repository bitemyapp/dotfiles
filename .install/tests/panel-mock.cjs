// Model panel reordering/configuration without connecting to a desktop.
const assert = require('node:assert/strict');
const fs = require('node:fs');
const vm = require('node:vm');
const script = fs.readFileSync(process.argv[2], 'utf8');
let items = [];
let nextId = 1;
const panel = {
    id: 1, location: 'bottom', widgets() { return items.slice(); },
    addWidget(type) {
        const config = {};
        const widget = {id: nextId++, type, readConfig(k, d) { return config[k] ?? d; },
            writeConfig(k, v) { config[k] = v; }, config};
        Object.defineProperty(widget, 'index', {
            get() { return items.indexOf(widget); },
            set(i) { items.splice(items.indexOf(widget), 1); items.splice(i, 0, widget); }
        });
        items.push(widget);
        return widget;
    }
};
let existing = [panel];
let created = 0;
const context = vm.createContext({
    panels() { return existing; },
    Panel: function () { created++; existing = [panel]; return panel; },
    print() {}, dotfilesOptions: {panelId: null, launchers: ['applications:new.desktop']}
});
const custom = panel.addWidget('user.widget');
const tasks = panel.addWidget('org.kde.plasma.icontasks');
tasks.writeConfig('launchers', ['applications:user.desktop']);
vm.runInContext(script, context);
const count = items.length;
const order = items.map(w => w.type);
vm.runInContext(script, context);
assert.equal(items.length, count);
assert.deepEqual(items.map(w => w.type), order);
assert.ok(items.includes(custom));
assert.deepEqual(tasks.config.launchers, ['applications:user.desktop', 'applications:new.desktop']);
assert.equal(panel.height, 36);
assert.equal(panel.floating, true);
assert.equal(items.find(w => w.type === 'org.kde.plasma.battery').config.showPercentage, true);
assert.deepEqual(order.slice(-6), ['org.kde.plasma.notifications', 'org.kde.plasma.clipboard',
    'org.kde.plasma.bluetooth', 'org.kde.plasma.battery', 'org.kde.plasma.digitalclock', 'org.kde.plasma.showdesktop']);
existing = [panel, {id: 2, location: 'bottom'}];
assert.throws(() => vm.runInContext(script, context), /Multiple bottom panels/);
context.dotfilesOptions.panelId = 1;
vm.runInContext(script, context);
context.dotfilesOptions.panelId = 99;
assert.throws(() => vm.runInContext(script, context), /does not exist/);
context.dotfilesOptions.panelId = null;
existing = [];
items = [];
vm.runInContext(script, context);
vm.runInContext(script, context);
assert.equal(created, 1);
console.log('Panel restore checks passed');
