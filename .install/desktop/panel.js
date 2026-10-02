// Applied through Plasma's scripting API, never by replacing appletsrc.
// The Python runner supplies dotfilesOptions. This file contains no session data.
(function () {
    var choices = panels();
    var panel;
    if (dotfilesOptions.panelId !== null) {
        for (var i = 0; i < choices.length; i++) {
            if (choices[i].id === dotfilesOptions.panelId) panel = choices[i];
        }
        if (!panel) throw new Error("Requested panel does not exist");
    } else {
        var bottom = choices.filter(function (p) { return p.location === "bottom"; });
        if (bottom.length > 1) throw new Error("Multiple bottom panels: use --panel-id");
        panel = bottom[0];
    }
    if (!panel) panel = new Panel();

    panel.location = "bottom";
    panel.height = 36;
    panel.alignment = "center";
    panel.hiding = "none";
    panel.offset = 0;
    panel.lengthMode = "fill";
    panel.opacity = "adaptive";
    panel.floating = true;

    function ensure(type) {
        var widgets = panel.widgets();
        for (var i = 0; i < widgets.length; i++) {
            if (widgets[i].type === type) return widgets[i];
        }
        return panel.addWidget(type);
    }

    ensure("org.kde.plasma.kickoff");
    ensure("org.kde.plasma.pager");
    var tasks = ensure("org.kde.plasma.icontasks");
    ensure("org.kde.plasma.marginsseparator");
    ensure("org.kde.plasma.systemtray");
    tasks.currentConfigGroup = ["General"];
    var launchers = tasks.readConfig("launchers", []);
    if (typeof launchers === "string") launchers = launchers ? launchers.split(",") : [];
    for (var i = 0; i < dotfilesOptions.launchers.length; i++) {
        if (launchers.indexOf(dotfilesOptions.launchers[i]) === -1) {
            launchers.push(dotfilesOptions.launchers[i]);
        }
    }
    tasks.writeConfig("launchers", launchers);

    var right = [
        "org.kde.plasma.notifications", "org.kde.plasma.clipboard",
        "org.kde.plasma.bluetooth", "org.kde.plasma.battery",
        "org.kde.plasma.digitalclock", "org.kde.plasma.showdesktop"
    ].map(ensure);
    var start = panel.widgets().length - right.length;
    for (var i = 0; i < right.length; i++) right[i].index = start + i;
    var battery = right[3];
    battery.currentConfigGroup = ["General"];
    battery.writeConfig("showPercentage", true);
    print("Restored bottom panel " + panel.id + "; existing widgets and launchers kept.");
}());
