/** @babel */
import fs from 'fs';
import stdPath from 'path';
import temp from 'temp'; temp.track();

import $ from 'jquery';
import mkdirp from 'mkdirp';
import osenv from 'osenv';
import touch from 'touch';

import {provideEventService} from '../lib/advanced-open-file';
import {
    DEFAULT_ACTIVE_FILE_DIR,
    DEFAULT_EMPTY,
    DEFAULT_PROJECT_ROOT
} from '../lib/config';
import {Path} from '../lib/models';


describe('Functional tests', () => {
    let workspaceElement = null;
    let activationPromise = null;
    let ui = null;
    let pathEditor = null;

    beforeEach(() => {
        workspaceElement = atom.views.getView(atom.workspace);
        jasmine.attachToDOM(workspaceElement);

        // Clear out any leftover panes.
        atom.workspace.getPanes().forEach((pane) => pane.destroy());

        activationPromise = atom.packages.activatePackage('advanced-open-file');
    });

    function getUI() {
        return workspaceElement.querySelector('.advanced-open-file');
    }

    function fixturePath(...parts) {
        return stdPath.join(__dirname, 'fixtures', ...parts);
    }

    function setPath(newPath) {
        pathEditor.setText(newPath);
    }

    function currentPath() {
        return pathEditor.getText();
    }

    function dispatch(command) {
        atom.commands.dispatch(ui[0], command);
    }

    function currentPathList() {
        return ui.find('.list-item:not(.hidden)')
                 .map((i, item) => $(item).text().trim())
                 .get();
    }

    function currentEditorPaths() {
        return atom.workspace.getTextEditors().map((editor) => editor.getPath());
    }

    function waitsForOpenPaths(count, timeout=2000) {
        waitsFor(
            () => currentEditorPaths().length >= count,
            `${count} paths to be opened`,
            timeout
        );
    }

    function openModal() {
        atom.commands.dispatch(workspaceElement, 'advanced-open-file:toggle');
        waitsForPromise(() => {
            return activationPromise.then(() => {
                ui = $(getUI());
                pathEditor = ui.find('.path-input')[0].getModel();
            });
        });
    }

    function resetConfig() {
        atom.config.unset('advanced-open-file.createFileInstantly');
        atom.config.unset('advanced-open-file.helmDirSwitch');
        atom.config.unset('advanced-open-file.defaultInputValue');
    }

    function fileExists(path) {
        try {
            fs.statSync(path);
        } catch (err) {
            if (err.code === 'ENOENT') {
                return false;
            }
        }

        return true;
    }

    function isDirectory(path) {
        try {
            return fs.statSync(path).isDirectory();
        } catch (err) {}

        return false;
    }

    function clickFile(filename) {
        ui.find(`.list-item[data-file-name$='${filename}']`).click();
    }

    function assertAutocompletesTo(inputPath, autocompletedPath) {
        setPath(inputPath);
        dispatch('advanced-open-file:autocomplete');
        expect(currentPath()).toEqual(autocompletedPath);
    }

    describe('Modal dialog', () => {
        beforeEach(resetConfig);

        it('appears when the toggle command is triggered', () => {
            openModal()
            runs(() => {
                expect(getUI()).not.toBeNull();
            });
        });

        it('disappears if the toggle command is triggered while it is visible', () => {
            openModal();
            runs(() => {
                atom.commands.dispatch(workspaceElement, 'advanced-open-file:toggle');
                expect(getUI()).toBeNull();
            });
        });

        it('disappears if the cancel command is triggered while it is visible', () => {
            openModal();
            runs(() => {
                dispatch('core:cancel');
                expect(getUI()).toBeNull();
            });
        });

        it('disappears if the user clicks outside of the modal', () => {
            openModal();
            runs(() => {
                ui.parent().click();
                expect(getUI()).toBeNull();
            });
        });
    });

    describe('Path listing', () => {
        beforeEach(resetConfig);
        beforeEach(openModal);

        it('lists the directory contents if the path ends in a separator', () => {
            setPath(fixturePath() + stdPath.sep);

            // Also includes the parent directory and is sorted alphabetically
            // grouped by directories and files.
            expect(currentPathList()).toEqual([
                '..',
                'examples',
                'prefix_match.js',
                'prefix_other_match.js',
                'sample.js'
            ]);
        });

        it('lists matching files if the path doesn\'t end in a separator', () => {
            setPath(fixturePath('prefix'));

            // Also shouldn't include the parent.
            expect(currentPathList()).toEqual([
                'prefix_match.js',
                'prefix_other_match.js'
            ]);
        });

        it('excludes files that don\'t have a prefix matching the fragment', () => {
            setPath(fixturePath('prefix_match'));
            expect(currentPathList()).toEqual(['prefix_match.js']);
        });

        it('considers relative paths to be relative to the project root', () => {
            atom.project.setPaths([fixturePath()]);
            setPath(stdPath.join('examples', 'subdir') + stdPath.sep);
            expect(currentPathList()).toEqual(['..', 'subsample.js']);
        });

        it('automatically updates when the path changes', () => {
            setPath(fixturePath('prefix'));
            expect(currentPathList()).toEqual([
                'prefix_match.js',
                'prefix_other_match.js'
            ]);

            setPath(fixturePath('prefix_match'));
            expect(currentPathList()).toEqual(['prefix_match.js']);
        });

        it(`matches files case-insensitively unless the fragment contains a
            capital`, () => {
            setPath(fixturePath('examples', 'caseSensitive', 'prefix_match'));
            expect(currentPathList()).toEqual([
                'prefix_match_lower.js',
                'prefix_Match_upper.js'
            ]);

            setPath(fixturePath('examples', 'caseSensitive', 'prefix_Match'));
            expect(currentPathList()).toEqual(['prefix_Match_upper.js']);
        });

        it(`shows a button next to folders that can be clicked to add them as
            project folders`, () => {
            atom.project.setPaths([]);
            setPath(fixturePath() + path.sep);

            let exampleListItem = ui.find('.list-item[data-file-name$=\'examples\']');
            let addProjectFolderButton = exampleListItem.find('.add-project-folder');
            expect(addProjectFolderButton.length).toEqual(1);

            addProjectFolderButton.click();
            expect(atom.project.getPaths()).toEqual([fixturePath('examples')]);

            // Do not open folder when clicking.
            expect(currentPath()).toEqual(fixturePath() + path.sep);

            // Remove button when clicked.
            addProjectFolderButton = ui.find(
                '.list-item[data-file-name$=\'examples\'] .add-project-folder'
            );
            expect(addProjectFolderButton.length).toEqual(0);
        });

        it(`does not show the add-project-folder button for folders that are
            already project folders`, () => {
            atom.project.setPaths([fixturePath('examples')]);
            setPath(fixturePath() + path.sep);

            let exampleListItem = ui.find('.list-item[data-file-name$=\'examples\']');
            let addProjectFolderButton = exampleListItem.find('.add-project-folder');
            expect(addProjectFolderButton.length).toEqual(0);
        });

        it('expands tildes at the start to the user\'s home directory', () => {
            spyOn(osenv, 'home').andReturn(fixturePath());
            setPath(stdPath.join('~', 'examples', 'subdir') + stdPath.sep);

            expect(currentPathList()).toEqual(['..', 'subsample.js']);
        });
    });

    describe('Path input', () => {
        beforeEach(resetConfig);
        beforeEach(openModal);

        it('can autocomplete the current input', () => {
            assertAutocompletesTo(
                fixturePath('prefix_ma'),
                fixturePath('prefix_match.js')
            );
        });

        it('can autocomplete the shared parts between two matching paths', () => {
            assertAutocompletesTo(
                fixturePath('pre'),
                fixturePath('prefix_')
            );
        });

        it('inserts a trailing separator when autocompleting a directory', () => {
            assertAutocompletesTo(
                fixturePath('exam'),
                fixturePath('examples') + stdPath.sep
            );
        });

        it('beeps if autocomplete finds no matchs', () => {
            spyOn(atom, 'beep');
            setPath(fixturePath('does_not_exist'));

            dispatch('advanced-open-file:autocomplete');
            expect(currentPath()).toEqual(fixturePath('does_not_exist'));
            expect(atom.beep).toHaveBeenCalled();
        });

        it('beeps if autocomplete cannot autocomplete any more shared parts', () => {
            spyOn(atom, 'beep');
            setPath(fixturePath('prefix_'));

            dispatch('advanced-open-file:autocomplete');
            expect(currentPath()).toEqual(fixturePath('prefix_'));
            expect(atom.beep).toHaveBeenCalled();
        });

        it(`is case-sensitive during autocomplete if the fragment has a capital
            letter`, () => {
            setPath(fixturePath('examples', 'caseSensitive', 'prefix_m'));
            dispatch('advanced-open-file:autocomplete');
            expect(currentPath()).toEqual(
                fixturePath('examples', 'caseSensitive', 'prefix_match_')
            );

            setPath(fixturePath('examples', 'caseSensitive', 'prefix_M'));
            dispatch('advanced-open-file:autocomplete');
            expect(currentPath()).toEqual(
                fixturePath('examples', 'caseSensitive', 'prefix_Match_upper.js')
            );
        });

        it(`can autocomplete when the path listing contains two paths where
            one path is the prefix of another`, () => {
            // The example has `planning` and `planning_backend`. The bug arises
            // because the entire `planning` path is a prefix of the other.
            assertAutocompletesTo(
                fixturePath('examples', 'matchPrefix', 'plan'),
                fixturePath('examples', 'matchPrefix', 'planning')
            );
        });

        it('fixes the case of letters in the fragment if necessary', () => {
            assertAutocompletesTo(
                fixturePath('examples', 'caseSensitive', 'prefix_match_up'),
                fixturePath('examples', 'caseSensitive', 'prefix_Match_upper.js')
            );
        });

        it('can remove the current path component', () => {
            setPath(fixturePath('fragment'));
            dispatch('advanced-open-file:delete-path-component');

            // Leaves trailing slash, as well.
            expect(currentPath()).toEqual(fixturePath() + stdPath.sep);
        });

        it(`removes the parent directory when removing a path component with no
            fragment`, () => {
            setPath(fixturePath('subdir') + stdPath.sep);
            dispatch('advanced-open-file:delete-path-component');
            expect(currentPath()).toEqual(fixturePath() + stdPath.sep);
        });

        it('can switch to the user\'s home directory using a shortcut', () => {
            atom.config.set('advanced-open-file.helmDirSwitch', true);
            setPath(fixturePath('subdir') + stdPath.sep + '~' + stdPath.sep);
            expect(currentPath()).toEqual(osenv.home() + stdPath.sep);

            // Also test when the rest of the path is empty.
            setPath('~' + stdPath.sep);
            expect(currentPath()).toEqual(osenv.home() + stdPath.sep);
        });

        it('can switch to the filesystem root using a shortcut', () => {
            // For cross-platformness, we cheat by using Path. Oh well.
            let fsRoot = new Path(fixturePath('subdir')).root().full;

            atom.config.set('advanced-open-file.helmDirSwitch', true);
            setPath(fixturePath('subdir') + stdPath.sep + stdPath.sep);
            expect(currentPath()).toEqual(fsRoot);

            // When the rest of path is empty, some platforms (Windows mainly)
            // can't infer a drive letter, so we can't use fsRoot from above.
            // Instead, we'll use the root of the path we're testing.
            fsRoot = new Path(stdPath.sep + stdPath.sep).root().full;

            // Also test when the rest of the path is empty.
            setPath(stdPath.sep + stdPath.sep);
            expect(currentPath()).toEqual(fsRoot);
        });

        it('can switch to the project root directory using a shortcut', () => {
            atom.config.set('advanced-open-file.helmDirSwitch', true);
            atom.project.setPaths([fixturePath('examples')]);
            setPath(fixturePath('subdir') + stdPath.sep + ':' + stdPath.sep);
            expect(currentPath()).toEqual(fixturePath('examples') + stdPath.sep);

            // Also test when the rest of the path is empty.
            setPath(':' + stdPath.sep);
            expect(currentPath()).toEqual(fixturePath('examples') + stdPath.sep);
        });

        it('does not reset the cursor position while typing', () => {
            setPath(fixturePath('subdir'));

            // Set cursor to be between the d and i in subdir.
            let end = pathEditor.getCursorBufferPosition();
            pathEditor.setCursorBufferPosition([end.row, end.column - 2])

            // Insert a new letter and check that the cursor is after it but
            // not at the end of the editor completely.
            pathEditor.insertText('a');
            let newEnd = pathEditor.getCursorBufferPosition();
            expect(newEnd.column).toEqual(end.column - 1);
        });
    });

    describe('Path input default value', () => {
        beforeEach(resetConfig);

        it('can be configured to be the current file\'s directory', () => {
            atom.config.set(
                'advanced-open-file.defaultInputValue',
                DEFAULT_ACTIVE_FILE_DIR
            );
            waitsForPromise(() => {
                return atom.workspace.open(fixturePath('sample.js')).then(() => {
                    openModal();
                });
            });

            runs(() => {
                expect(currentPath()).toEqual(fixturePath() + stdPath.sep);
            });
        });

        it('can be configured to be the current project root', () => {
            atom.config.set(
                'advanced-open-file.defaultInputValue',
                 DEFAULT_PROJECT_ROOT
            );
            atom.project.setPaths([fixturePath('examples')]);
            openModal();

            runs(() => {
                expect(currentPath()).toEqual(fixturePath('examples') + stdPath.sep);
            });
        });

        it('can be configured to be blank', () => {
            atom.config.set('advanced-open-file.defaultInputValue', DEFAULT_EMPTY);
            openModal();

            runs(() => {
                expect(currentPath()).toEqual('');
            });
        });
    });

    describe('Undo', () => {
        beforeEach(resetConfig);
        beforeEach(openModal);

        it('can undo tab completion', () => {
            setPath(fixturePath('exam'));
            dispatch('advanced-open-file:autocomplete');
            expect(currentPath()).toEqual(fixturePath('examples') + path.sep);
            dispatch('advanced-open-file:undo');
            expect(currentPath()).toEqual(fixturePath('exam'));
        });

        it('can undo deleting path components', () => {
            setPath(fixturePath('exam'));
            dispatch('advanced-open-file:delete-path-component');
            expect(currentPath()).toEqual(fixturePath() + path.sep);
            dispatch('advanced-open-file:undo');
            expect(currentPath()).toEqual(fixturePath('exam'));
        });

        it('can undo clicking a folder', () => {
            setPath(fixturePath() + path.sep);
            clickFile('examples');
            expect(currentPath()).toEqual(fixturePath('examples') + path.sep);
            dispatch('advanced-open-file:undo');
            expect(currentPath()).toEqual(fixturePath() + path.sep);
        });

        it('beeps when it cannot undo any farther', () => {
            spyOn(atom, 'beep');
            dispatch('advanced-open-file:undo');
            expect(atom.beep).toHaveBeenCalled();
        });
    });

    describe('Opening files', () => {
        beforeEach(resetConfig);
        beforeEach(openModal);

        it('opens an existing file if the current path points to one', () => {
            let path = fixturePath('sample.js');
            setPath(path);
            dispatch('core:confirm');

            waitsForOpenPaths(1);
            runs(() => {
                expect(currentEditorPaths()).toEqual([path]);
            });
        });

        it('replaces the path when attempting to open an existing directory', () => {
            setPath(fixturePath('examples'));
            dispatch('core:confirm');
            expect(currentPath()).toEqual(fixturePath('examples') + path.sep);
        });

        it(`beeps when attempting to open a path ending in a separator (a
            non-existant directory)`, () => {
            spyOn(atom, 'beep');
            setPath(fixturePath('notthere') + stdPath.sep);
            dispatch('core:confirm');
            expect(atom.beep).toHaveBeenCalled();
        });

        it(`creates the directory when opening a path ending a separator if
            configured`, () => {
            let tempDir = fs.realpathSync(temp.mkdirSync());
            let path = stdPath.join(tempDir, 'newdir') + stdPath.sep;
            atom.config.set('advanced-open-file.createDirectories', true);
            setPath(path);
            expect(isDirectory(path)).toEqual(false);

            dispatch('core:confirm');
            expect(isDirectory(path)).toEqual(true);
        });

        it('opens a new file without saving it if opening a non-existant path', () => {
            let path = fixturePath('does.not.exist');
            setPath(path);
            dispatch('core:confirm');

            waitsForOpenPaths(1);
            runs(() => {
                expect(currentEditorPaths()).toEqual([path]);
                expect(fileExists(path)).toEqual(false);
            });
        });

        it('creates a new file when configured to', () => {
            let tempDir = fs.realpathSync(temp.mkdirSync());
            let path = stdPath.join(tempDir, 'newfile.js');
            atom.config.set('advanced-open-file.createFileInstantly', true);
            setPath(path);
            expect(fileExists(path)).toEqual(false);

            dispatch('core:confirm');
            waitsForOpenPaths(1);
            runs(() => {
                expect(currentEditorPaths()).toEqual([path]);
                expect(fileExists(path)).toEqual(true);
            });
        });

        it('creates intermediate directories when necessary', () => {
            let tempDir = fs.realpathSync(temp.mkdirSync());
            let newDir = stdPath.join(tempDir, 'newDir');
            let path = stdPath.join(newDir, 'newFile.js');
            setPath(path);
            expect(fileExists(newDir)).toEqual(false);

            dispatch('core:confirm');
            waitsForOpenPaths(1);
            runs(() => {
                expect(currentEditorPaths()).toEqual([path]);
                expect(fileExists(newDir)).toEqual(true);
            });
        });

        it('can create files from relative paths', () => {
            let tempDir = fs.realpathSync(temp.mkdirSync());
            let path = stdPath.join('newDir', 'newFile.js');
            let absolutePath = stdPath.join(tempDir, path);

            atom.project.setPaths([tempDir]);
            atom.config.set('advanced-open-file.createFileInstantly', true);

            setPath(path);
            expect(fileExists(absolutePath)).toEqual(false);

            dispatch('core:confirm');
            waitsForOpenPaths(1);
            runs(() => {
                expect(currentEditorPaths()).toEqual([absolutePath]);
                expect(fileExists(absolutePath)).toEqual(true);
            });
        });

        it('can open files from tilde-prefixed paths', () => {
            spyOn(osenv, 'home').andReturn(fixturePath());
            setPath(stdPath.join('~', 'examples', 'subdir', 'subsample.js'));

            dispatch('core:confirm');
            waitsForOpenPaths(1);
            runs(() => {
                expect(currentEditorPaths()).toEqual([
                    fixturePath('examples', 'subdir', 'subsample.js')
                ]);
            });
        });

        it('can open files in new split panes', () => {
            atom.workspace.open(fixturePath('sample.js'));
            expect(atom.workspace.getPanes().length).toEqual(1);

            setPath(fixturePath('prefix_match.js'));
            dispatch('pane:split-left');

            waitsForOpenPaths(2);
            runs(() => {
                expect(new Set(currentEditorPaths())).toEqual(new Set([
                    fixturePath('sample.js'),
                    fixturePath('prefix_match.js'),
                ]));
                expect(atom.workspace.getPanes().length).toEqual(2);
            });
        });

        it(`shows an error notification when creating a subdirectory throws an
            error`, () => {
            debugger;
            spyOn(atom.notifications, 'addError');
            spyOn(mkdirp, 'sync').andCallFake(() => {
                throw new Error('OH NO');
            });
            setPath(fixturePath('examples', 'noPermission', 'subdir', 'file.txt'));
            dispatch('core:confirm');
            expect(atom.notifications.addError).toHaveBeenCalled();
        });

        it(`shows an error notification when creating a file in a directory
            throws an error`, () => {
            spyOn(atom.notifications, 'addError');
            spyOn(touch, 'sync').andCallFake(() => {
                throw new Error('OH NO');
            });
            atom.config.set('advanced-open-file.createFileInstantly', true);

            setPath(fixturePath('examples', 'noPermission', 'file.txt'));
            dispatch('core:confirm');
            expect(atom.notifications.addError).toHaveBeenCalled();
        });
    });

    describe('Keyboard navigation', () => {
        beforeEach(resetConfig);
        beforeEach(openModal);

        /*
            For reference, expected listing in fixtures is:
            ..
            examples
            prefix_match.js
            prefix_other_match.js
            sample.js
        */

        function moveDown(times) {
            for (let k = 0; k < times; k++) {
                dispatch('advanced-open-file:move-cursor-down');
            }
        }

        function moveUp(times) {
            for (let k = 0; k < times; k++) {
                dispatch('advanced-open-file:move-cursor-up');
            }
        }

        it('allows moving a cursor to a file and confirming to select a path', () => {
            setPath(fixturePath() + stdPath.sep);
            moveDown(4);
            moveUp(1); // Test movement both down and up.
            dispatch('core:confirm');

            waitsForOpenPaths(1);
            runs(() => {
                expect(currentEditorPaths()).toEqual([fixturePath('prefix_match.js')]);
            });
        });

        it('wraps the cursor at the edges', () => {
            setPath(fixturePath() + stdPath.sep);
            moveUp(2);
            moveDown(4);
            moveUp(5);
            dispatch('core:confirm');

            waitsForOpenPaths(1);
            runs(() => {
                expect(currentEditorPaths()).toEqual([fixturePath('prefix_match.js')]);
            });
        });

        it('replaces the current path when selecting a directory', () => {
            setPath(fixturePath() + path.sep);
            moveDown(2);
            dispatch('core:confirm');
            expect(currentPath()).toEqual(fixturePath('examples') + path.sep)
        });

        it('moves to the parent directory when the .. element is selected', () => {
            setPath(fixturePath('examples') + path.sep);
            moveDown(1);
            dispatch('core:confirm');
            expect(currentPath()).toEqual(fixturePath() + path.sep)
        });

        it('can add folders as project directories using a keyboard command', () => {
            atom.project.setPaths([]);
            setPath(fixturePath() + path.sep);
            moveDown(2); // examples folder
            dispatch('application:add-project-folder');
            expect(atom.project.getPaths()).toEqual([fixturePath('examples')]);
        });

        it('beeps when trying to add the parent folder as a project directory', () => {
            spyOn(atom, 'beep');
            atom.project.setPaths([]);

            setPath(fixturePath() + path.sep);
            moveDown(1); // Parent folder
            dispatch('application:add-project-folder');

            expect(atom.beep).toHaveBeenCalled();
            expect(atom.project.getPaths()).toEqual([]);
        });

        it('beeps when trying to add a file as a project directory', () => {
            spyOn(atom, 'beep');
            atom.project.setPaths([]);

            setPath(fixturePath() + path.sep);
            moveDown(3); // prefix_match.js
            dispatch('application:add-project-folder');

            expect(atom.beep).toHaveBeenCalled();
            expect(atom.project.getPaths()).toEqual([]);
        });

        it(`beeps when trying to add a folder as a project directory that is
                already one`, () => {
            spyOn(atom, 'beep');
            atom.project.setPaths([fixturePath('examples')]);

            setPath(fixturePath() + path.sep);
            moveDown(2); // examples folder
            dispatch('application:add-project-folder');

            expect(atom.beep).toHaveBeenCalled();
            expect(atom.project.getPaths()).toEqual([fixturePath('examples')]);
        });

        it(`can select the first item in the list if none are selected using
            special command`, () => {
            setPath(fixturePath('prefix'));
            dispatch('advanced-open-file:confirm-selected-or-first');

            waitsForOpenPaths(1);
            runs(() => {
                expect(currentEditorPaths()).toEqual([fixturePath('prefix_match.js')]);
            });
        })
    });

    describe('Mouse navigation', () => {
        beforeEach(resetConfig);
        beforeEach(openModal);

        it('opens a path when it is clicked on', () => {
            setPath(fixturePath() + stdPath.sep);
            clickFile('sample.js')

            waitsForOpenPaths(1);
            runs(() => {
                expect(currentEditorPaths()).toEqual([fixturePath('sample.js')]);
            });
        });

        it('replaces the current path when clicking a directory', () => {
            setPath(fixturePath() + path.sep);
            clickFile('examples');
            expect(currentPath()).toEqual(fixturePath('examples') + path.sep)
        });

        it('moves to the parent directory when the .. element is clicked', () => {
            setPath(fixturePath('examples') + path.sep);
            ui.find('.parent-directory').click();
            expect(currentPath()).toEqual(fixturePath() + path.sep)
        });
    });

    describe('Events', () => {
        beforeEach(resetConfig);
        beforeEach(openModal);

        it('allows subscription to events when paths are opened', () => {
            let handler = jasmine.createSpy('handler');
            let sub = provideEventService().onDidOpenPath(handler);
            let path = fixturePath('sample.js');

            setPath(path);
            dispatch('core:confirm');
            expect(handler).toHaveBeenCalledWith(path);
            sub.dispose();
        });

        it('allows subscription to events when paths are created', () => {
            atom.config.set('advanced-open-file.createFileInstantly', true);
            let tempDir = fs.realpathSync(temp.mkdirSync());
            let path = stdPath.join(tempDir, 'newfile.js');
            let handler = jasmine.createSpy('handler');
            let sub = provideEventService().onDidCreatePath(handler);

            setPath(path);
            dispatch('core:confirm');
            expect(handler).toHaveBeenCalledWith(path);
            sub.dispose();
        });

        it('emits the create event when creating a directory', () => {
            atom.config.set('advanced-open-file.createDirectories', true);
            let tempDir = fs.realpathSync(temp.mkdirSync());
            let path = stdPath.join(tempDir, 'newdir') + stdPath.sep;
            let handler = jasmine.createSpy('handler');
            let sub = provideEventService().onDidCreatePath(handler);

            setPath(path);
            dispatch('core:confirm');
            expect(handler).toHaveBeenCalledWith(new Path(path).absolute);
            sub.dispose();
        });
    });

    // Only run Windows-specific tests when enabled.
    let windowsDescribe = process.env.AOF_WINDOWS_TESTS ? describe : xdescribe;
    windowsDescribe('Windows-specific tests', () => {
        // Just as a note, we're assuming C:\ exists and is the root
        // system drive. It is on AppVeyor, and that's good enough.

        it('can read the root directory without failing', () => {
            // This potentially fails because we stat in-use files like
            // pagefile.sys.
            expect(() => {setPath('C:\\')}).not.toThrow();
        });

        it('does not replace drive letters with the project root', () => {
            atom.project.setPaths([fixturePath()]);
            setPath('C:/');
            expect(currentPath()).toEqual('C:/');
        });
    });

    describe('Fuzzy filename matching', () => {
        beforeEach(resetConfig);
        beforeEach(() => {
            atom.config.set('advanced-open-file.fuzzyMatch', true);
        });
        beforeEach(openModal);

        it('lists files and folders as normal when no fragment is being matched', () => {
            setPath(fixturePath() + stdPath.sep);

            expect(currentPathList()).toEqual([
                '..',
                'examples',
                'prefix_match.js',
                'prefix_other_match.js',
                'sample.js'
            ]);
        });

        it('uses a fuzzy algorithm for matching files instead of prefix matching', () => {
            setPath(fixturePath('ix'));

            expect(currentPathList()).toEqual([
                'prefix_match.js',
                'prefix_other_match.js',
            ]);
        });

        it('sorts matches by weight instead of by name', () => {
            setPath(fixturePath('examples', 'fuzzyWeight', 'heavy_'));

            expect(currentPathList()).toEqual([
                'more_heavy_heavy.js',
                'less_heavy.js',
            ]);
        });

        it('chooses the first match for autocomplete when nothing is highlighted', () => {
            assertAutocompletesTo(
                fixturePath('ix'),
                fixturePath('prefix_match.js')
            );
        });
    });
});
