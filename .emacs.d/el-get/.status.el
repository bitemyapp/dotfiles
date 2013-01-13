((el-get status "installed" recipe
	 (:name el-get :website "https://github.com/dimitri/el-get#readme" :description "Manage the external elisp bits and pieces you depend upon." :type github :branch "4.stable" :pkgname "dimitri/el-get" :features el-get :info "." :load "el-get.el"))
 (lua-mode status "installed" recipe
	   (:name lua-mode :description "A major mode for editing Lua scripts." :website "https://github.com/immerrr/lua-mode" :description "A major mode for editing Lua scripts." :type git :url "https://github.com/immerrr/lua-mode")))
