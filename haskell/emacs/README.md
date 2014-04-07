Dependencies:

    sudo apt-get update
    sudo apt-get install ghc cabal-install
    sudo apt-get install texinfo
    cabal update
    cabal install cabal-install

Update ```$PATH``` to use the newer cabal:

    echo 'PATH=~/.cabal/bin:$PATH'|tee -a ~/.bashrc
    source ~/.bashrc
    which cabal


Following: http://tim.dysinger.net/posts/2014-02-18-haskell-with-emacs.html

    mkdir -p ~/.emacs.d
    git clone -b haskell --depth=1 git://github.com/dysinger/el-get ~/.emacs.d/el-get-haskell

Drop into ```~/.emacs.d/init.el```:

    ;; NO FRILLS
    (dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
      (when (fboundp mode) (funcall mode -1)))
    (setq inhibit-startup-screen t)
    ;; NO JUNK
    (setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
          backup-directory-alist `((".*" . ,temporary-file-directory)))
    ;; EL-GET
    (add-to-list 'load-path (locate-user-emacs-file "el-get/el-get"))
    (unless (require 'el-get nil 'noerror)
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
        (goto-char (point-max))
        (eval-print-last-sexp)))
    (defun el-get-sync-recipes (overlay)
      (let* ((recipe-glob (locate-user-emacs-file (concat overlay "/recipes/*.rcp")))
             (recipe-files (file-expand-wildcards recipe-glob))
             (recipes (mapcar 'el-get-read-recipe-file recipe-files)))
        (mapcar (lambda (r) (add-to-list 'el-get-sources r)) recipes)
        (el-get 'sync (mapcar 'el-get-source-name recipes))))
    (setq el-get-user-package-directory user-emacs-directory)
    ;; EL-GET SYNC OVERLAYS
    (el-get-sync-recipes "el-get-haskell")
    (el-get-sync-recipes "el-get-user")
    ;; CUSTOM FILE
    (setq custom-file (locate-user-emacs-file "custom.el"))
    (load custom-file 'noerror)

Run emacs and wait for it to compile everything:

    emacs

When it's done you should see

    Congrats, el-get is installed and ready to serve!



