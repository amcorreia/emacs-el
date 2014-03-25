;;; Compiled snippets and support files for `cake2'
;;; Snippet definitions:
;;;
(yas-define-snippets 'cake2
                     '(("$fc" "\\$this->Form->create('${1:Model}', array('action' => '${2:add}'));" "$this->Form->create(...)" nil nil nil nil nil nil)
                       ("$fe" "\\$this->Form->end();" "$this->Form->end();" nil nil nil nil nil nil)
                       ("$fi" "\\$this->Form->input('${1:field_name}', array('type' => '${2:text}'${3:}));" "$this->Form->input(...)" nil nil nil nil nil nil)
                       ("$fs" "\\$this->Form->submit(__('${1:Submit}'));" "$this->Form->submit(__('Submit'));" nil nil nil nil nil nil)))


;;; Snippet definitions:
;;;
(yas-define-snippets 'cake2
                     '(("$hc" "\\$this->Html->css('${1:css_path}');" "$this->Html->css(...)" nil
                        ("Html")
                        nil nil nil nil)
                       ("$hi" "\\$this->Html->image('${1:image_path}', array(${2:param}));" "$this->Html->image(...)" nil
                        ("Html")
                        nil nil nil nil)
                       ("$hl" "\\$this->Html->link(${1:'link_title'}, array('controller' => '${2:controller}', 'action' => '${3:action}'${4:}));" "$this->Html->link(...)" nil
                        ("Html")
                        nil nil nil nil)
                       ("$hu" "\\$this->Html->url(${1:'url_path'});" "$this->Html->url(...)" nil
                        ("Html")
                        nil nil nil nil)
                       ("php" "<?php echo \\$${1:variavel} ; ?>\n" "Insert php tag" nil
                        ("Html")
                        nil nil nil nil)))


;;; Snippet definitions:
;;;
(yas-define-snippets 'cake2
                     '(("clog" "\\$this->log($0,LOG_DEBUG);" "$this->log(...,LOG_DEBUG);" nil
                        ("general")
                        nil nil nil nil)
                       ("pr" "pr($0);" "pr(...)" nil
                        ("general")
                        nil nil nil nil)))


;;; Do not edit! File generated at Tue Mar 25 00:08:52 2014
