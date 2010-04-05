# Makefile to maintain vim-xire

REPOS_TYPE := vim-script
INSTALLATION_DIR := $(HOME)/.vim
TARGETS_STATIC = $(filter bin/% %.vim %.txt,$(all_files_in_repos))
TARGETS_ARCHIVED = $(all_files_in_repos) mduem/Makefile

define TEST_RULE
	@if [ '$(patsubst xire-%,xire,$*)' = 'xire' ]; then \
	   gosh $< &>$@; \
	 else \
	   $(vim_script_test_rule); \
	 fi
endef

test/,%.vim: test/%.xire ./bin/xirec
	@./bin/xirec <$< >$@




include mduem/Makefile




# BUGS: The following dependencies must be declared after including
# mduem/Makefile, otherwise the dependencies are ignored.

define generate_rule_to_test_xired_vim_script
test/,vim-$(1).output: test/vim-$(1).input test/,vim-$(1).vim

endef
$(eval \
  $(foreach t, \
    $(patsubst test/vim-%.input,%, \
      $(filter test/vim-%.input,$(all_files_in_repos))), \
    $(call generate_rule_to_test_xired_vim_script,$(t))))

# __END__
