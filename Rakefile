# This "Rakefile" is written to continuously execute miscellaneous tasks with
# arake [1].  This project is a Gauche package, so that most and basic tasks
# should be written in "Makefile.in".
#
# [1] https://github.com/kana/ruby-arake

file ',README.html' => 'README.html' do |t|
  sh "sha1sum #{t.prerequisites[0]} >#{t.name}"
end

file 'README.html' => 'README.asciidoc' do |t|
  sh "asciidoc -b xhtml11 #{t.prerequisites[0]}"
end

__END__
