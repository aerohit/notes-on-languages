require 'fileutils'
require 'redcloth'

OUT_DIR         = "html"
INSERT_LOCATION = "<!-- INSERT HERE -->"
LAYOUT_FILE     = "layouts/default.html"

SRC_DIRS        = {
  "clojure" => "clj",
  #"haskell" => "hs"
}

def comment_for(ext)
  {
    "clj" => ";",
    "hs"  => "--"
  }[ext]
end

def get_files(dir, ext)
  Dir["#{dir}/**/*.#{ext}"]
end

def write_file(out_dir, fname, content)
  out_file = "#{out_dir}/#{fname}.html"
  FileUtils.mkdir_p(File.dirname(out_file))
  f = open(out_file, "w")
  f.write(content)
  f.close
end

# Use lambdas
def strip_comments(lines, ext)
  comment = comment_for(ext)
  lines.map do |line|
    line.gsub(/^#{comment}/, "")
  end
end

# Use lambdas
def indent(lines, indentation)
  prefix = " " * indentation
  lines.map do |line|
    prefix + line
  end
end

def textile_to_html(content)
  RedCloth.new(content).to_html
end

def readlines(fname)
  File.open(fname).readlines
end

def format_code(fname, indentation, ext)
  lines = readlines(fname)
  stripped = strip_comments(lines, ext)
  content = stripped.join("")
  html = textile_to_html(content)
  indent(html.split("\n"), indentation).join("\n") + "\n"
end

def htmlify(layout, fname, ext)
  h = File.open(layout).readlines.map do |line|
    loc = line.index(INSERT_LOCATION)
    if loc then
      format_code(fname, loc, ext)
    else
      line
    end
  end
  h.join("")
end

def generate_html(src_dirs, out_dir, layout)
  FileUtils.rm_rf(out_dir)
  src_dirs.each do |dir, ext|
    get_files(dir, ext).each do |fname|
      html = htmlify(layout, fname, ext)
      print html
      write_file(out_dir, fname, html)
    end
  end
end

generate_html(SRC_DIRS, "html", "layouts/default.html")
