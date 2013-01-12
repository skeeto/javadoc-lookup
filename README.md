# javadoc-lookup

This package provides a `javadoc-lookup` function for quickly looking
up Javadoc for any library from within Emacs, optionally integrating
with Maven. A browser is launched to view the documentation.

`javadoc-lookup` is not bound to any key by default, so you may want
to add this to your initialization file,

```el
(global-set-key (kbd "C-h j") 'javadoc-lookup)
````

You need to tell javadoc-lookup what you would like to have
indexed. There are two ways to do this. You can point it to the root
of a library's documentation on your filesystem. For example,

```el
(javadoc-add-roots "/usr/share/doc/openjdk-6-jdk/api"
                   "~/src/project/doc")
```

Or, more conveniently, you can **fetch and index documentation from
Maven**! This is done by specifying an artifact as a sequence of three
strings: `[groupId artifactId version]`. For example,

```el
(javadoc-add-artifacts ["org.lwjgl.lwjgl" "lwjgl" "2.8.2"]
                       ["com.nullprogram" "native-guide" "0.2"]
                       ["org.apache.commons" "commons-math3" "3.0"])
```

This feature requires that you have Maven and the command-line unzip
utility installed on your system. The initial fetch is slow but Emacs
will operate from its own cache after that.

## History

This package obsoletes my previous java-docs package. Use this one
instead.
