var gulp = require('gulp');
var elm  = require('gulp-elm');
var minify = require('gulp-minify');
var rename = require('gulp-rename');

gulp.task('elm-init', elm.init);

gulp.task('build', ['elm-init'], function () {
  return gulp.src('src/Main.elm')
    .pipe(elm({warn: true}))
    .pipe(rename('index.js'))
    .pipe(gulp.dest('.'));
});

gulp.task('dist', ['elm-init'], function () {
  return gulp.src('src/Main.elm')
    .pipe(elm({warn: true}))
    .pipe(minify({noSource: true}))
    .pipe(rename('index.js'))
    .pipe(gulp.dest('.'));
});

gulp.task('watch', function () {
  gulp.watch('src/*.elm', ['build']);
});

gulp.task('default', ['build']);
