var gulp = require('gulp');
var elm  = require('gulp-elm');
var minify = require('gulp-minify');
var rename = require('gulp-rename');

var main = 'src/Brainfuck/Main.elm';
var output = 'index.js';

gulp.task('elm-init', elm.init);

gulp.task('build', ['elm-init'], function () {
  return gulp.src(main)
    .pipe(elm({warn: true}))
    .pipe(rename(output))
    .pipe(gulp.dest('.'));
});

gulp.task('dist', ['elm-init'], function () {
  return gulp.src(main)
    .pipe(elm({warn: true}))
    .pipe(minify({noSource: true}))
    .pipe(rename(output))
    .pipe(gulp.dest('.'));
});

gulp.task('watch', ['build'], function () {
  gulp.watch('src/*.elm', ['build']);
});

gulp.task('default', ['build']);
