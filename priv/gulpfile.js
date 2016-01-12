var gulp = require('gulp');

var clean = require('gulp-clean');
var requirejs = require('gulp-requirejs-optimize');

var paths = {
    dist: 'static/',
    app: 'src/',
};

gulp.task('clean', function() {
  return gulp.src(paths.dist)
    .pipe(clean());
});

gulp.task('copy', function() {
  gulp.src(['index.html'], {cwd: paths.app})
    .pipe(gulp.dest(paths.dist));

  gulp.src(['styles/**/*.css'], {cwd: paths.app})
    .pipe(gulp.dest(paths.dist + 'styles'));

  gulp.src(['bower_components/**/*.css'], {cwd: paths.app})
    .pipe(gulp.dest(paths.dist + 'bower_components'));

  gulp.src(['images/**/*.png'], {cwd: paths.app})
    .pipe(gulp.dest(paths.dist + 'images'));

  gulp.src(['bower_components/requirejs'], {cwd: paths.app})
    .pipe(gulp.dest(paths.dist + 'bower_components'));
});

gulp.task('build', function() {
  return gulp.src('./src/scripts/init.js')
    .pipe(requirejs({
      mainConfigFile: './src/scripts/init.js',
      baseUrl: './src/scripts',
      optimize: 'uglify2',
      findNestedDependencies: true,
      include: [
        'modules/index/index.ctrl',
        'modules/admin/customer.ctrl',
        'modules/admin/device.ctrl',
        'modules/admin/openresty.ctrl'
      ]
    }))
    .pipe(gulp.dest('./static/script'));
});

gulp.task('default', ['clean', 'build', 'copy'], function() {
});
