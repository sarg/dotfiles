;;; GNU Guix --- Functional package management for GNU
;;;
;;; This file is part of GNU Guix.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (personal packages skia)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages graphics)
  #:use-module (gnu packages icu4c))

(define %skia-patch-directory
  ;; Do not rely on (current-filename): it is not reliable when this module
  ;; is loaded from compiled .go files.
  (dirname
   (or (search-path %load-path
                    "personal/packages/patches/skia-remove-angle2.patch")
       (error "skia-next: cannot locate its patch directory"))))

(define-public icu4c-for-skia-next
  ;; The Skia 143 sources require this exact ICU commit for their tests.
  ;; See the DEPS file in the Skia sources.
  (let ((commit "364118a1d9da24bb5b770ac3d762ac144d6da5a4")
        (revision "1"))
    (package/inherit icu4c-for-skia
      (name "icu4c-for-skia-next")
      (version (git-version "74" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://chromium.googlesource.com/chromium/deps/icu.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1y4y24xamafvghs4n2rjz477r7zsg5fgf15va122q5i2hg0jdfvy")))))))

(define-public skia-next
  ;; Releases follow those of Chromium.  See the release page at
  ;; <https://skia.googlesource.com/skia/+/refs/heads/main/RELEASE_NOTES.md>.
  (let ((version "143")
        (revision "0")
        (commit "b88b661b7fca3a45cf6d5d684d1259939eca1c71"))
    (package/inherit skia
      (name "skia-next")
      (version (git-version version revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://skia.googlesource.com/skia.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "18zr356ibr8ps193fibj3xzqb9zk3k5fjrdnsmnf2qg9svkvkkm0"))
         (patches
          (list (local-file
                 (string-append %skia-patch-directory
                                "/skia-remove-angle2.patch"))
                (local-file
                 (string-append %skia-patch-directory
                                "/skia-gif-stubs.patch"))))))
      (arguments
       (substitute-keyword-arguments arguments
         ((#:phases phases)
          #~(modify-phases #$phases
              (replace 'configure
                (lambda* (#:key inputs #:allow-other-keys)
                  (substitute* "BUILD.gn"
                    ;; Workaround a bug in the zlib third_party definition,
                    ;; that fails the build even when zlib is found from the
                    ;; system.
                    (("deps = \\[ \"//third_party/zlib\" ]")
                     "deps = []"))
                  (invoke "gn" "gen" "build"
                          (string-append
                           ;;
                           "--args="
                           "cc=\"" #$(cc-for-target) "\" " ;defaults to 'cc'
                           "is_official_build=true " ;use system libraries
                           "is_component_build=true " ;build as shared library
                           "skia_use_system_zlib=true " ;use system zlib
                           ;; Specify where to locate the harfbuzz and freetype
                           ;; includes.
                           (format #f "extra_cflags=[\"-I~a\",\"-I~a\"] "
                                   (search-input-directory
                                    inputs "include/harfbuzz")
                                   (search-input-directory
                                    inputs "include/freetype2"))
                           ;; Otherwise the validate-runpath phase fails.
                           "extra_ldflags=[\"-Wl,-rpath=" #$output "/lib\"] "
                           ;; Disabled, otherwise the build system attempts to
                           ;; download the SDK at build time.
                           "skia_use_dng_sdk=false "
                           ;; Wuffs is a Google language that may improve
                           ;; performance; disabled while unpackaged.
                           "skia_use_wuffs=false "))
                  ;; Later phases may need to regenerate the Ninja files.
                  (mkdir-p "bin")
                  (symlink
                   (string-append #$(this-package-native-input "gn") "/bin/gn")
                   "./bin/gn")))
              (replace 'check
                (lambda* (#:key tests? inputs native-inputs #:allow-other-keys)
                  (if tests?
                      (let ((icu #$(this-package-native-input
                                    "icu4c-for-skia")))
                        ;; Unbundle SPIRV-Tools dependency.
                        (substitute* "BUILD.gn"
                          (("deps \\+= \\[ \"//third_party/externals/spirv-tools:spvtools_val\" \\]")
                           "libs += [ \"SPIRV-Tools\" ]"))
                        (substitute* "src/sksl/SkSLCompiler.cpp"
                          (("\"spirv-tools/libspirv.hpp\"")
                           "<libspirv.hpp>"))
                        ;; Configure ICU dependency.
                        (substitute* "third_party/icu/BUILD.gn"
                          (("data_dir = \"\\.\\./externals/icu/\"")
                           (string-append "data_dir = \"" icu
                                          "/share/data/\""))
                          (("script = \"\\.\\./externals/icu/scripts/")
                           (string-append "script = \"" icu
                                          "/share/scripts/"))
                          (("\\.\\./externals/icu/common/icudtl\\.dat")
                           (string-append icu "/share/data/icudtl.dat"))
                          (("sources = icu_sources")
                           "")
                          (("sources \\+= \\[ \"\\$data_assembly\" \\]")
                           "sources = [ \"$data_assembly\" ]"))
                        ;; Enable system libraries without
                        ;; is_official_build=true.  This is necessary because
                        ;; is_official_build prevents building dm.
                        (for-each
                         (lambda (libname)
                           (let ((snake (string-join
                                         (string-split libname #\-) "_")))
                             (substitute*
                                 (string-append "third_party/" libname
                                                "/BUILD.gn")
                               (((string-append "skia_use_system_"
                                                snake
                                                " = is_official_build.*"))
                                (string-append "skia_use_system_" snake
                                               " = true")))))
                         '("zlib" "libjpeg-turbo" "harfbuzz" "libpng"
                           "libwebp"))
                        ;; Configure with gn.
                        (invoke "gn" "gen" "out/Debug"
                                (string-append
                                 "--args="
                                 "cc=\"" #$(cc-for-target) "\" "
                                 "skia_compile_sksl_tests=false "
                                 "skia_use_perfetto=false "
                                 "skia_use_wuffs=false "
                                 "skia_use_system_expat=true "
                                 "skia_use_system_zlib=true "
                                 ;; Specify where to locate the includes.
                                 "extra_cflags=["
                                 (string-join
                                  (map
                                   (lambda (lib)
                                     (string-append
                                      "\"-I"
                                      (search-input-directory
                                       inputs
                                       (string-append "include/" lib)) "\""))
                                   '("harfbuzz"
                                     "freetype2"
                                     "spirv-tools"
                                     "spirv"
                                     "unicode"))
                                  ",")
                                 "] "
                                 ;; Otherwise the validate-runpath phase fails.
                                 "extra_ldflags=["
                                 "\"-Wl,-rpath=" #$output "/lib\""
                                 "] "
                                 ;; Disabled, otherwise the build system
                                 ;; attempts to download the SDK at build time.
                                 "skia_use_dng_sdk=false "
                                 "skia_use_runtime_icu=true "
                                 "skunicode_tests_enabled=false "))
                        ;; Build dm testing tool.
                        (invoke "ninja" "-C" "out/Debug" "dm")
                        ;; The test suite requires an X server.
                        (let ((xvfb (search-input-file (or native-inputs inputs)
                                                       "bin/Xvfb"))
                              (display ":1"))
                          (setenv "DISPLAY" display)
                          ;; Prevent irrelevant errors that cause test output
                          ;; mismatches: "Fontconfig error: No writable cache
                          ;; directories".
                          (setenv "XDG_CACHE_HOME" (getcwd))
                          (system (string-append xvfb " " display " &")))
                        ;; Run tests.
                        (invoke "out/Debug/dm" "-v"
                                "-w" "dm_output"
                                "--codecWritePath" "dm_output"
                                "--simpleCodec"
                                "--skip"
                                ;; The underscores are part of the dm syntax
                                ;; for skipping tests.
                                ;; These tests fail with segmentation fault.
                                "_" "_" "_" "Codec_trunc"
                                "_" "_" "_" "AnimCodecPlayer"
                                "_" "_" "_" "Codec_partialAnim"
                                "_" "_" "_" "Codec_InvalidImages"
                                "_" "_" "_" "Codec_GifInterlacedTruncated"
                                ;; This test started failing possibly after
                                ;; mesa being updated to 23.2.1 and possibly
                                ;; only on some hardware.
                                "_" "_" "_" "SkRuntimeBlender_GPU"
                                "_" "_" "_" "SkRuntimeBlender_Ganesh"
                                "_" "_" "_" "SkText_UnicodeText_Flags"
                                "_" "_" "_" "SkParagraph_FontStyle"
                                "_" "_" "_" "flight_animated_image"
                                ;; These started failing on the last update.
                                "_" "_" "_" "SkParagraph"
                                "_" "_" "_" "Shaper_cluster"
                                ;; These tests fail because of Codec/Sk
                                ;; failure.
                                "_" "_" "_" "AndroidCodec_computeSampleSize"
                                "_" "_" "_" "AnimatedImage_invalidCrop"
                                "_" "_" "_" "AnimatedImage_scaled"
                                "_" "_" "_" "AnimatedImage_copyOnWrite"
                                "_" "_" "_" "AnimatedImage"
                                "_" "_" "_" "BRD_types"
                                "_" "_" "_" "Codec_frames"
                                "_" "_" "_" "Codec_partial"
                                "_" "_" "_" "Codec_partialWuffs"
                                "_" "_" "_" "Codec_requiredFrame"
                                "_" "_" "_" "Codec_rewind"
                                "_" "_" "_" "Codec_incomplete"
                                "_" "_" "_" "Codec_InvalidAnimated"
                                "_" "_" "_" "Codec_ossfuzz6274"
                                "_" "_" "_" "Codec_gif_out_of_palette"
                                "_" "_" "_" "Codec_xOffsetTooBig"
                                "_" "_" "_" "Codec_gif"
                                "_" "_" "_" "Codec_skipFullParse"
                                "_" "_" "_" "AndroidCodec_animated_gif"
                                ;; This one is just resource hungry.
                                "_" "_" "_" "BigImageTest_Ganesh"
                                ;; These fail for unknown reasons.
                                "_" "_" "_" "Gif"
                                "_" "_" "_" "Wuffs_seek_and_decode"
                                "_" "_" "_" "Skottie_Shaper_ExplicitFontMgr"
                                "8888" "skp" "_" "_"
                                "8888" "lottie" "_" "_"
                                "gl" "skp" "_" "_"
                                "gl" "lottie" "_" "_"
                                "_" "_" "_" "ES2BlendWithNoTexture"))
                      (format #t "test suite not run~%"))))))))
      (native-inputs
       (modify-inputs native-inputs
         (replace "icu4c-for-skia" icu4c-for-skia-next))))))
