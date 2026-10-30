;;; https://git.sr.ht/~abcdw/rde/tree/6ccd368e0a0484e724205f9d608d2eb1ab706ac3/item/src/rde/packages/ghostty.scm
;;; rde --- Reproducible development environment.
;;;
;;; SPDX-FileCopyrightText: 2026 Andrew Tropin <andrew@trop.in>
;;; SPDX-License-Identifier: GPL-3.0-or-later
;;;
;;; This file is part of rde.
;;;
;;; rde is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; rde is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with rde.  If not, see <http://www.gnu.org/licenses/>.

(define-module (personal packages ghostty)
  #:use-module (gnu packages image)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system emacs)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module ((guix licenses) #:prefix license:))


;;;
;;; Zig.
;;;

;; Guix only packages Zig up to 0.16, but ghostel requires exactly 0.17.0.
;; Use the official statically linked release binaries until Guix catches up.
(define-public zig-0.17-bin
  (package
    (name "zig-bin")
    (version "0.17.0")
    (source
     (let ((arch (if (target-aarch64?) "aarch64" "x86_64")))
       (origin
         (method url-fetch)
         (uri (string-append "https://ziglang.org/download/" version
                             "/zig-" arch "-linux-" version ".tar.xz"))
         (sha256
          (base32
           (if (string=? arch "aarch64")
               "1y2g4djkyl6dbimf362pvqfibb933rw34f1af1bvvqsa3mk133cy"
               "09i09s6hhzqxj3g67i7ikvvlwh1sf2v47jnb9k8phsvyybwrvghw"))))))
    (build-system copy-build-system)
    (arguments
     (list
      #:install-plan
      #~'(("zig" "bin/")
          ("lib" "lib/zig")
          ("LICENSE" "share/doc/zig/"))
      #:phases
      #~(modify-phases %standard-phases
          (delete 'strip)
          (delete 'validate-runpath))))
    (supported-systems '("aarch64-linux" "x86_64-linux"))
    (home-page "https://ziglang.org/")
    (synopsis "General purpose programming language and toolchain (binary)")
    (description
     "This package provides the official prebuilt release of the Zig
compiler and toolchain.")
    (license license:expat)))


;;;
;;; Ghostel.
;;;

(define-public emacs-ghostel
  (let* ((version "0.60.0")
         (ghostty-version "1.4.1-dev")
         (ghostty-commit "b2c78c9bd9bc88a16d9c3769b0fb17ddde1b78b1")
         (uucode-commit "ea62149739404a73c202b48a33bf6dd2af4bd9b0")
         (translate-c-commit "02ff0c523fb92a38939b04d0145c4a9602a74d56")
         (aro-commit "d0c8c4d9c55daa7ef6e40cf0f630a5b5e900989b")
         (ghostty-source
          (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/ghostty-org/ghostty")
                  (commit ghostty-commit)))
            (file-name (git-file-name "ghostty" ghostty-commit))
            (sha256
             (base32
              "0dczw3ms81aic6kx37pyxvp902g7vmb837i4znifr8jxrm5hpsqr"))))
         (uucode-source
          (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/jacobsandlund/uucode")
                  (commit uucode-commit)))
            (file-name (git-file-name "uucode" uucode-commit))
            (sha256
             (base32
              "03qmrv9045vx7c0cnhjlmhh9klb9fr0s8flrpyfn912k24kd3akl"))))
         (translate-c-source
          (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://codeberg.org/ziglang/translate-c")
                  (commit translate-c-commit)))
            (file-name (git-file-name "translate-c" translate-c-commit))
            (sha256
             (base32
              "1vbmblhk77y35x6w1b6hn0i5vdf2ray2kh2yhhbi916lcp4qyy6p"))))
         (aro-source
          (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://codeberg.org/ziglang/arocc")
                  (commit aro-commit)))
            (file-name (git-file-name "arocc" aro-commit))
            (sha256
             (base32
              "0wwwfdvyc9ssq885xjc08xs0kgdls48cqlrsw0g2d74sv68i6wkm")))))
    (package
      (name "emacs-ghostel")
      (version version)
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/dakra/ghostel")
               (commit (string-append "v" version))))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0w8787fwfcarcwby1n4b686v53h5hcl2ldc24v7nna07lb6jfpck"))))
      (build-system emacs-build-system)
      (arguments
       (list
        #:lisp-directory "lisp"
        #:tests? #f
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'unpack-zig-dependencies
              (lambda* (#:key inputs #:allow-other-keys)
                (let ((deps (string-append (dirname (getcwd)) "/deps")))
                  (mkdir-p deps)
                  (copy-recursively #$ghostty-source
                                    (string-append deps "/ghostty")
                                    #:log (%make-void-port "w"))
                  (copy-recursively #$uucode-source
                                    (string-append deps "/uucode")
                                    #:log (%make-void-port "w"))
                  (copy-recursively #$translate-c-source
                                    (string-append deps "/translate-c")
                                    #:log (%make-void-port "w"))
                  (copy-recursively #$aro-source
                                    (string-append deps "/aro")
                                    #:log (%make-void-port "w"))
                  (install-file (search-input-file inputs
                                                   "include/wuffs-v0.4.c")
                                (string-append deps "/wuffs/release/c"))
                  (for-each make-file-writable
                            (find-files deps #:directories? #t)))))
            (add-after 'unpack-zig-dependencies 'patch-zig-dependencies
              (lambda _
                (substitute* "ghostel-debug.el"
                  (("\\\\\"/bin/sh\\\\\"") "\\\"sh\\\""))
                (substitute* '("ghostel-shell.el" "ghostel.el")
                  (("\\\\\"/bin/zsh\\\\\"") "\\\"zsh\\\"")
                  (("\\\\\"/bin/bash\\\\\"") "\\\"bash\\\""))
                (let ((root (dirname (getcwd))))
                  (with-directory-excursion root
                    (with-output-to-file "build.zig.zon"
                      (lambda ()
                        (format #t "\
.{
    .name = .ghostel,
    .version = \"~a\",
    .minimum_zig_version = \"0.17.0\",
    .paths = .{\"\"},
    .fingerprint = 0x5a44bdd1198a0f4b,
    .dependencies = .{
        .ghostty = .{ .path = \"./deps/ghostty\" },
    },
}
"
                                #$version)))
                    (with-output-to-file "deps/ghostty/build.zig.zon"
                      (lambda ()
                        (format #t "\
.{
    .name = .ghostty,
    .version = \"~a\",
    .paths = .{\"\"},
    .fingerprint = 0x64407a2a0b4147e5,
    .minimum_zig_version = \"0.17.0\",
    .dependencies = .{
        .android_ndk = .{ .path = \"./pkg/android-ndk\" },
        .apple_sdk = .{ .path = \"./pkg/apple-sdk\" },
        .translate_c = .{ .path = \"./pkg/translate-c\" },
        .uucode = .{ .path = \"../uucode\" },
        .wuffs = .{ .path = \"./pkg/wuffs\" },
    },
}
"
                                #$ghostty-version)))
                    (substitute* "deps/ghostty/pkg/translate-c/build.zig.zon"
                      (("\\.url = .*")
                       ".path = \"../../../translate-c\",\n")
                      (("\\.hash = .*") ""))
                    (substitute* "deps/translate-c/build.zig.zon"
                      (("\\.url = .*") ".path = \"../aro\",\n")
                      (("\\.hash = .*") ""))
                    (with-output-to-file "deps/ghostty/pkg/wuffs/build.zig.zon"
                      (lambda ()
                        (display "\
.{
    .name = .wuffs,
    .version = \"0.0.0\",
    .fingerprint = 0x67c0c059de921c4f,
    .dependencies = .{
        .translate_c = .{ .path = \"../translate-c\" },
        .wuffs = .{ .path = \"../../../wuffs\" },
    },
    .paths = .{\"\"},
}
")))
                    ;; The pixels dependency only provides unit test fixtures.
                    (substitute* "deps/ghostty/pkg/wuffs/build.zig"
                      (("b\\.lazyDependency\\(\"pixels\", \\.\\{\\}\\)")
                       "@as(?*std.Build.Dependency, null)"))

                    ;; Ghostel only needs libghostty-vt.  Ghostty's normal
                    ;; build graph still mentions UI resources, benchmarks,
                    ;; platform packages, and optional SIMD code.  Keep this
                    ;; dependency graph local and small by replacing non-VT
                    ;; helpers with inert stubs and using path dependencies.
                    (substitute* "build.zig"
                      (("ghostty_simd = .*") "ghostty_simd = false;"))
                    (let ((ghostty-exe-pattern
                           (string-append
                            "    if \\(!cfg\\.emit_lib_vt\\) _ = try "
                            "deps\\.add\\(exe\\);")))
                      (substitute* "deps/ghostty/src/build/GhosttyExe.zig"
                        ((ghostty-exe-pattern) "    _ = deps;")))
                    (with-output-to-file
                        "deps/ghostty/src/build/GhosttyBench.zig"
                      (lambda ()
                        (display "\
const GhosttyBench = @This();
const std = @import(\"std\");
const SharedDeps = @import(\"SharedDeps.zig\");

steps: []*std.Build.Step.Compile,

pub fn init(
    b: *std.Build,
    deps: *const SharedDeps,
) !GhosttyBench {
    _ = deps;
    return .{
        .steps = try b.allocator.alloc(
            *std.Build.Step.Compile,
            0,
        ),
    };
}

pub fn install(self: *const GhosttyBench) void {
    _ = self;
}
")))
                    (with-output-to-file
                        "deps/ghostty/src/build/GhosttyFrameData.zig"
                      (lambda ()
                        (display "\
const GhosttyFrameData = @This();
const std = @import(\"std\");
const DistResource = @import(\"GhosttyDist.zig\").Resource;

output: std.Build.LazyPath,

pub fn init(b: *std.Build) !GhosttyFrameData {
    const wf = b.addWriteFiles();
    const zig_file = wf.add(
        \"framedata.zig\",
        \"pub const compressed = \\\"\\\";\\n\",
    );
    return .{ .output = zig_file };
}

pub fn addImport(
    self: *const GhosttyFrameData,
    step: *std.Build.Step.Compile,
) void {
    self.output.addStepDependencies(&step.step);
    step.root_module.addAnonymousImport(
        \"framedata\",
        .{ .root_source_file = self.output },
    );
}

pub fn distResources(b: *std.Build) struct {
    framedata: DistResource,
} {
    const file = b.addWriteFiles().add(
        \"framedata.compressed\",
        \"\",
    );
    return .{
        .framedata = .{
            .dist = \"src/build/framegen/framedata.compressed\",
            .generated = file,
        },
    };
}
")))
                    (with-output-to-file
                        "deps/ghostty/src/build/GhosttyResources.zig"
                      (lambda ()
                        (display "\
const GhosttyResources = @This();
const std = @import(\"std\");
const Config = @import(\"Config.zig\");
const SharedDeps = @import(\"SharedDeps.zig\");

steps: []*std.Build.Step,

pub fn init(
    b: *std.Build,
    cfg: *const Config,
    deps: *const SharedDeps,
) !GhosttyResources {
    _ = cfg;
    _ = deps;
    return .{
        .steps = try b.allocator.alloc(
            *std.Build.Step,
            0,
        ),
    };
}

pub fn install(self: *const GhosttyResources) void {
    _ = self;
}

pub fn addStepDependencies(
    self: *const GhosttyResources,
    other_step: *std.Build.Step,
) void {
    _ = self;
    _ = other_step;
}
")))))))
            (add-after 'patch-el-files 'patch-guix-specific-shell-paths
              (lambda _
                ;; Keep local /bin/sh references patched to the store, but
                ;; do not leak store paths into remote TRAMP and Docker hosts.
                (substitute* "ghostel-shell.el"
                  (("\\(\"docker\" \"[^\"]*/bin/sh\"\\)")
                   "(\"docker\" \"/bin/sh\")"))
                (substitute* "ghostel.el"
                  (("\\(list \"([^\"]*/bin/sh)\" \"-c\"" _ shell)
                   (string-append
                    "(list (if remote-p \"/bin/sh\" \""
                    shell "\") \"-c\"")))))
            (add-after 'patch-zig-dependencies 'build-native-module
              (lambda* (#:key inputs #:allow-other-keys)
                (let ((root (dirname (getcwd))))
                  (with-directory-excursion root
                    (mkdir-p "zig-cache/global")
                    (mkdir-p "zig-cache/local")
                    (setenv "HOME" root)
                    (setenv "EMACS_INCLUDE_DIR"
                            (string-append (assoc-ref inputs "emacs")
                                           "/include"))
                    (setenv "ZIG_GLOBAL_CACHE_DIR"
                            (string-append root "/zig-cache/global"))
                    (setenv "ZIG_LOCAL_CACHE_DIR"
                            (string-append root "/zig-cache/local"))
                    ;; An explicit target makes Zig use its bundled glibc
                    ;; headers instead of probing for native system ones.
                    (invoke "zig" "build"
                            "-Doptimize=ReleaseFast"
                            (string-append
                             "-Dtarget="
                             (car (string-split %host-type #\-))
                             "-linux-gnu")
                            "-Dcpu=baseline")))))
            (add-after 'install 'install-resources
              (lambda* (#:key outputs #:allow-other-keys)
                (let* ((out (assoc-ref outputs "out"))
                       (root (dirname (getcwd)))
                       (elpa-dir (elpa-directory out)))
                  (copy-recursively (string-append root "/etc")
                                    (string-append elpa-dir "/etc"))
                  (for-each (lambda (file)
                              (install-file (string-append root "/zig-out/" file)
                                            elpa-dir))
                            '("ghostel-module.so" "ghostel-module.version"))))))))
      (native-inputs (list wuffs zig-0.17-bin))
      (home-page "https://github.com/dakra/ghostel")
      (synopsis "Terminal emulator powered by libghostty")
      (description
       "Ghostel is an Emacs terminal emulator powered by @code{libghostty-vt},
the VT engine from Ghostty.  It uses a native Zig dynamic module for terminal
state and rendering, while Emacs Lisp manages buffers, processes, keymaps,
shell integration, and user commands.")
      (license (list license:gpl3+
                     license:expat
                     license:asl2.0
                     license:unicode)))))
