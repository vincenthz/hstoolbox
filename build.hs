import Development.Shake
import Development.Shake.FilePath

distDir = "dist/"

opts = shakeOptions
    { shakeFiles    = distDir
    , shakeProgress = progressSimple
    }

main = shake opts $ do
    phony "clean" $ removeFilesAfter distDir ["//*"]
    want ["gitcache"]

    "gitcache" *> \out -> do
        let hs = "Tools/gitcache.hs"
        need [hs]
        cmd "ghc" "-O2" "--make" "-o" ["gitcache", hs]
