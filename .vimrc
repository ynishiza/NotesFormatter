lua<<EOF
local nvim_lsp = require("lspconfig")
yt_lsp_update_settings("hls", {
  filetypes={"haskell", "lhaskell" },
  -- cmd={ "haskell-language-server-wrapper", "--logfile", "/tmp/hls2.log", "--lsp" },
  cmd={ "haskell-language-server-9.4.7~2.2.0.0", "--logfile", "/tmp/hls2.log", "--lsp" },
  settings = {
    haskell = {
      formattingProvider = "fourmolu"
    }
  }
})
EOF
