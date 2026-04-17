{
  perSystem =
    {
      pkgs,
      ...
    }:
    {
      make-shells.default.packages = [
        (pkgs.emacs.pkgs.withPackages (
          epkgs:
          let
            ox-json-toplevel-properties = epkgs.ox-json.overrideAttrs (_old: {
              src = pkgs.fetchFromGitHub {
                owner = "bitbloxhub";
                repo = "ox-json";
                rev = "toplevel-properties";
                hash = "sha256-u9maLzFVGl1Q5hC53hE81UR5V6Mg9059sKz9TVGdtJw=";
              };
            });
          in
          [
            epkgs.async
            epkgs.elisp-autofmt
            ox-json-toplevel-properties
          ]
        ))
      ];
    };
}
