{
  perSystem =
    {
      pkgs,
      ...
    }:
    {
      make-shells.default.packages = [
        pkgs.lua
        pkgs.stylua
      ];

      treefmt = {
        programs.stylua = {
          enable = true;
          settings = {
            indent_type = "Tabs";
            indent_width = 4;
          };
        };
      };
    };
}
