{ obelisk ? import ./.obelisk/impl {
    system = builtins.currentSystem;
    iosSdkVersion = "10.2";
    # You must accept the Android Software Development Kit License Agreement at
    # https://developer.android.com/studio/terms in order to build Android apps.
    # Uncomment and set this to `true` to indicate your acceptance:
    config.android_sdk.accept_license = true;
  }
}:
with obelisk;
project ./. ({...}: {
  android.applicationId = "systems.obsidian.obelisk.examples.minimal";
  android.displayName = "Obelisk Minimal Example";
  ios.bundleIdentifier = "systems.obsidian.obelisk.examples.minimal";
  ios.bundleName = "Obelisk Minimal Example";
  overrides = self: super:
    let nixpkgs = import <nixpkgs> {};
        higgledyPkg = nixpkgs.haskellPackages.higgledy;
    in {
    reflex-dom-storage =
        super.callCabal2nix "reflex-dom-storage" ../reflex-dom-storage {};
    # higgledy = self.callCabal2nix "higgledy";
    higgledy = higgledyPkg;
  };
})
