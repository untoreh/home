self: super:
{
   pies = super.pies.overrideAttrs ( old: rec {
   name = "pies-1.5";
   src = self.fetchurl {
    	url = "mirror://gnu/pies/${name}.tar.bz2";
    	sha256 = "0e738105b4d4cd7881e3a00df64c844f852feab4e755de8a54366a4931324186";
	  };
  });
}
