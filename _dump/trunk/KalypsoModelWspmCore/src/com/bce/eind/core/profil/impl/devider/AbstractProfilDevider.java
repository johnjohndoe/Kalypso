package com.bce.eind.core.profil.impl.devider;

import com.bce.eind.core.profil.IProfilDevider;
import com.bce.eind.core.profil.IProfilPoint;
import com.bce.eind.core.profil.IPlainProfil.DeviderTyp;

public abstract class AbstractProfilDevider implements IProfilDevider
{

  public AbstractProfilDevider( )
  {
    super();
    // TODO Auto-generated constructor stub
  }

  public double getValueFor( PROPERTY property )
  {
    // TODO Auto-generated method stub
    return 0;
  }

  public void setValueFor( PROPERTY property, double value )
  {
    // TODO Auto-generated method stub

  }

  public IProfilPoint getPoint( )
  {
    // TODO Auto-generated method stub
    return null;
  }

  public DeviderTyp getDeviderTyp( )
  {
    // TODO Auto-generated method stub
    return null;
  }

}
