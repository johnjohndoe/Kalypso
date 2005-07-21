package com.bce.eind.core.profil;

import com.bce.eind.core.profil.IPlainProfil.DeviderTyp;

public interface IProfilDevider
{
  public static enum PROPERTY
  {
    RAUHEIT, BEIWERT
  };

  public double getValueFor( final PROPERTY property );

  public void setValueFor( final PROPERTY property, final double value );

  public IProfilPoint getPoint( );

  public DeviderTyp getDeviderTyp( );

}
