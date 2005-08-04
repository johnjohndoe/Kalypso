package com.bce.eind.core.profil;

import com.bce.eind.core.profil.IPlainProfil.DEVIDER_TYP;

public interface IProfilDevider
{
  public static enum PROPERTY
  {
    RAUHEIT, BEIWERT, 
  };

  public Object getValueFor( final Object key);

  public void setValueFor( final Object key,final Object value );

  public IProfilPoint getPoint( );

  public DEVIDER_TYP getTyp( );

}
