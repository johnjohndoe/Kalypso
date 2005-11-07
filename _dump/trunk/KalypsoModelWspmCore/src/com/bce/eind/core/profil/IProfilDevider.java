package com.bce.eind.core.profil;



public interface IProfilDevider
{
  public static enum DEVIDER_TYP
  {
    BORDVOLL, DURCHSTROEMTE, TRENNFLAECHE, WEHR;
  }

  public static enum DEVIDER_PROPERTY
  {
    BOESCHUNG, RAUHEIT, BEIWERT
  };
  public Object getValueFor( final Object key);

  public IProfilPoint getPoint( );

  public DEVIDER_TYP getTyp( );
  


}
