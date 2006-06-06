package org.kalypso.model.wspm.core.profil;

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

  public Object getValueFor( final Object key );

  public void setValueFor( final Object key, final Object value );

  public IProfilPoint getPoint( );

  /**
   * @return the old position
   */
  public IProfilPoint setPoint( final IProfilPoint point );

  public DEVIDER_TYP getTyp( );
}
