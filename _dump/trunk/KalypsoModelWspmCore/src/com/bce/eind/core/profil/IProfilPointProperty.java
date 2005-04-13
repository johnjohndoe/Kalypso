package com.bce.eind.core.profil;

public interface IProfilPointProperty
{
  public boolean isVisible( );
  public String getLabel( );
  public boolean isOptional( );
  public boolean isInterpolation( );
  public void setInterpolation( boolean interpolation );
  public int getPrecision( );
  public void setPrecision( int precision );
}
