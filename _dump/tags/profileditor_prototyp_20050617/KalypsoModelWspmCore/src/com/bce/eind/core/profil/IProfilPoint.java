/*
 * Created on 22.02.2005
 */
package com.bce.eind.core.profil;

import java.util.Collection;

/**
 * @author kimwerner
 */
public interface IProfilPoint
{
  public double getValueFor( final ProfilPointProperty pointProperty ) throws ProfilDataException;

  public boolean hasProperty( final ProfilPointProperty pointProperty );

  public IProfilPoint clonePoint( );

  public Collection<ProfilPointProperty> getProperties( );

  public boolean isPosition( final double breite, final double hoehe ) throws ProfilDataException;
}
