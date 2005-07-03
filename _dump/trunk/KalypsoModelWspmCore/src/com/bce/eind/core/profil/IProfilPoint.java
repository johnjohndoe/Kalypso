/*
 * Created on 22.02.2005
 */
package com.bce.eind.core.profil;

import java.util.Collection;

/**
 * Kim: Document the interface methods
 * 
 * @author kimwerner
 */
public interface IProfilPoint
{
  /**
   * @param pointProperty
   * @return
   * @throws ProfilDataException
   */
  public double getValueFor( final ProfilPointProperty pointProperty ) throws ProfilDataException;

  /**
   * @param pointProperty
   * @return
   */
  public boolean hasProperty( final ProfilPointProperty pointProperty );

  public IProfilPoint clonePoint( );

  public Collection<ProfilPointProperty> getProperties( );

  /** Kim: Doc */
  public boolean isPosition( final double breite, final double hoehe ) throws ProfilDataException;
}
