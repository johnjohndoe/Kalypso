/*
 * Created on 22.02.2005
  */
package com.bce.eind.core.profil;

import com.bce.eind.core.profil.impl.points.ProfilPointProperty;

/**
 * @author kimwerner
 */
public interface IProfilPoint
{
public double getValueFor(final ProfilPointProperty pointProperty) throws ProfilDataException;
public boolean setValueFor(final ProfilPointProperty pointProperty, final double value) throws ProfilDataException;
public boolean hasProperty(final ProfilPointProperty pointProperty);
public IProfilPoint clonePoint();

}
