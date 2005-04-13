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
public double getValueFor(final IProfilPointProperty pointProperty) throws ProfilDataException;
public boolean setValueFor(final IProfilPointProperty pointProperty, final double value) throws ProfilDataException;
public boolean hasProperty(final IProfilPointProperty pointProperty);
public IProfilPoint clonePoint();
public Collection<IProfilPointProperty> getProperties();

}
