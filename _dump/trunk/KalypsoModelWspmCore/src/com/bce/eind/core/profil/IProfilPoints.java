package com.bce.eind.core.profil;

import java.util.LinkedList;

import com.bce.eind.core.profil.IProfilPoint.POINT_PROPERTY;

public interface IProfilPoints
{
  /**
   * @return a valid ProfilPoint if operation succeeds, othwerwise null
   */
  abstract IProfilPoint addPoint( final double breite, final double hoehe );

  abstract IProfilPoint addPoint( final IProfilPoint thePointBefore );

  abstract void addProperty( final POINT_PROPERTY pointProperty );

  abstract POINT_PROPERTY[] getDependenciesFor( final POINT_PROPERTY pointProperty );

  abstract LinkedList<POINT_PROPERTY> getExistingProperties( );

  abstract LinkedList<POINT_PROPERTY> getVisibleProperties( );

  abstract boolean insertPoint( final IProfilPoint thePointBefore, final IProfilPoint point )
      throws ProfilDataException;

  abstract boolean propertyExists( final POINT_PROPERTY pointProperty );

  abstract boolean removePoint( final IProfilPoint point );

  abstract boolean removeProperty( final POINT_PROPERTY pointProperty );

  abstract LinkedList<IProfilPoint> getPoints( );
}