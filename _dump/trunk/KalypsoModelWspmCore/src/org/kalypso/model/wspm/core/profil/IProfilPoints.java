/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 * 
 *  and
 *  
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 * 
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 * 
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 * 
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 * 
 *  Contact:
 * 
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *   
 *  ---------------------------------------------------------------------------*/
package org.kalypso.model.wspm.core.profil;

import java.util.LinkedList;

import org.kalypso.model.wspm.core.profil.IProfilPoint.POINT_PROPERTY;


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

  abstract IProfilPoint insertPoint( final IProfilPoint thePointBefore, final IProfilPoint point )
      throws ProfilDataException;

  abstract boolean propertyExists( final POINT_PROPERTY pointProperty );

  abstract boolean removePoint( final IProfilPoint point );

  abstract boolean removeProperty( final POINT_PROPERTY pointProperty );

  abstract LinkedList<IProfilPoint> getPoints( );
}