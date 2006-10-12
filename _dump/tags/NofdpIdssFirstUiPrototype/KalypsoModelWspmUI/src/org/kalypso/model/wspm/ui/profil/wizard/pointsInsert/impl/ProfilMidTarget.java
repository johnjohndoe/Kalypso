/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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
package org.kalypso.model.wspm.ui.profil.wizard.pointsInsert.impl;

import java.util.Collection;

import org.kalypso.model.wspm.core.profil.IProfilChange;
import org.kalypso.model.wspm.core.profil.IProfilEventManager;
import org.kalypso.model.wspm.core.profil.IProfilPoint;
import org.kalypso.model.wspm.core.profil.IProfilPoints;
import org.kalypso.model.wspm.core.profil.ProfilDataException;
import org.kalypso.model.wspm.core.profil.IProfilPoint.POINT_PROPERTY;
import org.kalypso.model.wspm.core.profil.changes.PointAdd;
import org.kalypso.model.wspm.ui.profil.operation.ProfilOperation;
import org.kalypso.model.wspm.ui.profil.operation.ProfilOperationJob;
import org.kalypso.model.wspm.ui.profil.wizard.pointsInsert.AbstractPointsTarget;


/**
 * @author Belger
 */
public class ProfilMidTarget extends AbstractPointsTarget
{
  /**
   * @see org.kalypso.model.wspm.ui.profil.wizard.pointsInsert.IPointsTarget#insertPoints(org.kalypso.model.wspm.core.profil.impl.ProfilEventManager,
   *      IProfilPoints)
   */
  public void insertPoints( final IProfilEventManager pem, final IProfilPoints points )
  {
    final int pointsCount = points.getPoints().size();
    final Collection<POINT_PROPERTY> existingProps = pem.getProfil().getPointProperties( false );
    
    final IProfilChange[] changes = new IProfilChange[pointsCount];
    try
    {
      final IProfilPoint activePkt = pem.getProfil().getActivePoint();
      final IProfilPoint targetPkt = (activePkt != null) ? activePkt : pem.getProfil().getPoints().getLast();
      final double deltaX = points.getPoints().getFirst().getValueFor( POINT_PROPERTY.BREITE ) - targetPkt.getValueFor( POINT_PROPERTY.BREITE );
      final double deltaY = points.getPoints().getFirst().getValueFor( POINT_PROPERTY.HOEHE ) - targetPkt.getValueFor( POINT_PROPERTY.HOEHE );
      int i = changes.length - 1;
      for( IProfilPoint point : points.getPoints() )
      {
        final IProfilPoint newPoint = targetPkt.clonePoint();
        newPoint.setValueFor(POINT_PROPERTY.BREITE , point.getValueFor( POINT_PROPERTY.BREITE ) - deltaX);
        newPoint.setValueFor(POINT_PROPERTY.HOEHE , point.getValueFor( POINT_PROPERTY.HOEHE ) - deltaY );
        for( POINT_PROPERTY prop : existingProps )
        {
          if( points.propertyExists( prop ) && (prop != POINT_PROPERTY.BREITE)&& (prop != POINT_PROPERTY.HOEHE))
           {
            newPoint.setValueFor( prop, point.getValueFor( prop ) );
          }
        }
        changes[i--] = new PointAdd( pem.getProfil(), targetPkt,  newPoint );
      }
    }
    catch( ProfilDataException e )
    {
      // should never happen, stops operation and raise NullPointerException in ProfilOperation.doChange
      changes[0] = null;
    }
    final ProfilOperation operation = new ProfilOperation( "Punkte einfügen", pem, changes, false );
    new ProfilOperationJob( operation ).schedule();
  }
}
