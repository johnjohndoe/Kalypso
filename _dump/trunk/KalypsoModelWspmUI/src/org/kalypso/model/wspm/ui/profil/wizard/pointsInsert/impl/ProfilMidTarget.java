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

import java.util.LinkedList;

import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.profil.IProfilChange;
import org.kalypso.model.wspm.core.profil.IProfilEventManager;
import org.kalypso.model.wspm.core.profil.IProfilPoint;
import org.kalypso.model.wspm.core.profil.IProfilPointProperty;
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
  public void insertPoints( final IProfilEventManager pem, final LinkedList<IProfilPoint> points )
  {
    final int pointsCount = points.size();
    final IProfilPointProperty[] existingProps = pem.getProfil().getPointProperties();

    final IProfilChange[] changes = new IProfilChange[pointsCount];
    try
    {
      final IProfilPoint activePkt = pem.getProfil().getActivePoint();
      final IProfilPoint targetPkt = (activePkt != null) ? activePkt : pem.getProfil().createProfilPoint();
      final double deltaX = points.getFirst().getValueFor( IWspmConstants.POINT_PROPERTY_BREITE ) - targetPkt.getValueFor( IWspmConstants.POINT_PROPERTY_BREITE );
      final double deltaY = points.getFirst().getValueFor( IWspmConstants.POINT_PROPERTY_HOEHE ) - targetPkt.getValueFor( IWspmConstants.POINT_PROPERTY_HOEHE );
      int i = changes.length - 1;
      for( IProfilPoint point : points )
      {
        final IProfilPoint newPoint = targetPkt.clonePoint();
        newPoint.setValueFor( IWspmConstants.POINT_PROPERTY_BREITE, point.getValueFor( IWspmConstants.POINT_PROPERTY_BREITE ) - deltaX );
        newPoint.setValueFor( IWspmConstants.POINT_PROPERTY_HOEHE, point.getValueFor( IWspmConstants.POINT_PROPERTY_HOEHE ) - deltaY );
        for( IProfilPointProperty prop : existingProps )
        {
          final String propId = prop.toString();
          if( pem.getProfil().hasPointProperty( propId ) && !IWspmConstants.POINT_PROPERTY_BREITE.equals( propId ) && !IWspmConstants.POINT_PROPERTY_HOEHE.equals( propId ) )
          {
            if( point.hasProperty( propId ) )
              newPoint.setValueFor( propId, point.getValueFor( propId ) );
          }
        }
        changes[i--] = new PointAdd( pem.getProfil(), targetPkt, newPoint );
      }
    }
    catch( Exception e )
    {
      // should never happen, stops operation and raise NullPointerException in ProfilOperation.doChange
      changes[0] = null;
    }
    final ProfilOperation operation = new ProfilOperation( "Punkte einfügen", pem, changes, false );
    new ProfilOperationJob( operation ).schedule();
  }
}
