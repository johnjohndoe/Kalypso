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
public class ProfilEndTarget extends AbstractPointsTarget
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
      final IProfilPoint activePkt = pem.getProfil().getPoints().getLast();
      final IProfilPoint targetPkt = (activePkt != null) ? activePkt : pem.getProfil().getPoints().getLast();
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
          final String propId = prop.getId();
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

    //    
    //    
    // final int pointsCount = points.getPoints().size();
    //
    // final Collection<POINT_PROPERTY> existingProps = pem.getProfil().getPointProperties( false );
    // final Collection<POINT_PROPERTY> newProps = points.getPoints().getFirst().getProperties();
    // Collection<POINT_PROPERTY> propsToAdd = new ArrayList<POINT_PROPERTY>();
    // for( POINT_PROPERTY prop : newProps )
    // {
    // if( !existingProps.contains( prop ) )
    // propsToAdd.add( prop );
    // }
    // final IProfilChange[] changes = new IProfilChange[pointsCount + propsToAdd.size()];
    // int ii = 0;
    // for( POINT_PROPERTY prop : existingProps )
    // {
    // points.addProperty( prop );
    // }
    // for( POINT_PROPERTY prop : propsToAdd )
    // {
    // changes[ii++] = new PointPropertyAdd( pem.getProfil(), prop, 0.0 );
    // }
    // try
    // {
    //
    // final IProfilPoint targetPkt = pem.getProfil().getPoints().getLast();
    // final double deltaX = points.getPoints().getFirst().getValueFor( POINT_PROPERTY.BREITE ) - targetPkt.getValueFor(
    // POINT_PROPERTY.BREITE );
    // final double deltaY = points.getPoints().getFirst().getValueFor( POINT_PROPERTY.HOEHE ) - targetPkt.getValueFor(
    // POINT_PROPERTY.HOEHE );
    // int i = changes.length - 1;
    // for( IProfilPoint point : points.getPoints() )
    // {
    // point.setValueFor( POINT_PROPERTY.BREITE, point.getValueFor( POINT_PROPERTY.BREITE ) - deltaX );
    // point.setValueFor( POINT_PROPERTY.HOEHE, point.getValueFor( POINT_PROPERTY.HOEHE ) - deltaY );
    // changes[i--] = new PointAdd( pem.getProfil(), targetPkt, point );
    // }
    // }
    // catch( IllegalProfileOperationException e )
    // {
    // // should never happen, raise NullPointerException in ProfilOperation.doChange
    // changes[0] = null;
    // }
    // final ProfilOperation operation = new ProfilOperation( "Punkte einfügen", pem, changes );
    // new ProfilOperationJob( operation ).schedule();

  }
}
