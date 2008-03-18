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
package org.kalypso.model.wspm.ui.profil.wizard.pointsInsert.impl;

import java.util.List;

import org.apache.commons.lang.ArrayUtils;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilChange;
import org.kalypso.model.wspm.core.profil.changes.PointAdd;
import org.kalypso.model.wspm.core.profil.util.ProfilObsHelper;
import org.kalypso.model.wspm.ui.Messages;
import org.kalypso.model.wspm.ui.profil.operation.ProfilOperation;
import org.kalypso.model.wspm.ui.profil.operation.ProfilOperationJob;
import org.kalypso.model.wspm.ui.profil.wizard.pointsInsert.AbstractPointsTarget;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;

/**
 * @author kimwerner
 */
public class ProfilStartTarget extends AbstractPointsTarget
{

  /**
   * @see org.kalypso.model.wspm.ui.profil.wizard.pointsInsert.IPointsTarget#insertPoints(org.kalypso.model.wspm.core.profil.impl.ProfilEventManager,
   *      IProfilPoints)
   */
  public void insertPoints( final IProfil profile, final List<IRecord> points )
  {
    if( points != null )
      insertPointsInternal( profile, points );
    else
      addPointInternal( profile );
  }

  private final void addPointInternal( final IProfil profile )
  {
    final TupleResult result = profile.getResult();
    IRecord record = null;
    if( result.size() > 0 )
      record = result.get( 0 );
    else if( record == null )
    {
      record = result.createRecord();
      result.add( 0, record );

      return;
    }

    final IRecord myPoint = record.cloneRecord();

    /* shift new point to an position located before old first point position */
    final IComponent cBreite = ProfilObsHelper.getPropertyFromId( myPoint, IWspmConstants.POINT_PROPERTY_BREITE );
    if( cBreite != null )
      myPoint.setValue( cBreite, (Double) myPoint.getValue( cBreite ) - 10 );

    // remove all markers from new point
    final IComponent[] pointMarkerTypes = profile.getPointMarkerTypes();
    for( final IComponent pmt : pointMarkerTypes )
      myPoint.setValue( pmt, null );

    result.add( 0, myPoint );
  }

  public void insertPointsInternal( final IProfil profile, final List<IRecord> points )
  {
    if( points == null )
    {

    }
    else
    {
      final int pointsToAdd = points.size();
      final IComponent[] existingProps = profile.getPointProperties();

      final IProfilChange[] changes = new IProfilChange[pointsToAdd];
      try
      {
        final IRecord[] existingPoints = profile.getPoints();
        final IRecord targetPkt = existingPoints.length == 0 ? null : existingPoints[0];
        final double deltaX = targetPkt == null ? 0.0
            : (Double) points.get( points.size() - 1 ).getValue( ProfilObsHelper.getPropertyFromId( points.get( points.size() - 1 ), IWspmConstants.POINT_PROPERTY_BREITE ) )
                - (Double) targetPkt.getValue( ProfilObsHelper.getPropertyFromId( targetPkt, IWspmConstants.POINT_PROPERTY_BREITE ) );
        final double deltaY = targetPkt == null ? 0.0
            : (Double) points.get( points.size() - 1 ).getValue( ProfilObsHelper.getPropertyFromId( points.get( points.size() - 1 ), IWspmConstants.POINT_PROPERTY_HOEHE ) )
                - (Double) targetPkt.getValue( ProfilObsHelper.getPropertyFromId( targetPkt, IWspmConstants.POINT_PROPERTY_HOEHE ) );
        int i = pointsToAdd - 1;
        for( final IRecord point : points )
        {

          final IRecord newPoint = targetPkt == null ? profile.createProfilPoint() : targetPkt.cloneRecord();
          newPoint.setValue( ProfilObsHelper.getPropertyFromId( newPoint, IWspmConstants.POINT_PROPERTY_BREITE ), (Double) point.getValue( ProfilObsHelper.getPropertyFromId( point, IWspmConstants.POINT_PROPERTY_BREITE ) )
              - deltaX );
          newPoint.setValue( ProfilObsHelper.getPropertyFromId( newPoint, IWspmConstants.POINT_PROPERTY_HOEHE ), (Double) point.getValue( ProfilObsHelper.getPropertyFromId( point, IWspmConstants.POINT_PROPERTY_HOEHE ) )
              - deltaY );
          for( final IComponent prop : existingProps )
            if( profile.hasPointProperty( prop ) && !IWspmConstants.POINT_PROPERTY_BREITE.equals( prop.getId() ) && !IWspmConstants.POINT_PROPERTY_HOEHE.equals( prop.getId() ) )
            {
              final IComponent[] components = point.getOwner().getComponents();

              if( ArrayUtils.contains( components, point ) )
                newPoint.setValue( prop, point.getValue( prop ) );
            }
          changes[i--] = new PointAdd( profile, null, newPoint );
        }
      }
      catch( final Exception e )
      {
        // should never happen, stops operation and raise NullPointerException in ProfilOperation.doChange
        changes[0] = null;
      }
      final ProfilOperation operation = new ProfilOperation( Messages.ProfilStartTarget_0, profile, changes, false );
      new ProfilOperationJob( operation ).schedule();
    }

  }
}
