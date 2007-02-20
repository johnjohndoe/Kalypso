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
package org.kalypso.model.wspm.tuhh.ui.resolutions;

import java.util.LinkedList;
import java.util.List;
import java.util.ListIterator;

import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilChange;
import org.kalypso.model.wspm.core.profil.IProfilPoint;
import org.kalypso.model.wspm.core.profil.IProfilPointMarker;
import org.kalypso.model.wspm.core.profil.changes.PointPropertyEdit;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;

/**
 * @author kimwerner
 */

public class AddBewuchsResolution extends AbstractProfilMarkerResolution
{
  final private Integer m_deviderIndex;

  /**
   * @see org.kalypso.model.wspm.tuhh.ui.resolutions.AbstractProfilMarkerResolution#resolve(org.kalypso.model.wspm.core.profil.IProfil,
   *      org.eclipse.core.resources.IMarker)
   */
  @Override
  protected IProfilChange[] resolve( IProfil profil )
  {
    final LinkedList<IProfilPoint> points = profil.getPoints();
    if( points.isEmpty() )
    {
      return null;
    }
    final IProfilPointMarker[] deviders = profil.getPointMarkerFor( IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE );
    if (deviders.length < 2) 
    {
      return null;
    }
    IProfilPointMarker devider = null;
    IProfilPoint point = null;
    final double[] params = new double[] { 0.0, 0.0, 0.0 };
    try
    {
      devider = deviders[m_deviderIndex];
    }
    catch( ArrayIndexOutOfBoundsException e )
    {
      return null;
    }
    if( m_deviderIndex == 0 )
    {
      final int i = points.indexOf( devider.getPoint() );
      final List<IProfilPoint> leftPts = points.subList( 0, i );
      for( final IProfilPoint pt : leftPts )
      {
        point = pt;
        try
        {
          final Double ax = pt.getValueFor( IWspmTuhhConstants.POINT_PROPERTY_BEWUCHS_AX );
          final Double ay = pt.getValueFor( IWspmTuhhConstants.POINT_PROPERTY_BEWUCHS_AY );
          final Double dp = pt.getValueFor( IWspmTuhhConstants.POINT_PROPERTY_BEWUCHS_DP );
          if( ax != 0.0 )
          {
            params[0] = ax;
          }
          if( ay != 0.0 )
          {
            params[1] = ay;
          }
          if( dp != 0.0 )
          {
            params[2] = dp;
          }
        }
        catch( Exception e )
        {
          point = null;
        }
      }
    }
    if( m_deviderIndex > 0 )
    {
      point = devider.getPoint();
      final int i = points.indexOf( point );
      final ListIterator<IProfilPoint> rightPts = points.listIterator( i );
      for( IProfilPoint pt; rightPts.hasNext(); )
      {
        pt = rightPts.next();
        try
        {
          final double ax = pt.getValueFor( IWspmTuhhConstants.POINT_PROPERTY_BEWUCHS_AX );
          final double ay = pt.getValueFor( IWspmTuhhConstants.POINT_PROPERTY_BEWUCHS_AY );
          final double dp = pt.getValueFor( IWspmTuhhConstants.POINT_PROPERTY_BEWUCHS_DP );
          if( params[0] == 0.0 )
          {
            params[0] = ax;
          }
          if( params[1] == 0.0 )
          {
            params[1] = ay;
          }
          if( params[2] == 0.0 )
          {
            params[2] = dp;
          }
        }
        catch( Exception e )
        {
          point = null;
        }
      }
    }
    if( point == null )
    {
      return null;
    }
    if( params[0] * params[1] * params[2] == 0.0 )
    {
      return null;
    }
    final IProfilChange[] changes = new IProfilChange[3];
    changes[0] = new PointPropertyEdit( point, IWspmTuhhConstants.POINT_PROPERTY_BEWUCHS_AX, params[0] );
    changes[1] = new PointPropertyEdit( point, IWspmTuhhConstants.POINT_PROPERTY_BEWUCHS_AY, params[1] );
    changes[2] = new PointPropertyEdit( point, IWspmTuhhConstants.POINT_PROPERTY_BEWUCHS_DP, params[2] );
    return changes;
  }

  public AddBewuchsResolution( final Integer deviderIndex )
  {
    super( "Bewuchsparameter an der Trennfläche erzeugen", null, null );
    m_deviderIndex = deviderIndex;
  }

}
