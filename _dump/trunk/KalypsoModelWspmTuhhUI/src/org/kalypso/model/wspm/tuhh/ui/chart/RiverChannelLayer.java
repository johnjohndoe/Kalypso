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
package org.kalypso.model.wspm.tuhh.ui.chart;

import org.eclipse.swt.graphics.Point;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilPointMarker;
import org.kalypso.model.wspm.core.profil.util.ProfilUtil;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.ui.view.ILayerStyleProvider;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;

import de.openali.odysseus.chart.framework.model.layer.EditInfo;

/**
 * @author kimwerner
 */
public class RiverChannelLayer extends PointMarkerLayer
{

  public RiverChannelLayer( IProfil profil, String targetRangeProperty, ILayerStyleProvider styleProvider, int offset, boolean close )
  {
    super( profil, targetRangeProperty, styleProvider, offset, close );
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.ui.chart.PointMarkerLayer#executeDrop(org.eclipse.swt.graphics.Point,
   *      de.openali.odysseus.chart.framework.model.layer.EditInfo)
   */
  @Override
  public void executeDrop( Point point, EditInfo dragStartData )
  {
    final Integer pos = dragStartData.m_data instanceof Integer ? (Integer) (dragStartData.m_data) : -1;
    if( pos > -1 )
    {
      final IProfil profil = getProfil();
      final IRecord profilPoint = profil.getPoint( pos );
      final IProfilPointMarker[] deviders = profil.getPointMarkerFor( profilPoint );
      for( final IProfilPointMarker devider : deviders )
      {
        if( devider.getId().getId().equals( getTargetComponent().getId() ) )
        {
          final IRecord newPoint = ProfilUtil.findNearestPoint( profil, toNumeric( point ).getX() );
          if( newPoint != profilPoint )
          {
            final IComponent roughness = getRoughness();
            if( checkValues( roughness ) )
            {
              final IProfilPointMarker[] trennflaechen = profil.getPointMarkerFor( IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE );
              final IProfilPointMarker[] durchstroemte = profil.getPointMarkerFor( IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE );
              final int index = profil.indexOfProperty( roughness );
              final IRecord p1 = durchstroemte[0].getPoint();
              final IRecord p2 = trennflaechen[0].getPoint();
              final IRecord p3 = durchstroemte[durchstroemte.length - 1].getPoint();
              final Double r1 = (Double) p1.getValue( index );
              final Double r2 = (Double) p2.getValue( index );
              final Double r3 = (Double) p3.getValue( index );
              devider.setPoint( newPoint );
              setRiverChannelRoughness( r1, r2, r3, index );
            }
            else
              devider.setPoint( newPoint );

            profil.setActivePoint( newPoint );
          }
        }
      }
    }
  }

  private final IComponent getRoughness( )
  {
    final IComponent cmpKS = getProfil().hasPointProperty( IWspmConstants.POINT_PROPERTY_RAUHEIT_KS );
    final IComponent cmpKST = getProfil().hasPointProperty( IWspmConstants.POINT_PROPERTY_RAUHEIT_KST );
    if( cmpKS != null && cmpKST != null )
      return null;
    if( cmpKS != null )
      return cmpKS;
    if( cmpKST != null )
      return cmpKST;
    return null;
  }

  protected boolean checkValues( final IComponent roughness )
  {
    if( roughness == null )
      return false;
    final IProfil profil = getProfil();

    final IProfilPointMarker[] trennflaechen = profil.getPointMarkerFor( IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE );

    if( trennflaechen.length < 2 )
      return false;
    return ProfilUtil.RangeIsConstantNumberFor( trennflaechen[0].getPoint(), trennflaechen[trennflaechen.length - 1].getPoint(), roughness );
  }

  private void setRiverChannelRoughness( final Double r1, final Double r2, final Double r3, final int index )

  {
    final IProfil profil = getProfil();

    final IProfilPointMarker[] trennflaechen = profil.getPointMarkerFor( IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE );
    final IProfilPointMarker[] durchstroemte = profil.getPointMarkerFor( IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE );
    final int i1 = profil.indexOfPoint( durchstroemte[0].getPoint() );
    final int i2 = profil.indexOfPoint( trennflaechen[0].getPoint() );
    final int i3 = profil.indexOfPoint( trennflaechen[trennflaechen.length - 1].getPoint() );

    final IRecord[] points = profil.getPoints();

    for( int i = 0; i < i2; i++ )
    {
      if( (Double) (points[i].getValue( index )) != r1 )
        points[i].setValue( index, r1 );
    }
    for( int i = i2; i < i3; i++ )
    {
      if( (Double) (points[i].getValue( index )) != r2 )
        points[i].setValue( index, r2 );
    }
    for( int i = i3; i < points.length-1; i++ )
    {
      if( (Double) (points[i].getValue( index )) != r3 )
        points[i].setValue( index, r3 );
    }
  }
}
