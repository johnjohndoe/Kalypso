/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
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

import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.RGB;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfileObject;
import org.kalypso.model.wspm.core.profil.changes.PointPropertyRemove;
import org.kalypso.model.wspm.core.profil.changes.ProfileObjectSet;
import org.kalypso.model.wspm.core.profil.util.ProfilUtil;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.ui.profil.operation.ProfilOperation;
import org.kalypso.model.wspm.ui.profil.operation.ProfilOperationJob;
import org.kalypso.model.wspm.ui.view.ILayerStyleProvider;
import org.kalypso.model.wspm.ui.view.chart.AbstractProfilTheme;
import org.kalypso.model.wspm.ui.view.chart.IProfilChartLayer;
import org.kalypso.observation.result.IRecord;

import de.openali.odysseus.chart.framework.model.data.IDataRange;
import de.openali.odysseus.chart.framework.model.figure.impl.PolylineFigure;
import de.openali.odysseus.chart.framework.model.mapper.ICoordinateMapper;
import de.openali.odysseus.chart.framework.model.style.ILineStyle;
import de.openali.odysseus.chart.framework.model.style.impl.LineStyle;

/**
 * @author kimwerner
 */
public class VegetationTheme extends AbstractProfilTheme

{

  /**
   * @see org.kalypso.model.wspm.ui.view.chart.AbstractProfilTheme#removeYourself()
   */
  @Override
  public void removeYourself( )
  {
    final IProfil profil = getProfil();
    final ProfilOperation operation = new ProfilOperation( "Bewuchs entfernen", getProfil(), true );
    operation.addChange( new ProfileObjectSet( profil, new IProfileObject[]{} ) );
    operation.addChange( new PointPropertyRemove( profil, profil.hasPointProperty( IWspmConstants.POINT_PROPERTY_BEWUCHS_AX) ) );
    operation.addChange( new PointPropertyRemove( profil, profil.hasPointProperty( IWspmTuhhConstants.POINT_PROPERTY_BEWUCHS_AY ) ) );
    operation.addChange( new PointPropertyRemove( profil, profil.hasPointProperty( IWspmTuhhConstants.POINT_PROPERTY_BEWUCHS_DP ) ) );
    new ProfilOperationJob( operation ).schedule();
  }

  public VegetationTheme( final IProfilChartLayer[] chartLayers, final ICoordinateMapper cm, final ILayerStyleProvider styleProvider )
  {
    super( IWspmTuhhConstants.LAYER_BEWUCHS, "Bewuchs", chartLayers, cm );
    final ILineStyle lineStyle = styleProvider.getStyleFor( chartLayers[0].getId() + "_LINE", LineStyle.class );
    lineStyle.setColor( new RGB( 0, 255, 0 ) );
    setLineStyle( lineStyle );
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.ui.chart.AbstractProfilTheme#getTargetRange()
   */
  @Override
  public IDataRange<Number> getTargetRange( )
  {
    // this theme will not be calculated, so supress the dimension of vegetation
    return null;
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.ui.chart.AbstractProfilTheme#paint(org.eclipse.swt.graphics.GC)
   */
  @Override
  public void paint( GC gc )
  {

    final IProfil profil = getProfil();

    if( profil == null )
      return;
    final IRecord[] profilPoints = profil.getPoints();
    final int len = profilPoints.length - 2;
    final PolylineFigure pf = new PolylineFigure();

    pf.setStyle( getLineStyle() );
    for( int i = 0; i < len; i++ )
    {
      if( segmenthasVegetation( profilPoints[i] ) )
      {
        final Double y1 = ProfilUtil.getDoubleValueFor( IWspmTuhhConstants.POINT_PROPERTY_HOEHE, profilPoints[i] );
        final Double y2 = ProfilUtil.getDoubleValueFor( IWspmTuhhConstants.POINT_PROPERTY_HOEHE, profilPoints[i + 1] );
        final Double x1 = ProfilUtil.getDoubleValueFor( IWspmTuhhConstants.POINT_PROPERTY_BREITE, profilPoints[i] );
        final Double x2 = ProfilUtil.getDoubleValueFor( IWspmTuhhConstants.POINT_PROPERTY_BREITE, profilPoints[i + 1] );

        final Point p1 = new Point( getDomainAxis().numericToScreen( x1 ), getTargetAxis().numericToScreen( y1 ) -3  );
        final Point p2 = new Point( getDomainAxis().numericToScreen( x2 ), getTargetAxis().numericToScreen( y2 ) - 3 );

        pf.setPoints( new Point[] { p1, p2 } );
        pf.paint( gc );

      }
    }
  }

  final boolean segmenthasVegetation( final IRecord point )
  {
    final Double ax = ProfilUtil.getDoubleValueFor( IWspmTuhhConstants.POINT_PROPERTY_BEWUCHS_AX, point );
    final Double ay = ProfilUtil.getDoubleValueFor( IWspmTuhhConstants.POINT_PROPERTY_BEWUCHS_AY, point );
    final Double dp = ProfilUtil.getDoubleValueFor( IWspmTuhhConstants.POINT_PROPERTY_BEWUCHS_DP, point );
    return !ax.isNaN() && !ay.isNaN() && !dp.isNaN() && ax * ay * dp != 0;
  }
}
