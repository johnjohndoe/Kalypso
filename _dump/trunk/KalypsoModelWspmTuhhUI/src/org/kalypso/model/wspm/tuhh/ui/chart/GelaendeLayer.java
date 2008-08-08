/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraï¿½e 22
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

import java.util.ArrayList;

import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.graphics.Rectangle;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilChange;
import org.kalypso.model.wspm.core.profil.changes.PointAdd;
import org.kalypso.model.wspm.core.profil.changes.PointPropertyEdit;
import org.kalypso.model.wspm.core.profil.changes.ProfilChangeHint;
import org.kalypso.model.wspm.core.profil.util.ProfilUtil;
import org.kalypso.model.wspm.tuhh.ui.panel.GelaendePanel;
import org.kalypso.model.wspm.ui.featureview.PolynomeChartLayer;
import org.kalypso.model.wspm.ui.view.IProfilView;
import org.kalypso.model.wspm.ui.view.ProfilViewData;
import org.kalypso.model.wspm.ui.view.chart.IProfilChartLayer;
import org.kalypso.model.wspm.ui.view.chart.ProfilChartView;
import org.kalypso.observation.result.IRecord;

import de.openali.odysseus.chart.framework.model.mapper.IAxis;
import de.openali.odysseus.chart.framework.model.style.IStyleConstants.LINECAP;
import de.openali.odysseus.chart.framework.model.style.IStyleConstants.LINEJOIN;
import de.openali.odysseus.chart.framework.model.style.impl.LineStyle;
import de.openali.odysseus.chart.framework.model.style.impl.PointStyle;

/**
 * @author kimwerner
 */
public class GelaendeLayer extends PolynomeChartLayer implements IProfilChartLayer//AbstractPolyLineLayer
{
  private final ProfilChartView m_pcv;

  public GelaendeLayer( final ProfilChartView pcv )
  {
                               
     
    
    super(null,4,new LineStyle(1, new RGB(0,0,0), 0, 0,null, LINEJOIN.MITER , LINECAP.FLAT ,  1, true),new PointStyle(new LineStyle(1, new RGB(0,0,0), 0, 0,null, LINEJOIN.MITER , LINECAP.FLAT ,  1, true),1,1,0,new RGB(0,0,0),null,true),true);
    
    m_pcv = pcv;     
    
  
 //   super( IWspmTuhhConstants.LAYER_GELAENDE, "Gelände", pcv, pcv.getDomainRange(), pcv.getValueRangeLeft(), new String[] { IWspmConstants.POINT_PROPERTY_HOEHE }, true, true, true );
//    setColors( setColor( pcv.getColorRegistry() ) );
  }

//  private final Color[] setColor( final ColorRegistry cr )
//  {
//    if( !cr.getKeySet().contains( IWspmTuhhConstants.LAYER_GELAENDE ) )
//      cr.put( IWspmTuhhConstants.LAYER_GELAENDE, new RGB( 255, 150, 0 ) );
//    return new Color[] { cr.get( IWspmTuhhConstants.LAYER_GELAENDE ) };
//  }

 
  public IProfilView createLayerPanel( final IProfil profile, final ProfilViewData viewData )
  {
    return new GelaendePanel( profile, viewData );
  }

 
//  public IRecord[] getPoints( )
//  {
//    return m_pcv.getProfil().getPoints();
//  }

  /**
   * @see com.bce.eind.core.profil.IProfilListener#onProfilChanged(com.bce.eind.core.profil.changes.ProfilChangeHint,
   *      com.bce.eind.core.profil.IProfilChange[])
   */
 
  public void onProfilChanged( final ProfilChangeHint hint, final IProfilChange[] changes )
  {
    // FIXME - insert new point at start or end of profile - chart view resized() event must be generated

    if( !(hint.isPointValuesChanged() || hint.isPointsChanged()) )
      return;
    

    final Double left = m_pcv.getDomainRange().getNumericRange().getMin().doubleValue();
    final Double right = m_pcv.getDomainRange().getNumericRange().getMax().doubleValue();
    final Double top = m_pcv.getValueRangeLeft().getNumericRange().getMax().doubleValue();
    final Double bottom = m_pcv.getValueRangeLeft().getNumericRange().getMin().doubleValue();
    for( final IProfilChange change : changes )
      if( change instanceof PointPropertyEdit || change instanceof PointAdd )
        for( final IRecord point : (IRecord[]) change.getObjects() )
        {
          final Double hoehe = ProfilUtil.getDoubleValueFor( IWspmConstants.POINT_PROPERTY_HOEHE, point );
          final Double breite = ProfilUtil.getDoubleValueFor( IWspmConstants.POINT_PROPERTY_BREITE, point );
          if( hoehe.isNaN() || breite.isNaN() )
            return;

          if( breite > right || breite < left || hoehe > top || hoehe < bottom )
          {
            //TODO KIM: tu das auch
//            m_valueRange.setLogicalRange( new LogicalRange( Math.min( hoehe, bottom ), Math.max( hoehe, top ) ) );
//            m_domainRange.setLogicalRange( new LogicalRange( Math.min( breite, left ), Math.max( breite, right ) ) );
          }
        }
  }


  public void paintLegend( final GC gc )
  {
    final Rectangle clipping = gc.getClipping();

    final int left = clipping.x;
    final int top = clipping.y;
    final int right = clipping.x + clipping.width;
    final int bottom = clipping.y + clipping.width;
    final int midx = (left + right) / 2;
    final int midy = (top + bottom) / 2;

   // drawStationline( gc, midx, midy, midx, bottom );
    gc.setLineWidth( 1 );
    gc.setLineStyle( SWT.LINE_SOLID );
   // gc.setForeground( m_colors[0] );
    gc.drawOval( midx - 2, midy - 2, 4, 4 );
    gc.drawLine( left, top, midx, midy );
    gc.drawLine( midx, midy, right, midy );
  }

  /**
   * @see IProfilChartLayer#removeYourself()
   */
  public void removeYourself( )
  {
    throw new UnsupportedOperationException();
  }

  @Override
  public String toString( )
  {
    return "Geländehöhe";
  }

  

  

 
  /**
   * @see org.kalypso.model.wspm.ui.view.chart.IProfilChartLayer#getProfilChartView()
   */
  public ProfilChartView getProfilChartView( )
  {
 
    return m_pcv;
  }

 
  /**
   * @see org.kalypso.swtchart.chart.layer.IChartLayer#paint(org.kalypso.contribs.eclipse.swt.graphics.GCWrapper,
   *      org.eclipse.swt.graphics.Device)
   */
  @SuppressWarnings("unchecked")
  public void paint( final GC gc )
  {
//    final IDataRange<Number> domainRange = m_data.getDomainRange();
//    final double min = (Double) domainRange.getMin();
//    final double max = (Double) domainRange.getMax();
//
//    PolylineFigure plf = getPolylineFigure();
//    PointFigure pf = getPointFigure();
//
    final ArrayList<Point> path = new ArrayList<Point>();

    final IAxis domainAxis = getDomainAxis();
    final IAxis targetAxis = getTargetAxis();

    final double logical0 = domainAxis.screenToNumeric( 0 ).doubleValue();
    final double logical1 = domainAxis.screenToNumeric( 1 ).doubleValue();
    final double logicalPixelWidth = Math.abs( logical0 - logical1 );

    final double tick = logicalPixelWidth * 5;//m_pixelsPerTick;
final IRecord[] points = m_pcv.getProfil().getPoints();
    
    
    for( double pos = 0; pos < points.length; pos += tick )
    {
//      final IPolynomial1D poly = PolynomialUtilities.getPoly( m_data.getPolyArray(), pos );
//      if( poly == null )
//        continue;
//
//      final double value = poly.computeResult( pos );

//      final int x = domainAxis.numericToScreen( pos );
//      final int y = targetAxis.numericToScreen( value );
//      path.add( new Point( x, y ) );
    }

//    plf.setPoints( path.toArray( new Point[] {} ) );
//    plf.paint( gc );
//
//    if( true)//m_showPoints )
//    {
//      pf.setPoints( path.toArray( new Point[] {} ) );
//      pf.paint( gc );
//    }

  }
  
}
