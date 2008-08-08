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

import java.util.ArrayList;

import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.graphics.Rectangle;
import org.kalypso.contribs.eclipse.swt.graphics.RectangleUtils;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilChange;
import org.kalypso.model.wspm.core.profil.changes.ProfilChangeHint;
import org.kalypso.model.wspm.core.profil.util.ProfilUtil;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.ui.panel.GelaendePanel;
import org.kalypso.model.wspm.ui.view.IProfilView;
import org.kalypso.model.wspm.ui.view.chart.IProfilChartLayer;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;

import de.openali.odysseus.chart.ext.base.layer.AbstractLineLayer;
import de.openali.odysseus.chart.framework.model.data.IDataRange;
import de.openali.odysseus.chart.framework.model.data.impl.DataRange;
import de.openali.odysseus.chart.framework.model.figure.impl.PointFigure;
import de.openali.odysseus.chart.framework.model.figure.impl.PolylineFigure;
import de.openali.odysseus.chart.framework.model.layer.EditInfo;
import de.openali.odysseus.chart.framework.model.layer.IEditableChartLayer;
import de.openali.odysseus.chart.framework.model.layer.ILegendEntry;
import de.openali.odysseus.chart.framework.model.layer.impl.LegendEntry;
import de.openali.odysseus.chart.framework.model.mapper.ICoordinateMapper;
import de.openali.odysseus.chart.framework.model.style.ILineStyle;
import de.openali.odysseus.chart.framework.model.style.IPointStyle;

/**
 * @author kimwerner
 */
public class TestProfilLayer extends AbstractLineLayer implements IProfilChartLayer, IEditableChartLayer
{

  

  private RGB m_colorActivePoint = new RGB( 255, 0, 255 );

  private RGB m_colorActiveLine = new RGB( 255, 255, 0 );

  private RGB m_colorDefault = new RGB( 0, 0, 0 );

  private IProfil m_profil;

  protected final static String TOOLTIP_FORMAT = "%-12s %10.4f [m]%n%-12s %10.4f [%s]";

  public TestProfilLayer( final IProfil profil, ILineStyle lineStyle, IPointStyle pointStyle )
  {
    super( lineStyle, pointStyle );
    m_profil = profil;
  }

  /**
   * @see de.openali.odysseus.chart.framework.model.layer.IChartLayer#getDomainRange()
   */
  public IDataRange<Number> getDomainRange( )
  {

    if( m_profil == null )
      return new DataRange<Number>( 0, 100 );
    final IComponent comp = m_profil.hasPointProperty( IWspmConstants.POINT_PROPERTY_BREITE );
    final Double max = ProfilUtil.getMaxValueFor( m_profil, comp );
    final Double min = ProfilUtil.getMinValueFor( m_profil, comp );
    return new DataRange<Number>( min, max );
  }

  /**
   * @see de.openali.odysseus.chart.framework.model.layer.IChartLayer#getTargetRange()
   */
  public IDataRange<Number> getTargetRange( )
  {

    if( m_profil == null )
      return new DataRange<Number>( 0, 100 );
    final IComponent comp = m_profil.hasPointProperty( IWspmConstants.POINT_PROPERTY_HOEHE );
    final Double max = ProfilUtil.getMaxValueFor( m_profil, comp );
    final Double min = ProfilUtil.getMinValueFor( m_profil, comp );
    return new DataRange<Number>( min, max );
  }

  /**
   * @see de.openali.odysseus.chart.framework.model.layer.IChartLayer#paint(org.eclipse.swt.graphics.GC)
   */
  public void paint( GC gc )
  {

    if( m_profil == null )
      return;
    final IRecord[] profilPoints = m_profil.getPoints();
    final int len = profilPoints.length;
    final Point[] points = new Point[len];
    final ICoordinateMapper cm = getCoordinateMapper();
    final int active = m_profil.indexOfPoint( m_profil.getActivePoint() );

    for( int i = 0; i < len; i++ )
    {
      points[i] = cm.numericToScreen( ProfilUtil.getDoubleValueFor( IWspmConstants.POINT_PROPERTY_BREITE, profilPoints[i] ), ProfilUtil.getDoubleValueFor( IWspmConstants.POINT_PROPERTY_HOEHE, profilPoints[i] ) );
    }

    final PolylineFigure pf = getPolylineFigure();
    ILineStyle style = pf.getStyle();
    style.setColor( m_colorDefault );
    //style.setDash( 1f, new float[] { 1, 1 } );
    final int alfa =  style.getAlpha();
    style.setAlpha( alfa / 4 );
    for( int i = 0; i < len; i++ )
    {
      final int x = points[i].x;
      final int y = cm.getTargetAxis().getScreenHeight();
      pf.setPoints( new Point[] { new Point( x, 0 ), new Point( x, y ) } );
      pf.paint( gc );
    }
    style.setAlpha( alfa );
    //style.setDash( 0f, new float[] {} );

    style.setColor( m_colorLine );
    pf.setPoints( points );
    pf.paint( gc );
    if( active < len - 1 )
    {
      style.setColor( m_colorActiveLine );
      pf.setPoints( new Point[] { points[active], points[active + 1] } );
      pf.paint( gc );
      style.setColor( m_colorLine );
    }
    PointFigure pf2 = getPointFigure();
    IPointStyle style2 = pf2.getStyle();
    style2.setInlineColor( m_colorPoint );
    pf2.setPoints( points );
    pf2.paint( gc );
    if( active < len - 1 )
    {
      style2.setInlineColor( m_colorActivePoint );
      pf2.setPoints( new Point[] { points[active] } );
      pf2.paint( gc );
      style2.setInlineColor( m_colorPoint );
    }

  }

  /**
   * @see org.kalypso.model.wspm.ui.view.chart.IProfilChartLayer#createLayerPanel(org.kalypso.model.wspm.core.profil.IProfil)
   */
  public IProfilView createLayerPanel( IProfil profile )
  {
    return new GelaendePanel( profile );
  }

  @Override
  public ILegendEntry[] createLegendEntries( )
  {

    ArrayList<ILegendEntry> entries = new ArrayList<ILegendEntry>();
    ILineStyle ls = getPolylineFigure().getStyle();
    if( ls.isVisible() )
    {
      ls.setColor( m_colorLine );
      LegendEntry le = new LegendEntry( this, ls.getTitle() )
      {

        @Override
        public void paintSymbol( GC gc, Point size )
        {
          int sizeX = size.x;
          int sizeY = size.y;

          final ArrayList<Point> path = new ArrayList<Point>();
          path.add( new Point( 0, sizeX / 2 ) );
          path.add( new Point( sizeX / 5, sizeY / 2 ) );
          path.add( new Point( sizeX / 5 * 2, sizeY / 4 ) );
          path.add( new Point( sizeX / 5 * 3, sizeY / 4 * 3 ) );
          path.add( new Point( sizeX / 5 * 4, sizeY / 2 ) );
          path.add( new Point( sizeX, sizeY / 2 ) );
          drawLine( gc, path );

          gc.setForeground( new Color( null, m_colorDefault ) );
          final Rectangle clipping = gc.getClipping();
          gc.setClipping( clipping.x + 1, clipping.y + 1, clipping.width - 1, clipping.height - 1 );
          gc.setClipping( clipping );
          gc.setLineWidth( 1 );
          gc.drawRectangle( clipping.x, clipping.y, clipping.width - 1, clipping.height - 1 );
        }

      };
      entries.add( le );

    }

    IPointStyle ps = getPointFigure().getStyle();
    if( ps.isVisible() )
    {

      LegendEntry le = new LegendEntry( this, ps.getTitle() )
      {
        @Override
        public void paintSymbol( GC gc, Point size )
        {
          final ArrayList<Point> path = new ArrayList<Point>();

          path.add( new Point( size.x / 2, size.y / 2 ) );
          drawPoints( gc, path );
        }

      };
      entries.add( le );
    }

    return entries.toArray( new ILegendEntry[] {} );
  }

  /**
   * @see de.openali.odysseus.chart.ext.base.layer.AbstractChartLayer#getTitle()
   */
  @Override
  public String getTitle( )
  {
    return "Gel‰ndeLayer";
  }

  /**
   * @see org.kalypso.model.wspm.ui.view.chart.IProfilChartLayer#onProfilChanged(org.kalypso.model.wspm.core.profil.changes.ProfilChangeHint,
   *      org.kalypso.model.wspm.core.profil.IProfilChange[])
   */
  public void onProfilChanged( ProfilChangeHint hint, IProfilChange[] changes )
  {
    if( hint.isActivePropertyChanged() )
    {
      final IComponent cmp = getProfil().getActiveProperty();
      if( cmp != null )
        setActive( cmp.getId().equals( IWspmConstants.POINT_PROPERTY_HOEHE ) );
    }
    getEventHandler().fireLayerContentChanged( this );
  }

  /**
   * @see org.kalypso.model.wspm.ui.view.chart.IProfilChartLayer#removeYourself()
   */
  public void removeYourself( )
  {
    // TODO Auto-generated method stub

  }

  private final String getTooltipInfo( final Double breite, final Double hoehe )
  {
    return String.format( TOOLTIP_FORMAT, new Object[] { "Breite", breite, "Hˆhe", hoehe, "m" } );
  }

  /**
   * @see de.openali.odysseus.chart.framework.model.layer.IEditableChartLayer#commitDrag(org.eclipse.swt.graphics.Point,
   *      de.openali.odysseus.chart.framework.model.layer.EditInfo)
   */
  public EditInfo commitDrag( Point point, EditInfo dragStartData )
  {
    if( dragStartData.m_pos == point )
      executeClick( dragStartData );
    else
      executeDrop( point, dragStartData );

    return null;
  }

  private final void executeDrop( Point point, EditInfo dragStartData )
  {
    Integer pos = dragStartData.m_data instanceof Integer ? (Integer) (dragStartData.m_data) : -1;
    if( pos > 0 )
    {
      final IRecord profilPoint = m_profil.getPoint( pos );
      final Integer hoehe = m_profil.indexOfProperty( IWspmConstants.POINT_PROPERTY_HOEHE );
      final Integer breite = m_profil.indexOfProperty( IWspmConstants.POINT_PROPERTY_BREITE );
      final ICoordinateMapper cm = getCoordinateMapper();
      final Double x = cm.getDomainAxis().screenToNumeric( point.x ).doubleValue();
      final Double y = cm.getTargetAxis().screenToNumeric( point.y ).doubleValue();

      profilPoint.setValue( breite, x );
      profilPoint.setValue( hoehe, y );
      m_profil.setActivePoint( profilPoint );
    }
  }

  private final void executeClick( final EditInfo clickInfo )
  {
    final int pos = ((Integer) clickInfo.m_data);
    m_profil.setActivePoint( m_profil.getPoint( pos ) );
    final IComponent cmp = m_profil.hasPointProperty( IWspmConstants.POINT_PROPERTY_HOEHE );
    if( cmp != null )
      m_profil.setActivePointProperty( cmp );
  }

  /**
   * @see de.openali.odysseus.chart.framework.model.layer.IEditableChartLayer#drag(org.eclipse.swt.graphics.Point,
   *      de.openali.odysseus.chart.framework.model.layer.EditInfo)
   */
  public EditInfo drag( Point newPos, EditInfo dragStartData )
  {
    final ICoordinateMapper cm = getCoordinateMapper();
    final Double breite = cm.getDomainAxis().screenToNumeric( newPos.x ).doubleValue();
    final Double hoehe = cm.getTargetAxis().screenToNumeric( newPos.y ).doubleValue();
    final PolylineFigure infoFigure = new PolylineFigure();
    final ILineStyle infoFigureLine = getPolylineFigure().getStyle().copy();
    infoFigureLine.setAlpha( (int) (getPolylineFigure().getStyle().getAlpha() * 0.5) );
    infoFigure.setStyle( infoFigureLine );

    final Integer index = (Integer) dragStartData.m_data;
    final IRecord[] profilPoints = m_profil.getPoints();
    final Point previous = cm.numericToScreen( ProfilUtil.getDoubleValueFor( IWspmConstants.POINT_PROPERTY_BREITE, profilPoints[index - 1] ), ProfilUtil.getDoubleValueFor( IWspmConstants.POINT_PROPERTY_HOEHE, profilPoints[index - 1] ) );
    final Point next = cm.numericToScreen( ProfilUtil.getDoubleValueFor( IWspmConstants.POINT_PROPERTY_BREITE, profilPoints[index + 1] ), ProfilUtil.getDoubleValueFor( IWspmConstants.POINT_PROPERTY_HOEHE, profilPoints[index + 1] ) );

    infoFigure.setPoints( new Point[] { previous, newPos, next } );

    return new EditInfo( this, null, infoFigure, dragStartData.m_data, getTooltipInfo( breite, hoehe ), newPos );
  }

  /**
   * @see de.openali.odysseus.chart.framework.model.layer.IEditableChartLayer#getHover(org.eclipse.swt.graphics.Point)
   */
  public EditInfo getHover( Point pos )
  {

    if( m_profil == null )
      return null;
    final IRecord[] profilPoints = m_profil.getPoints();
    final int len = profilPoints.length;
    final ICoordinateMapper cm = getCoordinateMapper();

    for( int i = 0; i < len; i++ )
    {
      final Double breite = ProfilUtil.getDoubleValueFor( IWspmConstants.POINT_PROPERTY_BREITE, profilPoints[i] );
      final Double hoehe = ProfilUtil.getDoubleValueFor( IWspmConstants.POINT_PROPERTY_HOEHE, profilPoints[i] );
      final Point point = cm.numericToScreen( breite, hoehe );
      final Rectangle hover = RectangleUtils.buffer( point );
      if( hover.contains( pos ) )
      {
        return new EditInfo( this, null, null, i, getTooltipInfo( breite, hoehe ), pos );
      }
    }
    return null;
  }

  /**
   * @see org.kalypso.model.wspm.ui.view.chart.IProfilChartLayer#getProfil()
   */
  public IProfil getProfil( )
  {
    return m_profil;
  }

  /**
   * @see de.openali.odysseus.chart.ext.base.layer.AbstractChartLayer#getId()
   */
  @Override
  public String getId( )
  {
    return IWspmTuhhConstants.LAYER_GELAENDE;
  }

}
