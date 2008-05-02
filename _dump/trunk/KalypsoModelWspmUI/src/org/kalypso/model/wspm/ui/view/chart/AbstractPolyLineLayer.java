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
package org.kalypso.model.wspm.ui.view.chart;

import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;

import org.apache.commons.lang.ArrayUtils;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.kalypso.contribs.eclipse.swt.graphics.GCWrapper;
import org.kalypso.contribs.eclipse.swt.graphics.RectangleUtils;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.changes.ActiveObjectEdit;
import org.kalypso.model.wspm.core.profil.changes.PointPropertyEdit;
import org.kalypso.model.wspm.core.profil.util.ProfilUtil;
import org.kalypso.model.wspm.ui.KalypsoModelWspmUIPlugin;
import org.kalypso.model.wspm.ui.Messages;
import org.kalypso.model.wspm.ui.profil.operation.ProfilOperation;
import org.kalypso.model.wspm.ui.profil.operation.ProfilOperationJob;
import org.kalypso.model.wspm.ui.view.chart.color.IProfilColorSet;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;

import de.belger.swtchart.EditInfo;
import de.belger.swtchart.axis.AxisRange;

/**
 * @author kimwerner
 */
public abstract class AbstractPolyLineLayer extends AbstractProfilChartLayer
{
  public final static class EditData
  {
    private final int m_index;

    private final String m_property;

    public EditData( final int index, final String property )
    {
      m_index = index;
      m_property = property;
    }

    public int getIndex( )
    {
      return m_index;
    }

    public String getProperty( )
    {
      return m_property;
    }
  }

  protected final static String TOOLTIP_FORMAT = "%-12s %10.4f [m]%n%-12s %10.4f [m]"; //$NON-NLS-1$

  protected Color[] m_colors;

  private final Color m_stationColor;

  private final Color m_editColor;

  private final Color m_selectedcolor;

  private final String[] m_lineProperties;

  private final boolean m_drawStationLines;

  private final boolean m_markActivePoint;

  private final boolean m_mayEditVert;

  public AbstractPolyLineLayer( final String layerId, final String label, final ProfilChartView pcv, final AxisRange domainRange, final AxisRange valueRange, final String[] lineProperties, final boolean drawStationLines, final boolean markActivePoint, final boolean mayEditVert )
  {
    super( layerId, pcv, domainRange, valueRange, label );
    m_selectedcolor = pcv.getColorRegistry().get( IProfilColorSet.COLOUR_GELAENDE_MARKED );
    m_drawStationLines = drawStationLines;
    m_markActivePoint = markActivePoint;

    m_stationColor = pcv.getColorRegistry().get( IProfilColorSet.COLOUR_STATIONS );
    m_editColor = pcv.getColorRegistry().get( IProfilColorSet.COLOUR_AXIS_FOREGROUND );
    m_lineProperties = lineProperties;
    m_mayEditVert = mayEditVert;
  }

  protected void drawEditLine( final GCWrapper gc, final Point editing, final Point2D[] points, final Point2D oldpoint, final int index )
  {
    if( 0 <= index && index < points.length )
    {
      gc.setLineStyle( SWT.LINE_DASH );
      gc.setLineWidth( 1 );
      gc.setForeground( m_editColor );

      final Point oldp = logical2screen( oldpoint );

      final int newx = m_mayEditVert && getViewData().isEdithorz() ? editing.x : oldp.x;

      final int newy = getViewData().isEditvert() ? editing.y : oldp.y;

      final Point p = logical2screen( points[index] );

      gc.drawLine( p.x, p.y, newx, newy );
    }
  }

  public void drawSegment( final GCWrapper gc, final Point leftPoint, final Point rightPoint, final boolean activeSegment, final Color color )
  {
    gc.setLineWidth( 3 );
    gc.setLineStyle( SWT.LINE_SOLID );
    gc.setForeground( activeSegment && m_markActivePoint ? m_selectedcolor : color );
    if( leftPoint != null )
      gc.drawLine( leftPoint.x, leftPoint.y, rightPoint.x, rightPoint.y );
    if( activeSegment && m_markActivePoint )
      gc.drawOval( leftPoint.x - 2, leftPoint.y - 2, 4, 4 );
    gc.setForeground( color );
    gc.drawOval( rightPoint.x - 2, rightPoint.y - 2, 4, 4 );

    if( m_drawStationLines )
      drawStationline( gc, rightPoint.x, getValueRange().getScreenFrom() + getValueRange().getGapSpace(), rightPoint.x, rightPoint.y );
  }

  protected void drawStationline( final GCWrapper gc, final int x1, final int y1, final int x2, final int y2 )
  {
    gc.setLineWidth( 1 );
    gc.setForeground( m_stationColor );
    gc.setLineStyle( SWT.LINE_DASH );
    gc.drawLine( x1, y1, x2, y2 );
  }

  @Override
  public void editProfil( final Point moveTo, final Object data )
  {
    final EditData editData = (EditData) data;

    final IRecord point = getProfil().getPoints()[editData.getIndex()];

    final Point2D logPoint = screen2logical( moveTo );

    final IProfil profil = getProfil();

    final ProfilOperation profilOperation = new ProfilOperation( Messages.AbstractPolyLineLayer_1, profil, true );

    final String propertyID = editData.getProperty();
    final IComponent property = profil.hasPointProperty( propertyID );
    if( getViewData().isEditvert() )
      profilOperation.addChange( new PointPropertyEdit( point, property, logPoint.getY() ) );
    if( m_mayEditVert && getViewData().isEdithorz() )
      profilOperation.addChange( new PointPropertyEdit( point, profil.hasPointProperty( IWspmConstants.POINT_PROPERTY_BREITE ), logPoint.getX() ) );

    profilOperation.addChange( new ActiveObjectEdit( profil, point, property ) );

    new ProfilOperationJob( profilOperation ).schedule();

  }

  /**
   * @see de.belger.swtchart.layer.IChartLayer#getBounds()
   */
  public Rectangle2D getBounds( )
  {
    Rectangle2D bounds = null;
    for( final String next : m_lineProperties )
    {
      final Point2D[] points = ProfilUtil.getPoints2D( getProfil(), next );

      if( bounds == null && points.length > 0 )
        bounds = new Rectangle2D.Double( points[0].getX(), points[0].getY(), 0, 0 );

      for( final Point2D element : points )
        bounds.add( element );
    }

    return bounds;
  }

  @Override
  public EditInfo getHoverInfo( final Point mousePos )
  {
    for( final String propertyID : m_lineProperties )
    {
      final Point2D[] points = ProfilUtil.getPoints2D( getProfil(), propertyID );

      final IProfil profil = getProfil();

      final IComponent property = profil.hasPointProperty( propertyID );

      for( final Point2D element : points )
      {
        final Point p = logical2screen( element );
        final Rectangle hover = RectangleUtils.buffer( p );
        if( hover.contains( mousePos ) )
        {
          final double delta = property.getPrecision();
          final IRecord[] found = ProfilUtil.findPoints( getProfil(), propertyID, element, delta );
          final int index = ArrayUtils.indexOf( getProfil().getPoints(), found[0] );

          return isPointVisible( found[0] ) ? new EditInfo( this, hover, new EditData( index, propertyID ), String.format( TOOLTIP_FORMAT, new Object[] { Messages.AbstractPolyLineLayer_2,
              element.getX(), property.getName(), element.getY() } ) ) : null;
        }
      }
    }

    return null;
  }

  public abstract IRecord[] getPoints( );

  protected boolean isPointVisible( @SuppressWarnings("unused")//$NON-NLS-1$
  final IRecord point )
  {
    return true;
  }

  /**
   * @see de.belger.swtchart.layer.IChartLayer#paint(org.kalypso.contribs.eclipse.swt.graphics.GCWrapper)
   */
  public void paint( final GCWrapper gc )
  {
    try
    {
      final IRecord[] ppoints = getPoints();
      final IProfil profil = getProfil();
      if( ppoints == null )
        return;

      final Point[] lastP = new Point[m_lineProperties.length];
      IRecord lastPP = null;
      final IRecord activePoint = getProfil().getActivePoint();
      for( final IRecord pp : ppoints )
      {
        int i = 0;
        for( int line = m_lineProperties.length - 1; line > -1; line-- )
        {
          final IComponent pprop = profil.hasPointProperty( m_lineProperties[line] );
          final Color pcol = m_colors[line];
          final Point pt = logical2screen( ProfilUtil.getPoint2D( pp, pprop ) );

          drawSegment( gc, lastP[i], pt, lastPP != null && lastPP == activePoint, pcol );

          if( m_markActivePoint && activePoint != null )
          {
            gc.setLineStyle( SWT.LINE_SOLID );
            gc.setLineWidth( 2 );
            gc.setForeground( m_editColor );
            final Rectangle markRect = RectangleUtils.buffer( logical2screen( ProfilUtil.getPoint2D( activePoint, pprop ) ) );
            gc.drawRectangle( markRect );
          }
          lastP[i] = pt;
          i++;
        }
        lastPP = pp;
      }
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }
  }

  // protected void drawPolyLine( final GCWrapper gc, final int x, final int y, final int x2, final int y2, final
  // boolean selected, final Color color )
  // {
  // gc.setLineWidth( 3 );
  // gc.setLineStyle( SWT.LINE_SOLID );
  // gc.setForeground( selected ? m_selectedcolor : color );
  // gc.drawLine( x, y, x2, y2 );
  // }

  // protected void drawPoint( final GCWrapper gc, final int x, final int y, final int w, final int h, final boolean
  // selected )
  // {
  // gc.setLineWidth( 3 );
  // gc.setLineStyle( SWT.LINE_SOLID );
  // gc.setForeground( selected ? m_selectedcolor : m_colors );
  // gc.drawOval( x, y, w, h );
  // }

  /**
   * @see de.belger.swtchart.layer.IChartLayer#paintDrag(org.kalypso.contribs.eclipse.swt.graphics.GCWrapper,
   *      org.eclipse.swt.graphics.Point, java.lang.Object)
   */
  @Override
  public void paintDrag( final GCWrapper gc, final Point editing, final Object data )
  {
    final EditData editData = (EditData) data;
    final int index = editData.getIndex();

    final Point2D[] points = ProfilUtil.getPoints2D( getProfil(), editData.getProperty() );

    drawEditLine( gc, editing, points, points[index], index - 1 );
    drawEditLine( gc, editing, points, points[index], index + 1 );

    final Rectangle editRect = RectangleUtils.buffer( editing );
    gc.drawFocus( editRect.x, editRect.y, editRect.width, editRect.height );
  }

  /**
   * @see de.belger.swtchart.layer.IChartLayer#paintLegend(org.kalypso.contribs.eclipse.swt.graphics.GCWrapper)
   */
  @Override
  public abstract void paintLegend( final GCWrapper gc );

  /**
   * @see de.belger.swtchart.layer.IChartLayer#setActivePoint(org.eclipse.swt.graphics.Point)
   */
  @Override
  public void setActivePoint( final Object data )
  {
    if( data instanceof EditData )
    {
      final EditData editData = (EditData) data;
      final IRecord activePoint = getProfil().getPoints()[editData.getIndex()];
      final ProfilOperation operation = new ProfilOperation( "", getProfil(), new ActiveObjectEdit( getProfil(), activePoint, null ), true ); //$NON-NLS-1$
      final IStatus status = operation.execute( new NullProgressMonitor(), null );
      operation.dispose();
      if( !status.isOK() )
        KalypsoModelWspmUIPlugin.getDefault().getLog().log( status );
    }
  }

  public void setColors( final Color[] colors )
  {
    m_colors = colors;
  }

  /**
   * Returns the line properties.
   * 
   * @return The line properties.
   */
  protected String[] getLineProperties( )
  {
    return m_lineProperties;
  }
}
