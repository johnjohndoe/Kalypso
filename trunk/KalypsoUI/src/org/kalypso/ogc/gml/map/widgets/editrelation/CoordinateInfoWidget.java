/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and
 
 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
 
 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.ogc.gml.map.widgets.editrelation;

import java.awt.Graphics;
import java.awt.Point;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;

import org.apache.commons.io.IOUtils;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Text;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.map.widgets.AbstractWidget;
import org.kalypso.ui.editor.mapeditor.views.IWidgetWithOptions;
import org.kalypso.util.command.ICommand;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

import com.braju.format.Format;

/**
 * 
 * class CoordinateInfoWidget
 * 
 * created by
 * 
 * @author doemming (19.04.2005)
 */
public class CoordinateInfoWidget extends AbstractWidget implements IWidgetWithOptions
{
  protected Composite m_topLevel;

  protected Text m_textInfo;

  private GM_Point m_p1 = null;

  private GM_Point m_p2 = null;

  private Point m_movePoint;

  /** Für die Ausgabe im Info-Panel */
  private final String COORD_FORMAT = "%10.4f";

  public CoordinateInfoWidget( String name, String toolTip )
  {
    super( name, toolTip );
  }

  /**
   * 
   * @see org.kalypso.ogc.gml.widgets.IWidget#leftPressed(java.awt.Point)
   */
  public void leftPressed( Point p )
  {
    final MapPanel mapPanel = getMapPanel();
    final GeoTransform transform = mapPanel.getProjection();
    m_p1 = GeometryFactory.createGM_Point( p, transform, mapPanel.getMapModell()
        .getCoordinatesSystem() );
    m_p2 = null;
    updateInfoText();
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#finish()
   */
  public void finish()
  {
    super.finish();
    m_p1 = null;
    m_p2 = null;
    updateInfoText();
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#perform()
   */
  public void perform()
  {
  // nothing to do here
  }

  /**
   * 
   * @see org.kalypso.ogc.gml.widgets.IWidget#dragged(java.awt.Point)
   */
  public void dragged( Point p )
  {
    m_movePoint = p;
    final MapPanel mapPanel = getMapPanel();
    final GeoTransform transform = mapPanel.getProjection();
    final GM_Point point = GeometryFactory.createGM_Point( p, transform, mapPanel.getMapModell()
        .getCoordinatesSystem() );
    if( m_p1 != null )
      m_p2 = point;
    updateInfoText();
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#moved(java.awt.Point)
   */
  public void moved( Point p )
  {
    m_movePoint = null;
    final MapPanel mapPanel = getMapPanel();
    final GeoTransform transform = mapPanel.getProjection();
    final GM_Point point = GeometryFactory.createGM_Point( p, transform, mapPanel.getMapModell()
        .getCoordinatesSystem() );
    if( m_p1 != null )
      m_p2 = point;
    updateInfoText();
  }

  /**
   * 
   * @see org.kalypso.ogc.gml.widgets.IWidget#leftReleased(java.awt.Point)
   */
  public void leftReleased( Point p )
  {
  // nothing to do here
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#rightClicked(java.awt.Point)
   */
  public void rightClicked( Point p )
  {
    m_p1 = null;
    m_p2 = null;
    updateInfoText();
  }

  /**
   * 
   * @see org.kalypso.ogc.gml.widgets.IWidget#paint(java.awt.Graphics)
   */
  public void paint( Graphics g )
  {
    if( m_p1 != null && m_p2 != null )
    {
      final MapPanel mapPanel = getMapPanel();
      final GeoTransform transform = mapPanel.getProjection();
      int x1 = (int)transform.getDestX( m_p1.getX() );
      int y1 = (int)transform.getDestY( m_p1.getY() );
      int x2 = (int)transform.getDestX( m_p2.getX() );
      int y2 = (int)transform.getDestY( m_p2.getY() );
      g.drawLine( x1, y1, x2, y2 );
      if( m_movePoint != null )
      {
        int width = mapPanel.getWidth();
        int height = mapPanel.getHeight();
        g.drawLine( 0, (int)m_movePoint.getY(), width, (int)m_movePoint.getY() );
        g.drawLine( (int)m_movePoint.getX(), 0, (int)m_movePoint.getX(), height );
      }
    }
  }

  /**
   * 
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#performIntern()
   */
  protected final ICommand performIntern()
  {
    // nothing to do here
    return null;
  }

  private void updateInfoText()
  {
    final Double x1 = m_p1 == null ? null : new Double( m_p1.getX() );
    final Double y1 = m_p1 == null ? null : new Double( m_p1.getY() );
    final Double x2 = m_p2 == null ? null : new Double( m_p2.getX() );
    final Double y2 = m_p2 == null ? null : new Double( m_p2.getY() );

    final StringWriter stringWriter = new StringWriter();
    
    try
    {
      final PrintWriter pw = new PrintWriter( stringWriter );

      pw.print( "p1:" );
      if( m_p1 == null )
        pw.print( "\n  nicht gewählt" );
      else
      {
        pw.print( "\n  " );
        Format.fprintf( pw, COORD_FORMAT, new Double[]
        { x1 } );
        pw.print( " / " );
        Format.fprintf( pw, COORD_FORMAT, new Double[]
        { y1 } );
      }

      pw.print( "\np2:" );
      if( m_p2 == null )
        pw.print( "\n  nicht gewählt" );
      else
      {
        pw.print( "\n  " );
        Format.fprintf( pw, COORD_FORMAT, new Double[]
        { x2 } );
        pw.print( " / " );
        Format.fprintf( pw, COORD_FORMAT, new Double[]
        { y2 } );
      }

      if( m_p1 != null && m_p2 != null )
      {
        final Double direkt = new Double( m_p1.distance( m_p2 ) );
        final Double horizontal = new Double( Math.abs( m_p1.getX() - m_p2.getX() ) );
        final Double vertical = new Double( Math.abs( m_p1.getY() - m_p2.getY() ) );

        pw.print( "\n\nDistanz:" );
        pw.print( "\n  direkt: \t\t" );

        Format.fprintf( pw, COORD_FORMAT, new Double[]
        { direkt } );

        pw.print( "\n  horizontal: \t" );
        Format.fprintf( pw, COORD_FORMAT, new Double[]
        { horizontal } );

        pw.print( "\n  vertikal: \t\t" );
        Format.fprintf( pw, COORD_FORMAT, new Double[]
        { vertical } );
      }
    }
    catch( final IOException e )
    {
      // will never happen
    }
    finally
    {
      IOUtils.closeQuietly( stringWriter );
    }

    if( m_textInfo != null && !m_textInfo.isDisposed() )
    {
      m_textInfo.getDisplay().asyncExec( new Runnable()
      {
        public void run()
        {
          if( m_textInfo != null && !m_textInfo.isDisposed() )
          {
            m_textInfo.setText( stringWriter.toString() );
            m_textInfo.setToolTipText( stringWriter.toString() );
            m_topLevel.layout();
          }
        }
      } );
    }
  }

  /**
   * @see org.kalypso.ui.editor.mapeditor.views.IWidgetWithOptions#disposeControl()
   */
  public void disposeControl()
  {
    if( m_topLevel != null && !m_topLevel.isDisposed() )
      m_topLevel.dispose();
    if( m_textInfo != null && !m_textInfo.isDisposed() )
    {
      m_textInfo.dispose();
      m_textInfo = null;
    }
  }

  /**
   * @see org.kalypso.ui.editor.mapeditor.views.IWidgetWithOptions#createControl(org.eclipse.swt.widgets.Composite)
   */
  public void createControl( Composite parent )
  {
    m_topLevel = new Composite( parent, SWT.NONE );
    m_topLevel.setLayout( new GridLayout( 1, false ) );

    m_topLevel.setLayoutData( new GridData( GridData.FILL_BOTH ) );

    m_textInfo = new Text( m_topLevel, SWT.READ_ONLY | SWT.MULTI | SWT.WRAP );
    m_textInfo.setText( "Info" );
    m_textInfo.setLayoutData( new GridData( GridData.FILL_BOTH ) );
  }
}