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
import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.Locale;

import org.apache.commons.io.IOUtils;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.kalypso.ogc.gml.IGetFeatureInfoResultProcessor;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.map.themes.KalypsoWMSTheme;
import org.kalypso.ogc.gml.map.widgets.AbstractWidget;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ui.editor.mapeditor.views.IWidgetWithOptions;
import org.kalypsodeegree.graphics.transformation.GeoTransform;

/**
 * class WMSGetFeatureInfoWidget created by
 * 
 * @author doemming (19.04.2005)
 */
public class WMSGetFeatureInfoWidget extends AbstractWidget implements IWidgetWithOptions
{
  protected Composite m_topLevel;

  protected Text m_textInfo;

  Combo m_formatCombo;

  private Point m_pointOfInterest = null;

  private Point m_movePoint = null;

  private final String COORD_FORMAT = "%10.4f";

  public WMSGetFeatureInfoWidget( final String name, final String toolTip )
  {
    super( name, toolTip );
  }

  /**
   * empty constructor so widget can be used with SelectWidgetHandler
   */
  public WMSGetFeatureInfoWidget( )
  {
    super( "wms get feature info", "" );
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#leftPressed(java.awt.Point)
   */
  @Override
  public void leftPressed( final Point p )
  {
    m_pointOfInterest = p;
    updateInfoText();
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#finish()
   */
  @Override
  public void finish( )
  {
    super.finish();
    m_pointOfInterest = null;
    updateInfoText();
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#dragged(java.awt.Point)
   */
  @Override
  public void dragged( final Point p )
  {
    m_movePoint = p;
    // TODO: check if this repaint is really necessary
    final MapPanel panel = getMapPanel();
    if( panel != null )
      panel.repaint();

  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#moved(java.awt.Point)
   */
  @Override
  public void moved( final Point p )
  {
    m_movePoint = null;
// TODO: check if this repaint is necessary for the widget
    final MapPanel panel = getMapPanel();
    if( panel != null )
      panel.repaint();
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#leftReleased(java.awt.Point)
   */
  @Override
  public void leftReleased( final Point p )
  {
    // nothing to do here
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#rightClicked(java.awt.Point)
   */
  @Override
  public void rightClicked( final Point p )
  {
    // m_pointOfInterest = null;
    // m_p2 = null;
    // updateInfoText();
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#paint(java.awt.Graphics)
   */
  @Override
  public void paint( final Graphics g )
  {
    final MapPanel mapPanel = getMapPanel();
    if( m_pointOfInterest != null )
    {
      final GeoTransform transform = mapPanel.getProjection();
      final int x1 = (int) transform.getDestX( m_pointOfInterest.getX() );
      final int y1 = (int) transform.getDestY( m_pointOfInterest.getY() );
      final int radius = 3;
      g.drawRect( x1 - 3, y1 - 3, 2 * radius, 2 * radius );
    }
    if( m_movePoint != null )
    {
      final int width = mapPanel.getWidth();
      final int height = mapPanel.getHeight();
      g.drawLine( 0, (int) m_movePoint.getY(), width, (int) m_movePoint.getY() );
      g.drawLine( (int) m_movePoint.getX(), 0, (int) m_movePoint.getX(), height );
    }
  }

  private void updateInfoText( )
  {
    final String themeName;
    final MapPanel mapPanel = getMapPanel();
    if( mapPanel == null )
      return;
    final IMapModell mapModell = mapPanel.getMapModell();
    if( mapModell == null )
      return;
    final IKalypsoTheme activeTheme = mapModell.getActiveTheme();
    if( activeTheme != null )
      themeName = activeTheme.getLabel();
    else
      themeName = "<Bitte Layer aktivieren>";
    final Point pointOfInterest = m_pointOfInterest;

    final IGetFeatureInfoResultProcessor processor = new IGetFeatureInfoResultProcessor()
    {

      /**
       * @see org.kalypso.ogc.gml.IGetFeatureInfoResultProcessor#write(java.lang.String)
       */
      public void write( final String message )
      {
        final Double x1 = pointOfInterest == null ? null : new Double( pointOfInterest.getX() );
        final Double y1 = pointOfInterest == null ? null : new Double( pointOfInterest.getY() );
        final StringWriter stringWriter = new StringWriter();
        try
        {
          final PrintWriter pw = new PrintWriter( stringWriter );
          pw.print( "GetFeatureInfo auf Thema : " + themeName + "\n" );
          pw.print( "Position :" );
          if( pointOfInterest == null )
            pw.print( "\n  nicht gewählt" );
          else
          {
            pw.print( "\n  " );
            pw.printf( Locale.US, COORD_FORMAT, x1 );
            pw.print( " / " );
            pw.printf( Locale.US, COORD_FORMAT, y1 );
          }
          pw.print( "\n Information vom WebMapServer:" );
          pw.print( "\n " + message );
        }
        catch( Exception e )
        {
          e.printStackTrace();
        }
        finally
        {
          IOUtils.closeQuietly( stringWriter );
        }

        if( m_textInfo != null && !m_textInfo.isDisposed() )
        {
          m_textInfo.getDisplay().asyncExec( new Runnable()
          {
            public void run( )
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
    };

    if( pointOfInterest == null )
      processor.write( "keine Position ausgewählt" );
    else if( !(activeTheme instanceof KalypsoWMSTheme) )
      processor.write( "keine WMS-Thema ausgewählt" );
    else
    {
      final KalypsoWMSTheme wmsTheme = (KalypsoWMSTheme) activeTheme;
      m_formatCombo.getDisplay().asyncExec( new Runnable()
      {
        public void run( )
        {
          try
          {
            if( m_formatCombo != null && !(m_formatCombo.isDisposed()) )
            {
              final int selectionIndex = m_formatCombo.getSelectionIndex();
              final String format = m_formatCombo.getItem( selectionIndex );
              wmsTheme.performGetFeatureinfoRequest( pointOfInterest, format, processor );
            }
          }
          catch( final Exception e )
          {
            e.printStackTrace();
          }
        }
      } );
    }
  }

  /**
   * @see org.kalypso.ui.editor.mapeditor.views.IWidgetWithOptions#disposeControl()
   */
  public void disposeControl( )
  {
    if( m_topLevel != null && !m_topLevel.isDisposed() )
      m_topLevel.dispose();
    if( m_textInfo != null && !m_textInfo.isDisposed() )
    {
      m_textInfo.dispose();
      m_textInfo = null;
    }
    if( m_formatCombo != null && !m_formatCombo.isDisposed() )
    {
      m_formatCombo.dispose();
      m_formatCombo = null;
    }
  }

  /**
   * @see org.kalypso.ui.editor.mapeditor.views.IWidgetWithOptions#createControl(org.eclipse.swt.widgets.Composite)
   */
  public Control createControl( final Composite parent, final FormToolkit toolkit )
  {
    m_topLevel = toolkit.createComposite( parent, SWT.NONE );
    m_topLevel.setLayout( new GridLayout( 1, false ) );

    m_formatCombo = new Combo( m_topLevel, SWT.SINGLE );
    toolkit.adapt( m_formatCombo );
    m_formatCombo.setLayoutData( new GridData( GridData.GRAB_HORIZONTAL ) );
    // formats are not queryable (bug in deegree, Collection of formats does not support toArray() )
    final String[] formats = new String[] { "application/vnd.ogc.gml"
    // ,
    // "text/plain",
    // "text/html"
    };
    m_formatCombo.setItems( formats );
    m_formatCombo.select( 0 );

    m_textInfo = toolkit.createText( m_topLevel, "WMS-GetFeatureInfo", SWT.READ_ONLY | SWT.MULTI | SWT.WRAP );
    m_textInfo.setLayoutData( new GridData( GridData.FILL_BOTH ) );

    return m_topLevel;
  }
}