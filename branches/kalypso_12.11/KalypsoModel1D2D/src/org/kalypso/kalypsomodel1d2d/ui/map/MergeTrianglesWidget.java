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
package org.kalypso.kalypsomodel1d2d.ui.map;

import java.awt.Color;
import java.awt.Graphics;
import java.awt.Point;
import java.awt.Rectangle;
import java.io.IOException;

import org.eclipse.core.runtime.NullProgressMonitor;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DEdge;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IPolyElement;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.DeleteCmdFactory;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.IFeatureChangeCommand;
import org.kalypso.kalypsomodel1d2d.ui.map.util.UtilMap;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.map.IMapPanel;
import org.kalypso.ogc.gml.map.utilities.MapUtilities;
import org.kalypso.ogc.gml.map.utilities.tooltip.ToolTipRenderer;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ogc.gml.widgets.DeprecatedMouseWidget;
import org.kalypsodeegree.graphics.displayelements.DisplayElement;
import org.kalypsodeegree.graphics.sld.Symbolizer;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Polygon;
import org.kalypsodeegree.xml.XMLParsingException;
import org.kalypsodeegree.xml.XMLTools;
import org.kalypsodeegree_impl.graphics.displayelements.DisplayElementFactory;
import org.kalypsodeegree_impl.graphics.sld.SLDFactory;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;
import org.w3c.dom.Document;
import org.xml.sax.SAXException;

import com.vividsolutions.jts.geom.Geometry;

/**
 * This widget allows to delete the edge of to adjacent triangles and thus combining them into a single quad-element.
 * 
 * @author Gernot Belger
 */
public class MergeTrianglesWidget extends DeprecatedMouseWidget
{
  private final int GRAB_RADIUS = 10;

  private IFE1D2DEdge m_currentEdge;

  private IKalypsoFeatureTheme m_theme;

  private IFEDiscretisationModel1d2d m_model1d2d;

  private final ToolTipRenderer m_toolTipRenderer = new ToolTipRenderer();

  private final ToolTipRenderer m_warningRenderer = new ToolTipRenderer();

  private String m_warning;

  private Document m_goodSld;

  private Document m_badSld;

  public MergeTrianglesWidget( )
  {
    super( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.MergeTrianglesWidget.0" ), Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.MergeTrianglesWidget.1" ) ); //$NON-NLS-1$ //$NON-NLS-2$
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#activate(org.kalypso.commons.command.ICommandTarget, org.kalypso.ogc.gml.map.MapPanel)
   */
  @Override
  public void activate( final ICommandTarget commandPoster, final IMapPanel mapPanel )
  {
    super.activate( commandPoster, mapPanel );
    reinit();
  }

  private final void reinit( )
  {
    final IMapPanel mapPanel = getMapPanel();
    final IMapModell mapModell = mapPanel.getMapModell();

    m_currentEdge = null;
    m_theme = UtilMap.findEditableTheme( mapPanel, IPolyElement.QNAME );
    m_model1d2d = UtilMap.findFEModelTheme( mapPanel );

    m_toolTipRenderer.setBackgroundColor( new Color( 1f, 1f, 0.6f, 0.70f ) );
    m_toolTipRenderer.setTooltip( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.MergeTrianglesWidget.2" ) ); //$NON-NLS-1$

    m_warningRenderer.setBackgroundColor( new Color( 1f, 0.4f, 0.4f, 0.80f ) );

    final IKalypsoTheme activeTheme = mapModell.getActiveTheme();
    if( activeTheme instanceof IKalypsoFeatureTheme )
      m_theme = (IKalypsoFeatureTheme)activeTheme;

    mapPanel.repaintMap();
  }

  @Override
  public void leftPressed( final Point p )
  {
    if( m_currentEdge == null )
    {
      // TODO: show message box
      return;
    }

    if( m_warning != null )
    {
      // TODO: show message box
      return;
    }

    try
    {
      final IFE1D2DElement[] elements2remove = m_currentEdge.getLinkedElements();
      final GM_Polygon newElement = createNewElement( (IPolyElement)elements2remove[0], (IPolyElement)elements2remove[1] );
      newElement.setCoordinateSystem( m_currentEdge.getGeometry().getCoordinateSystem() );
      final CommandableWorkspace workspace = m_theme.getWorkspace();

      // add remove element command
      for( final IFE1D2DElement element : elements2remove )
      {
        final IFeatureChangeCommand deleteCmd = DeleteCmdFactory.createDeleteCmd( element, m_model1d2d );
        workspace.postCommand( deleteCmd );
      }

      ElementGeometryHelper.createFE1D2DfromSurface( workspace, m_model1d2d, newElement );
    }
    catch( final Exception e )
    {
      // TODO: show message box
      e.printStackTrace();
    }

    reinit();
  }

  /**
   * Creates the new element from the two adjacent triangles.<br>
   * We just make the geometric union, that should do it.
   */
  private GM_Polygon createNewElement( final IPolyElement element1, final IPolyElement element2 ) throws GM_Exception
  {
    final GM_Polygon geom1 = element1.getGeometry();
    final GM_Polygon geom2 = element2.getGeometry();

    final Geometry jtsGeom1 = JTSAdapter.export( geom1 );
    final Geometry jtsGeom2 = JTSAdapter.export( geom2 );

    final Geometry union = jtsGeom1.union( jtsGeom2 );
    // TRICK: buffer with 0 in order to get a single geometry
    final Geometry buffer = union.buffer( 0 );

    return (GM_Polygon)JTSAdapter.wrap( buffer );
  }

  @Override
  public void moved( final Point p )
  {
    final IMapPanel mapPanel = getMapPanel();
    if( mapPanel == null || m_model1d2d == null )
      return;

    final GM_Point currentPoint = MapUtilities.transform( mapPanel, p );
    final double grabDistance = MapUtilities.calculateWorldDistance( mapPanel, currentPoint, GRAB_RADIUS );
    m_currentEdge = m_model1d2d.findEdge( currentPoint, grabDistance );

    m_warning = null;

    // Validate and set warning
    if( m_currentEdge != null )
    {
      final IFE1D2DElement[] adjacentElements = m_currentEdge.getLinkedElements();
      if( adjacentElements.length != 2 )
        m_warning = "Edge must have two adjacent 2D-elements"; //$NON-NLS-1$
      else
      {
        if( adjacentElements[0].getNodes().length != 4 || adjacentElements[1].getNodes().length != 4 )
          m_warning = "Both adjacent elements must be triangles"; //$NON-NLS-1$
      }
    }

    repaintMap();
  }

  @Override
  public void paint( final Graphics g )
  {
    final IMapPanel mapPanel = getMapPanel();
    if( mapPanel == null )
      return;

    final GeoTransform projection = mapPanel.getProjection();
    if( projection == null )
      return;

    /* always paint a small rectangle of current position */
    if( m_currentEdge != null )
    {
      try
      {
        final Symbolizer symbolizer = m_warning == null ? createGoodSymbolizer() : createBadSymbolizer();
        final DisplayElement lde = DisplayElementFactory.buildDisplayElement( m_currentEdge, symbolizer, null );
        lde.paint( g, projection, new NullProgressMonitor() );
      }
      catch( final Exception e )
      {
        e.printStackTrace();
      }
    }

    final Rectangle bounds = mapPanel.getScreenBounds();
    m_toolTipRenderer.paintToolTip( new Point( 5, bounds.height - 5 ), g, bounds );

    m_warningRenderer.setTooltip( m_warning );
    if( m_warning != null )
      m_warningRenderer.paintToolTip( new Point( 5, 30 ), g, bounds );
  }

  private Symbolizer createGoodSymbolizer( ) throws IOException, SAXException, XMLParsingException
  {
    if( m_goodSld == null )
      m_goodSld = XMLTools.parse( getClass().getResource( "styles/goodEdge.sld" ) ); //$NON-NLS-1$

    return SLDFactory.createSymbolizer( null, m_goodSld.getDocumentElement() );
  }

  private Symbolizer createBadSymbolizer( ) throws IOException, SAXException, XMLParsingException
  {
    if( m_badSld == null )
      m_badSld = XMLTools.parse( getClass().getResource( "styles/badEdge.sld" ) ); //$NON-NLS-1$

    return SLDFactory.createSymbolizer( null, m_badSld.getDocumentElement() );
  }

}
