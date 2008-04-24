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
package org.kalypso.ogc.gml.map.widgets;

import java.awt.Color;
import java.awt.Cursor;
import java.awt.Graphics;
import java.awt.Point;
import java.util.ArrayList;
import java.util.List;

import javax.xml.namespace.QName;

import org.eclipse.jface.viewers.ISelection;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.gmlschema.GMLSchemaUtilities;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.ogc.gml.command.Handle;
import org.kalypso.ogc.gml.command.ModifyFeatureGeometryCommand;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.map.widgets.providers.IHandlesProvider;
import org.kalypso.ogc.gml.map.widgets.providers.handles.IHandle;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.selection.EasyFeatureWrapper;
import org.kalypso.ogc.gml.selection.IFeatureSelection;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Envelope;

/**
 * This class is a widget for selecting and dragging a complete feature.
 * 
 * @author Holger Albert
 */
public class DragFeatureWidget extends AbstractWidget
{
  /**
   * This list stores all handles of the selected feature.
   */
  private List<IHandle> m_handles;

  /**
   * This is the provider, which provides the handles for the geometries of the feature.
   */
  private IHandlesProvider m_handlesProvider;

  /**
   * The radius, in which a handle should be selectable.
   */
  private final int m_radius;

  /**
   * The start point, if dragged.
   */
  private Point m_startPoint;

  /**
   * The current point, if dragged, otherwise null.
   */
  private Point m_currentPoint;

  /**
   * The workspace, the feature, which is edited is in.
   */
  private CommandableWorkspace m_workspace;

  /**
   * The QNames of the feature, from whose one should be substituted by the editable features.
   */
  private final QName[] m_qnames;

  /**
   * The constructor.
   * 
   * @param name
   *            The name of this widget.
   * @param toolTip
   *            The tooltip of this widget.
   * @param handlesProvider
   *            The class which provides the handles the geometries of a feature.
   * @param radius
   *            The radius in which the handles should be selectable.
   * @param qnames
   *            The QNames the editable features should substitute from.
   */
  public DragFeatureWidget( final String name, final String toolTip, final IHandlesProvider handlesProvider, final int radius, final QName[] qnames )
  {
    super( name, toolTip );

    m_handles = null;
    m_handlesProvider = handlesProvider;
    m_radius = radius;

    /* The start & current points are null. */
    m_startPoint = null;
    m_currentPoint = null;

    m_qnames = qnames;
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#activate(org.kalypso.commons.command.ICommandTarget,
   *      org.kalypso.ogc.gml.map.MapPanel)
   */
  @Override
  public void activate( ICommandTarget commandPoster, MapPanel mapPanel )
  {
    super.activate( commandPoster, mapPanel );

    /* Init the cursor. */
    Cursor cursor = Cursor.getPredefinedCursor( Cursor.HAND_CURSOR );
    getMapPanel().setCursor( cursor );
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#leftReleased(java.awt.Point)
   */
  @Override
  public void leftReleased( Point p )
  {
    if( (m_handles == null) || (m_startPoint == null) || (m_currentPoint == null) )
      return;

    /* Memory to collect all active handles. */
    ArrayList<Handle> list = new ArrayList<Handle>();

    /* Set all handles inactive. */
    for( IHandle handle : m_handles )
    {
      if( handle.isActive() )
        list.add( new Handle( handle.getFeature(), handle.getValuePropertyType(), handle.getPosition() ) );

      handle.setActive( false );
    }

    if( list.size() == 0 )
    {
      /* Reset. */
      m_currentPoint = null;
      m_startPoint = null;

      return;
    }

    getMapPanel().setMessage( "Erstelle Geometry ..." );

    /* Create the new geometry. */
    GeoTransform projection = getMapPanel().getProjection();

    final double gStartX = projection.getSourceX( m_startPoint.getX() );
    final double gStartY = projection.getSourceY( m_startPoint.getY() );
    final double gDragX = projection.getSourceX( p.getX() );
    final double gDragY = projection.getSourceY( p.getY() );

    final double[] translation = new double[] { gDragX - gStartX, gDragY - gStartY };

    final ModifyFeatureGeometryCommand command = new ModifyFeatureGeometryCommand( m_workspace, list, translation );

    try
    {
      m_workspace.postCommand( command );
    }
    catch( Exception e )
    {
      e.printStackTrace();
    }

    /* Reset. */
    m_currentPoint = null;
    m_startPoint = null;

    getMapPanel().setMessage( "Klicken Sie erneut in das Feature und halten Sie den Mausknopf, um es zu verschieben." );
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#dragged(java.awt.Point)
   */
  @Override
  public void dragged( Point p )
  {
    if( m_handles == null || m_handles.size() == 0 )
      return;

    if( m_startPoint == null )
    {
      /* Store the start point. */
      m_startPoint = p;

      /* Get the feature of the first handle. */
      Feature feature = m_handles.get( 0 ).getFeature();
      GM_Envelope envelope = feature.getEnvelope();

      GeoTransform projection = getMapPanel().getProjection();
      double x = projection.getSourceX( p.getX() );
      double y = projection.getSourceY( p.getY() );

      if( envelope.contains( x, y ) )
      {
        /* Activate all handles, because we want to drag the whole feature. */
        for( IHandle handle : m_handles )
          handle.setActive( true );

        getMapPanel().setMessage( "Lassen Sie den Mausknopf los, um das Feature hier abzusetzen." );
      }
    }

    /* Store the current mouse position. */
    m_currentPoint = p;

    // TODO: check if this repaint is really necessary
    // Answer: It is necessary, but it was at the wrong scope. Moved it out of the upper if-block.
    MapPanel panel = getMapPanel();
    if( panel != null )
      panel.repaint();
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#paint(java.awt.Graphics)
   */
  @Override
  public void paint( Graphics g )
  {
    if( m_handles == null || m_handles.size() == 0 )
      return;

    Feature feature = m_handles.get( 0 ).getFeature();
    GM_Envelope envelope = feature.getEnvelope();

    double x = envelope.getMin().getX();
    double y = envelope.getMin().getY();
    double xx = envelope.getMax().getX();
    double yy = envelope.getMax().getY();

    GeoTransform projection = getMapPanel().getProjection();

    int destX = (int) projection.getDestX( x );
    int destY = (int) projection.getDestY( yy );
    int destXX = (int) projection.getDestX( xx );
    int destYY = (int) projection.getDestY( y );

    int width = destXX - destX;
    int height = destYY - destY;

    g.drawRect( destX, destY, width, height );

    /*
     * There must be a start and end point. Further more, the handles must be active. If the first is active, all others
     * are active, too.
     */
    if( (m_startPoint != null) && (m_currentPoint != null) && (m_handles.get( 0 ).isActive()) )
    {
      /* Calculate the difference between the two points. */
      int da = (int) (m_currentPoint.getX() - m_startPoint.getX());
      int db = (int) (m_currentPoint.getY() - m_startPoint.getY());

      int a = (int) m_startPoint.getX();
      int b = (int) m_startPoint.getY();

      g.setColor( Color.BLUE );
      g.drawLine( a, b, a + da, b + db );
    }
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#finish()
   */
  @Override
  public void finish( )
  {
    /* Reset the widget. */
    reset();

    /* The parents finish method. */
    super.finish();
  }

  /**
   * Resets the widget.
   */
  public void reset( )
  {
    getMapPanel().setMessage( "" );

    /* Set the handles to null. */
    m_handles = null;

    /* Reset the start & current points. */
    m_startPoint = null;
    m_currentPoint = null;

    /* Resets the workspace. */
    m_workspace = null;

    /* Reset the cursor to default. */
    Cursor cursor = Cursor.getPredefinedCursor( Cursor.DEFAULT_CURSOR );
    getMapPanel().setCursor( cursor );
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#setSelection(org.kalypso.ogc.gml.selection.IFeatureSelection)
   */
  @Override
  public void setSelection( ISelection selection )
  {
    EasyFeatureWrapper wrapper = checkSelection( selection );

    /* If no feature with geometries is found, do nothing. */
    if( wrapper == null )
    {
      reset();
      return;
    }

    /* Store the feature and the workspace. */
    Feature feature = wrapper.getFeature();
    m_workspace = wrapper.getWorkspace();

    /* Create a new list for the handles. */
    m_handles = new ArrayList<IHandle>();

    /* Collect all handles from the handle provider. */
    m_handles.addAll( m_handlesProvider.collectHandles( feature, m_radius ) );

    getMapPanel().setMessage( "Klicken Sie in den Rahmen und halten Sie den Mausknopf gedr¸ckt, um das Feature zu verschieben." );

    return;
  }

  private EasyFeatureWrapper checkSelection( ISelection selection )
  {
    if( !(selection instanceof IFeatureSelection) )
      return null;

    /* On activation collect all handles of the selected feature. */
    EasyFeatureWrapper[] allFeatures = ((IFeatureSelection) selection).getAllFeatures();

    for( EasyFeatureWrapper wrapper : allFeatures )
    {
      /* Take the first feature, which is in this list and which has geometries. */
      if( wrapper.getFeature().getGeometryProperties().length > 0 )
      {
        IFeatureType featureType = wrapper.getFeature().getFeatureType();

        /*
         * If a list of qnames is specified and at least one qname exists in it, search only for features, which
         * substitutes one of the qnames in the list.
         */
        if( (m_qnames != null) && (m_qnames.length > 0) )
        {
          for( QName qname : m_qnames )
          {
            /* And it has to susbtitute one of the qnames. */
            if( GMLSchemaUtilities.substitutes( featureType, qname ) )
            {
              return wrapper;
            }
          }
        }
        else
        {
          return wrapper;
        }
      }
    }

    return null;
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#canBeActivated(org.eclipse.jface.viewers.ISelection,
   *      org.kalypso.ogc.gml.map.MapPanel)
   */
  @Override
  public boolean canBeActivated( ISelection selection, MapPanel mapPanel )
  {
    if( mapPanel == null )
      return false;

    return checkSelection( mapPanel.getSelectionManager() ) != null;
  }
}