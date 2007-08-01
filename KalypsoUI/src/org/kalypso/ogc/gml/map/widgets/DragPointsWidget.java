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

import java.awt.Graphics;
import java.awt.Point;
import java.util.ArrayList;
import java.util.List;

import javax.xml.namespace.QName;

import org.eclipse.jface.viewers.ISelection;
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

/**
 * This class is a widget for selecting and dragging a handle (point) from a geometry.
 * 
 * @author Holger Albert
 */
public class DragPointsWidget extends AbstractWidget
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
   *          The name of this widget.
   * @param toolTip
   *          The tooltip of this widget.
   * @param handlesProvider
   *          The class which provides the handles the geometries of a feature.
   * @param radius
   *          The radius in which the handles should be selectable.
   * @param qnames
   *          The QNames the editable features should substitute from.
   */
  public DragPointsWidget( final String name, final String toolTip, final IHandlesProvider handlesProvider, final int radius, final QName[] qnames )
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

    getMapPanel().setMessage( "Erstelle Geometrie ..." );

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

    getMapPanel().setMessage( "Klicken Sie erneut auf einen Punkt und halten Sie den Mausknopf, um ihn zu verschieben." );
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#dragged(java.awt.Point)
   */
  @Override
  public void dragged( Point p )
  {
    if( m_handles == null )
      return;

    if( m_startPoint == null )
    {
      /* Store the start point. */
      m_startPoint = p;

      /* Check, if the mouse cursor is near some handles. */
      for( IHandle handle : m_handles )
      {
        /* If the curser is near the handle, set it active, otherwise inactive. */
        if( handle.isSelectable( p, getMapPanel().getProjection() ) )
        {
          handle.setActive( true );
          getMapPanel().setMessage( "Lassen Sie den Mausknopf los, um den Punkt hier abzusetzen." );
        }
        else
          handle.setActive( false );
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
    if( m_handles == null )
      return;

    /* Paint all handles. */
    for( IHandle handle : m_handles )
    {
      if( handle.isActive() )
        handle.paint( g, getMapPanel().getProjection(), m_startPoint, m_currentPoint );
      else
        handle.paint( g, getMapPanel().getProjection(), null, null );
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

    getMapPanel().setMessage( "Klicken Sie in einen Rahmen und halten Sie den Mausknopf gedr¸ckt, um einen Punkt zu verschieben." );
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