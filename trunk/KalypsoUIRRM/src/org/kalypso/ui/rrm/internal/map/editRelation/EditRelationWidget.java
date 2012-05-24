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
package org.kalypso.ui.rrm.internal.map.editRelation;

import java.awt.Cursor;
import java.awt.Graphics;
import java.awt.Point;
import java.awt.event.MouseEvent;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.map.IMapPanel;
import org.kalypso.ogc.gml.map.utilities.MapUtilities;
import org.kalypso.ogc.gml.map.widgets.advanced.utils.WidgetCursors;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.widgets.AbstractWidget;
import org.kalypso.ui.editor.mapeditor.views.IWidgetWithOptions;
import org.kalypso.ui.rrm.internal.i18n.Messages;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

/**
 * Widget where the user can create relations between selected features.
 *
 * @author doemming
 */
public class EditRelationWidget extends AbstractWidget implements IWidgetWithOptions
{
  private static final int GRAB_RADIUS = 30;

  private final EditRelationData m_data = new EditRelationData();

  private EditRelationViewer m_editRelationViewer;

  private final EditRelationPainter m_painter = new EditRelationPainter( m_data );

  private EditRelationOperation m_performRunner = null;

  private Cursor m_addCursor;

  private Cursor m_removeCursor;

  public EditRelationWidget( final String name, final String toolTip )
  {
    super( name, toolTip );
  }

  /**
   * empty constructor so widget can be used with SelectWidgetHandler
   */
  public EditRelationWidget( )
  {
    super( Messages.getString( "org.kalypso.ogc.gml.map.widgets.editrelation.EditRelationWidget.2" ), StringUtils.EMPTY ); //$NON-NLS-1$

    m_data.addPropertyChangeListener( EditRelationData.PROPERTY_MODIFICATION_MODE, new PropertyChangeListener()
    {
      @Override
      public void propertyChange( final PropertyChangeEvent evt )
      {
        updateCursor();
      }
    } );
  }

  @Override
  public void disposeControl( )
  {
  }

  @Override
  public Control createControl( final Composite parent, final FormToolkit toolkit )
  {
    m_editRelationViewer = new EditRelationViewer( parent, toolkit, m_data );
    refreshSettings();
    updateCursor();

    m_performRunner = new EditRelationOperation( m_data, parent.getShell(), this );

    return m_editRelationViewer;
  }

  @Override
  public void activate( final ICommandTarget commandPoster, final IMapPanel mapPanel )
  {
    super.activate( commandPoster, mapPanel );

    refreshSettings();
  }

  @Override
  public void finish( )
  {
    /* FIXME: should be handled by framework */
    final IMapPanel mapPanel = getMapPanel();
    if( mapPanel != null )
      mapPanel.setCursor( Cursor.getDefaultCursor() );
  }

  @Override
  public void mouseDragged( final MouseEvent e )
  {
    final Feature sourceFeature = m_data.getSourceFeature();
    if( sourceFeature == null )
      return;

    final Point p = e.getPoint();

    final IMapPanel mapPanel = getMapPanel();
    final GeoTransform transform = mapPanel.getProjection();
    final GM_Point point = GeometryFactory.createGM_Point( p, transform, mapPanel.getMapModell().getCoordinatesSystem() );
    final double grabDistance = MapUtilities.calculateWorldDistance( mapPanel, point, GRAB_RADIUS );

    final FeatureList allowedFeatures = m_data.getAllowedTargetFeatures();
    final Feature target = EditRelationUtils.findNearestFeature( point, grabDistance, allowedFeatures );
    /* Never allow self reference */
    final Feature newTarget = target == sourceFeature ? null : target;

    m_data.setFeatures( sourceFeature, newTarget );

    repaintMap();
  }

  @Override
  public void mouseMoved( final MouseEvent e )
  {
    final IMapPanel mapPanel = getMapPanel();
    if( mapPanel == null )
      return;

    final GeoTransform transform = mapPanel.getProjection();
    if( transform == null )
      return;

    final Point p = e.getPoint();
    final GM_Point point = GeometryFactory.createGM_Point( p, transform, mapPanel.getMapModell().getCoordinatesSystem() );
    final double grabDistance = MapUtilities.calculateWorldDistance( mapPanel, point, GRAB_RADIUS );

    /* Update hover source */
    final FeatureList allowedSourceFeatures = m_data.getAllowedSourceFeatures();
    final Feature newSource = EditRelationUtils.findNearestFeature( point, grabDistance, allowedSourceFeatures );
    m_data.setFeatures( newSource, null );

    repaintMap();

    return;
  }

  @Override
  public void mouseReleased( final MouseEvent e )
  {
    if( e.getButton() != MouseEvent.BUTTON1 )
      return;

    m_performRunner.execute();
  }

  @Override
  public void paint( final Graphics g )
  {
    m_painter.paint( g, getMapPanel() );
  }

  private void refreshSettings( )
  {
    final IKalypsoTheme activeTheme = getActiveTheme();
    final EditRelationInput input = buildInput( activeTheme );
    m_data.setInput( input );
  }

  private EditRelationInput buildInput( final IKalypsoTheme activeTheme )
  {
    if( activeTheme instanceof IKalypsoFeatureTheme )
    {
      final IKalypsoFeatureTheme featureTheme = (IKalypsoFeatureTheme) activeTheme;
      final CommandableWorkspace workspace = featureTheme.getWorkspace();
      if( workspace == null )
        return null;

      return new EditRelationInput( workspace );
    }

    return null;
  }

  @Override
  public String getPartName( )
  {
    return null;
  }

  void reset( )
  {
    m_data.setFeatures( null, null );
    repaintMap();
  }

  protected void updateCursor( )
  {
    final IMapPanel mapPanel = getMapPanel();
    if( mapPanel == null )
      return;

    final Cursor cursor = getCursor();
    mapPanel.setCursor( cursor );
  }

  private Cursor getCursor( )
  {
    switch( m_data.getModificationMode() )
    {
      case ADD:
        if( m_addCursor == null )
          m_addCursor = WidgetCursors.createAddCursor();
        return m_addCursor;

      case REMOVE:
        if( m_removeCursor == null )
          m_removeCursor = WidgetCursors.createRemoveCursor();
        return m_removeCursor;

    }
    return null;
  }
}