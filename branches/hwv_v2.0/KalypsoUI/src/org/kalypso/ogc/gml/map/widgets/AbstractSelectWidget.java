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
package org.kalypso.ogc.gml.map.widgets;

import java.awt.Graphics;
import java.awt.Point;
import java.awt.event.KeyEvent;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.jface.viewers.ISelection;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.i18n.Messages;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.KalypsoFeatureThemeSelection;
import org.kalypso.ogc.gml.command.DeleteFeatureCommand;
import org.kalypso.ogc.gml.command.JMSelector;
import org.kalypso.ogc.gml.map.IMapPanel;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ogc.gml.selection.EasyFeatureWrapper;
import org.kalypso.ogc.gml.selection.IFeatureSelectionManager;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

/**
 * @author doemming
 */
public abstract class AbstractSelectWidget extends AbstractWidget
{
  protected static final int MODE_SELECT = 0;

  protected static final int MODE_TOGGLE = 1;

  protected static final int MODE_UNSELECT = 2;

  public AbstractSelectWidget( final String name, final String toolTip )
  {
    super( name, toolTip );
  }

  /**
   * pixel coordinates
   */
  private Point m_endPoint = null;

  /**
   * pixel coordinates
   */
  private Point m_startPoint = null;

  /**
   * radius in pixel
   */
  private final int m_radius = 20;

  private final int KEY_COMBINATION_CTRL_MOUSE_BUTTON_LEFT = KeyEvent.CTRL_DOWN_MASK | KeyEvent.BUTTON1_DOWN_MASK;

  abstract int getSelectionMode( );

  abstract boolean allowOnlyOneSelectedFeature( );

  @Override
  public void dragged( final Point p )
  {
    if( m_startPoint == null )
    {
      m_startPoint = p;
      m_endPoint = null;
    }
    else
      m_endPoint = p;

    // TODO: check if this repaint is really necessary
    final IMapPanel panel = getMapPanel();
    if( panel != null )
      panel.repaintMap();

  }

  @Override
  public void leftPressed( final Point p )
  {
    m_startPoint = p;
    m_endPoint = null;
  }

  @Override
  public void leftReleased( final Point p )
  {
    if( m_endPoint != null ) // last update of endPoint
      m_endPoint = p;
    else
      m_startPoint = p;

    try
    {
      select( m_startPoint, m_endPoint, m_radius, getSelectionMode(), allowOnlyOneSelectedFeature() );
    }
    finally
    {
      m_startPoint = null;
      m_endPoint = null;

      /* Always repaint, maybe the selection has not changed, then we dont get any repaint from the selection manager */
      getMapPanel().repaintMap();
    }
  }

  @Override
  public void paint( final Graphics g )
  {
    if( m_startPoint != null && m_endPoint != null )
    {
      final int px = (int) (m_startPoint.getX() < m_endPoint.getX() ? m_startPoint.getX() : m_endPoint.getX());
      final int py = (int) (m_startPoint.getY() < m_endPoint.getY() ? m_startPoint.getY() : m_endPoint.getY());
      final int dx = (int) Math.abs( m_endPoint.getX() - m_startPoint.getX() );
      final int dy = (int) Math.abs( m_endPoint.getY() - m_startPoint.getY() );

      if( dx != 0 && dy != 0 )
        g.drawRect( px, py, dx, dy );
    }
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#finish()
   */
  @Override
  public void finish( )
  {
    m_startPoint = null;
    m_endPoint = null;
    super.finish();
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#keyReleased(java.awt.event.KeyEvent)
   */
  @Override
  public void keyReleased( final KeyEvent e )
  {
    if( e.getKeyCode() == KeyEvent.VK_DELETE )
    {
      final ISelection selection = getMapPanel().getSelection();
      if( selection instanceof KalypsoFeatureThemeSelection )
      {
        final KalypsoFeatureThemeSelection fts = (KalypsoFeatureThemeSelection) selection;
        final EasyFeatureWrapper[] allFeatures = fts.getAllFeatures();
        final DeleteFeatureCommand command = new DeleteFeatureCommand( allFeatures );
        final IMapModell mapModell = getMapPanel().getMapModell();
        final IKalypsoTheme activeTheme = mapModell.getActiveTheme();
        if( activeTheme instanceof IKalypsoFeatureTheme )
        {
          final IKalypsoFeatureTheme theme = (IKalypsoFeatureTheme) activeTheme;
          final CommandableWorkspace workspace = theme.getWorkspace();
          try
          {
            workspace.postCommand( command );
          }
          catch( final Exception e1 )
          {
            e1.printStackTrace();
          }
        }
      }
    }
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#keyPressed(java.awt.event.KeyEvent)
   */
  @Override
  public void keyPressed( final KeyEvent e )
  {
    final int modifiersEx = e.getModifiersEx();
    if( modifiersEx == KEY_COMBINATION_CTRL_MOUSE_BUTTON_LEFT )
    {
      // TODO add Feature to selection by pressing strg+LeftMouseKey
      // final ISelection oldSelection = getMapPanel().getSelection();
      // if( oldSelection instanceof KalypsoFeatureThemeSelection )
      // {
      // final KalypsoFeatureThemeSelection oldFts = (KalypsoFeatureThemeSelection) oldSelection;
      // final EasyFeatureWrapper[] oldFeatures = oldFts.getAllFeatures();
      // getMapPanel().select( m_startPoint, m_endPoint, m_radius, getSelectionMode(), allowOnlyOneSelectedFeature() );
      // final ISelection newSelection = getMapPanel().getSelection();
      // EasyFeatureWrapper[] newFeatures = new EasyFeatureWrapper[0];
      // if( newSelection instanceof KalypsoFeatureThemeSelection )
      // {
      // final KalypsoFeatureThemeSelection newFts = (KalypsoFeatureThemeSelection) newSelection;
      // newFeatures = newFts.getAllFeatures();
      // }
      // EasyFeatureWrapper[] newWrappedSelection = FeatureSelectionHelper.mergeWrapper( oldFeatures, newFeatures );
      // System.out.println();
      // }

    }
  }

  private void select( final Point startPoint, final Point endPoint, final int radius, final int selectionMode, final boolean useOnlyFirstChoosen )
  {
    final IMapPanel mapPanel = getMapPanel();
    final GeoTransform transform = mapPanel.getProjection();
    final IMapModell model = mapPanel.getMapModell();
    final IFeatureSelectionManager selectionManager = mapPanel.getSelectionManager();

    final IKalypsoTheme activeTheme = model.getActiveTheme();
    if( (activeTheme == null) || !(activeTheme instanceof IKalypsoFeatureTheme) )
      return;

    if( startPoint != null )
    {
      final double g1x = transform.getSourceX( startPoint.getX() );
      final double g1y = transform.getSourceY( startPoint.getY() );

      if( endPoint == null ) // not dragged
      {
        // TODO depends on featuretype
        // line and point with radius
        // polygon without radius
        final double gisRadius = Math.abs( transform.getSourceX( startPoint.getX() + radius ) - g1x );
        final GM_Point pointSelect = GeometryFactory.createGM_Point( g1x, g1y, model.getCoordinatesSystem() );

        final Feature fe = (Feature) JMSelector.selectNearest( pointSelect, gisRadius, ((IKalypsoFeatureTheme) activeTheme).getFeatureListVisible( null ), false );

        final List<Feature> listFe = new ArrayList<Feature>();
        if( fe != null )
          listFe.add( fe );

        changeSelection( listFe, (IKalypsoFeatureTheme) activeTheme, selectionManager, selectionMode );
      }
      else
        // dragged
      {
        final double g2x = transform.getSourceX( endPoint.getX() );
        final double g2y = transform.getSourceY( endPoint.getY() );
        boolean withinStatus = false;

        if( (endPoint.getX() > startPoint.getX()) && (endPoint.getY() > startPoint.getY()) )
          withinStatus = true;

        final double minX = g1x < g2x ? g1x : g2x;
        final double maxX = g1x > g2x ? g1x : g2x;
        final double minY = g1y < g2y ? g1y : g2y;
        final double maxY = g1y > g2y ? g1y : g2y;

        if( (minX != maxX) && (minY != maxY) )
        {
          final GM_Envelope envSelect = GeometryFactory.createGM_Envelope( minX, minY, maxX, maxY, model.getCoordinatesSystem() );
          final List<Object> features = JMSelector.select( envSelect, ((IKalypsoFeatureTheme) activeTheme).getFeatureListVisible( envSelect ), withinStatus );

          if( useOnlyFirstChoosen && !features.isEmpty() )
          {
            // delete all but first if we shall only the first selected
            final Feature object = (Feature) features.get( 0 );
            features.clear();
            features.add( object );
          }

          changeSelection( features, (IKalypsoFeatureTheme) activeTheme, selectionManager, selectionMode );
        }
      }
    }
  }

  private void changeSelection( final List< ? > features, final IKalypsoFeatureTheme theme, final IFeatureSelectionManager selectionManager2, final int selectionMode )
  {
    // nothing was chosen by the user, clear selection
    if( features.isEmpty() )
      selectionManager2.clear();
    // TODO: this should do the widget-manager?

    // remove all selected features from this theme
    // TODO: maybe only visible??
    final FeatureList featureList = theme.getFeatureList();
    if( featureList == null )
      return;

    final Feature parentFeature = featureList.getParentFeature();
    final IRelationType parentProperty = featureList.getParentFeatureTypeProperty();

    // add all selected features
    final EasyFeatureWrapper[] selectedWrapped = new EasyFeatureWrapper[features.size()];
    for( int i = 0; i < features.size(); i++ )
    {
      final Feature f = (Feature) features.get( i );
      selectedWrapped[i] = new EasyFeatureWrapper( theme.getWorkspace(), f, parentFeature, parentProperty );
    }

    final Feature[] toRemove;
    final EasyFeatureWrapper[] toAdd;

    switch( selectionMode )
    {
      case MODE_TOGGLE: // dreht die selection der auswahl um
        // BUG: past nicht mehr zur beschreibung!
        toRemove = new Feature[0];
        toAdd = selectedWrapped;
        break;

      case MODE_SELECT: // selectert genau das, was ausgewählt wurde
        // toRemove = featureList.toFeatures();
        final EasyFeatureWrapper[] allFeatures = selectionManager2.getAllFeatures();
        toRemove = new Feature[allFeatures.length];
        for( int i = 0; i < allFeatures.length; i++ )
          toRemove[i] = allFeatures[i].getFeature();
        toAdd = selectedWrapped;
        break;

      case MODE_UNSELECT: // löscht alles augewählte aus der selection
        toRemove = featureList.toFeatures();
        toAdd = new EasyFeatureWrapper[0];

      default:
        throw new UnsupportedOperationException( Messages.getString( "org.kalypso.ogc.gml.map.MapPanel.18" ) + selectionMode ); //$NON-NLS-1$
    }

    selectionManager2.changeSelection( toRemove, toAdd );
  }
}