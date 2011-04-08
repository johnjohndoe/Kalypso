/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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
package org.kalypso.kalypsomodel1d2d.ui.map.util;

import java.awt.Graphics;
import java.awt.Point;
import java.util.List;

import javax.xml.namespace.QName;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.gmlschema.GMLSchemaUtilities;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.map.IMapPanel;
import org.kalypso.ogc.gml.map.utilities.MapUtilities;
import org.kalypso.ogc.gml.map.widgets.mapfunctions.RectangleSelector;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ogc.gml.selection.EasyFeatureWrapper;
import org.kalypso.ogc.gml.selection.FeatureSelectionHelper;
import org.kalypso.ogc.gml.selection.IFeatureSelectionManager;
import org.kalypso.ogc.gml.widgets.AbstractWidget;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.kalypsodeegree_impl.tools.GeometryUtilities;

/**
 * This widget lets the user grab a roughness polygon.
 * 
 * @author Gernot Belger
 */
public abstract class AbstractSelectRoughnessPolygonWidget extends AbstractWidget
{
  private final int m_grabRadius = 5;

  private IKalypsoFeatureTheme m_theme = null;

  private FeatureList m_featureList = null;

  private Feature m_foundFeature = null;

  private RectangleSelector m_rectangleSelector = null;

  private final boolean m_allowMultipleSelection;

  private QName m_geomQName = null;

  private final QName m_qnameToSelect;

  public AbstractSelectRoughnessPolygonWidget( final String name, final String toolTip, final boolean allowMultipleSelection, final QName qnameToSelect, final QName geomQName )
  {
    super( name, toolTip );

    m_allowMultipleSelection = allowMultipleSelection;
    m_qnameToSelect = qnameToSelect;
    m_geomQName = geomQName;
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#activate(org.kalypso.commons.command.ICommandTarget,
   *      org.kalypso.ogc.gml.map.MapPanel)
   */
  @Override
  public void activate( final ICommandTarget commandPoster, final IMapPanel mapPanel )
  {
    super.activate( commandPoster, mapPanel );

    reinit();
  }

  private void reinit( )
  {
    m_theme = null;
    m_featureList = null;
    m_foundFeature = null;
    m_rectangleSelector = null;

    final IMapPanel mapPanel = getMapPanel();
    final IMapModell mapModell = mapPanel.getMapModell();

    final IKalypsoTheme activeTheme = mapModell.getActiveTheme();
    final IFeatureType activeFT = activeTheme instanceof IKalypsoFeatureTheme ? ((IKalypsoFeatureTheme) activeTheme).getFeatureType() : null;
    if( activeFT != null && GMLSchemaUtilities.substitutes( activeFT, m_qnameToSelect ) )
      m_theme = (IKalypsoFeatureTheme) activeTheme;

    if( m_theme == null )
      return;

    m_featureList = m_theme.getFeatureList();
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#moved(java.awt.Point)
   */
  @Override
  public void moved( final Point p )
  {
    m_foundFeature = null;

    final GM_Point currentPos = MapUtilities.transform( getMapPanel(), p );

    /* Grab next flowrelation */
    if( m_featureList == null )
      return;

    final double grabDistance = MapUtilities.calculateWorldDistance( getMapPanel(), currentPos, m_grabRadius * 2 );
    m_foundFeature = GeometryUtilities.findNearestFeature( currentPos, grabDistance, m_featureList, m_geomQName );

    final IMapPanel panel = getMapPanel();
    if( panel != null )
      panel.repaintMap();
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#dragged(java.awt.Point)
   */
  @Override
  public void dragged( final Point p )
  {
    if( m_rectangleSelector != null )
    {
      m_rectangleSelector.setEndPoint( new org.eclipse.swt.graphics.Point( p.x, p.y ) );

      final IMapPanel panel = getMapPanel();
      if( panel != null )
        panel.repaintMap();
    }
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#paint(java.awt.Graphics)
   */
  @Override
  public void paint( final Graphics g )
  {
    /* Draw drag rect if rectangle is big enoeugh */
    if( m_rectangleSelector != null )
    {
      final Rectangle rectangle = m_rectangleSelector.getRectangle();
      if( rectangle != null && (rectangle.width > m_grabRadius || rectangle.height > m_grabRadius) )
      {
        g.drawRect( rectangle.x, rectangle.y, rectangle.width, rectangle.height );
        return;
      }
    }

    if( m_foundFeature == null )
      return;
    final GM_Object geom = (GM_Object) m_foundFeature.getProperty( m_geomQName );
    if( geom == null )
      return;

    final int smallRect = 10;
    final Point nodePoint = MapUtilities.retransform( getMapPanel(), geom.getCentroid() );
    g.drawRect( (int) nodePoint.getX() - smallRect, (int) nodePoint.getY() - smallRect, smallRect * 2, smallRect * 2 );
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#leftPressed(java.awt.Point)
   */
  @Override
  public void leftPressed( final Point p )
  {
    if( m_allowMultipleSelection && m_featureList != null )
      m_rectangleSelector = new RectangleSelector( new org.eclipse.swt.graphics.Point( p.x, p.y ) );
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#leftReleased(java.awt.Point)
   */
  @Override
  public void leftReleased( final Point p )
  {
    /* If we have a drag rect handle that first */
    if( m_rectangleSelector != null )
    {
      final Rectangle rectangle = m_rectangleSelector.getRectangle();
      if( rectangle != null && (rectangle.width > m_grabRadius || rectangle.height > m_grabRadius) )
      {
        final IMapPanel mapPanel = getMapPanel();
        try
        {
          /* Find all flow relation inside the rect */
          final Point pmin = new Point( rectangle.x, rectangle.y );
          final Point pmax = new Point( rectangle.x + rectangle.width, rectangle.y + rectangle.height );

          final GM_Point minPoint = MapUtilities.transform( mapPanel, pmin );
          final GM_Point maxPoint = MapUtilities.transform( mapPanel, pmax );

          final GM_Envelope envelope = GeometryFactory.createGM_Envelope( minPoint.getPosition(), maxPoint.getPosition(), minPoint.getCoordinateSystem() );
          final GMLWorkspace workspace = m_featureList.getParentFeature().getWorkspace();
          final List result = m_featureList.query( envelope, null );
          final Feature[] selectedFeatures = new Feature[result.size()];
          for( int i = 0; i < selectedFeatures.length; i++ )
            selectedFeatures[i] = FeatureHelper.getFeature( workspace, result.get( i ) );

          elementGrabbed( m_theme.getWorkspace(), selectedFeatures );
        }
        catch( final Throwable t )
        {
          final IStatus status = StatusUtilities.statusFromThrowable( t );
          final Display display = PlatformUI.getWorkbench().getDisplay();
          display.asyncExec( new Runnable()
          {
            @Override
            public void run( )
            {
              final Shell shell = display.getActiveShell();
              ErrorDialog.openError( shell, getName(), Messages.getString("org.kalypso.kalypsomodel1d2d.ui.map.util.AbstractSelectRoughnessPolygonWidget.0"), status ); //$NON-NLS-1$
            }
          } );
        }
        finally
        {
          reinit();
          mapPanel.repaintMap();
        }
      }
    }
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#rightClicked(java.awt.Point)
   */
  @Override
  public void rightClicked( Point p )
  {
    // super.rightClicked( p );
    try
    {
      final IFeatureSelectionManager selectionManager = getMapPanel().getSelectionManager();
      final Feature[] featuresToRemove = FeatureSelectionHelper.getFeatures( selectionManager );
      selectionManager.changeSelection( featuresToRemove, new EasyFeatureWrapper[] {} );
      getMapPanel().repaintMap();
    }
    catch( final Throwable e )
    {
      final IStatus status = StatusUtilities.statusFromThrowable( e );
      final Display display = PlatformUI.getWorkbench().getDisplay();
      display.asyncExec( new Runnable()
      {
        @Override
        public void run( )
        {
          final Shell shell = display.getActiveShell();
          ErrorDialog.openError( shell, getName(), Messages.getString("org.kalypso.kalypsomodel1d2d.ui.map.util.AbstractSelectRoughnessPolygonWidget.1"), status ); //$NON-NLS-1$
        }
      } );
    }

    // TODO: kill popup!

  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#leftReleased(java.awt.Point)
   */
  @Override
  public void leftClicked( final Point p )
  {
    // final String problemMessage;
    // if( m_foundFeature == null )
    // problemMessage = "Kein Feature gefunden.";
    // else
    // problemMessage = null;
    //
    // if( problemMessage != null )
    // {
    // final Display display = PlatformUI.getWorkbench().getDisplay();
    // display.asyncExec( new Runnable()
    // {
    // public void run( )
    // {
    // final Shell shell = display.getActiveShell();
    // MessageDialog.openWarning( shell, getName(), problemMessage );
    // }
    // } );
    // return;
    // }

    /* Check preconditions */
    if( m_featureList == null )
      return;

    try
    {
      if( m_foundFeature != null )
        elementGrabbed( m_theme.getWorkspace(), new Feature[] { m_foundFeature } );
      else
      {
        final IFeatureSelectionManager selectionManager = getMapPanel().getSelectionManager();
        final Feature[] featuresToRemove = FeatureSelectionHelper.getFeatures( selectionManager );
        selectionManager.changeSelection( featuresToRemove, new EasyFeatureWrapper[] {} );
      }

      getMapPanel().repaintMap();
    }
    catch( final Throwable e )
    {
      final IStatus status = StatusUtilities.statusFromThrowable( e );
      final Display display = PlatformUI.getWorkbench().getDisplay();
      display.asyncExec( new Runnable()
      {
        @Override
        public void run( )
        {
          final Shell shell = display.getActiveShell();
          ErrorDialog.openError( shell, getName(), Messages.getString("org.kalypso.kalypsomodel1d2d.ui.map.util.AbstractSelectRoughnessPolygonWidget.2"), status ); //$NON-NLS-1$
        }
      } );
    }
  }

  protected abstract void elementGrabbed( final CommandableWorkspace workspace, final Feature[] selectedFeatures ) throws Exception;
}
