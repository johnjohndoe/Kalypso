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
package org.kalypso.kalypso1d2d.pjt.map;

import java.awt.Graphics;
import java.awt.Point;
import java.awt.event.MouseEvent;
import java.util.List;

import javax.xml.namespace.QName;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.kalypso1d2d.internal.i18n.Messages;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.map.IMapPanel;
import org.kalypso.ogc.gml.map.utilities.MapUtilities;
import org.kalypso.ogc.gml.map.widgets.mapfunctions.RectangleSelector;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.util.MapUtils;
import org.kalypso.ogc.gml.widgets.AbstractWidget;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.kalypsodeegree_impl.tools.GeometryUtilities;

/**
 * @author Thomas Jung
 *
 */
public abstract class AbstractEditHydrographWidget extends AbstractWidget
{
  private final int m_grabRadius = 20;

  private FeatureList m_featureList = null;

  private Feature m_foundFeature = null;

  private RectangleSelector m_rectangleSelector = null;

  private final boolean m_allowMultipleSelection;

  private QName m_geomQName = null;

  private final IKalypsoFeatureTheme m_theme;

  public AbstractEditHydrographWidget( final String name, final String toolTip, final boolean allowMultipleSelection, final QName geomQName, final IKalypsoFeatureTheme theme )
  {
    super( name, toolTip );
    m_allowMultipleSelection = allowMultipleSelection;
    m_geomQName = geomQName;
    m_theme = theme;
  }

  @Override
  public void activate( final ICommandTarget commandPoster, final IMapPanel mapPanel )
  {
    super.activate( commandPoster, mapPanel );

    reinit();
  }

  @Override
  public void paint( final Graphics g )
  {
    MapUtils.paintRect( g, getMapPanel(), m_foundFeature, m_geomQName, m_rectangleSelector, m_grabRadius );
  }

  @Override
  public void mouseReleased( final MouseEvent e )
  {
    if( e.getButton() != MouseEvent.BUTTON1 )
      return;

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
          final GMLWorkspace workspace = m_featureList.getOwner().getWorkspace();
          final List< ? > result = m_featureList.query( envelope, null );
          final Feature[] selectedFeatures = new Feature[result.size()];
          for( int i = 0; i < selectedFeatures.length; i++ )
            selectedFeatures[i] = FeatureHelper.getFeature( workspace, result.get( i ) );

          featureGrabbed( m_theme.getWorkspace(), selectedFeatures );
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
              ErrorDialog.openError( shell, getName(), Messages.getString( "org.kalypso.kalypso1d2d.pjt.map.AbstractEditHydrographWidget.0" ), status ); //$NON-NLS-1$
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

  protected final void reinit( )
  {
    m_featureList = null;
    m_foundFeature = null;
    m_rectangleSelector = null;

    if( m_theme == null )
      return;

    m_featureList = m_theme.getFeatureList();
  }

  @Override
  public void mouseDragged( final MouseEvent e )
  {
    if( m_rectangleSelector != null )
    {
      final Point point = e.getPoint();
      m_rectangleSelector.setEndPoint( new org.eclipse.swt.graphics.Point( point.x, point.y ) );

      repaintMap();
    }
  }

  @Override
  public void mousePressed( final MouseEvent e )
  {
    if( m_allowMultipleSelection && m_featureList != null )
    {
      final Point p = e.getPoint();

      m_rectangleSelector = new RectangleSelector( new org.eclipse.swt.graphics.Point( p.x, p.y ) );
    }
  }

  @Override
  public void mouseClicked( final MouseEvent event )
  {
    if( event.getButton() != MouseEvent.BUTTON1 )
      return;

    final String problemMessage;
    if( m_foundFeature == null )
      problemMessage = Messages.getString( "org.kalypso.kalypso1d2d.pjt.map.AbstractEditHydrographWidget.1" ); //$NON-NLS-1$
    else
      problemMessage = null;

    if( problemMessage != null )
    {
      final Display display = PlatformUI.getWorkbench().getDisplay();
      display.asyncExec( new Runnable()
      {
        @Override
        public void run( )
        {
          final Shell shell = display.getActiveShell();
          MessageDialog.openWarning( shell, getName(), problemMessage );
        }
      } );
      return;
    }

    /* Check preconditions */
    if( m_featureList == null )
      return;

    try
    {
      featureGrabbed( m_theme.getWorkspace(), new Feature[] { m_foundFeature } );

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
          ErrorDialog.openError( shell, getName(), Messages.getString( "org.kalypso.kalypso1d2d.pjt.map.AbstractEditHydrographWidget.2" ), status ); //$NON-NLS-1$
        }
      } );
    }
  }

  @Override
  public void mouseMoved( final MouseEvent e )
  {
    m_foundFeature = null;

    final GM_Point currentPos = MapUtilities.transform( getMapPanel(), e.getPoint() );

    /* Grab next feature */
    if( m_featureList == null )
      return;

    final double grabDistance = MapUtilities.calculateWorldDistance( getMapPanel(), currentPos, m_grabRadius * 2 );
    m_foundFeature = GeometryUtilities.findNearestFeature( currentPos, grabDistance, m_featureList, m_geomQName );

    final IMapPanel panel = getMapPanel();
    if( panel != null )
      panel.repaintMap();
  }

  protected abstract void featureGrabbed( CommandableWorkspace workspace, Feature[] selectedFeatures ) throws Exception;

}
