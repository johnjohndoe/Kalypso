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
package org.kalypso.kalypsomodel1d2d.ui.map.flowrel;

import java.awt.Graphics;
import java.awt.Point;
import java.util.List;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.gmlschema.GMLSchemaUtilities;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.kalypsosimulationmodel.core.flowrel.FlowRelationship;
import org.kalypso.kalypsosimulationmodel.core.flowrel.IFlowRelationship;
import org.kalypso.kalypsosimulationmodel.core.flowrel.IFlowRelationshipCollection;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.map.utilities.MapUtilities;
import org.kalypso.ogc.gml.map.widgets.AbstractWidget;
import org.kalypso.ogc.gml.map.widgets.mapfunctions.RectangleSelector;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

/**
 * This widget lets the user grab a flow relation.
 * 
 * @author Gernot Belger
 */
public abstract class AbstractSelectFlowrelationWidget extends AbstractWidget
{
  private final int m_grabRadius = 20;

  private IFlowRelationshipCollection m_flowRelCollection = null;

  private IKalypsoFeatureTheme m_flowTheme = null;

  private IFlowRelationship m_flowRelation = null;

  private RectangleSelector m_rectangleSelector = null;

  private final boolean m_allowMultipleSelection;

  public AbstractSelectFlowrelationWidget( final String name, final String toolTip, final boolean allowMultipleSelection )
  {
    super( name, toolTip );

    m_allowMultipleSelection = allowMultipleSelection;
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#activate(org.kalypso.commons.command.ICommandTarget,
   *      org.kalypso.ogc.gml.map.MapPanel)
   */
  @Override
  public void activate( final ICommandTarget commandPoster, final MapPanel mapPanel )
  {
    super.activate( commandPoster, mapPanel );

    reinit();
  }

  private void reinit( )
  {
    m_flowRelCollection = null;
    m_rectangleSelector = null;

    final MapPanel mapPanel = getMapPanel();
    final IMapModell mapModell = mapPanel.getMapModell();

    mapPanel.setMessage( "Klicken Sie in die Karte um einen Parameter hinzuzufügen." );

    final IKalypsoTheme activeTheme = mapModell.getActiveTheme();
    final IFeatureType activeFT = activeTheme instanceof IKalypsoFeatureTheme ? ((IKalypsoFeatureTheme) activeTheme).getFeatureType() : null;
    if( activeFT != null && GMLSchemaUtilities.substitutes( activeFT, IFlowRelationship.QNAME ) )
      m_flowTheme = (IKalypsoFeatureTheme) activeTheme;

    if( m_flowTheme == null )
      return;

    final FeatureList featureList = m_flowTheme.getFeatureList();
    final Feature parentFeature = featureList.getParentFeature();
    m_flowRelCollection = (IFlowRelationshipCollection) parentFeature.getAdapter( IFlowRelationshipCollection.class );
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#moved(java.awt.Point)
   */
  @Override
  public void moved( final Point p )
  {
    m_flowRelation = null;

    final GM_Point currentPos = MapUtilities.transform( getMapPanel(), p );

    /* Grab next flowrelation */
    if( m_flowTheme == null || m_flowRelCollection == null )
      return;

    final double grabDistance = MapUtilities.calculateWorldDistance( getMapPanel(), currentPos, m_grabRadius * 2 );
    m_flowRelation = m_flowRelCollection.findFlowrelationship( currentPos.getPosition(), grabDistance );

    final MapPanel panel = getMapPanel();
    if( panel != null )
      panel.repaint();
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

      final MapPanel panel = getMapPanel();
      if( panel != null )
        panel.repaint();
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

    if( m_flowRelation == null )
      return;
    final int smallRect = 10;
    final Point nodePoint = MapUtilities.retransform( getMapPanel(), m_flowRelation.getPosition() );
    g.drawRect( (int) nodePoint.getX() - smallRect, (int) nodePoint.getY() - smallRect, smallRect * 2, smallRect * 2 );
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#leftPressed(java.awt.Point)
   */
  @Override
  public void leftPressed( Point p )
  {
    if( m_allowMultipleSelection )
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
        final MapPanel mapPanel = getMapPanel();
        try
        {
          /* Find all flow relation inside the rect */
          final Point pmin = new Point( rectangle.x, rectangle.y );
          final Point pmax = new Point( rectangle.x + rectangle.width, rectangle.y + rectangle.height );

          final GM_Point minPoint = MapUtilities.transform( mapPanel, pmin );
          final GM_Point maxPoint = MapUtilities.transform( mapPanel, pmax );

          final GM_Envelope envelope = GeometryFactory.createGM_Envelope( minPoint.getPosition(), maxPoint.getPosition() );
          final GMLWorkspace workspace = m_flowRelCollection.getWrappedFeature().getWorkspace();
          final List result = m_flowRelCollection.getWrappedList().query( envelope, null );
          final IFlowRelationship[] flowRels = new IFlowRelationship[result.size()];
          for( int i = 0; i < flowRels.length; i++ )
          {
            final Feature feature = FeatureHelper.getFeature( workspace, result.get( i ) );
            // we cannot adapt to IFlowRelation, but this works
            flowRels[i] = new FlowRelationship( feature, IFlowRelationship.QNAME )
            {
            };
          }

          flowRelationGrabbed( m_flowTheme, flowRels );
        }
        catch( final Throwable t )
        {
          final IStatus status = StatusUtilities.statusFromThrowable( t );
          final Display display = PlatformUI.getWorkbench().getDisplay();
          display.asyncExec( new Runnable()
          {
            public void run( )
            {
              final Shell shell = display.getActiveShell();
              ErrorDialog.openError( shell, getName(), "Fehler beim Hinzufügen eines Parameters", status );
            }
          } );
        }

        reinit();
        mapPanel.repaint();
        return;
      }
    }
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#leftReleased(java.awt.Point)
   */
  @Override
  public void leftClicked( final Point p )
  {
    final String problemMessage;
    if( m_flowRelation == null )
      problemMessage = "Hier ist kein 1D-Netzparameter.";
    else
      problemMessage = null;

    if( problemMessage != null )
    {
      final Display display = PlatformUI.getWorkbench().getDisplay();
      display.asyncExec( new Runnable()
      {
        public void run( )
        {
          final Shell shell = display.getActiveShell();
          MessageDialog.openWarning( shell, getName(), problemMessage );
        }
      } );
      return;
    }

    /* Check preconditions */
    if( m_flowRelCollection == null )
      return;

    try
    {
      flowRelationGrabbed( m_flowTheme, new IFlowRelationship[] { m_flowRelation } );

      getMapPanel().repaint();
    }
    catch( final Throwable e )
    {
      final IStatus status = StatusUtilities.statusFromThrowable( e );
      final Display display = PlatformUI.getWorkbench().getDisplay();
      display.asyncExec( new Runnable()
      {
        public void run( )
        {
          final Shell shell = display.getActiveShell();
          ErrorDialog.openError( shell, getName(), "Fehler beim Hinzufügen eines Parameters", status );
        }
      } );
    }
  }

  protected abstract void flowRelationGrabbed( final IKalypsoFeatureTheme flowTheme, final IFlowRelationship[] flowRels ) throws Exception;
}
