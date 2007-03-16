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

import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.gmlschema.GMLSchemaUtilities;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.kalypsosimulationmodel.core.flowrel.IFlowRelationship;
import org.kalypso.kalypsosimulationmodel.core.flowrel.IFlowRelationshipCollection;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.map.utilities.MapUtilities;
import org.kalypso.ogc.gml.map.widgets.AbstractWidget;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.geometry.GM_Point;

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

  private GM_Point m_currentPos = null;

  private IFlowRelationship m_flowRelation;

  public AbstractSelectFlowrelationWidget( final String name, final String toolTip )
  {
    super( name, toolTip );
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

    m_currentPos = MapUtilities.transform( getMapPanel(), p );

    /* Grab next flowrelation */
    if( m_flowTheme == null || m_flowRelCollection == null )
      return;

    final double grabDistance = MapUtilities.calculateWorldDistance( getMapPanel(), m_currentPos, m_grabRadius * 2 );
    m_flowRelation = m_flowRelCollection.findFlowrelationship( m_currentPos, grabDistance );
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#paint(java.awt.Graphics)
   */
  @Override
  public void paint( final Graphics g )
  {
    if( m_currentPos == null )
      return;

    final Point currentPoint = MapUtilities.retransform( getMapPanel(), m_currentPos );

    g.drawRect( (int) currentPoint.getX() - m_grabRadius, (int) currentPoint.getY() - m_grabRadius, m_grabRadius * 2, m_grabRadius * 2 );

    if( m_flowRelation == null )
      return;

    final int smallRect = 10;
    final Point nodePoint = MapUtilities.retransform( getMapPanel(), m_flowRelation.getPosition() );
    g.drawRect( (int) nodePoint.getX() - smallRect, (int) nodePoint.getY() - smallRect, smallRect * 2, smallRect * 2 );
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#leftClicked(java.awt.Point)
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
      flowRelationGrabbed( m_flowTheme, m_flowRelation );
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

  protected abstract void flowRelationGrabbed( final IKalypsoFeatureTheme flowTheme, final IFlowRelationship flowRelation ) throws Exception;
}
