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
package org.kalypso.model.wspm.sobek.core.digitools.crosssection;

import java.awt.Cursor;
import java.awt.Graphics;
import java.awt.Point;

import javax.xml.namespace.QName;

import org.apache.commons.lang.NotImplementedException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.ui.progress.UIJob;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.model.wspm.sobek.core.SobekModelMember;
import org.kalypso.model.wspm.sobek.core.interfaces.ISobekConstants;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.map.utilities.MapUtilities;
import org.kalypso.ogc.gml.map.widgets.AbstractWidget;
import org.kalypso.ogc.gml.map.widgets.changers.SingleSelectionChanger;
import org.kalypso.ogc.gml.map.widgets.mapfunctions.IRectangleMapFunction;
import org.kalypso.ogc.gml.map.widgets.mapfunctions.RectangleSelector;
import org.kalypso.ogc.gml.map.widgets.mapfunctions.SelectFeaturesMapFunction;
import org.kalypso.ogc.gml.map.widgets.providers.QNameFeaturesProvider;
import org.kalypso.ogc.gml.selection.EasyFeatureWrapper;
import org.kalypso.ogc.gml.selection.IFeatureSelectionManager;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Point;

/**
 * @author kuch
 */
public class FNCreateCrossSectionNode extends AbstractWidget
{
  protected FNSnapPainterCreateProfileNode m_snapPainter = null;

  private GM_Point m_pos;

  private Point m_currentPoint;

  private GM_Point m_snappedBranchPoint = null;

  private RectangleSelector m_selector;

  private final IRectangleMapFunction m_clickFunction = new SelectFeaturesMapFunction( SelectFeaturesMapFunction.DEFAULT_RADIUS, new QNameFeaturesProvider( ISobekConstants.QN_NOFDP_HYDRAULIC_PROFILE ), new SingleSelectionChanger( true ), KalypsoCorePlugin.getDefault().getSelectionManager() );

// new IRectangleMapFunction()
// {
// public void execute( final MapPanel mapPanel, final Rectangle rectangle )
// {
// final IKalypsoTheme[] themes = mapPanel.getMapModell().getAllThemes();
//
// final List<EasyFeatureWrapper> myFeatures = new ArrayList<EasyFeatureWrapper>();
//
// for( final IKalypsoTheme theme : themes )
// if( theme instanceof IKalypsoCascadingTheme )
// {
// IKalypsoCascadingTheme ct = (IKalypsoCascadingTheme) theme;
//
// int asdfasdf = 0;
//
// }
//
// //
// // /* measure are type of gml - all other are type of shape, etc */
// // if( "GML".equals( theme.getType() ) )
// // {
// // final IKalypsoFeatureTheme featureTheme = (IKalypsoFeatureTheme) theme;
// // final FeatureList list = featureTheme.getFeatureList();
// // for( final Object object : list )
// // if( object instanceof Feature )
// // {
// // final Feature f = (Feature) object;
// // final EasyFeatureWrapper eft = new EasyFeatureWrapper( new CommandableWorkspace( f.getWorkspace() ), f,
// // f.getParent(), f.getParentRelation() );
// //
// // myFeatures.add( eft );
// // }
// // }
//
// /* no features found?!? */
// if( myFeatures.size() == 0 )
// return;
//
// final EasyFeatureWrapper[] selected = MapfunctionHelper.findFeatureToSelect( mapPanel, rectangle, myFeatures.toArray(
// new EasyFeatureWrapper[] {} ), 10 );
// if( selected.length <= 0 )
// return;
//
// final IFeatureSelectionManager manager = mapPanel.getSelectionManager();
// manager.setSelection( selected );
// }
// };

  public FNCreateCrossSectionNode( )
  {
    super( "Create a new Flow Network geometry", "Create a new Flow Network geometry" );

    new UIJob( "loading hydraulic model workspcae" )
    {
      @Override
      public IStatus runInUIThread( final IProgressMonitor monitor )
      {

        m_snapPainter = new FNSnapPainterCreateProfileNode( SobekModelMember.getModel( null ) );
        return Status.OK_STATUS;
      }
    }.schedule();

  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#activate(org.kalypso.commons.command.ICommandTarget,
   *      org.kalypso.ogc.gml.map.MapPanel)
   */
  @Override
  public void activate( final ICommandTarget commandPoster, final MapPanel mapPanel )
  {
    super.activate( commandPoster, mapPanel );

  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#dragged(java.awt.Point)
   */
  @Override
  public void dragged( final Point p )
  {
    if( m_selector != null )
    {
      m_selector.setEndPoint( new org.eclipse.swt.graphics.Point( p.x, p.y ) );
      getMapPanel().setMessage( "Processing selection ..." );
    }

    final MapPanel panel = getMapPanel();
    if( panel != null )
      panel.repaint();

  }

  private Feature getProfileFeature( final EasyFeatureWrapper[] features )
  {
    for( final EasyFeatureWrapper eft : features )
    {
      final Feature feature = eft.getFeature();

      final QName ftName = feature.getFeatureType().getQName();
      if( ISobekConstants.QN_NOFDP_HYDRAULIC_PROFILE.equals( ftName ) )
        return feature;
    }

    // TODO Auto-generated method stub
    return null;
  }

  /**
   * leftClicked differs between two actions:<br>
   * 1. snap branch profile point (m_snappedBranchPoint == null)<br>
   * 2. select profile
   */
  @Override
  public void leftClicked( final Point p )
  {
    m_pos = MapUtilities.transform( getMapPanel(), p );

    // $ANALYSIS-IGNORE
    try
    {
      // 1. snap branch profile point
      if( m_snappedBranchPoint == null )
      {
        final GM_Point point = m_snapPainter.getSnapPoint( getMapPanel(), m_pos );
        if( point != null )
        {
          m_snappedBranchPoint = point;

          final java.awt.Cursor cursor = java.awt.Cursor.getPredefinedCursor( Cursor.CROSSHAIR_CURSOR );
          getMapPanel().setCursor( cursor );
          getMapPanel().setMessage( "select profile by drawing a rectangle with help of left mouse button..." );
        }

      }
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }
  }

  /**
   * draw rectangle to select profile
   */
  @Override
  public void leftPressed( final Point p )
  {
    if( m_snappedBranchPoint != null )
    {
      m_selector = new RectangleSelector( new org.eclipse.swt.graphics.Point( p.x, p.y ) );
      getMapPanel().setMessage( "Starting selection of profile ..." );
    }
  }

  /**
   * rectangle includes an profile?!?
   */
  @Override
  public void leftReleased( final Point p )
  {
    if( (m_snappedBranchPoint != null) && (m_selector != null) )
    {
      /* Set the end point. */
      m_selector.setEndPoint( new org.eclipse.swt.graphics.Point( p.x, p.y ) );

      // profile selected?!?
      final IFeatureSelectionManager sm = KalypsoCorePlugin.getDefault().getSelectionManager();
      m_clickFunction.execute( getMapPanel(), m_selector.getRectangle() );
      final Feature profile = getProfileFeature( sm.getAllFeatures() );

      if( profile != null )
        performFinish( profile );

    }

  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#moved(java.awt.Point)
   */
  @Override
  public void moved( final Point p )
  {
    m_currentPoint = p;

    /* Repaint. */
    final MapPanel panel = getMapPanel();
    if( panel != null )
      panel.repaint();
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#paint(java.awt.Graphics)
   */
  @Override
  public void paint( final Graphics g )
  {

    if( m_selector != null )
      m_selector.paint( g );

    if( m_snappedBranchPoint == null )
      if( m_snapPainter != null && m_currentPoint != null )
      {
        final Point point = m_snapPainter.paint( g, getMapPanel(), m_currentPoint );
        if( point != null )
          m_currentPoint = point;
      }
  }

  private void performFinish( final Feature profile )
  {
    try
    {
      final java.awt.Cursor cursor = java.awt.Cursor.getPredefinedCursor( Cursor.DEFAULT_CURSOR );
      getMapPanel().setCursor( cursor );

      getMapPanel().setMessage( "" );

// FNGmlUtils.createProfileNode( pool, m_snapPainter.getLastSnappedFeature(), m_snappedBranchPoint, profile );
      throw (new NotImplementedException());
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }

    m_snappedBranchPoint = null;
    m_selector = null;

    super.finish();
  }

}
