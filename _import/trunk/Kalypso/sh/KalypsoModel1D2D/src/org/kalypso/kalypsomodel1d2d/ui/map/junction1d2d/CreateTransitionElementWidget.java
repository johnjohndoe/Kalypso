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
package org.kalypso.kalypsomodel1d2d.ui.map.junction1d2d;

import java.awt.Color;
import java.awt.Graphics;
import java.awt.Point;
import java.awt.event.KeyEvent;
import java.util.HashMap;

import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IContinuityLine1D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IContinuityLine2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFELine;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.CreateTransitionElementCommand;
import org.kalypso.kalypsomodel1d2d.ui.map.util.UtilMap;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.map.utilities.MapUtilities;
import org.kalypso.ogc.gml.map.widgets.AbstractWidget;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ogc.gml.selection.EasyFeatureWrapper;
import org.kalypso.ogc.gml.selection.IFeatureSelectionManager;
import org.kalypsodeegree.graphics.displayelements.DisplayElement;
import org.kalypsodeegree.graphics.sld.LineSymbolizer;
import org.kalypsodeegree.graphics.sld.Stroke;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.graphics.displayelements.DisplayElementFactory;
import org.kalypsodeegree_impl.graphics.sld.LineSymbolizer_Impl;
import org.kalypsodeegree_impl.graphics.sld.Stroke_Impl;

public class CreateTransitionElementWidget extends AbstractWidget
{
  private final int m_grabRadius = 10;

  private IKalypsoFeatureTheme m_mapTheme;

  private IFEDiscretisationModel1d2d m_discretisationModel;

  private IContinuityLine1D m_line1D;

  private IContinuityLine2D m_line2D;

  private IFELine m_currentSelectedLine;
  
  public CreateTransitionElementWidget( )
  {
    this( "name", "tooltip" );
  }
  
  public CreateTransitionElementWidget( String name, String toolTip )
  {
    super( name, toolTip );
  }

  @Override
  public void activate( final ICommandTarget commandPoster, final MapPanel mapPanel )
  {
    super.activate( commandPoster, mapPanel );
    reinit();
  }

  private void reinit( )
  {
    m_line1D = null;
    m_line2D = null;

    final MapPanel mapPanel = getMapPanel();
    final IMapModell mapModell = mapPanel.getMapModell();

    mapPanel.setMessage( "Klicken Sie in die Karte :)" );

    m_mapTheme = UtilMap.findEditableTheme( mapModell, IFELine.QNAME );
    m_discretisationModel = UtilMap.findFEModelTheme( mapModell );
    if( m_mapTheme == null || m_discretisationModel == null )
      return;

    final FeatureList featureList = m_mapTheme.getFeatureList();
    final Feature parentFeature = featureList.getParentFeature();
    m_discretisationModel = (IFEDiscretisationModel1d2d) parentFeature.getAdapter( IFEDiscretisationModel1d2d.class );
    final IFeatureSelectionManager selectionManager = getMapPanel().getSelectionManager();
    selectionManager.clear();
    mapPanel.repaint();
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#keyPressed(java.awt.event.KeyEvent)
   */
  @Override
  public void keyPressed( KeyEvent e )
  {
    if( e.getKeyChar() == KeyEvent.VK_ESCAPE )
      reinit();
    else if( e.getKeyChar() == KeyEvent.VK_ENTER )
    {
      if( m_line1D != null && m_line2D != null )
      {
        final CreateTransitionElementCommand command = new CreateTransitionElementCommand( m_discretisationModel, m_line1D, m_line2D );
        try
        {
          m_mapTheme.getWorkspace().postCommand( command );
        }
        catch( Exception e1 )
        {
          // TODO Auto-generated catch block
          e1.printStackTrace();
        }
      }
    }
    super.keyPressed( e );
    getMapPanel().repaint();
  }

  @Override
  public void moved( final Point p )
  {
    final MapPanel mapPanel = getMapPanel();
    if( mapPanel == null )
      return;
    if( m_discretisationModel == null )
    {
      m_line1D = null;
      m_line2D = null;
      mapPanel.repaint();
      return;
    }
    final GM_Point currentPos = MapUtilities.transform( mapPanel, p );
    final double grabDistance = MapUtilities.calculateWorldDistance( mapPanel, currentPos, m_grabRadius );
    m_currentSelectedLine = m_discretisationModel.findContinuityLine( currentPos, grabDistance / 2 );
    mapPanel.repaint();
  }

  @Override
  public void paint( final Graphics g )
  {
    if( m_currentSelectedLine == null )
      return;
    final GM_Curve line = m_currentSelectedLine.getGeometry();
    final LineSymbolizer symb = new LineSymbolizer_Impl();
    final Stroke stroke = new Stroke_Impl( new HashMap<Object, Object>(), null, null );
    stroke.setWidth( 3 );
    stroke.setStroke( new Color( 255, 0, 0 ) );
    symb.setStroke( stroke );
    final DisplayElement de = DisplayElementFactory.buildLineStringDisplayElement( m_currentSelectedLine.getWrappedFeature(), line, symb );
    de.paint( g, getMapPanel().getProjection() );
  }

  @Override
  public void leftClicked( final Point p )
  {
    if( m_currentSelectedLine == null )
      return;
    if( m_currentSelectedLine instanceof IContinuityLine1D )
      m_line1D = (IContinuityLine1D) m_currentSelectedLine;
    else
      m_line2D = (IContinuityLine2D) m_currentSelectedLine;
    final IFeatureSelectionManager selectionManager = getMapPanel().getSelectionManager();
    final Feature featureToSelect = m_currentSelectedLine.getWrappedFeature();
    final EasyFeatureWrapper easyToSelect = new EasyFeatureWrapper( m_mapTheme.getWorkspace(), featureToSelect, featureToSelect.getParent(), featureToSelect.getParentRelation() );
    selectionManager.changeSelection( new Feature[] {}, new EasyFeatureWrapper[] { easyToSelect } );
  }

  @Override
  public void finish( )
  {
    /* Deselect all */
    final IFeatureSelectionManager selectionManager = getMapPanel().getSelectionManager();
    selectionManager.clear();
    super.finish();
  }
}