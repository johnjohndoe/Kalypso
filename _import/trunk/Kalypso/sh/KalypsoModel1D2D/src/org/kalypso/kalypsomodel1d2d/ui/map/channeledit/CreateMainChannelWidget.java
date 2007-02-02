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
package org.kalypso.kalypsomodel1d2d.ui.map.channeledit;

import java.awt.Graphics;
import java.awt.Point;
import java.awt.event.KeyEvent;

import org.eclipse.jface.viewers.ISelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.model.wspm.core.gml.WspmProfile;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.map.widgets.AbstractWidget;
import org.kalypso.ogc.gml.widgets.IWidget;
import org.kalypso.ui.editor.mapeditor.views.IWidgetWithOptions;
import org.kalypsodeegree.graphics.displayelements.DisplayElement;
import org.kalypsodeegree.graphics.displayelements.IncompatibleGeometryTypeException;
import org.kalypsodeegree.graphics.sld.LineSymbolizer;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree_impl.graphics.displayelements.DisplayElementFactory;
import org.kalypsodeegree_impl.graphics.sld.LineSymbolizer_Impl;

/**
 * @author Thomas Jung
 */
public class CreateMainChannelWidget extends AbstractWidget implements IWidgetWithOptions
{
  private final CreateChannelData m_data = new CreateChannelData( this );

  private CreateMainChannelComposite m_composite;

  private IWidget m_delegateWidget = null;

  public CreateMainChannelWidget( )
  {
    super( "Flussschlauch", "Flussschlauch anhand von Profilen generieren" );
  }

  /**
   * @see org.kalypso.ui.editor.mapeditor.views.IWidgetWithOptions#createControl(org.eclipse.swt.widgets.Composite)
   */
  public Control createControl( final Composite parent )
  {
    m_composite = new CreateMainChannelComposite( parent, SWT.BORDER, m_data, this );
    return m_composite;
  }

  /**
   * @see org.kalypso.ui.editor.mapeditor.views.IWidgetWithOptions#disposeControl()
   */
  public void disposeControl( )
  {
    if( m_composite != null )
    {
      m_composite.dispose();
      m_composite = null;
    }
  }

  public void setDelegate( final IWidget delegateWdget )
  {
    if( m_delegateWidget != null )
      m_delegateWidget.finish();
    
    m_delegateWidget = delegateWdget;

    if( m_delegateWidget != null )
      m_delegateWidget.activate( getCommandTarget(), getMapPanel() );
  }
  
  public MapPanel getPanel( )
  {
    return super.getMapPanel();
  }
  
  /**
   * @param g
   * @see org.kalypso.ogc.gml.widgets.IWidget#paint(java.awt.Graphics)
   */
  @Override
  public void paint( final Graphics g )
  {
    final Feature[] selectedProfiles = m_data.getSelectedProfiles();
    for( final Feature feature : selectedProfiles )
    {
      final WspmProfile profile = new WspmProfile(feature);
      final GM_Curve line = profile.getLine();
      
      try
      {
        final LineSymbolizer symb = new LineSymbolizer_Impl();
        final DisplayElement de = DisplayElementFactory.buildLineStringDisplayElement( feature, line, symb );
        de.paint( g, getMapPanel().getProjection() );
      }
      catch( final IncompatibleGeometryTypeException e )
      {
        // TODO Auto-generated catch block
        e.printStackTrace();
      }
    }
    
    if( m_delegateWidget != null )
      m_delegateWidget.paint( g );
  }
  
  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#activate(org.kalypso.commons.command.ICommandTarget, org.kalypso.ogc.gml.map.MapPanel)
   */
  @Override
  public void activate( ICommandTarget commandPoster, MapPanel mapPanel )
  {
    super.activate(commandPoster, mapPanel);
  }
  
  /*************
   * 
   * Delegate methods for m_delegateWidget
   * 
   *************/

  /**
   * @param selection
   * @param mapPanel
   * @return
   * @see org.kalypso.ogc.gml.widgets.IWidget#canBeActivated(org.eclipse.jface.viewers.ISelection,
   *      org.kalypso.ogc.gml.map.MapPanel)
   */
  @Override
  public boolean canBeActivated( ISelection selection, MapPanel mapPanel )
  {
    return true;
  }

  /**
   * @param p
   * @see org.kalypso.ogc.gml.widgets.IWidget#clickPopup(java.awt.Point)
   */
  @Override
  public void clickPopup( Point p )
  {
    if( m_delegateWidget != null )
      m_delegateWidget.clickPopup( p );
  }

  /**
   * @param p
   * @see org.kalypso.ogc.gml.widgets.IWidget#doubleClickedLeft(java.awt.Point)
   */
  @Override
  public void doubleClickedLeft( Point p )
  {
    if( m_delegateWidget != null )
      m_delegateWidget.doubleClickedLeft( p );
  }

  /**
   * @param p
   * @see org.kalypso.ogc.gml.widgets.IWidget#doubleClickedRight(java.awt.Point)
   */
  @Override
  public void doubleClickedRight( Point p )
  {
    if( m_delegateWidget != null )
      m_delegateWidget.doubleClickedRight( p );
  }

  /**
   * @param p
   * @see org.kalypso.ogc.gml.widgets.IWidget#dragged(java.awt.Point)
   */
  @Override
  public void dragged( Point p )
  {
    if( m_delegateWidget != null )
      m_delegateWidget.dragged( p );
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#finish()
   */
  @Override
  public void finish( )
  {
    // TODO!
    if( m_delegateWidget != null )
      m_delegateWidget.finish();
  }

  /**
   * @param e
   * @see org.kalypso.ogc.gml.widgets.IWidget#keyPressed(java.awt.event.KeyEvent)
   */
  public void keyPressed( KeyEvent e )
  {
    if( m_delegateWidget != null )
      m_delegateWidget.keyPressed( e );
  }

  /**
   * @param e
   * @see org.kalypso.ogc.gml.widgets.IWidget#keyReleased(java.awt.event.KeyEvent)
   */
  @Override
  public void keyReleased( KeyEvent e )
  {
    if( m_delegateWidget != null )
      m_delegateWidget.keyReleased( e );
  }

  /**
   * @param e
   * @see org.kalypso.ogc.gml.widgets.IWidget#keyTyped(java.awt.event.KeyEvent)
   */
  public void keyTyped( KeyEvent e )
  {
    if( m_delegateWidget != null )
      m_delegateWidget.keyTyped( e );
  }

  /**
   * @param p
   * @see org.kalypso.ogc.gml.widgets.IWidget#leftClicked(java.awt.Point)
   */
  public void leftClicked( Point p )
  {
    if( m_delegateWidget != null )
      m_delegateWidget.leftClicked( p );
  }

  /**
   * @param p
   * @see org.kalypso.ogc.gml.widgets.IWidget#leftPressed(java.awt.Point)
   */
  public void leftPressed( Point p )
  {
    if( m_delegateWidget != null )
      m_delegateWidget.leftPressed( p );
  }

  /**
   * @param p
   * @see org.kalypso.ogc.gml.widgets.IWidget#leftReleased(java.awt.Point)
   */
  public void leftReleased( Point p )
  {
    if( m_delegateWidget != null )
      m_delegateWidget.leftReleased( p );
  }

  /**
   * @param p
   * @see org.kalypso.ogc.gml.widgets.IWidget#middleClicked(java.awt.Point)
   */
  public void middleClicked( Point p )
  {
    if( m_delegateWidget != null )
      m_delegateWidget.middleClicked( p );
  }

  /**
   * @param p
   * @see org.kalypso.ogc.gml.widgets.IWidget#middlePressed(java.awt.Point)
   */
  public void middlePressed( Point p )
  {
    if( m_delegateWidget != null )
      m_delegateWidget.middlePressed( p );
  }

  /**
   * @param p
   * @see org.kalypso.ogc.gml.widgets.IWidget#middleReleased(java.awt.Point)
   */
  public void middleReleased( Point p )
  {
    if( m_delegateWidget != null )
      m_delegateWidget.middleReleased( p );
  }

  /**
   * @param p
   * @see org.kalypso.ogc.gml.widgets.IWidget#moved(java.awt.Point)
   */
  @Override
  public void moved( Point p )
  {
    if( m_delegateWidget != null )
      m_delegateWidget.moved( p );
  }

  /**
   * @param p
   * @see org.kalypso.ogc.gml.widgets.IWidget#rightClicked(java.awt.Point)
   */
  @Override
  public void rightClicked( Point p )
  {
    if( m_delegateWidget != null )
      m_delegateWidget.rightClicked( p );
  }

  /**
   * @param p
   * @see org.kalypso.ogc.gml.widgets.IWidget#rightPressed(java.awt.Point)
   */
  public void rightPressed( Point p )
  {
    if( m_delegateWidget != null )
      m_delegateWidget.rightPressed( p );
  }

  /**
   * @param p
   * @see org.kalypso.ogc.gml.widgets.IWidget#rightReleased(java.awt.Point)
   */
  public void rightReleased( Point p )
  {
    if( m_delegateWidget != null )
      m_delegateWidget.rightReleased( p );
  }

  /**
   * @param selection
   * @see org.kalypso.ogc.gml.widgets.IWidget#setSelection(org.eclipse.jface.viewers.ISelection)
   */
  public void setSelection( ISelection selection )
  {
    if( m_delegateWidget != null )
      m_delegateWidget.setSelection( selection );
  }

}
