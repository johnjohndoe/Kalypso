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

import java.awt.Color;
import java.awt.Graphics;
import java.awt.Point;
import java.awt.event.KeyEvent;
import java.util.HashMap;

import org.eclipse.jface.viewers.ISelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.model.wspm.core.gml.WspmProfile;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.map.widgets.AbstractWidget;
import org.kalypso.ogc.gml.widgets.IWidget;
import org.kalypso.ui.editor.mapeditor.views.IWidgetWithOptions;
import org.kalypsodeegree.graphics.displayelements.DisplayElement;
import org.kalypsodeegree.graphics.displayelements.IncompatibleGeometryTypeException;
import org.kalypsodeegree.graphics.sld.LineSymbolizer;
import org.kalypsodeegree.graphics.sld.Stroke;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree_impl.graphics.displayelements.DisplayElementFactory;
import org.kalypsodeegree_impl.graphics.sld.LineSymbolizer_Impl;
import org.kalypsodeegree_impl.graphics.sld.Stroke_Impl;

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
  public Control createControl( final Composite parent, final FormToolkit toolkit )
  {
    m_composite = new CreateMainChannelComposite( parent, toolkit, SWT.NONE, m_data, this );
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
    if( m_composite == null || m_composite.isDisposed() )
      return;

    final Feature[] selectedProfiles = m_data.getSelectedProfiles();

    for( final Feature feature : selectedProfiles )
    {
      final WspmProfile profile = new WspmProfile( feature );
      final GM_Curve line = profile.getLine();

      try
      {
        final LineSymbolizer symb = getProfilLineSymbolizer();
        final DisplayElement de = DisplayElementFactory.buildLineStringDisplayElement( feature, line, symb );
        de.paint( g, getMapPanel().getProjection() );
      }
      catch( final IncompatibleGeometryTypeException e )
      {
        e.printStackTrace();
      }
    }

    paintBanks( g, CreateChannelData.SIDE.RIGHT, new Color( 255, 0, 0 ) );
    paintBanks( g, CreateChannelData.SIDE.LEFT, new Color( 0, 255, 0 ) );

    final MapPanel mapPanel = getMapPanel();

    /* draw intersected profile */
    drawIntersProfiles( g, new Color( 0, 153, 255 ) );
    /* draw cropped profile */
    // drawCroppedProfiles( g, new Color( 100, 153, 255 ) );
    /* draw intersection points */
    drawIntersPoints( g, new Color( 255, 153, 0 ) );

    /* draw mesh */
    if( m_data.getMeshStatus() == true )
      m_data.paintAllSegments( g, mapPanel );

    /* draw editable bankline */
    if( m_composite.m_bankEdit1 == true & m_data.getMeshStatus() == true )
    {
      m_data.drawBankLine( m_composite.m_currentSegmentNum, 1, g );
    }
    if( m_composite.m_bankEdit2 == true & m_data.getMeshStatus() == true )
    {
      m_data.drawBankLine( m_composite.m_currentSegmentNum, 2, g );
    }
    if( m_delegateWidget != null )
      m_delegateWidget.paint( g );

  }

  private void drawIntersProfiles( Graphics g, Color color )
  {
    if( m_data.getSelectedSegment() != 0 )
    {
      final SegmentData currentSegment = m_data.getCurrentSegment( m_data.getSelectedSegment() );
      if( currentSegment != null )
        if( currentSegment.complete() == true )
          currentSegment.paintProfile( m_data.getCurrentProfile(), getPanel(), g, color );
    }
  }

  private void drawIntersPoints( Graphics g, Color color )
  {
    if( m_data.getSelectedSegment() != 0 )
    {
      final SegmentData currentSegment = m_data.getCurrentSegment( m_data.getSelectedSegment() );
      if( currentSegment != null )
        if( currentSegment.complete() == true )
          currentSegment.paintIntersectionPoints( getPanel(), g, color, m_data.getCurrentProfile() );
    }
  }

  private void paintBanks( final Graphics g, final CreateChannelData.SIDE side, final Color color )
  {
    final Feature[] selectedBanks = m_data.getSelectedBanks( side );
    for( final Feature feature : selectedBanks )
    {
      final GM_Curve line = (GM_Curve) feature.getDefaultGeometryProperty();

      try
      {
        final LineSymbolizer symb = new LineSymbolizer_Impl();
        final Stroke stroke = new Stroke_Impl( new HashMap(), null, null );

        Stroke defaultstroke = new Stroke_Impl( new HashMap(), null, null );

        defaultstroke = symb.getStroke();

        stroke.setWidth( 1 );
        stroke.setStroke( color );
        symb.setStroke( stroke );

        final DisplayElement de = DisplayElementFactory.buildLineStringDisplayElement( null, line, symb );
        de.paint( g, getMapPanel().getProjection() );

        symb.setStroke( defaultstroke );
      }
      catch( final IncompatibleGeometryTypeException e )
      {
        e.printStackTrace();
      }
    }
  }

  private LineSymbolizer getProfilLineSymbolizer( )
  {
    final LineSymbolizer symb = new LineSymbolizer_Impl();
    final Stroke stroke = new Stroke_Impl( new HashMap(), null, null );
    stroke.setWidth( 1 );
    symb.setStroke( stroke );
    return symb;
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#activate(org.kalypso.commons.command.ICommandTarget,
   *      org.kalypso.ogc.gml.map.MapPanel)
   */
  @Override
  public void activate( ICommandTarget commandPoster, MapPanel mapPanel )
  {
    super.activate( commandPoster, mapPanel );
  }

  /*********************************************************************************************************************
   * Delegate methods for m_delegateWidget
   ********************************************************************************************************************/

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

    // m_delegateWidget = null;
  }

  /**
   * @param e
   * @see org.kalypso.ogc.gml.widgets.IWidget#keyPressed(java.awt.event.KeyEvent)
   */
  @Override
  public void keyPressed( KeyEvent e )
  {

    // TODO:
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
  @Override
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
  @Override
  public void rightPressed( Point p )
  {
    if( m_delegateWidget != null )
      m_delegateWidget.rightPressed( p );
  }

  /**
   * @param p
   * @see org.kalypso.ogc.gml.widgets.IWidget#rightReleased(java.awt.Point)
   */
  @Override
  public void rightReleased( Point p )
  {
    if( m_delegateWidget != null )
      m_delegateWidget.rightReleased( p );
  }

  /**
   * @param selection
   * @see org.kalypso.ogc.gml.widgets.IWidget#setSelection(org.eclipse.jface.viewers.ISelection)
   */
  @Override
  public void setSelection( ISelection selection )
  {
    if( m_delegateWidget != null )
      m_delegateWidget.setSelection( selection );
  }

  public void update( )
  {
    if( m_composite == null || m_composite.isDisposed() )
      return;

    m_composite.getDisplay().syncExec( new Runnable()
    {
      public void run( )
      {
        // check if all needed data is specified
        // m_data.completationCheck();
        if( !m_composite.isDisposed() )
          m_composite.updateControl( false ); // false means calc all again
      }
    } );

    getPanel().invalidate();
  }

}
