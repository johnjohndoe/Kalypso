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

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.ogc.gml.map.IMapPanel;
import org.kalypso.ogc.gml.widgets.AbstractWidget;
import org.kalypso.ogc.gml.widgets.IWidget;
import org.kalypso.ui.editor.mapeditor.views.IWidgetWithOptions;
import org.kalypsodeegree.graphics.displayelements.DisplayElement;
import org.kalypsodeegree.graphics.sld.CssParameter;
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
    super( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.channeledit.CreateMainChannelWidget.0" ), Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.channeledit.CreateMainChannelWidget.1" ) ); //$NON-NLS-1$ //$NON-NLS-2$
  }

  /**
   * @see org.kalypso.ui.editor.mapeditor.views.IWidgetWithOptions#createControl(org.eclipse.swt.widgets.Composite)
   */
  @Override
  public Control createControl( final Composite parent, final FormToolkit toolkit )
  {
    m_composite = new CreateMainChannelComposite( parent, toolkit, SWT.NONE, m_data, this );
    return m_composite;
  }

  /**
   * @see org.kalypso.ui.editor.mapeditor.views.IWidgetWithOptions#disposeControl()
   */
  @Override
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

  public IMapPanel getPanel( )
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

    try
    {
      final Feature[] selectedProfiles = m_data.getSelectedProfiles();

      for( final Feature feature : selectedProfiles )
      {
        final IProfileFeature profile = (IProfileFeature) (feature);
        final GM_Curve line = profile.getLine();

        final LineSymbolizer symb = getProfilLineSymbolizer( new Color( 255, 255, 0 ) );
        final DisplayElement de = DisplayElementFactory.buildLineStringDisplayElement( feature, line, symb );
        de.paint( g, getMapPanel().getProjection(), new NullProgressMonitor() );
      }

      paintBanks( g, CreateChannelData.SIDE.RIGHT, new Color( 255, 0, 0 ) );
      paintBanks( g, CreateChannelData.SIDE.LEFT, new Color( 0, 255, 0 ) );

      final IMapPanel mapPanel = getMapPanel();

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
      if( (m_composite.isBankEdit() == true && m_data.getMeshStatus() == true) )
        m_data.drawBankLines( g );
      if( m_delegateWidget != null )
        m_delegateWidget.paint( g );
    }
    catch( final CoreException e )
    {
      e.printStackTrace();
    }

  }

  private void drawIntersProfiles( final Graphics g, final Color color )
  {
    if( m_data.getSelectedSegment() != null )
    {
      final SegmentData currentSegment = m_data.getSelectedSegment();
      if( currentSegment != null )
        if( currentSegment.complete() == true )
          currentSegment.paintProfile( m_data.getCurrentProfile(), getPanel(), g, color );
    }
  }

  private void drawIntersPoints( final Graphics g, final Color color )
  {
    if( m_data.getSelectedSegment() != null )
    {
      final SegmentData currentSegment = m_data.getSelectedSegment();
      if( currentSegment != null )
        if( currentSegment.complete() == true )
          currentSegment.paintIntersectionPoints( getPanel(), g, color, m_data.getCurrentProfile() );
    }
  }

  private void paintBanks( final Graphics g, final CreateChannelData.SIDE side, final Color color ) throws CoreException
  {

    final GM_Curve curve = m_data.getBanklineForSide( side );

    if( curve == null )
      return;

    final IMapPanel mapPanel = getMapPanel();
    if( mapPanel == null )
      return;

    final LineSymbolizer symb = new LineSymbolizer_Impl();
    final Stroke stroke = new Stroke_Impl( new HashMap<String, CssParameter>(), null, null );

    Stroke defaultstroke = new Stroke_Impl( new HashMap<String, CssParameter>(), null, null );

    defaultstroke = symb.getStroke();

    stroke.setWidth( 1 );
    stroke.setStroke( color );
    symb.setStroke( stroke );

    final DisplayElement de = DisplayElementFactory.buildLineStringDisplayElement( null, curve, symb );
    de.paint( g, mapPanel.getProjection(), new NullProgressMonitor() );

    symb.setStroke( defaultstroke );
  }

  private LineSymbolizer getProfilLineSymbolizer( final Color color )
  {
    final LineSymbolizer symb = new LineSymbolizer_Impl();
    final Stroke stroke = new Stroke_Impl( new HashMap<String, CssParameter>(), null, null );
    stroke.setWidth( 3 );
    stroke.setStroke( color );
    symb.setStroke( stroke );
    return symb;
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#activate(org.kalypso.commons.command.ICommandTarget,
   *      org.kalypso.ogc.gml.map.MapPanel)
   */
  @Override
  public void activate( final ICommandTarget commandPoster, final IMapPanel mapPanel )
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
  public boolean canBeActivated( final ISelection selection, final IMapPanel mapPanel )
  {
    return true;
  }

  /**
   * @param p
   * @see org.kalypso.ogc.gml.widgets.IWidget#doubleClickedLeft(java.awt.Point)
   */
  @Override
  public void doubleClickedLeft( final Point p )
  {
    if( m_delegateWidget != null )
      m_delegateWidget.doubleClickedLeft( p );
  }

  /**
   * @param p
   * @see org.kalypso.ogc.gml.widgets.IWidget#doubleClickedRight(java.awt.Point)
   */
  @Override
  public void doubleClickedRight( final Point p )
  {
    if( m_delegateWidget != null )
      m_delegateWidget.doubleClickedRight( p );
  }

  /**
   * @param p
   * @see org.kalypso.ogc.gml.widgets.IWidget#dragged(java.awt.Point)
   */
  @Override
  public void dragged( final Point p )
  {
    if( m_delegateWidget != null )
      m_delegateWidget.dragged( p );
    // TODO: check if this repaint is really necessary
    final IMapPanel panel = getMapPanel();
    if( panel != null )
      panel.repaintMap();
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
  public void keyPressed( final KeyEvent e )
  {
    if( m_delegateWidget != null )
      m_delegateWidget.keyPressed( e );
  }

  /**
   * @param e
   * @see org.kalypso.ogc.gml.widgets.IWidget#keyReleased(java.awt.event.KeyEvent)
   */
  @Override
  public void keyReleased( final KeyEvent e )
  {
    if( m_delegateWidget != null )
      m_delegateWidget.keyReleased( e );
  }

  /**
   * @param e
   * @see org.kalypso.ogc.gml.widgets.IWidget#keyTyped(java.awt.event.KeyEvent)
   */
  @Override
  public void keyTyped( final KeyEvent e )
  {
    if( m_delegateWidget != null )
      m_delegateWidget.keyTyped( e );
  }

  /**
   * @param p
   * @see org.kalypso.ogc.gml.widgets.IWidget#leftClicked(java.awt.Point)
   */
  @Override
  public void leftClicked( final Point p )
  {
    if( m_delegateWidget != null )
      m_delegateWidget.leftClicked( p );
  }

  /**
   * @param p
   * @see org.kalypso.ogc.gml.widgets.IWidget#leftPressed(java.awt.Point)
   */
  @Override
  public void leftPressed( final Point p )
  {
    if( m_delegateWidget != null )
    {
      // TODO: check, if there is already a preview and if it would be deleted.
      m_delegateWidget.leftPressed( p );
    }
  }

  /**
   * @param p
   * @see org.kalypso.ogc.gml.widgets.IWidget#leftReleased(java.awt.Point)
   */
  @Override
  public void leftReleased( final Point p )
  {
    if( m_delegateWidget != null )
      m_delegateWidget.leftReleased( p );
  }

  /**
   * @param p
   * @see org.kalypso.ogc.gml.widgets.IWidget#moved(java.awt.Point)
   */
  @Override
  public void moved( final Point p )
  {
    if( m_delegateWidget != null )
      m_delegateWidget.moved( p );
  }

  /**
   * @param selection
   * @see org.kalypso.ogc.gml.widgets.IWidget#setSelection(org.eclipse.jface.viewers.ISelection)
   */
  @Override
  public void setSelection( final ISelection selection )
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
      @Override 
      @SuppressWarnings("synthetic-access")
      public void run( )
      {
        // check if all needed data is specified
        // m_data.completationCheck();
        if( !m_composite.isDisposed() ){
          m_composite.updateControl( false ); // false means calculate all again
        }
        getPanel().repaintMap();
      }
    } );

  }

  /**
   * @see org.kalypso.ui.editor.mapeditor.views.IWidgetWithOptions#getPartName()
   */
  @Override
  public String getPartName( )
  {
    return null;
  }
}