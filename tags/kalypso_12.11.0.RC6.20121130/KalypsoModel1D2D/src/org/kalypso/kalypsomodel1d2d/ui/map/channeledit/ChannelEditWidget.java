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

import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypso.ogc.gml.map.IMapPanel;
import org.kalypso.ogc.gml.map.widgets.AbstractDelegateWidget2;
import org.kalypso.ogc.gml.widgets.IWidget;
import org.kalypso.transformation.transformer.GeoTransformerException;
import org.kalypso.ui.editor.mapeditor.views.IWidgetWithOptions;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.geometry.GM_Exception;

/**
 * TODOs:
 * <ul>
 * <li>let user decide wether to use douglas peucker or equidistant segmentation</li>
 * <li>let user toggle area auto adjustment</li>
 * <li>allow for more than one bank line per side</li>
 * <li>Vizualize bank line as profile</li>
 * <li>validate meshes and show result in map (general tool for QuadMesh and IPolygonWithName))</li>
 * <li>Implement a varying segmentation between segments (segments a,b,c: a has 4 points, c has 8 points, c should have a smoth transfer frmo 4 to 8</li>
 * <li>Instead of selecting a profile, select the border of a 2D-mesh as a profile</li>
 * <li>general: when adding mesh as 2d-elements: use z-values new mesh should replace z-values of existing nodes</li>
 * </ul>
 *
 * @author Thomas Jung
 * @author Gernot Belger
 */
public class ChannelEditWidget extends AbstractDelegateWidget2 implements IWidgetWithOptions
{
  private final ChannelEditData m_data = new ChannelEditData( this );

  private final ChannelEditPainter m_painter = new ChannelEditPainter( m_data );

  private ChannelEditComposite m_composite;

  private final IWidget m_infoWidget = new ChannelEditInfoWidget( m_data );

  public ChannelEditWidget( )
  {
    super( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.channeledit.CreateMainChannelWidget.0" ), Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.channeledit.CreateMainChannelWidget.1" ), null ); //$NON-NLS-1$ //$NON-NLS-2$

    m_data.addPropertyChangeListener( new ChannelEditMapControler( this ) );
  }

  @Override
  public Control createControl( final Composite parent, final FormToolkit toolkit )
  {
    resetData();

    m_composite = new ChannelEditComposite( parent, toolkit, SWT.NONE, m_data, m_infoWidget );

    // REMARK: set specific shell to data, else modal dialogs will be system modal instead of modal to this shell
    m_data.setShell( parent.getShell() );

    return m_composite;
  }

  private void resetData( )
  {
    final ChannelEditDataInit init = new ChannelEditDataInit( m_data, getMapPanel() );
    init.init();
  }

  @Override
  public void disposeControl( )
  {
    if( m_composite != null )
    {
      m_composite.dispose();
      m_composite = null;
    }
  }

  @Override
  public void paint( final Graphics g )
  {
    if( m_composite == null || m_composite.isDisposed() )
      return;

    final IMapPanel mapPanel = getMapPanel();
    if( mapPanel == null )
      return;

    final GeoTransform projection = mapPanel.getProjection();
    if( projection == null )
      return;

    try
    {
      m_painter.paint( g, projection );
    }
    catch( final GM_Exception | GeoTransformerException e )
    {
      e.printStackTrace();
    }

    super.paint( g );
  }

  /**
   * Overwritten in order to make public.
   */
  @Override
  public void repaintMap( )
  {
    super.repaintMap();
  }

  /**
   * Overwritten in order to make public.
   */
  @Override
  public void setDelegate( final IWidget delegate )
  {
    if( delegate == null )
      super.setDelegate( m_infoWidget );
    else
      super.setDelegate( delegate );
  }

  @Override
  public IWidget getDelegate( )
  {
    return super.getDelegate();
  }

  @Override
  public String getPartName( )
  {
    return null;
  }
}