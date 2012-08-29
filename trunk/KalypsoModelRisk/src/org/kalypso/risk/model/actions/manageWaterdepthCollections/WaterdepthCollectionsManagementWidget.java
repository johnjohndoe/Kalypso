/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra?e 22
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
package org.kalypso.risk.model.actions.manageWaterdepthCollections;

import java.awt.Color;
import java.awt.Graphics;

import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.kalypso.ogc.gml.map.IMapPanel;
import org.kalypso.ogc.gml.mapmodel.IMapPanelProvider;
import org.kalypso.ogc.gml.widgets.AbstractWidget;
import org.kalypso.risk.i18n.Messages;
import org.kalypso.ui.editor.mapeditor.views.IWidgetWithOptions;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Position;

/**
 * A widget with option pane, which allows the user to manage (add/remove) run-off events and to import water level data
 * for each event.
 *
 * @author Thomas Jung
 */
public class WaterdepthCollectionsManagementWidget extends AbstractWidget implements IWidgetWithOptions
{
  private WaterdepthCollectionsManagementComposite m_composite;

  public WaterdepthCollectionsManagementWidget( )
  {
    super( Messages.getString( "org.kalypso.risk.model.actions.manageWaterdepthCollections.WaterdepthCollectionsManagementWidget.0" ), Messages.getString( "org.kalypso.risk.model.actions.manageWaterdepthCollections.WaterdepthCollectionsManagementWidget.1" ) ); //$NON-NLS-1$ //$NON-NLS-2$
  }

  @Override
  public Control createControl( final Composite parent, final FormToolkit toolkit )
  {
    final IMapPanelProvider mapProvider = new IMapPanelProvider()
    {
      @Override
      public IMapPanel getMapPanel( )
      {
        return WaterdepthCollectionsManagementWidget.this.getMapPanel();
      }
    };

    m_composite = new WaterdepthCollectionsManagementComposite( parent, toolkit, mapProvider );
    return m_composite;
  }

  @Override
  public void disposeControl( )
  {
    m_composite = null;
  }

  @Override
  public String getPartName( )
  {
    return null;
  }

  @Override
  public void paint( final Graphics g )
  {
    if( m_composite == null )
      return;

    final GM_Envelope[] envelopes = m_composite.getEnvelopesForPaint();
    for( final GM_Envelope envelope : envelopes )
      paintEnvelope( g, envelope );
  }

  private void paintEnvelope( final Graphics g, final GM_Envelope envelope )
  {
    if( envelope == null )
      return;

    final GeoTransform projection = getMapPanel().getProjection();
    if( projection != null )
    {
      final GM_Position minPoint = projection.getDestPoint( envelope.getMin() );
      final GM_Position maxPoint = projection.getDestPoint( envelope.getMax() );
      final int x = (int) Math.min( minPoint.getX(), maxPoint.getX() );
      final int y = (int) Math.min( minPoint.getY(), maxPoint.getY() );

      final int width = (int) Math.abs( minPoint.getX() - maxPoint.getX() );
      final int height = (int) Math.abs( minPoint.getY() - maxPoint.getY() );

      g.setColor( Color.RED );
      g.drawRect( x, y, width, height );
    }
  }
}