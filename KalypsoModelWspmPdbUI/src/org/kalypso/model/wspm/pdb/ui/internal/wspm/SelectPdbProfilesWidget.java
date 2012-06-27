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
package org.kalypso.model.wspm.pdb.ui.internal.wspm;

import java.awt.Graphics;
import java.awt.Point;
import java.awt.Rectangle;

import javax.xml.namespace.QName;

import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.model.wspm.pdb.ui.internal.i18n.Messages;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReachProfileSegment;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.map.IMapPanel;
import org.kalypso.ogc.gml.map.utilities.tooltip.ToolTipRenderer;
import org.kalypso.ogc.gml.map.widgets.SelectFeatureWidget;
import org.kalypso.ogc.gml.mapmodel.IKalypsoThemeVisitor;
import org.kalypso.ogc.gml.mapmodel.IMapModell;

/**
 * FIXME: move to wpsm light
 * 
 * @author Gernot Belger
 */
public class SelectPdbProfilesWidget extends SelectFeatureWidget
{
  private final ToolTipRenderer m_toolTipRenderer = new ToolTipRenderer();

  private static final QName[] QNAMES_TO_SELECT = new QName[] { TuhhReachProfileSegment.QNAME_PROFILEREACHSEGMENT };

  public SelectPdbProfilesWidget( )
  {
    super( Messages.getString( "SelectPdbProfilesWidget.0" ), Messages.getString( "SelectPdbProfilesWidget.1" ), QNAMES_TO_SELECT, TuhhReachProfileSegment.PROPERTY_PROFILE_LOCATION ); //$NON-NLS-1$ //$NON-NLS-2$
  }

  @Override
  public void activate( final ICommandTarget commandPoster, final IMapPanel mapPanel )
  {
    super.activate( commandPoster, mapPanel );

    final IKalypsoFeatureTheme[] themes = findThemes();
    setThemes( themes );
  }

  private IKalypsoFeatureTheme[] findThemes( )
  {
    final IMapPanel mapPanel = getMapPanel();
    if( mapPanel == null )
      return null;

    final IMapModell modell = mapPanel.getMapModell();
    if( modell == null )
      return null;

    final FindReachThemesVisitor visitor = new FindReachThemesVisitor();
    modell.accept( visitor, IKalypsoThemeVisitor.DEPTH_INFINITE );
    return visitor.getThemes();
  }

  @Override
  public void paint( final Graphics g )
  {
    super.paint( g );

    final IMapPanel panel = getMapPanel();
    if( panel == null )
      return;

    final Rectangle bounds = panel.getScreenBounds();
    final String tooltip = getToolTip();

    m_toolTipRenderer.setTooltip( tooltip ); //$NON-NLS-1$
    m_toolTipRenderer.paintToolTip( new Point( 5, bounds.height - 5 ), g, bounds );
  }
}
