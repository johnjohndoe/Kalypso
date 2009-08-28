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
package org.kalypso.kalypsomodel1d2d.ui.map.fenetRoughness;

import java.awt.Color;
import java.awt.Graphics;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.event.KeyEvent;

import javax.xml.namespace.QName;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.swt.awt.SWT_AWT_Utilities;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.command.DeleteFeatureCommand;
import org.kalypso.ogc.gml.map.IMapPanel;
import org.kalypso.ogc.gml.map.utilities.tooltip.ToolTipRenderer;
import org.kalypso.ogc.gml.map.widgets.AbstractDelegateWidget;
import org.kalypso.ogc.gml.map.widgets.SelectFeatureWidget;
import org.kalypso.ogc.gml.selection.EasyFeatureWrapper;
import org.kalypso.ogc.gml.selection.IFeatureSelectionManager;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author Stefan Kurzbach
 * @author Dejan Antanaskovic
 *
 * Wraps a {@link CreateGeometeryWidget2} for 0-argument constructor instantiation
 *
 */
public class DeletePolygonWidgetWrapper extends AbstractDelegateWidget
{
  private final ToolTipRenderer m_toolTipRenderer = new ToolTipRenderer();

  public DeletePolygonWidgetWrapper( )
  {
    super( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.fenetRoughness.DeletePolygonWidgetWrapper.0" ), Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.fenetRoughness.DeletePolygonWidgetWrapper.0" ), new SelectFeatureWidget( "", "", new QName[] { Feature.QNAME_FEATURE }, null ) ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$

    m_toolTipRenderer.setBackgroundColor( new Color( 1f, 1f, 0.6f, 0.70f ) );
    m_toolTipRenderer.setTooltip( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.fenetRoughness.DeletePolygonWidgetWrapper.1" ) ); //$NON-NLS-1$
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractDelegateWidget#activate(org.kalypso.commons.command.ICommandTarget,
   *      org.kalypso.ogc.gml.map.IMapPanel)
   */
  @Override
  public void activate( final ICommandTarget commandPoster, final IMapPanel mapPanel )
  {
    super.activate( commandPoster, mapPanel );

    // TODO: should not the select feature widget do this?
    if( mapPanel != null )
      mapPanel.getSelectionManager().clear();
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#paint(java.awt.Graphics)
   */
  @Override
  public void paint( final Graphics g )
  {
    super.paint( g );

    final IMapPanel mapPanel = getMapPanel();
    if( mapPanel != null )
    {
      final Rectangle bounds = mapPanel.getScreenBounds();
      final String delegateTooltip = getDelegate().getToolTip();

      m_toolTipRenderer.setTooltip( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.fenetRoughness.DeletePolygonWidgetWrapper.1" ) + "\n" + delegateTooltip ); //$NON-NLS-1$ //$NON-NLS-2$

      m_toolTipRenderer.paintToolTip( new Point( 5, bounds.height - 5 ), g, bounds );
    }
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#keyTyped(java.awt.event.KeyEvent)
   */
  @Override
  public void keyPressed( final KeyEvent e )
  {
    if( e.getKeyCode() == KeyEvent.VK_DELETE || e.getKeyCode() == KeyEvent.VK_BACK_SPACE )
    {
      e.consume();

      final IMapPanel mapPanel = getMapPanel();
      if( mapPanel == null )
        return;

      final IFeatureSelectionManager selectionManager = mapPanel.getSelectionManager();
      final IStatus status = deleteSelectedElements( selectionManager );
      if( !status.isOK() )
      {
        // TODO: should be error instead, but probably never happens at all
        SWT_AWT_Utilities.showSwtMessageBoxInformation( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.fenetRoughness.DeletePolygonWidgetWrapper.0" ), status.getMessage() ); //$NON-NLS-1$
      }
    }
    super.keyPressed( e );
  }

  private IStatus deleteSelectedElements( final IFeatureSelectionManager selectionManager )
  {
    final EasyFeatureWrapper[] selected = selectionManager.getAllFeatures();
    if( selected.length == 0 )
      return StatusUtilities.createErrorStatus( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.del.DeleteFeElementsHelper.0" ) ); //$NON-NLS-1$

    if( !SWT_AWT_Utilities.showSwtMessageBoxConfirm( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.fenetRoughness.DeletePolygonWidgetWrapper.0" ), Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.del.DeleteFeElementsHelper.2" ) ) ) //$NON-NLS-1$ //$NON-NLS-2$
      return Status.OK_STATUS;

    selectionManager.clear();

    final DeleteFeatureCommand deleteCommand = new DeleteFeatureCommand( selected );
    final IKalypsoFeatureTheme theme = (IKalypsoFeatureTheme) getMapPanel().getMapModell().getActiveTheme();
    theme.postCommand( deleteCommand, null );

    return Status.OK_STATUS;
  }


}
