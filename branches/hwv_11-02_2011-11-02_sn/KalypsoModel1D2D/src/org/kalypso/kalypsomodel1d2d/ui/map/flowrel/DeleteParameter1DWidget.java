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
package org.kalypso.kalypsomodel1d2d.ui.map.flowrel;

import java.awt.Color;
import java.awt.Graphics;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.event.KeyEvent;

import javax.xml.namespace.QName;

import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IBuildingFlowRelation;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.ITeschkeFlowRelation;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypso.kalypsosimulationmodel.core.flowrel.IFlowRelationship;
import org.kalypso.ogc.gml.map.IMapPanel;
import org.kalypso.ogc.gml.map.utilities.tooltip.ToolTipRenderer;
import org.kalypso.ogc.gml.map.widgets.AbstractDelegateWidget;
import org.kalypso.ogc.gml.map.widgets.SelectFeatureWidget;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.selection.EasyFeatureWrapper;
import org.kalypso.ogc.gml.selection.FeatureSelectionHelper;
import org.kalypso.ogc.gml.selection.IFeatureSelectionManager;
import org.kalypso.ogc.gml.util.MapUtils;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author Gernot Belger
 * @author Thomas Jung
 */
public class DeleteParameter1DWidget extends AbstractDelegateWidget
{
  private final ToolTipRenderer m_toolTipRenderer = new ToolTipRenderer();

  public DeleteParameter1DWidget( )
  {
    super( Messages.getString("org.kalypso.kalypsomodel1d2d.ui.map.flowrel.DeleteParameter1DWidget.0"), Messages.getString("org.kalypso.kalypsomodel1d2d.ui.map.flowrel.DeleteParameter1DWidget.1"), new SelectFeatureWidget( "", "", new QName[] { ITeschkeFlowRelation.QNAME, //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
        IBuildingFlowRelation.QNAME }, IFlowRelationship.QNAME_PROP_POSITION ) );

    m_toolTipRenderer.setTooltip( Messages.getString("org.kalypso.kalypsomodel1d2d.ui.map.flowrel.DeleteParameter1DWidget.4") ); //$NON-NLS-1$
    m_toolTipRenderer.setBackgroundColor( new Color( 1f, 1f, 0.6f, 0.70f ) );
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

      m_toolTipRenderer.setTooltip( Messages.getString("org.kalypso.kalypsomodel1d2d.ui.map.flowrel.DeleteParameter1DWidget.5") + delegateTooltip ); //$NON-NLS-1$

      m_toolTipRenderer.paintToolTip( new Point( 5, bounds.height - 5 ), g, bounds );
    }
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#keyTyped(java.awt.event.KeyEvent)
   */
  @Override
  public void keyPressed( final KeyEvent e )
  {
    if( e.getKeyCode() == KeyEvent.VK_DELETE )
    {
      e.consume();

      final IFeatureSelectionManager selectionManager = getMapPanel().getSelectionManager();
      final EasyFeatureWrapper[] eFeatures = selectionManager.getAllFeatures();

      Feature[] features = FeatureSelectionHelper.getFeatures( selectionManager );

      if( features.length == 0 )
        return;

      CommandableWorkspace workspace = eFeatures[0].getWorkspace();

      try
      {
        MapUtils.removeFeature( workspace, getMapPanel(), features );
      }
      catch( Exception e1 )
      {
        // TODO Auto-generated catch block
        e1.printStackTrace();
      }
    }
    super.keyPressed( e );
  }

}
