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
import java.awt.Rectangle;
import java.awt.event.KeyEvent;
import java.util.ArrayList;
import java.util.List;

import javax.xml.namespace.QName;

import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IContinuityLine1D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IContinuityLine2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFELine;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.CreateJunctionElementCommand;
import org.kalypso.kalypsomodel1d2d.ui.map.util.UtilMap;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.map.IMapPanel;
import org.kalypso.ogc.gml.map.utilities.tooltip.ToolTipRenderer;
import org.kalypso.ogc.gml.map.widgets.AbstractDelegateWidget;
import org.kalypso.ogc.gml.map.widgets.SelectFeatureWidget;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ogc.gml.selection.EasyFeatureWrapper;
import org.kalypso.ogc.gml.selection.IFeatureSelectionManager;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;

public class CreateJunctionElementWidget extends AbstractDelegateWidget
{
  private IKalypsoFeatureTheme m_mapActiveTheme;

  private IFEDiscretisationModel1d2d m_discretisationModel;

  private final ToolTipRenderer m_toolTipRenderer = new ToolTipRenderer();

  private List<IContinuityLine1D> m_lines = new ArrayList<>();

  private final SelectFeatureWidget m_selDelegateWidget;

  public CreateJunctionElementWidget( )
  {
    super( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.junction1d2d.CreateJunctionElementWidget.0" ), Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.junction1d2d.CreateJunctionElementWidget.1" ), new SelectFeatureWidget( "", "", new QName[] { IContinuityLine1D.QNAME, IContinuityLine2D.QNAME }, IFELine.PROP_GEOMETRY ) ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$

    m_toolTipRenderer.setTooltip( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.junction1d2d.CreateJunctionElementWidget.4" ) ); //$NON-NLS-1$
    m_toolTipRenderer.setBackgroundColor( new Color( 1f, 1f, 0.6f, 0.70f ) );

    m_selDelegateWidget = (SelectFeatureWidget) getDelegate();
  }

  @Override
  public void paint( final Graphics g )
  {
    super.paint( g );

    final IMapPanel mapPanel = getMapPanel();
    if( mapPanel != null )
    {
      final Rectangle bounds = mapPanel.getScreenBounds();
      final String delegateTooltip = getDelegate().getToolTip();

      m_toolTipRenderer.setTooltip( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.junction1d2d.CreateJunctionElementWidget.5" ) + delegateTooltip ); //$NON-NLS-1$

      m_toolTipRenderer.paintToolTip( new Point( 5, bounds.height - 5 ), g, bounds );
    }
  }

  @Override
  public void activate( final ICommandTarget commandPoster, final IMapPanel mapPanel )
  {
    super.activate( commandPoster, mapPanel );
    reinit();
  }

  private void reinit( )
  {
    m_lines.clear();
    final IMapPanel mapPanel = getMapPanel();
    final IMapModell mapModell = mapPanel.getMapModell();
    final IKalypsoTheme activeTheme = mapModell.getActiveTheme();
    if( activeTheme instanceof IKalypsoFeatureTheme )
      m_mapActiveTheme = (IKalypsoFeatureTheme) activeTheme;
    else
      return;
    m_discretisationModel = UtilMap.findFEModelTheme( mapPanel );
    if( m_discretisationModel == null )
      return;

    final IKalypsoTheme[] themes = mapModell.getAllThemes();
    for( final IKalypsoTheme theme : themes )
    {
      if( theme instanceof IKalypsoFeatureTheme )
      {
        final IKalypsoFeatureTheme ft = (IKalypsoFeatureTheme) theme;
        final IFeatureType featureType = ft.getFeatureType();
        if( featureType == null )
          continue;

        final QName qName = featureType.getQName();
        if( qName.equals( IFELine.QNAME ) )
        {
          final IKalypsoFeatureTheme[] fts = new IKalypsoFeatureTheme[1];
          fts[0] = ft;
          m_selDelegateWidget.setThemes( fts );
        }
      }
    }

    final FeatureList featureList = m_mapActiveTheme.getFeatureList();
    final Feature parentFeature = featureList.getOwner();
    m_discretisationModel = (IFEDiscretisationModel1d2d) parentFeature.getAdapter( IFEDiscretisationModel1d2d.class );
    final IFeatureSelectionManager selectionManager = getMapPanel().getSelectionManager();
    selectionManager.clear();
    mapPanel.repaintMap();
  }

  @Override
  public void keyPressed( final KeyEvent e )
  {
    if( e.getKeyChar() == KeyEvent.VK_ESCAPE )
      reinit();

    else if( e.getKeyChar() == KeyEvent.VK_ENTER )
    {
      final IMapPanel mapPanel = getMapPanel();
      if( mapPanel == null )
        return;

      final IFeatureSelectionManager selectionManager = mapPanel.getSelectionManager();
      final EasyFeatureWrapper[] features = selectionManager.getAllFeatures();
      m_lines = new ArrayList<>( features.length );
      for( final EasyFeatureWrapper feature : features )
      {
        final IContinuityLine1D adapter = (IContinuityLine1D) feature.getFeature().getAdapter( IContinuityLine1D.class );
        if( adapter != null )
          m_lines.add( adapter );
      }
      if( m_lines.size() > 1 && m_lines.size() < 9 )
      {
        final CreateJunctionElementCommand command = new CreateJunctionElementCommand( m_discretisationModel, m_lines );
        final CommandableWorkspace workspace = m_mapActiveTheme.getWorkspace();
        try
        {
          workspace.postCommand( command );
          getMapPanel().getSelectionManager().clear();
        }
        catch( final Exception e1 )
        {
          e1.printStackTrace();
        }
        finally
        {
          reinit();
        }
      }
    }
    super.keyPressed( e );
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