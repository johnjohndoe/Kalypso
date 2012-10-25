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
package org.kalypso.model.wspm.tuhh.ui.chart.themes;

import java.util.ArrayList;
import java.util.List;

import org.apache.commons.lang3.ArrayUtils;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Point;
import org.kalypso.model.wspm.core.KalypsoModelWspmCoreExtensions;
import org.kalypso.model.wspm.core.profil.IProfile;
import org.kalypso.model.wspm.core.profil.IProfilePointMarkerProvider;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;
import org.kalypso.model.wspm.tuhh.ui.panel.TrennerPanel;
import org.kalypso.model.wspm.ui.view.IProfilView;
import org.kalypso.model.wspm.ui.view.chart.AbstractProfilTheme;
import org.kalypso.model.wspm.ui.view.chart.IProfilChartLayer;
import org.kalypso.observation.result.IComponent;

import de.openali.odysseus.chart.framework.model.layer.IChartLayer;
import de.openali.odysseus.chart.framework.model.layer.ILegendEntry;
import de.openali.odysseus.chart.framework.model.layer.impl.LegendEntry;
import de.openali.odysseus.chart.framework.model.mapper.ICoordinateMapper;

/**
 * @author kimwerner
 */
public class DeviderTheme extends AbstractProfilTheme
{
  public static final String TITLE = Messages.getString( "org.kalypso.model.wspm.tuhh.ui.chart.DeviderTheme.0" ); //$NON-NLS-1$

  private ILegendEntry[] m_legendEntries;

  public DeviderTheme( final IProfile profil, final IProfilChartLayer[] chartLayers, final ICoordinateMapper cm )
  {
    super( profil, IWspmTuhhConstants.LAYER_DEVIDER, TITLE, chartLayers, cm );
  }

  @Override
  public IProfilView createLayerPanel( )
  {
    return new TrennerPanel( getProfil() );
  }

  @Override
  public IChartLayer[] getLegendNodes( )
  {
    final List<IChartLayer> cl = new ArrayList<>();
    for( final IChartLayer layer : getLayerManager().getLayers() )
    {
      if( layer instanceof IProfilChartLayer )
      {
        if( getProfil().hasPointProperty( ((IProfilChartLayer)layer).getTargetComponent() ) )
        {
          cl.add( layer );
        }
      }
    }
    return cl.toArray( new IChartLayer[] {} );
  }

  @Override
  public synchronized ILegendEntry[] getLegendEntries( )
  {
    if( ArrayUtils.isEmpty( m_legendEntries ) )
    {
      m_legendEntries = createLegendEntries();
    }
    return m_legendEntries;
  }

  private ILegendEntry[] createLegendEntries( )
  {
    final IProfilePointMarkerProvider markerProvider = KalypsoModelWspmCoreExtensions.getMarkerProviders( getProfil().getType() );

    final IComponent[] deviders = getProfil().getPointMarkerTypes();
    final String[] deviderIds = new String[deviders.length];
    int i = 0;

    for( final IComponent devider : deviders )
    {
      deviderIds[i++] = devider.getId();
    }

    final LegendEntry le = new LegendEntry( this, getTitle() )
    {
      @Override
      public void paintSymbol( final GC gc, final Point size )
      {
        markerProvider.drawMarker( deviderIds, gc );
      }
    };

    return new ILegendEntry[] { le };
  }
}
