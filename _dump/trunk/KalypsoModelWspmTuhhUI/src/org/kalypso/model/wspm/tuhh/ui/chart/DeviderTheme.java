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
package org.kalypso.model.wspm.tuhh.ui.chart;

import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Point;
import org.kalypso.model.wspm.core.KalypsoModelWspmCoreExtensions;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilChange;
import org.kalypso.model.wspm.core.profil.IProfilPointMarkerProvider;
import org.kalypso.model.wspm.core.profil.changes.ProfilChangeHint;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;
import org.kalypso.model.wspm.tuhh.ui.panel.TrennerPanel;
import org.kalypso.model.wspm.ui.view.IProfilView;
import org.kalypso.model.wspm.ui.view.chart.AbstractProfilTheme;
import org.kalypso.model.wspm.ui.view.chart.IProfilChartLayer;

import de.openali.odysseus.chart.framework.model.layer.IChartLayer;
import de.openali.odysseus.chart.framework.model.layer.ILegendEntry;
import de.openali.odysseus.chart.framework.model.layer.impl.LegendEntry;
import de.openali.odysseus.chart.framework.model.mapper.ICoordinateMapper;

/**
 * @author kimwerner
 */
public class DeviderTheme extends AbstractProfilTheme
{
  public DeviderTheme( final IProfil profil, final IProfilChartLayer[] chartLayers, final ICoordinateMapper cm )
  {
    super( profil, IWspmTuhhConstants.LAYER_DEVIDER, Messages.getString( "org.kalypso.model.wspm.tuhh.ui.chart.DeviderTheme.0" ), chartLayers, cm ); //$NON-NLS-1$
  }

  /**
   * @see org.kalypso.model.wspm.ui.view.chart.IProfilChartLayer#createLayerPanel(org.kalypso.model.wspm.core.profil.IProfil)
   */
  @Override
  public IProfilView createLayerPanel( )
  {
    return new TrennerPanel( getProfil() );
  }

  @Override
  public ILegendEntry[] createLegendEntries( )
  {
    final IProfilPointMarkerProvider markerProvider = KalypsoModelWspmCoreExtensions.getMarkerProviders( getProfil().getType() );
    final String[] deviderIds = new String[getLayerManager().getLayers().length];
    int i = 0;
    for( final IChartLayer layer : getLayerManager().getLayers() )
      deviderIds[i++] = layer.getId();

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

  /**
   * @see org.kalypso.model.wspm.ui.view.chart.IProfilChartLayer#onProfilChanged(org.kalypso.model.wspm.core.profil.changes.ProfilChangeHint,
   *      org.kalypso.model.wspm.core.profil.IProfilChange[])
   */
  @Override
  public void onProfilChanged( final ProfilChangeHint hint, final IProfilChange[] changes )
  {
    if( hint.isPointPropertiesChanged() || hint.isMarkerMoved() )
    {
      getEventHandler().fireLayerContentChanged( this );
    }
  }
}
