/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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

import org.eclipse.swt.graphics.Point;
import org.kalypso.model.wspm.core.profil.IProfile;
import org.kalypso.model.wspm.core.profil.changes.PointPropertyRemove;
import org.kalypso.model.wspm.core.profil.changes.ProfileChangeHint;
import org.kalypso.model.wspm.core.profil.changes.ProfileObjectRemove;
import org.kalypso.model.wspm.core.profil.operation.ProfileOperation;
import org.kalypso.model.wspm.core.profil.operation.ProfileOperationJob;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.core.profile.profileobjects.building.BuildingWehr;
import org.kalypso.model.wspm.tuhh.core.util.river.line.WspmSohlpunkte;
import org.kalypso.model.wspm.tuhh.ui.chart.layers.PointMarkerLayer;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;
import org.kalypso.model.wspm.tuhh.ui.panel.buildings.WeirPanel;
import org.kalypso.model.wspm.ui.view.IProfilView;
import org.kalypso.model.wspm.ui.view.chart.AbstractProfilTheme;
import org.kalypso.model.wspm.ui.view.chart.IProfilChartLayer;

import de.openali.odysseus.chart.framework.model.event.ILayerManagerEventListener.ContentChangeType;
import de.openali.odysseus.chart.framework.model.layer.EditInfo;
import de.openali.odysseus.chart.framework.model.layer.IChartLayer;
import de.openali.odysseus.chart.framework.model.mapper.ICoordinateMapper;

/**
 * @author kimwerner
 */
public class BuildingWeirTheme extends AbstractProfilTheme
{
  public static final String TITLE = Messages.getString( "org.kalypso.model.wspm.tuhh.ui.chart.BuildingWeirTheme.0" ); //$NON-NLS-1$

  public BuildingWeirTheme( final IProfile profil, final IProfilChartLayer[] chartLayers, final ICoordinateMapper cm, final ICoordinateMapper cmDevider )
  {
    super( profil, IWspmTuhhConstants.THEME_WEHR, TITLE, chartLayers, cm );
    // spezialfall für PointMarker und LineLayer
    for( final IChartLayer layer : chartLayers )
    {
      if( layer instanceof PointMarkerLayer )
      {
        layer.setCoordinateMapper( cmDevider );
      }
    }
  }

  @Override
  public void onProfilChanged( final ProfileChangeHint hint )
  {
    if( hint.isSelectionChanged() || hint.isMarkerMoved() || hint.isPointPropertiesChanged() || hint.isPointValuesChanged() || hint.isPointsChanged() )
    {
      fireLayerContentChanged( ContentChangeType.value );
    }
  }

  @Override
  public void removeYourself( )
  {
    final IProfile profil = getProfil();

    final BuildingWehr building = WspmSohlpunkte.getBuilding( profil, BuildingWehr.class );
    if( building == null )
      return;

    final ProfileOperation operation = new ProfileOperation( Messages.getString( "org.kalypso.model.wspm.tuhh.ui.chart.BuildingWeirTheme.1" ), getProfil(), true ); //$NON-NLS-1$
    operation.addChange( new ProfileObjectRemove( profil, building ) );
    operation.addChange( new PointPropertyRemove( profil, profil.hasPointProperty( IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEWEHR ) ) );
    new ProfileOperationJob( operation ).schedule();
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
  public IProfilView createLayerPanel( )
  {
    return new WeirPanel( getProfil() );
  }

  @Override
  public EditInfo getHover( final Point pos )
  {
    return null;
  }
}