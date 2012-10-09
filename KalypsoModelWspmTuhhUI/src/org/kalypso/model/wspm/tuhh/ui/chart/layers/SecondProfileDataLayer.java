/** This file is part of Kalypso
 *
 *  Copyright (c) 2012 by
 *
 *  Björnsen Beratende Ingenieure GmbH, Koblenz, Germany (Bjoernsen Consulting Engineers), http://www.bjoernsen.de
 *  Technische Universität Hamburg-Harburg, Institut für Wasserbau, Hamburg, Germany
 *  (Technical University Hamburg-Harburg, Institute of River and Coastal Engineering), http://www.tu-harburg.de/wb/
 *
 *  Kalypso is free software: you can redistribute it and/or modify it under the terms
 *  of the GNU Lesser General Public License (LGPL) as published by the Free Software
 *  Foundation, either version 3 of the License, or (at your option) any later version.
 *
 *  Kalypso is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 *  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with Kalypso.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.kalypso.model.wspm.tuhh.ui.chart.layers;

import org.kalypso.model.wspm.core.profil.IProfile;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.ui.chart.SecondProfileData;
import org.kalypso.model.wspm.tuhh.ui.chart.SecondProfileDataManager;
import org.kalypso.model.wspm.ui.view.chart.AbstractProfilTheme;
import org.kalypso.model.wspm.ui.view.chart.IProfilChartLayer;

import de.openali.odysseus.chart.framework.model.mapper.ICoordinateMapper;

/**
 * @author Gernot Belger
 */
public class SecondProfileDataLayer extends AbstractProfilTheme
{
  private final SecondProfileData m_data;

  public SecondProfileDataLayer( final IProfile profile, final SecondProfileData data, final ICoordinateMapper mapper )
  {
    super( profile, IWspmTuhhConstants.LAYER_SECOND_PROFILE, "eingeblendetes Profil", new IProfilChartLayer[] {}, mapper );

    m_data = data;
  }

  @Override
  public void removeYourself( )
  {
    SecondProfileDataManager.instance().removeData( getProfil(), m_data );

    getLayerManager().removeLayer( this );
  }
}