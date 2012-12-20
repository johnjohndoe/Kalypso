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

import org.kalypso.model.wspm.core.profil.IProfile;
import org.kalypso.model.wspm.core.profil.changes.PointPropertyRemove;
import org.kalypso.model.wspm.core.profil.operation.ProfileOperation;
import org.kalypso.model.wspm.core.profil.operation.ProfileOperationJob;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;
import org.kalypso.model.wspm.ui.view.chart.AbstractProfilTheme;
import org.kalypso.model.wspm.ui.view.chart.IProfilChartLayer;

import de.openali.odysseus.chart.framework.model.layer.IChartLayer;

/**
 * This is just a placeholder for themes that only represents columns of the table.<br/>
 * Used to add/remove the columns to/from the table.
 * 
 * @author Gernot Belger
 */
public class AbstractPlaceholderProfileTheme extends AbstractProfilTheme
{
  private final String[] m_componentIDs;

  public AbstractPlaceholderProfileTheme( final IProfile profil, final String id, final String title, final IProfilChartLayer[] chartLayers, final String[] componentIDs )
  {
    super( profil, id, title, chartLayers, null );

    m_componentIDs = componentIDs;
  }

  @Override
  public IChartLayer[] getLegendNodes( )
  {
    return new IChartLayer[] {};
  }

  @Override
  public final void removeYourself( )
  {
    final IProfile profil = getProfil();

    final ProfileOperation operation = new ProfileOperation( Messages.getString( "org.kalypso.model.wspm.tuhh.ui.chart.GeoCoordinateTheme.1" ), getProfil(), true ); //$NON-NLS-1$

    for( final String component : m_componentIDs )
      operation.addChange( new PointPropertyRemove( profil, profil.hasPointProperty( component ) ) );
    new ProfileOperationJob( operation ).schedule();
  }
}