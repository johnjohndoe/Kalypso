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
package org.kalypso.model.wspm.tuhh.ui.internal.chart.waterlevel;

import java.util.ArrayList;
import java.util.Collection;

import org.apache.commons.lang3.StringUtils;
import org.kalypso.model.wspm.core.profil.IProfileObject;
import org.kalypso.model.wspm.core.profil.IProfileObjectRecord;
import org.kalypso.model.wspm.tuhh.ui.chart.layers.PartTypeAccessor;
import org.kalypso.model.wspm.tuhh.ui.chart.layers.ProfileObjectsInfoBuilder;
import org.kalypso.model.wspm.ui.view.chart.IProfilChartLayer;

/**
 * @author Gernot Belger
 */
public class WspObjectsInfoBuilder extends ProfileObjectsInfoBuilder
{
  private String m_label;

  public WspObjectsInfoBuilder( final IProfilChartLayer layer, final PartTypeAccessor partInfo )
  {
    super( layer, partInfo );
  }

  void setLabel( final String label )
  {
    m_label = label;
  }

  @Override
  protected String getPointHeader( final IProfileObject object, final IProfileObjectRecord record )
  {
    return formatLineTooltip( object );
  }

  @Override
  protected String formatLineTooltip( final IProfileObject object )
  {
    final Collection<String> labels = new ArrayList<>( 3 );

    final String typeLabel = getPartInfo().getTypeLabel();
    labels.add( typeLabel );

    final String label = m_label;
    if( !StringUtils.isBlank( label ) )
      labels.add( label );

    final String description = object.getDescription();
    if( !StringUtils.isBlank( description ) )
      labels.add( description );

    return StringUtils.join( labels, " - " );//$NON-NLS-1$
  }
}