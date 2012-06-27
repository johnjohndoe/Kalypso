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
package org.kalypso.model.hydrology.operation.hydrotope;

import org.eclipse.core.runtime.IStatus;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.model.hydrology.binding.Geology;
import org.kalypso.model.hydrology.binding.GeologyCollection;
import org.kalypso.model.hydrology.internal.ModelNA;
import org.kalypso.model.hydrology.internal.i18n.Messages;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;

/**
 * @author Gernot Belger
 */
class GeologyHydrotopeInput extends AbstractHydrotopeInput<Geology>
{
  public GeologyHydrotopeInput( final GeologyCollection geology )
  {
    super( geology.getGeologies() );
  }

  @Override
  public String getLabel( )
  {
    return Messages.getString("GeologyHydrotopeInput_0"); //$NON-NLS-1$
  }

  @Override
  public IStatus validateInput( )
  {
    final IStatusCollector log = new StatusCollector( ModelNA.PLUGIN_ID );

    final IFeatureBindingCollection<Geology> features = getFeatures();
    for( final Geology geology : features )
    {
      final Double gwFactor = geology.getGWFactor();
      if( gwFactor == null )
        log.add( IStatus.ERROR, formatMessage( Messages.getString("GeologyHydrotopeInput_1"), geology ) ); //$NON-NLS-1$
      else if( gwFactor < 0.0 || gwFactor > 1.0 )
        log.add( IStatus.ERROR, formatMessage( Messages.getString("GeologyHydrotopeInput_2"), geology ) ); //$NON-NLS-1$

      final Double maxPerkRate = geology.getMaxPercolationRate();
      if( maxPerkRate == null )
        log.add( IStatus.ERROR, formatMessage( Messages.getString("GeologyHydrotopeInput_3"), geology ) ); //$NON-NLS-1$
      // TODO: range check?
    }

    return log.asMultiStatus( STR_ATTRIBUTES );
  }
}