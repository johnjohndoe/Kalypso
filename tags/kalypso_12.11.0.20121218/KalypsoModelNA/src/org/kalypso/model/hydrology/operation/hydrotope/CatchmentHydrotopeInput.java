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

import javax.xml.namespace.QName;

import org.eclipse.core.runtime.IStatus;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.model.hydrology.binding.model.Catchment;
import org.kalypso.model.hydrology.binding.model.NaModell;
import org.kalypso.model.hydrology.internal.ModelNA;
import org.kalypso.model.hydrology.internal.i18n.Messages;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;

/**
 * @author Gernot Belger
 */
class CatchmentHydrotopeInput extends AbstractHydrotopeInput<Catchment>
{
  public CatchmentHydrotopeInput( final NaModell naModel )
  {
    super( naModel.getCatchments() );
  }

  @Override
  public String getLabel( )
  {
    return Messages.getString( "CatchmentHydrotopeInput_1" ); //$NON-NLS-1$
  }

  @Override
  public IStatus validateInput( )
  {
    final IStatusCollector log = new StatusCollector( ModelNA.PLUGIN_ID );

    final IFeatureBindingCollection<Catchment> features = getFeatures();
    for( final Catchment catchment : features )
    {
      final double sealingFactor = catchment.getCorrSealing();
      if( sealingFactor < 0 )
        errorCorrectionFactor( log, catchment, Catchment.PROPERTY_CORRSEALING );

      final double maxPercFactor = catchment.getCorrMaxPercolation();
      if( maxPercFactor < 0 )
        errorCorrectionFactor( log, catchment, Catchment.PROPERTY_CORR_MAX_PERC );

      final double gwInflowFactor = catchment.getCorrGwInflowRate();
      if( gwInflowFactor < 0 )
        errorCorrectionFactor( log, catchment, Catchment.PROPERTY_CORR_GW_INFLOW_RATE );
    }

    return log.asMultiStatus( STR_ATTRIBUTES );
  }

  private void errorCorrectionFactor( final IStatusCollector log, final Catchment catchment, final QName property )
  {
    final IPropertyType propertyType = catchment.getFeatureType().getProperty( property );
    final String propertyLabel = propertyType.getAnnotation().getLabel();

    final String message = Messages.getString( "CatchmentHydrotopeInput_0", propertyLabel ); //$NON-NLS-1$
    log.add( IStatus.ERROR, formatMessage( message, catchment ) );
  }
}