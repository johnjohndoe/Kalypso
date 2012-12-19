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
package org.kalypso.model.wspm.pdb.internal.waterlevel2d;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedHashSet;
import java.util.Set;

import org.apache.commons.lang3.Range;
import org.apache.commons.lang3.StringUtils;
import org.kalypso.model.wspm.pdb.db.mapping.CrossSectionPart;
import org.kalypso.model.wspm.pdb.db.utils.PdbMappingUtils;
import org.opengis.geometry.MismatchedDimensionException;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.operation.TransformException;

/**
 * Aggregates properties of several waterlevels into one.
 * 
 * @author Gernot Belger
 */
class AggregatedWaterlevel
{
  private final ProjectedWaterlevel[] m_waterlevels;

  private final Range<Double> m_widthRange;

  public AggregatedWaterlevel( final ProjectedWaterlevel[] waterlevels, final Range<Double> widthRange )
  {
    m_waterlevels = waterlevels;
    m_widthRange = widthRange;
  }

  ProjectedWaterlevel[] getWaterlevels( ) throws MismatchedDimensionException, FactoryException, TransformException
  {
    if( m_widthRange == null )
      return m_waterlevels;

    final Collection<ProjectedWaterlevel> restrictedWaterlevels = new ArrayList<>();

    for( final ProjectedWaterlevel waterlevel : m_waterlevels )
    {
      final double width = waterlevel.getWidth();
      if( !Double.isNaN( width ) && m_widthRange.contains( width ) )
        restrictedWaterlevels.add( waterlevel );
    }

    return restrictedWaterlevels.toArray( new ProjectedWaterlevel[restrictedWaterlevels.size()] );
  }

  BigDecimal getDischarge( ) throws MismatchedDimensionException, FactoryException, TransformException
  {
    final ProjectedWaterlevel[] waterlevels = getWaterlevels();

    for( final ProjectedWaterlevel waterlevel : waterlevels )
    {
      final BigDecimal discharge = waterlevel.getDischarge();
      // FIXME: calculate mean? at least log if we have different waterlevels?
      if( discharge != null )
        return discharge;
    }

    return null;
  }

  /** build description from all waterlevels */
  String getDescription( ) throws MismatchedDimensionException, FactoryException, TransformException
  {
    final ProjectedWaterlevel[] waterlevels = getWaterlevels();

    final Set<String> descriptions = new LinkedHashSet<>();

    for( final ProjectedWaterlevel waterlevel : waterlevels )
    {
      /* collect description, ignore blanks/empty */
      final String description = waterlevel.getDescription();
      descriptions.add( StringUtils.trimToNull( description ) );
    }

    /* Build combined description without null elements */
    descriptions.remove( null );

    final String joined = StringUtils.join( descriptions, ", " ); //$NON-NLS-1$

    final int maxLength = PdbMappingUtils.findLength( CrossSectionPart.class, CrossSectionPart.PROPERTY_DESCRIPTION );
    return StringUtils.abbreviate( joined, maxLength );
  }
}