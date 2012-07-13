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
package org.kalypso.model.hydrology.timeseries;

import java.net.URL;
import java.util.LinkedHashSet;
import java.util.Set;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceVisitor;
import org.joda.time.Period;
import org.kalypso.commons.java.lang.Objects;
import org.kalypso.commons.time.PeriodUtils;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.contribs.java.net.UrlResolverSingleton;
import org.kalypso.model.hydrology.binding.timeseries.IStation;
import org.kalypso.model.hydrology.binding.timeseries.ITimeseries;
import org.kalypso.ogc.sensor.metadata.ParameterTypeLabelProvider;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;

/**
 * @author Dirk Kuch
 */
public final class Timeserieses
{
  private Timeserieses( )
  {
  }

  public static String getTreeLabel( final ITimeseries timeseries )
  {
    final Period timestep = timeseries.getTimestep();
    final String quality = timeseries.getQuality();

    final String periodName = PeriodUtils.formatDefault( timestep );

    if( StringUtils.isBlank( quality ) )
      return periodName;

    return String.format( "%s (%s)", periodName, quality ); //$NON-NLS-1$
  }

  public static String[] findTimeseriereses( final IStation station )
  {
    final Set<String> fileNames = new LinkedHashSet<>();
    try
    {
      final URL context = station.getWorkspace().getContext();
      final URL urlFolder = UrlResolverSingleton.resolveUrl( context, station.getTimeseriesFoldername() );

      final IFolder source = ResourceUtilities.findFolderFromURL( urlFolder );
      if( !source.exists() )
        return new String[] {};

      source.accept( new IResourceVisitor()
      {
        @Override
        public boolean visit( final IResource resource )
        {
          if( resource instanceof IFile )
          {
            final IFile file = (IFile) resource;

            if( StringUtils.equalsIgnoreCase( "zml", file.getFileExtension() ) ) //$NON-NLS-1$
            {
              fileNames.add( file.getName() );
            }
          }

          return true;
        }
      } );

    }
    catch( final Throwable t )
    {
      t.printStackTrace();
    }

    return fileNames.toArray( new String[] {} );

  }

  public static boolean hasType( final IFeatureBindingCollection<ITimeseries> collection, final String type )
  {
    for( final ITimeseries timeseries : collection )
    {
      if( timeseries.getParameterType().equals( type ) )
        return true;
    }

    return false;
  }

  public static String toLinkLabel( final ITimeseries timeseries )
  {
    if( timeseries == null )
      return StringUtils.EMPTY;

    final Feature owner = timeseries.getOwner();

    final String type = timeseries.getParameterType();
    final Period timestep = timeseries.getTimestep();
    final String quality = timeseries.getQuality();

    final StringBuffer buffer = new StringBuffer();

    if( owner instanceof IStation )
    {
      final IStation station = (IStation) owner;
      final String label = station.getDescription();

      if( StringUtils.isNotEmpty( label ) )
        buffer.append( label ).append( ": " ); //$NON-NLS-1$
    }

    if( Objects.isNotNull( timestep ) )
    {
      final String dateString = PeriodUtils.formatDefault( timestep );
      buffer.append( dateString ).append( " - " ); //$NON-NLS-1$
    }

    if( StringUtils.isNotEmpty( quality ) )
    {
      buffer.append( quality ); //$NON-NLS-1$
    }

    if( StringUtils.isNotEmpty( type ) )
    {
      final ParameterTypeLabelProvider provider = new ParameterTypeLabelProvider();
      buffer.append( String.format( " (%s)", provider.getText( type ) ) ); //$NON-NLS-1$
    }

    return buffer.toString().trim();
  }

  public static String formatTimeseriesFilename( final String parameterType, final String quality, final Period timestep )
  {
    final String periodText = PeriodUtils.formatDefault( timestep );

    final StringBuffer buffer = new StringBuffer();
    buffer.append( format( parameterType ) );
    buffer.append( format( periodText ) );
    buffer.append( format( quality ) );
    buffer.append( ".zml" ); //$NON-NLS-1$

    return buffer.toString().substring( 1 ); // remove first '_' character
  }

  private static String format( final String part )
  {
    if( StringUtils.isEmpty( part ) )
      return StringUtils.EMPTY;

    return StringUtils.trim( String.format( "_%s", part ) ); //$NON-NLS-1$
  }
}