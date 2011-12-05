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
package org.kalypso.kalypsosimulationmodel.core.terrainmodel;

import java.io.File;
import java.io.IOException;
import java.net.URLDecoder;

import org.kalypso.kalypsosimulationmodel.core.Assert;
import org.kalypso.kalypsosimulationmodel.i18n.Messages;
import org.shiftone.cache.Cache;
import org.shiftone.cache.adaptor.CacheMap;
import org.shiftone.cache.policy.fifo.FifoCacheFactory;

import com.vividsolutions.jts.io.ParseException;

public class NativeTerrainElevationModelFactory
{
  private static final Cache cache = new FifoCacheFactory().newInstance( "NativeTerrainElevationModelFactory_CACHE", 60 * 60 * 1000, 8 ); //$NON-NLS-1$

  CacheMap chCacheMap;

  private NativeTerrainElevationModelFactory( )
  {
    // yes empty
  }

  public static final IElevationProvider getTerrainElevationModel( final File nativeTerrainModelFile_ ) throws IllegalArgumentException, IOException
  {
    Assert.throwIAEOnNullParam( nativeTerrainModelFile_, "nativeTerrainModelFile_" ); //$NON-NLS-1$
    // Decoding the White Spaces present in the File Paths. Sometimes requires to decode twice.
    // One particular case is having %2520 instead of a single white space.
    final File nativeTerrainModelFile__ = new File( URLDecoder.decode( nativeTerrainModelFile_.toString(), "UTF-8" ) ); //$NON-NLS-1$
    final File nativeTerrainModelFile = new File( URLDecoder.decode( nativeTerrainModelFile__.toString(), "UTF-8" ) ); //$NON-NLS-1$
    if( nativeTerrainModelFile.isDirectory() )
    {
      return null;
      // throw new IllegalArgumentException( Messages.getString("NativeTerrainElevationModelFactory.5") +
      // nativeTerrainModelFile ); //$NON-NLS-1$
    }
    if( !nativeTerrainModelFile.exists() )
    {
      return null;
      // throw new IllegalArgumentException( Messages.getString("NativeTerrainElevationModelFactory.6") +
      // nativeTerrainModelFile ); //$NON-NLS-1$
    }
    return resolveTerrainElevationModel( nativeTerrainModelFile );
  }

  private static final IElevationProvider resolveTerrainElevationModel( final File ascFile ) throws IllegalArgumentException, IOException
  {
    final String filePath = ascFile.getAbsolutePath();
    final Object cachedEleModel = cache.getObject( filePath );
    if( cachedEleModel != null )
    {
      return (IElevationProvider) cachedEleModel;
    }
    if( filePath.endsWith( ".asc" ) ) //$NON-NLS-1$
    {
      final ASCTerrainElevationModel terrainElevationModel = new ASCTerrainElevationModel( ascFile.toURL() );
      cache.addObject( filePath, terrainElevationModel );
      return terrainElevationModel;
    }
    // It is the same as asc!!!
    else if( filePath.endsWith( ".asg" ) ) //$NON-NLS-1$
    {
      final ASCTerrainElevationModel terrainElevationModel = new ASCTerrainElevationModel( ascFile.toURL() );
      cache.addObject( filePath, terrainElevationModel );
      return terrainElevationModel;
    }
    else if( filePath.endsWith( ".hmo" ) ) //$NON-NLS-1$
    {
      try
      {
        final HMOTerrainElevationModel terrainElevationModel = new HMOTerrainElevationModel( ascFile.toURL() );
        cache.addObject( filePath, terrainElevationModel );
        return terrainElevationModel;
      }
      catch( final ParseException e )
      {
        e.printStackTrace();
        throw new IllegalArgumentException( Messages.getString( "org.kalypso.kalypsosimulationmodel.core.terrainmodel.NativeTerrainElevationModelFactory.4" ), e ); //$NON-NLS-1$
      }
    }
    else
    {
      throw new RuntimeException( Messages.getString( "org.kalypso.kalypsosimulationmodel.core.terrainmodel.NativeTerrainElevationModelFactory.10" ) + filePath ); //$NON-NLS-1$
    }
  }

  public static final void removeFromCache( final File ascFile )
  {
    try
    {
      final String filePath = ascFile.getAbsolutePath();
      // TODO: is the next line necessary?
      /* final Object cachedEleModel = */cache.getObject( filePath );
      cache.remove( filePath );
    }
    catch( final Throwable e )
    {
      e.printStackTrace();
    }
  }

}