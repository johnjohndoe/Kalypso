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
package org.kalypso.kalypsomodel1d2d.conv.wind;

import java.io.File;
import java.net.URL;
import java.util.logging.Logger;

import org.apache.commons.io.FileUtils;
import org.deegree.framework.util.Pair;
import org.kalypso.grid.BinaryGeoGridWriter;
import org.kalypso.grid.IGeoGrid;
import org.kalypsodeegree_impl.gml.binding.commons.RectifiedGridDomain;

/**
 * @author ig
 *         performs the actual conversion of row data given as array of {@link Double} {@link Pair}s into {@link BinaryGeoGridPairsValues} object
 *         and saving it to given directory.
 */
@SuppressWarnings( "rawtypes" )
public class Primitives2GeoGridConverter
{
  private Pair[][] m_arrayData = null;

  private RectifiedGridDomain m_gridDescriptor = null;

  private String m_strFileName = null;

  private URL m_urlGeoGridDataFile = null;

  private URL m_urlOutputGridDirectory = null;

  private final int m_intScale = 2;

  private static final Logger logger = Logger.getLogger( WindDataDWDVectorReader.class.getName() );

  public Primitives2GeoGridConverter( final RectifiedGridDomain gridDescriptor, final Pair[][] data, final URL urlOutputGridDirectory, final String fileName )
  {
    m_arrayData = data;
    m_gridDescriptor = gridDescriptor;
    m_strFileName = fileName;
    m_urlOutputGridDirectory = urlOutputGridDirectory;

  }

  public IGeoGrid createGeoGrid( ) throws Exception
  {
    final File lOutputDir = FileUtils.toFile( m_urlOutputGridDirectory );
    if( !lOutputDir.exists() || !lOutputDir.isDirectory() )
    {
      return null;
    }
    final File lNewFile = new File( lOutputDir, m_strFileName );
    if( lNewFile.exists() )
    {
      logger.warning( "File with this name was found on disk: " + lNewFile.getAbsolutePath() + "; this file will be overwritten." ); //$NON-NLS-1$ //$NON-NLS-2$
//      return null;
    }
    lNewFile.createNewFile();
    /*
     * use BinaryGeoGridWriter for better performance by writing the output files
     * RandomAccessFile lRandomAccessFile = new RandomAccessFile( lNewFile, "rw" );
     * String lStrCoordinateSystem = KalypsoDeegreePlugin.getDefault().getCoordinateSystem();
     * GM_Point lPointOrigin = m_gridDescriptor.getOrigin( lStrCoordinateSystem );
     * Coordinate lCoordinateOrigin = new Coordinate( lPointOrigin.getX(), lPointOrigin.getY(), lPointOrigin.getZ() );
     * Coordinate lCoordinateOffsetX = new Coordinate( m_gridDescriptor.getOffsetX().getGeoX(), m_gridDescriptor.getOffsetX().getGeoY() );
     * Coordinate lCoordinateOffsetY = new Coordinate( m_gridDescriptor.getOffsetY().getGeoX(), m_gridDescriptor.getOffsetY().getGeoY() );
     * BinaryGeoGridWrapperForPairsModel lGrid = new BinaryGeoGridWrapperForPairsModel( lRandomAccessFile, m_gridDescriptor.getNumColumns(), m_gridDescriptor.getNumRows(), m_intScale ,
     * lCoordinateOrigin, lCoordinateOffsetX, lCoordinateOffsetY, lStrCoordinateSystem, true );
     */
    final int xSize = m_gridDescriptor.getNumColumns() * 2;
    final int ySize = m_gridDescriptor.getNumRows();
    final BinaryGeoGridWriter lGrid = new BinaryGeoGridWriter( lNewFile.getAbsolutePath(), xSize, ySize, m_intScale );
    m_urlGeoGridDataFile = lNewFile.toURI().toURL();

    for( int j = 0; j < ySize; ++j )
    {
      for( int i = 0; i < xSize; i += 2 )
      {
        lGrid.setValue( i, j, (Double)m_arrayData[i / 2][j].first );
        lGrid.setValue( i + 1, j, (Double)m_arrayData[i / 2][j].second );
      }
    }

    lGrid.dispose();
    return lGrid;
  }

  public final URL getUrlGeoGridDataFile( )
  {
    return m_urlGeoGridDataFile;
  }
}