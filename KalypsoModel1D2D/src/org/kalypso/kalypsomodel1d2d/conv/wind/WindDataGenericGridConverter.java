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

import java.net.URL;
import java.text.SimpleDateFormat;
import java.util.Date;

import org.deegree.framework.util.Pair;
import org.kalypso.kalypsosimulationmodel.core.wind.IWindDataWrapper;
import org.kalypsodeegree_impl.gml.binding.commons.RectifiedGridDomain;

/**
 * @author ig
 * 
 */
@SuppressWarnings("rawtypes")
public class WindDataGenericGridConverter implements IWindDataWrapper// IWindDataReader
{
  public static String STR_INTERNAL_DATA_FILE_NAME_PREFIX = "windDataInternal_"; //$NON-NLS-1$

  public static String STR_INTERNAL_DATA_FILE_EXT = ".bin"; //$NON-NLS-1$

  private boolean m_boolIsRegular = true;

  private Date m_date = null;

  private RectifiedGridDomain m_gridDescriptor = null;

  private URL m_urlDataFile = null;

  private Pair[][] m_arrayData;

  private URL m_urlOutputDir;

  private String m_strGridFileSuffix;

  public WindDataGenericGridConverter( final Date pDate, final Pair[][] pArrayData, final RectifiedGridDomain pGridDescriptor, final URL pUrlOutputGridDirectory, String fileNameSuffix )
  {
    m_gridDescriptor = pGridDescriptor;
    m_date = pDate;
    m_arrayData = pArrayData;
    m_urlOutputDir = pUrlOutputGridDirectory;
    m_strGridFileSuffix = fileNameSuffix;
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.wind.IWindDataWrapper#getCoordinateSystem()
   */
  @Override
  public String getCoordinateSystem( )
  {
    return m_gridDescriptor.getCoordinateSystem();
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.wind.IWindDataWrapper#setCoordinateSystem(java.lang.String)
   */
  @Override
  public void setCoordinateSystem( String coordinateSystem )
  {
    // TODO: what to do? replace descriptor?
    if( m_gridDescriptor != null && m_gridDescriptor.getCoordinateSystem().equals( coordinateSystem ) )
    {
      throw new UnsupportedOperationException( "cannot reset actual coordinate system..." ); //$NON-NLS-1$
    }
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.wind.IWindDataWrapper#isRegularGrid()
   */
  @Override
  public boolean isRegularGrid( )
  {
    return m_boolIsRegular;
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.wind.IWindDataWrapper#getDataFile()
   */
  @Override
  public URL getDataFileURL( )
  {
    return m_urlDataFile;
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.wind.IWindDataWrapper#getDateStep()
   */
  @Override
  public Date getDateStep( )
  {
    return m_date;
  }

  /**
   * converts the values to the binary grid with help of {@link Primitives2GeoGridConverter}
   * 
   */
  public boolean convert( )
  {
    SimpleDateFormat lDateFormater = new SimpleDateFormat( "yyyyMMdd_HHmmss" ); //$NON-NLS-1$
    Primitives2GeoGridConverter lConverter = new Primitives2GeoGridConverter( m_gridDescriptor, m_arrayData, m_urlOutputDir, STR_INTERNAL_DATA_FILE_NAME_PREFIX + m_strGridFileSuffix + "_" //$NON-NLS-1$
        + lDateFormater.format( m_date ) + STR_INTERNAL_DATA_FILE_EXT );
    try
    {
      lConverter.createGeoGrid();
      m_urlDataFile = lConverter.getUrlGeoGridDataFile();
    }
    catch( Exception e )
    {
      e.printStackTrace();
      return false;
    }
    return true;
  }

}
