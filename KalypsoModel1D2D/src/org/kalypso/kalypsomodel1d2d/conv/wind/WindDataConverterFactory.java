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
import java.util.ArrayList;
import java.util.List;

/**
 * @author ig
 *
 */
public class WindDataConverterFactory
{
  public static enum EWindFileTypes
  {
    DAT("dwd.dat"), //$NON-NLS-1$
    // ASC("asc"),
    DAT_GZ("dwd.dat.gz"), //$NON-NLS-1$
    // ASC("asc"),
    CSV_POINT_EXTRAPOLATION("point.xtrp.csv"); //$NON-NLS-1$

    private final String value;

    private static List<String> m_listExtensions = null;

    EWindFileTypes( final String v )
    {
      value = v;
    }

    public String value( )
    {
      return value;
    }

    public static EWindFileTypes fromValue( final String v )
    {
      for( final EWindFileTypes c : EWindFileTypes.values() )
      {
        if( v != null && (c.value.equals( v.toLowerCase() ) || v.toLowerCase().endsWith( c.value )) )
        {
          return c;
        }
      }
      throw new IllegalArgumentException( v );
    }

    public static List<String> getExtentionsList( )
    {
      if( m_listExtensions == null )
      {
        m_listExtensions = new ArrayList<>();
        for( final EWindFileTypes c : EWindFileTypes.values() )
        {
          m_listExtensions.add( c.value.toString() );
        }
      }
      return m_listExtensions;
    }
  }

  public static IWindDataReader getWindDataConverterForFileType( final URL pUrlInputFileName, final URL pUrlOuputDir, final String pStrFileNameSuffix, final String pSelectedCoordinateSystem )
  {
    switch( getInputFileType( pUrlInputFileName ) )
    {
      case DAT:
      case DAT_GZ:
        return new WindDataDWDVectorReader( pUrlInputFileName, pUrlOuputDir, pStrFileNameSuffix, pSelectedCoordinateSystem );

      case CSV_POINT_EXTRAPOLATION:
        return new WindPointDataCsvReader( pUrlInputFileName, pUrlOuputDir, pStrFileNameSuffix, pSelectedCoordinateSystem );

      default:
        break;
    }
    return null;
  }

  public static EWindFileTypes getInputFileType( final URL pUrlInputFile )
  {
    final EWindFileTypes lActInputType = EWindFileTypes.fromValue( pUrlInputFile.toExternalForm() );

    return lActInputType;
  }

}
