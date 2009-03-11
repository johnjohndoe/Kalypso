/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
package org.kalypso.ogc.sensor.view.wizard.cvssheet;

import java.util.HashMap;
import java.util.Map;

import org.apache.commons.lang.NotImplementedException;

/**
 * @author Dirk Kuch
 */
public class CsvSheetImportDataModel
{
  public enum CSV_COLUMN_SEPERATORS
  {
    eSpace,
    eTab,
    eSemicolon;

    public char getValue( )
    {
      final CSV_COLUMN_SEPERATORS kind = CSV_COLUMN_SEPERATORS.valueOf( name() );

      switch( kind )
      {
        case eSpace:
          return ' ';
        case eTab:
          return '\t';
        case eSemicolon:
          return ';';
        default:
          throw new NotImplementedException();
      }
    }

    /**
     * @see java.lang.Enum#toString()
     */
    @Override
    public String toString( )
    {
      final CSV_COLUMN_SEPERATORS kind = CSV_COLUMN_SEPERATORS.valueOf( name() );

      switch( kind )
      {
        case eSpace:
          return "\" \" (Space)"; //$NON-NLS-1$
        case eTab:
          return "TAB"; //$NON-NLS-1$
        case eSemicolon:
          return ";"; //$NON-NLS-1$
        default:
          throw new NotImplementedException();
      }
    }
  }

  public enum DECIMAL_NUMBER_SEPERATORS
  {
    eComma,
    ePoint;

    public String getRegEx( )
    {
      final DECIMAL_NUMBER_SEPERATORS kind = DECIMAL_NUMBER_SEPERATORS.valueOf( name() );

      switch( kind )
      {
        case eComma:
          return "\\."; //$NON-NLS-1$
        case ePoint:
          return "\\,"; //$NON-NLS-1$
        default:
          throw new NotImplementedException();
      }
    }

    public String getValue( )
    {
      final DECIMAL_NUMBER_SEPERATORS kind = DECIMAL_NUMBER_SEPERATORS.valueOf( name() );

      switch( kind )
      {
        case eComma:
          return "."; //$NON-NLS-1$
        case ePoint:
          return ","; //$NON-NLS-1$
        default:
          throw new NotImplementedException();
      }
    }

    /**
     * @see java.lang.Enum#toString()
     */
    @Override
    public String toString( )
    {
      final DECIMAL_NUMBER_SEPERATORS kind = DECIMAL_NUMBER_SEPERATORS.valueOf( name() );

      switch( kind )
      {
        case eComma:
          return "\".\" (Point)"; //$NON-NLS-1$
        case ePoint:
          return "\",\" (Comma)"; //$NON-NLS-1$
        default:
          throw new NotImplementedException();
      }
    }
  }

  public enum TSM_KEY
  {
    eTsmImportType,
    eCsvTimeSeriesIsWorQ,
    eCsvTimeSeriesFile,
    eCsvTimeSeriesWqConnectionFile,
    eCsvColumnSeperator,
    eCsvDateFormat,
    eCsvTimeZone,
    eCsvDecimalNumberSeperator,
    eCsvName,
    eCsvDescription,
    eCsvRiver,
    eCsvPosition,
    eZmlFile,
    eDestinationDir,
    eCsvWqConnectionFirstColumn,
    eFileItem,
    ePegelNullPunkt;
  }

  public enum WQ_KIND
  {
    eW,
    eQ;

    /**
     * @see java.lang.Enum#toString()
     */
    @Override
    public String toString( )
    {
      final WQ_KIND kind = WQ_KIND.valueOf( name() );

      switch( kind )
      {
        case eW:
          return "W [cm]"; //$NON-NLS-1$
        case eQ:
          return "Q [m�/s]"; //$NON-NLS-1$
        default:
          throw new NotImplementedException();
      }
    }

    public String toType( )
    {
      final WQ_KIND kind = WQ_KIND.valueOf( name() );

      switch( kind )
      {
        case eW:
          return "W"; //$NON-NLS-1$
        case eQ:
          return "Q"; //$NON-NLS-1$
        default:
          throw new NotImplementedException();
      }

    }

  }

  Map<TSM_KEY, Object> data = new HashMap<TSM_KEY, Object>();

  public Object getValue( final TSM_KEY key )
  {
    if( key == null )
      throw new IllegalStateException();

    return data.get( key );
  }

  public void setValue( final TSM_KEY key, final Object value )
  {
    if( key == null )
      throw new IllegalStateException();

    data.put( key, value );
  }
}
