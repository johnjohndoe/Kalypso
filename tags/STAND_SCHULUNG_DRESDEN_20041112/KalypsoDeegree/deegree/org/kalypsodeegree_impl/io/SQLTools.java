/*----------------    FILE HEADER  ------------------------------------------

 This file is part of deegree.
 Copyright (C) 2001 by:
 EXSE, Department of Geography, University of Bonn
 http://www.giub.uni-bonn.de/exse/
 lat/lon Fitzke/Fretter/Poth GbR
 http://www.lat-lon.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 Andreas Poth
 lat/lon Fitzke/Fretter/Poth GbR
 Meckenheimer Allee 176
 53115 Bonn
 Germany
 E-Mail: poth@lat-lon.de

 Jens Fitzke
 Department of Geography
 University of Bonn
 Meckenheimer Allee 166
 53115 Bonn
 Germany
 E-Mail: jens.fitzke@uni-bonn.de

 
 ---------------------------------------------------------------------------*/
/*
 * SQLTools.java
 * 
 * Created on 26. November 2002, 17:27
 */
package org.deegree_impl.io;

import org.deegree_impl.tools.TimeTools;

/**
 * 
 * @author Administrator
 */
public class SQLTools
{
  /**
   * formats a date expression for request against different database vendors
   */
  public static String formatDate( String dbVendor, String val )
  {
    val = val.trim();

    String y = "" + TimeTools.get( TimeTools.YEAR, val );
    String mo = "" + TimeTools.get( TimeTools.MONTH, val );

    if( TimeTools.get( TimeTools.MONTH, val ) < 10 )
    {
      mo = "0" + TimeTools.get( TimeTools.MONTH, val );
    }

    String d = "" + TimeTools.get( TimeTools.DAY, val );

    if( TimeTools.get( TimeTools.DAY, val ) < 10 )
    {
      d = "0" + TimeTools.get( TimeTools.DAY, val );
    }

    String h = null;

    if( val.length() > 11 )
    {
      h = "" + TimeTools.get( TimeTools.HOUR, val );

      if( TimeTools.get( TimeTools.HOUR, val ) < 10 )
      {
        h = "0" + TimeTools.get( TimeTools.HOUR, val );
      }
    }

    String mi = null;

    if( val.length() > 14 )
    {
      mi = "" + TimeTools.get( TimeTools.MINUTE, val );

      if( TimeTools.get( TimeTools.MINUTE, val ) < 10 )
      {
        mi = "0" + TimeTools.get( TimeTools.MINUTE, val );
      }
    }

    String s = null;

    if( val.length() > 17 )
    {
      s = "" + TimeTools.get( TimeTools.SECOND, val );

      if( TimeTools.get( TimeTools.SECOND, val ) < 10 )
      {
        s = "0" + TimeTools.get( TimeTools.SECOND, val );
      }
    }

    if( "ACCESS".equals( dbVendor ) )
    {
      val = "#" + mo + "/" + d + "/" + y + "#";
    }
    else if( "ORACLE".equals( dbVendor ) )
    {
      val = val.replace( 'T', ' ' );

      String date = "yyyy-mm-dd hh:min:sec";

      if( h == null )
      {
        date = "yyyy-mm-dd";
      }
      else if( mi == null )
      {
        date = "yyyy-mm-dd hh";
      }
      else if( s == null )
      {
        date = "yyyy-mm-dd hh:min";
      }

      val = "to_date('" + val + "','" + date + "')";
    }
    else if( "POSTGRES".equalsIgnoreCase( dbVendor ) || "POSTGis".equalsIgnoreCase( dbVendor ) )
    {
      if( y.length() > 2 )
      {
        y = y.substring( 2, y.length() );
      }
      val = d + "/" + mo + "/" + y;
    }

    return val;
  }

}