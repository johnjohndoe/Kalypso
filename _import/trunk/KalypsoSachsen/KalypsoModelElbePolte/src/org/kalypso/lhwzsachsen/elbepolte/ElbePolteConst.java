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
 *  g.belger@bjoernsen.de
 *  m.schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *   
 *  ---------------------------------------------------------------------------*/
package org.kalypso.lhwzsachsen.elbepolte;

import java.text.SimpleDateFormat;
import java.util.TimeZone;

/**
 * @author thuel2
 */
public class ElbePolteConst
{
  public static final String ELBEPOLTE_CODEPAGE = "Cp1252";
  public static final int ELBEPOLTE_TIMESTEP = 3;

  // Metadaten-Properties for ZML files
  public static final String PROP_COMMENT = "comment";

  public static SimpleDateFormat HWVS_DATE_FORMAT = new SimpleDateFormat( "yyyy MM dd HH" );
  static
  {
    HWVS_DATE_FORMAT.setTimeZone( TimeZone.getTimeZone( "GMT+1" ) );
  }
  public static String HWVS_FORMAT_DATA_ROW = "%5d%9s%4s%4s%5s%9.1f";

  public static SimpleDateFormat HWVS_DATE_FORMAT_YEAR = new SimpleDateFormat( "yyyy" );
  static
  {
    HWVS_DATE_FORMAT_YEAR.setTimeZone( TimeZone.getTimeZone( "GMT+1" ) );
  }
  public static SimpleDateFormat HWVS_DATE_FORMAT_MONTH = new SimpleDateFormat( "M" );
  static
  {
    HWVS_DATE_FORMAT_MONTH.setTimeZone( TimeZone.getTimeZone( "GMT+1" ) );
  }
  public static SimpleDateFormat HWVS_DATE_FORMAT_DAY = new SimpleDateFormat( "d" );
  static
  {
    HWVS_DATE_FORMAT_DAY.setTimeZone( TimeZone.getTimeZone( "GMT+1" ) );
  }
  public static SimpleDateFormat HWVS_DATE_FORMAT_HOUR = new SimpleDateFormat( "H" );
  static
  {
    HWVS_DATE_FORMAT_HOUR.setTimeZone( TimeZone.getTimeZone( "GMT+1" ) );
  }

  //
  public static final String CALCJOB_SPEC = "elbepoltecalcjob_spec.xml";

  public static final String DATA_STARTFORECAST_DATE = "startforecast_date";

  public static final String DATA_GML = "data_gml";

  public static final String GML_STRECKE_COLL = "streckeMember";//[Strecke]
  public static final String GML_START_UND_ELBE_PEGEL_COLL = "pegelMember";
  public static final String GML_START_PEGEL_COLL = "pegelMember[StartPegel]";//[StartPegel]

  public static final String GML_ELBE_PEGEL_COLL = "pegelMember[ElbePegel]";//[ElbePegel]
  public static final String GML_ZWG_ZUFLUSS_COLL = "zwgZuflussMember";//[ZwgZufluss]
  public static final String GML_H_PEGEL_COLL = "internerHPegelMember";//[InternerHPegel]

  public static final String PAR_FILE_SEP = "\t";
  public static final String PAR_LINE_END = " / ";

  public static final String PAR_NUMBER_FORMAT_LONG = "%9.5f";
  public static final String PAR_NUMBER_FORMAT_SHORT = "%3.1f";
  public static final String PAR_NUMBER_FORMAT_INTEGER = "%i";

  public static final String RECHENKERN_ZIP = "HWObereElbe.zip";
  public static final String RECHENKERN_EXE = "HWObereElbe.exe";
}
