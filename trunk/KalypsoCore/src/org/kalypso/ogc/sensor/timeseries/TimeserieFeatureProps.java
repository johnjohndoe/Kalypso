/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and
 
 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

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

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
  
---------------------------------------------------------------------------------------------------*/
package org.kalypso.ogc.sensor.timeseries;

import java.util.Properties;

/**
 * Holds properties for a timeseries-feature. This class is used as a struct.
 * 
 * @author schlienger
 */
public class TimeserieFeatureProps
{
  /** If set, use this property of the feature to create name */
  private final static String PROP_NAMECOLUMN = "nameColumn";

  /** If {@link #PROP_NAMECOLUMN} is not set, use this name instead */
  private final static String PROP_NAMESTRING = "nameString";

  private final static String PROP_LINKCOLUM = "linkColumn";

  private final static String PROP_COLOR = "color";
  
  private final static String DEFAULT_NAMESTRING = "%obsname% (%axisname%)";

  private final String m_nameColumn;

  private final String m_linkColumn;

  private final String m_filter;

  private static final String PROP_FILTER = "filter";

  private final String m_nameString;

  private final String m_color;

  public TimeserieFeatureProps( final Properties props )
  {
    this( props.getProperty( PROP_NAMECOLUMN ), props.getProperty( PROP_NAMESTRING ), props.getProperty( PROP_LINKCOLUM ),props.getProperty( PROP_FILTER ), props.getProperty( PROP_COLOR ) );
  }

  public TimeserieFeatureProps( final String nameColumn, final String nameString, final String linkColumn, final String filter, final String color )
  {
    m_nameColumn = nameColumn;
    m_color = color;

    if( nameColumn != null && nameString == null )
      m_nameString = "%featureprop%";
    else if( nameString == null )
      m_nameString = DEFAULT_NAMESTRING;
    else
      m_nameString = nameString;
    
    m_linkColumn = linkColumn;
    m_filter = filter;
  }
  
  public String getFilter()
  {
    return m_filter;
  }
  
  /**
   * @return Returns the linkColumn.
   */
  public String getLinkColumn( )
  {
    return m_linkColumn;
  }
  
  /**
   * @return Returns the nameColumn.
   */
  public String getNameColumn( )
  {
    return m_nameColumn;
  }
  
  public String getNameString()
  {
    return m_nameString;
  }
  
  public String getColor()
  {
    return m_color;
  }
}