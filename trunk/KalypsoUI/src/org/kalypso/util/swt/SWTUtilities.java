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
package org.kalypso.util.swt;

import java.util.HashMap;
import java.util.Map;

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;

/**
 * @author doemming
 */
public class SWTUtilities
{
  private static final Map<String, Integer> GRID_DATA_MAP = new HashMap<String, Integer>();
  static
  {
    GRID_DATA_MAP.put( "GridData.BEGINNING", new Integer( GridData.BEGINNING ) );
    GRID_DATA_MAP.put( "GridData.CENTER", new Integer( GridData.CENTER ) );
    GRID_DATA_MAP.put( "GridData.END", new Integer( GridData.END ) );
    GRID_DATA_MAP.put( "GridData.FILL", new Integer( GridData.FILL ) );
    GRID_DATA_MAP.put( "GridData.FILL_BOTH", new Integer( GridData.FILL_BOTH ) );
    GRID_DATA_MAP.put( "GridData.FILL_HORIZONTAL", new Integer( GridData.FILL_HORIZONTAL ) );
    GRID_DATA_MAP.put( "GridData.FILL_VERTICAL", new Integer( GridData.FILL_VERTICAL ) );
    GRID_DATA_MAP.put( "GridData.GRAB_HORIZONTAL", new Integer( GridData.GRAB_HORIZONTAL ) );
    GRID_DATA_MAP.put( "GridData.GRAB_VERTICAL", new Integer( GridData.GRAB_VERTICAL ) );
    GRID_DATA_MAP.put( "GridData.HORIZONTAL_ALIGN_BEGINNING", new Integer( GridData.HORIZONTAL_ALIGN_BEGINNING ) );
    GRID_DATA_MAP.put( "GridData.HORIZONTAL_ALIGN_CENTER", new Integer( GridData.HORIZONTAL_ALIGN_CENTER ) );
    GRID_DATA_MAP.put( "GridData.HORIZONTAL_ALIGN_END", new Integer( GridData.HORIZONTAL_ALIGN_END ) );
    GRID_DATA_MAP.put( "GridData.HORIZONTAL_ALIGN_FILL", new Integer( GridData.HORIZONTAL_ALIGN_FILL ) );
    GRID_DATA_MAP.put( "GridData.VERTICAL_ALIGN_BEGINNING", new Integer( GridData.VERTICAL_ALIGN_BEGINNING ) );
    GRID_DATA_MAP.put( "GridData.VERTICAL_ALIGN_CENTER", new Integer( GridData.VERTICAL_ALIGN_CENTER ) );
    GRID_DATA_MAP.put( "GridData.VERTICAL_ALIGN_END", new Integer( GridData.VERTICAL_ALIGN_END ) );
    GRID_DATA_MAP.put( "GridData.VERTICAL_ALIGN_FILL", new Integer( GridData.VERTICAL_ALIGN_FILL ) );
  }

  private static final Map<String, Integer> SWT_MAP = new HashMap<String, Integer>();
  static
  {
    SWT_MAP.put( "SWT.BEGINNING", SWT.BEGINNING );
    SWT_MAP.put( "SWT.BOLD", SWT.BOLD );
    SWT_MAP.put( "SWT.BORDER", SWT.BORDER );
    SWT_MAP.put( "SWT.CENTER", SWT.CENTER );
    SWT_MAP.put( "SWT.CHECK", SWT.CHECK );
    SWT_MAP.put( "SWT.END", SWT.END );
    SWT_MAP.put( "SWT.FILL", SWT.FILL );
    SWT_MAP.put( "SWT.HORIZONTAL", SWT.HORIZONTAL );
    SWT_MAP.put( "SWT.LEAD", SWT.LEAD );
    SWT_MAP.put( "SWT.LEFT", SWT.LEFT );
    SWT_MAP.put( "SWT.MULTI", SWT.MULTI );
    SWT_MAP.put( "SWT.NONE", SWT.NONE );
    SWT_MAP.put( "SWT.PUSH", SWT.PUSH );
    SWT_MAP.put( "SWT.RADIO", SWT.RADIO );
    SWT_MAP.put( "SWT.RIGHT", SWT.RIGHT );
    SWT_MAP.put( "SWT.TRAIL", SWT.TRAIL );
    SWT_MAP.put( "SWT.VERTICAL", SWT.VERTICAL );
    SWT_MAP.put( "SWT.DROP_DOWN", SWT.DROP_DOWN );
    SWT_MAP.put( "SWT.READ_ONLY", SWT.READ_ONLY );
    SWT_MAP.put( "SWT.FULL_SELECTION", SWT.FULL_SELECTION );
    SWT_MAP.put( "SWT.WRAP", SWT.WRAP );
    SWT_MAP.put( "SWT.V_SCROLL", SWT.V_SCROLL );
    SWT_MAP.put( "SWT.H_SCROLL", SWT.H_SCROLL );
    SWT_MAP.put( "SWT.FILL", SWT.FILL );
    SWT_MAP.put( "SWT.TOP", SWT.TOP );
    SWT_MAP.put( "SWT.BOTTOM", SWT.BOTTOM );
    // Add other keys to complete the map
  }

  public static int createStyleFromString( final String key )
  {
    return SWTUtilities.createStyleFromString( SWT_MAP, key );
  }

  public static int getGridData( final String key )
  {
    return SWTUtilities.createStyleFromString( GRID_DATA_MAP, key );
  }

  private static int createStyleFromString( final Map<String, Integer> map, final String style )
  {
    try
    {
      if( style == null )
        return SWT.NONE;

      int result = SWT.NONE;
      if( style.matches( "[0-9]+" ) )
        return Integer.parseInt( style );

      final String[] keys = style.split( "\\|" );
      for( final String element : keys )
      {
        final String key = element.trim();
        final Integer value = map.get( key );
        if( value != null )
          result |= value.intValue();
        else
          System.out.println( "// TODO: implement " + key );
      }
      return result;
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      return SWT.NONE;
    }
  }
}