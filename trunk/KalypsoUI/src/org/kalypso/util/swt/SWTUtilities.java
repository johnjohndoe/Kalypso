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

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;

/**
 * @author doemming
 */
public class SWTUtilities
{
  static final HashMap<String, Integer> m_gridDataMap = new HashMap<String, Integer>();
  static
  {
    putGridData( "GridData.BEGINNING", GridData.BEGINNING );
    putGridData( "GridData.CENTER", GridData.CENTER );
    putGridData( "GridData.END", GridData.END );
    putGridData( "GridData.FILL", GridData.FILL );
    putGridData( "GridData.FILL_BOTH", GridData.FILL_BOTH );
    putGridData( "GridData.FILL_HORIZONTAL", GridData.FILL_HORIZONTAL );
    putGridData( "GridData.FILL_VERTICAL", GridData.FILL_VERTICAL );
    putGridData( "GridData.GRAB_HORIZONTAL", GridData.GRAB_HORIZONTAL );
    putGridData( "GridData.GRAB_VERTICAL", GridData.GRAB_VERTICAL );
    putGridData( "GridData.HORIZONTAL_ALIGN_BEGINNING", GridData.HORIZONTAL_ALIGN_BEGINNING );
    putGridData( "GridData.HORIZONTAL_ALIGN_CENTER", GridData.HORIZONTAL_ALIGN_CENTER );
    putGridData( "GridData.HORIZONTAL_ALIGN_END", GridData.HORIZONTAL_ALIGN_END );
    putGridData( "GridData.HORIZONTAL_ALIGN_FILL", GridData.HORIZONTAL_ALIGN_FILL );
    putGridData( "GridData.VERTICAL_ALIGN_BEGINNING", GridData.VERTICAL_ALIGN_BEGINNING );
    putGridData( "GridData.VERTICAL_ALIGN_CENTER", GridData.VERTICAL_ALIGN_CENTER );
    putGridData( "GridData.VERTICAL_ALIGN_END", GridData.VERTICAL_ALIGN_END );
    putGridData( "GridData.VERTICAL_ALIGN_FILL", GridData.VERTICAL_ALIGN_FILL );
  }

  static final HashMap<String, Integer> m_swtMap = new HashMap<String, Integer>();
  static
  {
    putSWT( "SWT.BEGINNING", SWT.BEGINNING );
    putSWT( "SWT.BOLD", SWT.BOLD );
    putSWT( "SWT.BORDER", SWT.BORDER );
    putSWT( "SWT.CENTER", SWT.CENTER );
    putSWT( "SWT.CHECK", SWT.CHECK );
    putSWT( "SWT.END", SWT.END );
    putSWT( "SWT.FILL", SWT.FILL );
    putSWT( "SWT.HORIZONTAL", SWT.HORIZONTAL );
    putSWT( "SWT.LEAD", SWT.LEAD );
    putSWT( "SWT.LEFT", SWT.LEFT );
    putSWT( "SWT.MULTI", SWT.MULTI );
    putSWT( "SWT.NONE", SWT.NONE );
    putSWT( "SWT.PUSH", SWT.PUSH );
    putSWT( "SWT.RADIO", SWT.RADIO );
    putSWT( "SWT.RIGHT", SWT.RIGHT );
    putSWT( "SWT.TRAIL", SWT.TRAIL );
    putSWT( "SWT.VERTICAL", SWT.VERTICAL );
    // TODO really a lot to complete SWT keys
  }

  private static void putSWT( final String key, final int value )
  {
    m_swtMap.put( key, new Integer( value ) );
  }

  private static void putGridData( final String key, final int value )
  {
    m_gridDataMap.put( key, new Integer( value ) );
  }

  public static int createStyleFromString( String key )
  {
    return createStyleFromString( m_swtMap, key );
  }

  public static int getGridData( String key )
  {
    return createStyleFromString( m_gridDataMap, key );
  }

  private static int createStyleFromString( final HashMap map, final String style )
  {
    try
    {
      int result = SWT.NONE;
      if( style.matches( "[0-9]+" ) )
        return Integer.parseInt( style );
      final String[] keys = style.split( "\\|" );
      for( int i = 0; i < keys.length; i++ )
      {
        final String key = keys[i].trim();
        final Integer value = (Integer) map.get( key );
        if( value != null )
          result |= value.intValue();
        else
          System.out.println( "// TODO: implement " + key );
      }
      return result;
    }
    catch( Exception e )
    {
      e.printStackTrace();
      return SWT.NONE;
    }
  }
}