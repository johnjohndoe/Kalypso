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
    SWTUtilities.putGridData( "GridData.BEGINNING", GridData.BEGINNING );
    SWTUtilities.putGridData( "GridData.CENTER", GridData.CENTER );
    SWTUtilities.putGridData( "GridData.END", GridData.END );
    SWTUtilities.putGridData( "GridData.FILL", GridData.FILL );
    SWTUtilities.putGridData( "GridData.FILL_BOTH", GridData.FILL_BOTH );
    SWTUtilities.putGridData( "GridData.FILL_HORIZONTAL", GridData.FILL_HORIZONTAL );
    SWTUtilities.putGridData( "GridData.FILL_VERTICAL", GridData.FILL_VERTICAL );
    SWTUtilities.putGridData( "GridData.GRAB_HORIZONTAL", GridData.GRAB_HORIZONTAL );
    SWTUtilities.putGridData( "GridData.GRAB_VERTICAL", GridData.GRAB_VERTICAL );
    SWTUtilities.putGridData( "GridData.HORIZONTAL_ALIGN_BEGINNING", GridData.HORIZONTAL_ALIGN_BEGINNING );
    SWTUtilities.putGridData( "GridData.HORIZONTAL_ALIGN_CENTER", GridData.HORIZONTAL_ALIGN_CENTER );
    SWTUtilities.putGridData( "GridData.HORIZONTAL_ALIGN_END", GridData.HORIZONTAL_ALIGN_END );
    SWTUtilities.putGridData( "GridData.HORIZONTAL_ALIGN_FILL", GridData.HORIZONTAL_ALIGN_FILL );
    SWTUtilities.putGridData( "GridData.VERTICAL_ALIGN_BEGINNING", GridData.VERTICAL_ALIGN_BEGINNING );
    SWTUtilities.putGridData( "GridData.VERTICAL_ALIGN_CENTER", GridData.VERTICAL_ALIGN_CENTER );
    SWTUtilities.putGridData( "GridData.VERTICAL_ALIGN_END", GridData.VERTICAL_ALIGN_END );
    SWTUtilities.putGridData( "GridData.VERTICAL_ALIGN_FILL", GridData.VERTICAL_ALIGN_FILL );
  }

  static final HashMap<String, Integer> m_swtMap = new HashMap<String, Integer>();
  static
  {
    SWTUtilities.putSWT( "SWT.BEGINNING", SWT.BEGINNING );
    SWTUtilities.putSWT( "SWT.BOLD", SWT.BOLD );
    SWTUtilities.putSWT( "SWT.BORDER", SWT.BORDER );
    SWTUtilities.putSWT( "SWT.CENTER", SWT.CENTER );
    SWTUtilities.putSWT( "SWT.CHECK", SWT.CHECK );
    SWTUtilities.putSWT( "SWT.END", SWT.END );
    SWTUtilities.putSWT( "SWT.FILL", SWT.FILL );
    SWTUtilities.putSWT( "SWT.HORIZONTAL", SWT.HORIZONTAL );
    SWTUtilities.putSWT( "SWT.LEAD", SWT.LEAD );
    SWTUtilities.putSWT( "SWT.LEFT", SWT.LEFT );
    SWTUtilities.putSWT( "SWT.MULTI", SWT.MULTI );
    SWTUtilities.putSWT( "SWT.NONE", SWT.NONE );
    SWTUtilities.putSWT( "SWT.PUSH", SWT.PUSH );
    SWTUtilities.putSWT( "SWT.RADIO", SWT.RADIO );
    SWTUtilities.putSWT( "SWT.RIGHT", SWT.RIGHT );
    SWTUtilities.putSWT( "SWT.TRAIL", SWT.TRAIL );
    SWTUtilities.putSWT( "SWT.VERTICAL", SWT.VERTICAL );
    SWTUtilities.putSWT( "SWT.DROP_DOWN", SWT.DROP_DOWN );
    SWTUtilities.putSWT( "SWT.READ_ONLY", SWT.READ_ONLY );
    SWTUtilities.putSWT( "SWT.FULL_SELECTION", SWT.FULL_SELECTION );
    SWTUtilities.putSWT( "SWT.WRAP", SWT.WRAP );
    SWTUtilities.putSWT( "SWT.V_SCROLL", SWT.V_SCROLL );
    SWTUtilities.putSWT( "SWT.H_SCROLL", SWT.H_SCROLL );
    SWTUtilities.putSWT( "SWT.FILL", SWT.FILL );
    // TODO really a lot to complete SWT keys
  }

  private static void putSWT( final String key, final int value )
  {
    SWTUtilities.m_swtMap.put( key, new Integer( value ) );
  }

  private static void putGridData( final String key, final int value )
  {
    SWTUtilities.m_gridDataMap.put( key, new Integer( value ) );
  }

  public static int createStyleFromString( final String key )
  {
    return SWTUtilities.createStyleFromString( SWTUtilities.m_swtMap, key );
  }

  public static int getGridData( final String key )
  {
    return SWTUtilities.createStyleFromString( SWTUtilities.m_gridDataMap, key );
  }

  private static int createStyleFromString( final HashMap map, final String style )
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
        final Integer value = (Integer) map.get( key );
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