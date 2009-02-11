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
    GRID_DATA_MAP.put( "GridData.BEGINNING", new Integer( GridData.BEGINNING ) ); //$NON-NLS-1$
    GRID_DATA_MAP.put( "GridData.CENTER", new Integer( GridData.CENTER ) ); //$NON-NLS-1$
    GRID_DATA_MAP.put( "GridData.END", new Integer( GridData.END ) ); //$NON-NLS-1$
    GRID_DATA_MAP.put( "GridData.FILL", new Integer( GridData.FILL ) ); //$NON-NLS-1$
    GRID_DATA_MAP.put( "GridData.FILL_BOTH", new Integer( GridData.FILL_BOTH ) ); //$NON-NLS-1$
    GRID_DATA_MAP.put( "GridData.FILL_HORIZONTAL", new Integer( GridData.FILL_HORIZONTAL ) ); //$NON-NLS-1$
    GRID_DATA_MAP.put( "GridData.FILL_VERTICAL", new Integer( GridData.FILL_VERTICAL ) ); //$NON-NLS-1$
    GRID_DATA_MAP.put( "GridData.GRAB_HORIZONTAL", new Integer( GridData.GRAB_HORIZONTAL ) ); //$NON-NLS-1$
    GRID_DATA_MAP.put( "GridData.GRAB_VERTICAL", new Integer( GridData.GRAB_VERTICAL ) ); //$NON-NLS-1$
    GRID_DATA_MAP.put( "GridData.HORIZONTAL_ALIGN_BEGINNING", new Integer( GridData.HORIZONTAL_ALIGN_BEGINNING ) ); //$NON-NLS-1$
    GRID_DATA_MAP.put( "GridData.HORIZONTAL_ALIGN_CENTER", new Integer( GridData.HORIZONTAL_ALIGN_CENTER ) ); //$NON-NLS-1$
    GRID_DATA_MAP.put( "GridData.HORIZONTAL_ALIGN_END", new Integer( GridData.HORIZONTAL_ALIGN_END ) ); //$NON-NLS-1$
    GRID_DATA_MAP.put( "GridData.HORIZONTAL_ALIGN_FILL", new Integer( GridData.HORIZONTAL_ALIGN_FILL ) ); //$NON-NLS-1$
    GRID_DATA_MAP.put( "GridData.VERTICAL_ALIGN_BEGINNING", new Integer( GridData.VERTICAL_ALIGN_BEGINNING ) ); //$NON-NLS-1$
    GRID_DATA_MAP.put( "GridData.VERTICAL_ALIGN_CENTER", new Integer( GridData.VERTICAL_ALIGN_CENTER ) ); //$NON-NLS-1$
    GRID_DATA_MAP.put( "GridData.VERTICAL_ALIGN_END", new Integer( GridData.VERTICAL_ALIGN_END ) ); //$NON-NLS-1$
    GRID_DATA_MAP.put( "GridData.VERTICAL_ALIGN_FILL", new Integer( GridData.VERTICAL_ALIGN_FILL ) ); //$NON-NLS-1$
  }

  private static final Map<String, Integer> SWT_MAP = new HashMap<String, Integer>();
  static
  {
    SWT_MAP.put( "SWT.BEGINNING", SWT.BEGINNING ); //$NON-NLS-1$
    SWT_MAP.put( "SWT.BOLD", SWT.BOLD ); //$NON-NLS-1$
    SWT_MAP.put( "SWT.BORDER", SWT.BORDER ); //$NON-NLS-1$
    SWT_MAP.put( "SWT.CENTER", SWT.CENTER ); //$NON-NLS-1$
    SWT_MAP.put( "SWT.CHECK", SWT.CHECK ); //$NON-NLS-1$
    SWT_MAP.put( "SWT.END", SWT.END ); //$NON-NLS-1$
    SWT_MAP.put( "SWT.FILL", SWT.FILL ); //$NON-NLS-1$
    SWT_MAP.put( "SWT.HORIZONTAL", SWT.HORIZONTAL ); //$NON-NLS-1$
    SWT_MAP.put( "SWT.LEAD", SWT.LEAD ); //$NON-NLS-1$
    SWT_MAP.put( "SWT.LEFT", SWT.LEFT ); //$NON-NLS-1$
    SWT_MAP.put( "SWT.MULTI", SWT.MULTI ); //$NON-NLS-1$
    SWT_MAP.put( "SWT.NONE", SWT.NONE ); //$NON-NLS-1$
    SWT_MAP.put( "SWT.PUSH", SWT.PUSH ); //$NON-NLS-1$
    SWT_MAP.put( "SWT.RADIO", SWT.RADIO ); //$NON-NLS-1$
    SWT_MAP.put( "SWT.RIGHT", SWT.RIGHT ); //$NON-NLS-1$
    SWT_MAP.put( "SWT.TRAIL", SWT.TRAIL ); //$NON-NLS-1$
    SWT_MAP.put( "SWT.VERTICAL", SWT.VERTICAL ); //$NON-NLS-1$
    SWT_MAP.put( "SWT.DROP_DOWN", SWT.DROP_DOWN ); //$NON-NLS-1$
    SWT_MAP.put( "SWT.READ_ONLY", SWT.READ_ONLY ); //$NON-NLS-1$
    SWT_MAP.put( "SWT.FULL_SELECTION", SWT.FULL_SELECTION ); //$NON-NLS-1$
    SWT_MAP.put( "SWT.WRAP", SWT.WRAP ); //$NON-NLS-1$
    SWT_MAP.put( "SWT.V_SCROLL", SWT.V_SCROLL ); //$NON-NLS-1$
    SWT_MAP.put( "SWT.H_SCROLL", SWT.H_SCROLL ); //$NON-NLS-1$
    SWT_MAP.put( "SWT.FILL", SWT.FILL ); //$NON-NLS-1$
    SWT_MAP.put( "SWT.TOP", SWT.TOP ); //$NON-NLS-1$
    SWT_MAP.put( "SWT.BOTTOM", SWT.BOTTOM ); //$NON-NLS-1$
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
      if( style.matches( "[0-9]+" ) ) //$NON-NLS-1$
        return Integer.parseInt( style );

      final String[] keys = style.split( "\\|" ); //$NON-NLS-1$
      for( final String element : keys )
      {
        final String key = element.trim();
        final Integer value = map.get( key );
        if( value != null )
          result |= value.intValue();
        else
          System.out.println( "// TODO: implement " + key ); //$NON-NLS-1$
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