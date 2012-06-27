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
package org.kalypso.kalypsomodel1d2d.conv;

import java.io.File;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Scanner;

import org.apache.commons.vfs2.FileObject;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.kalypsomodel1d2d.sim.ISimulation1D2DConstants;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

/**
 * @author ilya
 * 
 */
public class SWANDataConverterHelper
{
  private static String getTimeStringFormatedForSWANWithDelim( final Object pObjTime, final String pStrDelim )
  {
    String lStrTimeRes = ""; //$NON-NLS-1$
    SimpleDateFormat lDateFormatter;
    lDateFormatter = new SimpleDateFormat( "yyyyMMdd" + pStrDelim + "HHmmss" ); //$NON-NLS-1$ //$NON-NLS-2$
    if( pObjTime instanceof String )
    {
      lStrTimeRes = (String) pObjTime;
      SimpleDateFormat lDateFormatterLoc = new SimpleDateFormat();
      try
      {
        Date lDate = lDateFormatterLoc.parse( lStrTimeRes );
        lStrTimeRes = lDateFormatter.format( lDate );
      }
      catch( Exception e )
      {
      }
    }
    else if( pObjTime instanceof Date )
    {

      Date lDate = (Date) pObjTime;
      lStrTimeRes = lDateFormatter.format( lDate );
    }
    return lStrTimeRes;
  }

  public static String getTimeStringFormatedForSWANInput( final Object pObjTime )
  {
    return getTimeStringFormatedForSWANWithDelim( pObjTime, "." ); //$NON-NLS-1$
  }

  public static String getTimeStringFormatedForSWANOutput( final Object pObjTime )
  {
    return getTimeStringFormatedForSWANWithDelim( pObjTime, "_" ); //$NON-NLS-1$
  }

  public static Date getDateForStepFromString( String strActDateSWAN )
  {
    try
    {
      String lStrTimeFormat = "yyyyMMdd.HHmmss"; //$NON-NLS-1$
      SimpleDateFormat lSimpleDateFormat = new SimpleDateFormat( lStrTimeFormat );
      Date lDateRes = lSimpleDateFormat.parse( strActDateSWAN );
      return lDateRes;
    }
    catch( Exception e )
    {
      KalypsoCorePlugin.getDefault().getLog().log( StatusUtilities.statusFromThrowable( e ) );
    }
    return null;

  }

  public static GM_Position readCoordinateShiftValues( final FileObject pFile )
  {
    GM_Position lPosRes = null;

    Scanner scannerFile = null;
    Scanner scannerLine = null;
    try
    {

      FileObject swanShiftCoordFileObject = pFile.getParent().getChild( ISimulation1D2DConstants.SIM_SWAN_COORD_SHIFT_FILE );
      if( swanShiftCoordFileObject == null )
      {
        return lPosRes;
      }
      File lFile = new File( swanShiftCoordFileObject.getURL().toURI() );

      scannerFile = new Scanner( lFile );
      Double lDoubleShiftY = null;
      Double lDoubleShiftX = null;
      while( scannerFile.hasNextLine() )
      {
        String lStrNextLine = scannerFile.nextLine();
        if( lStrNextLine.contains( "=" ) ) { //$NON-NLS-1$
          scannerLine = new Scanner( lStrNextLine );
          scannerLine.useDelimiter( "=" ); //$NON-NLS-1$
          String lStrValueName = scannerLine.next();
          String lStrValue = scannerLine.next();
          if( ISimulation1D2DConstants.SIM_SWAN_COORD_SHIFT_X.equalsIgnoreCase( lStrValueName ) )
          {
            lDoubleShiftX = Double.parseDouble( lStrValue );
          }
          else if( ISimulation1D2DConstants.SIM_SWAN_COORD_SHIFT_Y.equalsIgnoreCase( lStrValueName ) )
          {
            lDoubleShiftY = Double.parseDouble( lStrValue );
          }
          scannerLine.close();
        }
        else
        {
          // System.out.println("Empty or invalid line. Unable to process. Processing the results without shift!");
        }
      }
      if( lDoubleShiftX != null && lDoubleShiftY != null )
        lPosRes = GeometryFactory.createGM_Position( lDoubleShiftX, lDoubleShiftY );

    }
    catch( Exception e )
    {
      e.printStackTrace();
    }
    finally
    {
      if( scannerFile != null )
        scannerFile.close();
      if( scannerLine != null )
        scannerLine.close();
    }

    return lPosRes;
  }

}
