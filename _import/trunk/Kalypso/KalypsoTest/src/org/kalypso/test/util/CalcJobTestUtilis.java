/*
 * --------------- Kalypso-Header --------------------------------------------------------------------
 * 
 * This file is part of kalypso. Copyright (C) 2004, 2005 by:
 * 
 * Technical University Hamburg-Harburg (TUHH) Institute of River and coastal engineering Denickestr. 22 21073 Hamburg,
 * Germany http://www.tuhh.de/wb
 * 
 * and
 * 
 * Bjoernsen Consulting Engineers (BCE) Maria Trost 3 56070 Koblenz, Germany http://www.bjoernsen.de
 * 
 * This library is free software; you can redistribute it and/or modify it under the terms of the GNU Lesser General
 * Public License as published by the Free Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser General Public License along with this library; if not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * 
 * Contact:
 * 
 * E-Mail: belger@bjoernsen.de schlienger@bjoernsen.de v.doemming@tuhh.de
 * 
 * ---------------------------------------------------------------------------------------------------
 */
package org.kalypso.test.util;

import java.io.File;

import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.services.calculation.job.ICalcMonitor;
import org.kalypso.services.calculation.job.ICalcResultEater;

/**
 * 
 * TODO: insert type comment here
 * 
 * @author doemming
 */
public class CalcJobTestUtilis
{

  public static ICalcResultEater createResultEater()
  {
    return new ICalcResultEater()
    {
      /**
       * @see org.kalypso.services.calculation.job.ICalcResultEater#addResult(java.lang.String, java.io.File)
       */
      public void addResult( String id, File file )
      {
        System.out.println( "ID" + id + " File:" + file.getAbsolutePath() );
      }
    };
  }

  public static ICalcMonitor createMonitor()
  {
    return new ICalcMonitor()
    {
      /**
       * @see org.kalypso.services.calculation.job.ICalcMonitor#cancel()
       */
      public void cancel()
      {
      //  
      }

      /**
       * @see org.kalypso.services.calculation.job.ICalcMonitor#isCanceled()
       */
      public boolean isCanceled()
      {
        return false;
      }

      /**
       * @see org.kalypso.services.calculation.job.ICalcMonitor#setProgress(int)
       */
      public void setProgress( int progress )
      {
      //
      }

      /**
       * @see org.kalypso.services.calculation.job.ICalcMonitor#getProgress()
       */
      public int getProgress()
      {
        return 0;
      }

      /**
       * @see org.kalypso.services.calculation.job.ICalcMonitor#getMessage()
       */
      public String getMessage()
      {
        return null;
      }

      /**
       * @see org.kalypso.services.calculation.job.ICalcMonitor#setMessage(java.lang.String)
       */
      public void setMessage( String message )
      {
        System.out.println( message + "\n" );
      }

      public void setFinishInfo( int status, String text )
      {
      // TODO Auto-generated method stub

      }

      public String getFinishText()
      {
        // TODO Auto-generated method stub
        return null;
      }

      public int getFinishStatus()
      {
        // TODO Auto-generated method stub
        return 0;
      }
    };

  }

  public static File getTmpDir()
  {
    File file = FileUtilities.createNewTempDir( "NA_TEST", new File( "C:\\tmp" ) );
    file.mkdirs();
    return file;
  }

}
