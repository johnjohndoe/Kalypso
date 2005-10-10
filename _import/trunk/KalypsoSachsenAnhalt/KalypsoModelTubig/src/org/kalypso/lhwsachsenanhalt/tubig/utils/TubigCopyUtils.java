/*
 * ---------------- FILE HEADER KALYPSO ------------------------------------------
 * 
 * This file is part of kalypso. Copyright (C) 2004 by:
 * 
 * Technical University Hamburg-Harburg (TUHH) Institute of River and coastal engineering Denickestraße 22 21073
 * Hamburg, Germany http://www.tuhh.de/wb
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
 * E-Mail: g.belger@bjoernsen.de m.schlienger@bjoernsen.de v.doemming@tuhh.de
 * 
 * -------------------------------------------------------------------------
 */
package org.kalypso.lhwsachsenanhalt.tubig.utils;

import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.StringTokenizer;

import org.kalypso.lhwsachsenanhalt.tubig.TubigConst;
import org.kalypso.lhwsachsenanhalt.tubig.exceptions.TubigBatchException;
import org.kalypso.services.calculation.job.ICalcMonitor;

/**
 * <p>
 * This class provides static utility method for buffered copying between source <code>Reader</code> and destination
 * <code>Writer</code>.
 * </p>
 * 
 * <p>
 * The <code>copy</code> methods use an internal buffer when copying. It is therefore advisable <em>not</em> to
 * deliberately wrap the stream arguments to the <code>copy</code> methods in <code>Buffered*</code> streams. For
 * example, don't do the following:
 * </p>
 * 
 * <code>copy( new BufferedInputStream( in ), new BufferedOutputStream( out ) );</code>
 * 
 * @author Thül
 */
public class TubigCopyUtils
{

  public TubigCopyUtils()
  {
  // wird nicht instantiiert.
  }

  // ----------------------------------------------------------------
  // Reader -> Writer
  // ----------------------------------------------------------------

  /**
   * Copy chars from a <code>Reader</code> to one of two <code>Writer</code> s <br>
   * depending on the beginning.
   * 
   * @param input
   *          the <code>Reader</code> to read from
   * @param pwLog
   *          the Log- <code>Writer</code> to write to (equals StdOut)
   * @param pwErr
   *          the Error- <code>Writer</code> to write to (equals StdErr)
   * @return true if ENDE token fond in input, else false
   * @throws TubigBatchException
   */
  public static boolean copyAndAnalyzeStreams( final StringWriter input, final PrintWriter pwLog,
      final PrintWriter pwErr, final ICalcMonitor monitor ) throws TubigBatchException
  {
    boolean bExeEnde = false;
    String sMess = "";
    String sLastWrtr = "";
    String delim = System.getProperty( "line.separator" );
    StringTokenizer strTok;

    strTok = new StringTokenizer( input.toString(), delim );

    while( strTok.hasMoreTokens() )
    {
      sMess = strTok.nextToken();
      sLastWrtr = "pwLog"; // prinzipielle Initialisierung
      if( sMess.startsWith( TubigConst.STDOUT ) )
      {
        sMess = sMess.replaceAll( TubigConst.STDOUT, "" );
        pwLog.println( sMess );
        sLastWrtr = "pwLog";
      }
      else if( sMess.startsWith( TubigConst.ENDE ) )
      {
        // gerade ausgeführtes m_xy.exe wurde normal beendet
        sMess = sMess.substring( TubigConst.ENDE.length() );
        pwLog.println( sMess + " beendet" );
        bExeEnde = true;
        sLastWrtr = "pwLog";
      }
      else if( sMess.startsWith( TubigConst.STDERR ) )
      {
        // Fehler in m_xy: Abbruch der Rechnung
        sMess = sMess.replaceAll( TubigConst.STDERR, "" );
        pwErr.println( sMess );
        sLastWrtr = "pwErr";
      }
      else
      {
        // ggf. Rest der vorherigen Message...
        if( "pwLog".equals( sLastWrtr ) )
        {
          pwLog.println( sMess );
        }
        else
        {
          pwErr.println( sMess );
        }
      }
    }
    if( "pwErr".equals( sLastWrtr ) )
    {
      throw new TubigBatchException( monitor, TubigBatchException.STATUS_ERROR, TubigConst.FINISH_ERROR_TEXT );
    }
    return bExeEnde;
  }
} // TubigCopyUtils
