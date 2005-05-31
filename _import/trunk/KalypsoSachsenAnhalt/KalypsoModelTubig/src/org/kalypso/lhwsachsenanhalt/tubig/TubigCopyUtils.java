package org.kalypso.lhwsachsenanhalt.tubig;

import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;

import org.kalypso.services.calculation.job.ICalcMonitor;

/**
 * <p>
 * This class provides static utility method for buffered copying between source
 * <code>Reader</code> and destination <code>Writer</code>.
 * </p>
 * 
 * <p>
 * The <code>copy</code> methods use an internal buffer when copying. It is
 * therefore advisable <em>not</em> to deliberately wrap the stream arguments
 * to the <code>copy</code> methods in <code>Buffered*</code> streams. For
 * example, don't do the following:
 * </p>
 * 
 * <code>copy( new BufferedInputStream( in ), new BufferedOutputStream( out ) );</code>
 * 
 * @author Thül
 */
public class TubigCopyUtils
{

  /**
   * The name says it all.
   */
  private static final int DEFAULT_BUFFER_SIZE = 1024 * 4;

  public TubigCopyUtils()
  {
  // wird nicht instantiiert.
  }

  // ----------------------------------------------------------------
  // Reader -> Writer
  // ----------------------------------------------------------------

  /**
   * Copy chars from a <code>Reader</code> to a <code>Writer</code>.
   * 
   * @param input
   *          the <code>Reader</code> to read from
   * @param pwLog
   *          the <code>Writer</code> to write to
   * @param pwErr
   * @return the number of characters copied
   * @throws IOException
   *           In case of an I/O problem
   * @throws TubigBatchException
   */
  public static boolean copyAndAnalyzeStreams( final StringWriter input, final PrintWriter pwLog,
      final PrintWriter pwErr, final ICalcMonitor monitor ) throws IOException, TubigBatchException
  {
    char[] buffer = new char[DEFAULT_BUFFER_SIZE];
    int count = 0;
    int n = 0;
    boolean bExeEnde = false;
    String sMess = "";
    String sLastWrtr = "";

//    // TODO Monika jede Zeile einzeln lesen
//    while( -1 != ( n = input.read( buffer ) ) )
//    {
//      sMess = sMess + new String( buffer, 0, n );
//      count += n;
//    }
//    if( count > 0 )
//    {
//      if( sMess.startsWith( TubigConst.STDOUT ) )
//      {
//        sMess = sMess.replaceAll( TubigConst.STDOUT, "" );
//        pwLog.println( sMess );
//        sLastWrtr = "pwLog";
//      }
//      else if( sMess.startsWith( TubigConst.ENDE ) )
//      {
//        // gerade ausgeführtes m_xy.exe wurde normal beendet
//        pwLog.println( sMess );
//        bExeEnde = true;
//        sLastWrtr = "pwLog";
//      }
//      else if( sMess.startsWith( TubigConst.STDERR ) )
//      {
//        // Fehler in m_xy: Abbruch der Rechnung
//        sMess = sMess.replaceAll( TubigConst.STDERR, "" );
//        pwErr.println( sMess );
//        sLastWrtr = "pwErr";
//        throw new TubigBatchException( monitor, TubigBatchException.STATUS_ERROR,
//            TubigConst.FINISH_ERROR_TEXT );
//      }
//      else
//      {
//        // ggf. Rest der vorherigen Message... (aber der Buffer ist
//        // wahrscheinlich mehr als ausreichend)
//        if( "pwLog".equals( sLastWrtr ) )
//        {
//          pwLog.println( sMess );
//        }
//        else
//        {
//          pwErr.println( sMess );
//        }
//      }
//    }
    return bExeEnde;
  }
} // TubigCopyUtils
