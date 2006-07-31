package org.kalypso.wspwin.core.prf.datablock;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.PrintWriter;
import java.text.NumberFormat;
import java.util.Locale;
import java.util.NoSuchElementException;
import java.util.StringTokenizer;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * @author belger
 */
public class CoordDataBlock extends AbstractDataBlock
{
  private static NumberFormat DF = NumberFormat.getNumberInstance( Locale.US );

  static
  {
    DF.setMaximumIntegerDigits( 8 );
    DF.setMinimumIntegerDigits( 8 );
    DF.setMaximumFractionDigits( 4 );
    DF.setMinimumFractionDigits( 4 );
  }

  private final static Logger m_logger = Logger.getLogger( CoordDataBlock.class.getName() );

  private double[] m_xs = new double[0];

  private double[] m_ys = new double[0];

  public CoordDataBlock( DataBlockHeader dbh )
  {
    super( dbh );
  }

  /**
   * Muss von allen Implementatoren zuerst aufgerufen werden
   * 
   * @see org.kalypso.model.wspm.profileeditor.serializer.datablock.IDataBlock#readFromReader(java.lang.String,
   *      java.io.BufferedReader)
   */
  @Override
  public final void readFromReader( final int count, final BufferedReader reader )
  {
    setCoords( readDoubleBlock( count, reader ), readDoubleBlock( count, reader ) );
  }

  /**
   * @see org.kalypso.model.wspm.profileeditor.serializer.datablock.ICoordDataBlock#getX()
   */
  public double[] getX( )
  {
    return m_xs;
  }

  /**
   * @see org.kalypso.model.wspm.profileeditor.serializer.datablock.ICoordDataBlock#getY()
   */
  public double[] getY( )
  {
    return m_ys;
  }

  private static double[] readDoubleBlock( final int count, final BufferedReader reader )

  {
    final double[] coords = new double[count];

    int counter = 0;

    while( true )
    {
      final StringTokenizer sT;
      try
      {
        sT = new StringTokenizer( reader.readLine() );
      }
      catch( IOException e )
      {
        m_logger.log( Level.SEVERE, "unbekannter Formatfehler. " + e.getMessage() );
        return coords;
      }
      if( count < 1 )
      {
        return coords;
      }
      String dblStr = "";
      final int ci = (sT.countTokens() / 2);
      for( int i = 0; i < ci; i++ )
      {
        try
        {
          sT.nextToken();// Zero vor dem Double überspringen
          dblStr = sT.nextToken();

          coords[counter] = Double.parseDouble( dblStr );
        }
        catch( NoSuchElementException e )
        {
          coords[counter] = 0.0;
          m_logger.log( Level.SEVERE, "Formatfehler: Profilpunkt Nr." + Integer.toString( counter + 1 ) + "(" + dblStr + ") -> " + e.getMessage() );
        }
        counter++;
        if( counter == count )
          return coords;
      }
    }
  }

  /**
   * @see org.kalypso.model.wspm.profileeditor.serializer.datablock.AbstractDataBlock#printToPrinterInternal(java.io.PrintWriter)
   */
  @Override
  public void printToPrinter( final PrintWriter pw )
  {
    m_dataBlockHeader.printToPrinter(pw);
    writeDoubleBlock( m_xs, pw );
    writeDoubleBlock( m_ys, pw );
  }

  /**
   * Schreibt einen Koordinatenblock raus
   * 
   * @param dbls -
   * @param pw -
   */
  private  void writeDoubleBlock( final double[] dbls, final PrintWriter pw )

  {
    for( int i = 0; i < dbls.length; i++ )
    {

      pw.write(formatDouble( dbls[i] ) );

      if( (i + 1) % 8 == 0 & i != dbls.length - 1 )
        pw.println();
    }

    pw.println();
  }

  /**
   * @see org.kalypso.model.wspm.profileeditor.serializer.datablock.IDataBlock#getCoordCount()
   */
  public int getCoordCount( )
  {
    return m_xs.length;
  }

  public final void setCoords( final double[] xs, final double[] ys )
  {
    if( xs == null | ys == null | xs.length != ys.length )
      throw new IllegalArgumentException();

    m_xs = xs;
    m_ys = ys;
  }

  /**
   * @see org.bce.wspm.core.prf.datablock.IDataBlock#getText()
   */
  public String[] getText( )
  {
    return new String[]{m_xs.toString(),m_ys.toString()};
  }
}