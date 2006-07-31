package org.kalypso.wspwin.core.prf.datablock;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.PrintWriter;

/**
 * @author belger
 */
public interface IDataBlock
{
  /**
   * Liest den Inhalt des Datenblock aus einem Reader, evtl. vorhandene Daten werden gelöscht
   */
  public void readFromReader( final int count, final BufferedReader reader ) throws IOException;

  public String getFirstLine( );

  public String getSecondLine( );

  public String getThirdLine( );

  public void setFirstLine( final String text );

  public void setSecondLine( final String text );

  public void setThirdLine( final String text );

  public double[] getX( );

  public double[] getY( );

  public String[] getText( );

  public DataBlockHeader getDataBlockHeader( );

  /**
   * Anzahl der Koordinaten für Zeile 14
   */
  public int getCoordCount( );

  /**
   * Schreibt Datenblock in einen Writer
   */
  public void printToPrinter( final PrintWriter pw ) throws IOException;
  
}