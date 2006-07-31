package org.kalypso.wspwin.core.prf;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.kalypso.wspwin.core.prf.datablock.DataBlockHeader;
import org.kalypso.wspwin.core.prf.datablock.IDataBlock;

public class PrfWriter
{

  private final Logger m_logger = Logger.getLogger( PrfWriter.class.getName() );

  private ArrayList<IDataBlock> m_dbs = new ArrayList<IDataBlock>();

  private Map<Integer, String[]> m_metaMap = new HashMap<Integer, String[]>();

  public void addKeyValue( final int lineNr, final String[] data )
  {
    m_metaMap.put( lineNr, data );
  }

  public void addDataBlock( final IDataBlock dataBlock )
  {
    m_dbs.add( dataBlock );
  }

  public void store( final PrintWriter pw )
  {
    writeMetadata( pw );
    writeZeile14( m_dbs, pw );
    pw.println( "0.0000  0.0000  0.0000  0.0000  0.0000  0.0000   0 0 0" );// Plotvorgaben
    try
    {
      for( IDataBlock db : m_dbs )
        db.printToPrinter( pw );
    }
    catch( IOException e )
    {
      m_logger.log( Level.SEVERE, e.getMessage() );
      e.printStackTrace();
    }
    finally
    {
      pw.close();
    }
  }

  /**
   * Schreibt die Anzahlen der Datenblöcke als Zeile 14 raus
   */
  private void writeZeile14( final List< ? extends IDataBlock> dbs, final PrintWriter pw )
  {
    pw.print( dbs.size() );
    for( final Iterator< ? extends IDataBlock> dbsIt = dbs.iterator(); dbsIt.hasNext(); )
    {
      pw.print( ' ' );
      final int i = dbsIt.next().getCoordCount();
      pw.print( i );
    }
    pw.println();
  }

  public static final DataBlockHeader createHeader( final String key )

  {
    final DataBlockHeader dbh = new DataBlockHeader();
    if( key.startsWith( "GEL" ) )
    {
      dbh.setFirstLine( "GELAENDE-" );
      dbh.setSecondLine( "HOEHE" );
    }
    if( key.startsWith( "TRENNF" ) )
    {
      dbh.setFirstLine( "TRENNFLAECHEN" );
    }
    if( key.startsWith( "DUR" ) )
    {
      dbh.setFirstLine( "DURCHSTROEMTE" );
      dbh.setSecondLine( "BEREICHE" );
    }
    if( key.startsWith( "RAU" ) )
    {
      dbh.setFirstLine( "RAUHEIT" );
    }
    if( key.startsWith( "REC" ) )
    {
      dbh.setFirstLine( "RECHTSWERT" );
    }
    if( key.startsWith( "HOC" ) )
    {
      dbh.setFirstLine( "HOCHWERT" );
    }
    if( key.startsWith( "UK-B" ) )
    {
      dbh.setFirstLine( "UK-BRUECKE" );
    }
    if( key.startsWith( "OK-B" ) )
    {
      dbh.setFirstLine( "OK-BRUECKE" );
    }
    if( key.startsWith( "KOM" ) )
    {
      dbh.setFirstLine( "KOMMENTAR:" );
    }
    if( key.startsWith( "BOR" ) )
    {
      dbh.setFirstLine( "BORDVOLL" );
    }
    if( key.startsWith( "AX" ) )
    {
      dbh.setFirstLine( "AX   m" );
    }
    if( key.startsWith( "AY" ) )
    {
      dbh.setFirstLine( "AY   m" );
    }
    if( key.startsWith( "DP" ) )
    {
      dbh.setFirstLine( "DP   m" );
    }
    if( key.startsWith( "EI" ) )
    {
      dbh.setFirstLine( "EI" );
    }
    if( key.startsWith( "KRE" ) )
    {
      dbh.setFirstLine( "KREIS" );
    }
    if( key.startsWith( "TRA" ) )
    {
      dbh.setFirstLine( "TRAPEZ" );
    }
    if( key.startsWith( "MAU" ) )
    {
      dbh.setFirstLine( "MAULPROFIL" );
    }
    if( key.startsWith( "OK-W" ) )
    {
      dbh.setFirstLine( "OK-WEHR" );
    }
    if( key.startsWith( "TRENNL" ) )
    {
      dbh.setFirstLine( "TRENNLINIE" );
      dbh.setSecondLine( "WEHR" );
    }

    return dbh;
  }

  
  private void writeMetadata( final PrintWriter pw )
  {
    for( int i = 1; i < 14; i++ )
    {
      final String[] line = m_metaMap.get( i );
      final StringBuffer buffer = new StringBuffer( "#" );
      buffer.append( line[0] );

      if( buffer.length() > 41 )
        buffer.setLength( 41 );
      else
      {
        final int l = 41 - buffer.length();
        final char[] space = new char[l];
        Arrays.fill( space, ' ' );

        buffer.append( space );
      }
      buffer.append( line[1] );
      pw.println( buffer.toString().trim().substring( 1 ) );
    }
  }
}