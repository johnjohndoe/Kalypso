package org.kalypso.ewawi.shape.writer;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.math.BigDecimal;
import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import org.apache.commons.io.IOUtils;

import au.com.bytecode.opencsv.CSVReader;

/**
 * @author Holger Albert
 */
public class FotoListReader
{
  private final DateFormat m_df;

  private List<FotoListData> m_fotoListData;

  public FotoListReader( )
  {
    m_df = new SimpleDateFormat( "dd.MM.yyyy" );
    m_fotoListData = null;
  }

  public void read( final File file ) throws IOException
  {
    CSVReader reader = null;

    try
    {
      final List<FotoListData> fotoListData = new ArrayList<>();

      final FileReader fileReader = new FileReader( file );
      final BufferedReader bufferedReader = new BufferedReader( fileReader );
      reader = new CSVReader( bufferedReader, ',' );

      String[] tokens = null;
      while( (tokens = reader.readNext()) != null )
      {
        final String filename = tokens[0];
        final BigDecimal rechtswert = asDecimal( tokens[1] );
        final BigDecimal hochwert = asDecimal( tokens[2] );
        final BigDecimal hoehe = asDecimal( tokens[3] );
        final Date datum = asDate( tokens[4] );

        fotoListData.add( new FotoListData( filename, rechtswert, hochwert, hoehe, datum ) );
      }

      m_fotoListData = fotoListData;
    }
    catch( final IOException ex )
    {
      m_fotoListData = null;
      throw ex;
    }
    catch( final ParseException ex )
    {
      m_fotoListData = null;
      throw new IOException( "Konnte Datum nicht parsen...", ex );
    }
    finally
    {
      IOUtils.closeQuietly( reader );
    }
  }

  public FotoListData[] getFotoListData( )
  {
    if( m_fotoListData == null )
      return new FotoListData[] {};

    return m_fotoListData.toArray( new FotoListData[] {} );
  }

  private BigDecimal asDecimal( final String text )
  {
    return new BigDecimal( text );
  }

  private Date asDate( final String text ) throws ParseException
  {
    return m_df.parse( text );
  }
}