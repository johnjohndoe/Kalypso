package org.kalypso.ogc.sensor.timeseries.wq.wqtable;

import java.io.InputStream;
import java.io.OutputStream;
import java.io.StringWriter;
import java.lang.reflect.InvocationTargetException;
import java.util.Calendar;
import java.util.Date;
import java.util.Iterator;
import java.util.List;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBElement;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;

import org.apache.commons.lang.ArrayUtils;
import org.kalypso.binding.ratingtable.ObjectFactory;
import org.kalypso.binding.ratingtable.RatingTable;
import org.kalypso.binding.ratingtable.RatingTableList;
import org.kalypso.commons.serializer.ISerializer;
import org.kalypso.jwsdp.JaxbUtilities;
import org.kalypso.ogc.sensor.timeseries.wq.WQException;
import org.xml.sax.InputSource;

/**
 * WQTableFactory
 * 
 * @author schlienger
 */
public class WQTableFactory implements ISerializer
{
  private final static ObjectFactory OF = new ObjectFactory();

  private final static JAXBContext JC = JaxbUtilities.createQuiet( ObjectFactory.class );

  private WQTableFactory( )
  {
    // not intended to be instanciated
  }

  public static WQTableFactory getInstance( )
  {
    return new WQTableFactory();
  }

  /**
   * Parses the xml and return the binding object
   */
  public static RatingTableList parseSimple( final InputSource ins ) throws WQException
  {
    try
    {
      final Unmarshaller unm = JC.createUnmarshaller();
      final JAXBElement<RatingTableList> element= (JAXBElement<RatingTableList>) unm.unmarshal( ins );
      final RatingTableList xmlTableList = element.getValue();

      return xmlTableList;
    }
    catch( final Exception e )
    {
      throw new WQException( e );
    }
  }

  /**
   * Parses the xml and creates a WQTableSet object.
   * 
   * @return newly created WQTableSet object
   */
  public static WQTableSet parse( final InputSource ins ) throws WQException
  {
    try
    {
      final RatingTableList xmlTableList = parseSimple( ins );
      final List xmlTables = xmlTableList.getTable();
      final WQTable[] tables = new WQTable[xmlTables.size()];
      int iTable = 0;
      for( final Iterator it = xmlTables.iterator(); it.hasNext(); )
      {
        final RatingTable xmlTable = (RatingTable) it.next();

        final Date validity = xmlTable.getValidity().getTime();
        final int offset = xmlTable.getOffset();

        final String[] strX = xmlTable.getX().split( "," );
        final String[] strY = xmlTable.getY().split( "," );

        if( strX.length != strY.length )
          throw new WQException( "Anzahl von W-Werte und Q-Werte ist nicht gleich" );

        final double[] W = new double[strX.length];
        final double[] Q = new double[strX.length];
        for( int i = 0; i < strX.length; i++ )
        {
          W[i] = Double.parseDouble( strX[i] );
          Q[i] = Double.parseDouble( strY[i] );
        }

        tables[iTable++] = new WQTable( validity, offset, W, Q );
      }

      return new WQTableSet( tables, xmlTableList.getFromType(), xmlTableList.getToType() );
    }
    catch( Exception e ) // generic exception caught for simplicity
    {
      throw new WQException( e );
    }
  }

  /**
   * Creates a XML-String from the given WQTableSet object.
   * 
   * @return xml String
   */
  public static String createXMLString( final WQTableSet wqset ) throws WQException
  {
    try
    {
      final RatingTableList xmlTables = OF.createRatingTableList();
      xmlTables.setFromType( wqset.getFromType() );
      xmlTables.setToType( wqset.getToType() );

      final WQTable[] tables = wqset.getTables();
      for( int i = 0; i < tables.length; i++ )
      {
        final RatingTable xmlTable = OF.createRatingTable();
        final Calendar cal = Calendar.getInstance();
        cal.setTime( tables[i].getValidity() );
        xmlTable.setValidity( cal );
        xmlTable.setOffset( tables[i].getOffset() );

        final WQPair[] pairs = tables[i].getPairs();
        final double[] W = new double[pairs.length];
        final double[] Q = new double[pairs.length];
        WQPair.convert2doubles( pairs, W, Q );
        xmlTable.setX( ArrayUtils.toString( W ).replaceAll( "\\{", "" ).replaceAll( "\\}", "" ) );
        xmlTable.setY( ArrayUtils.toString( Q ).replaceAll( "\\{", "" ).replaceAll( "\\}", "" ) );

        xmlTables.getTable().add( xmlTable );
      }

      final Marshaller marshaller = JC.createMarshaller();
      marshaller.setProperty( Marshaller.JAXB_FORMATTED_OUTPUT, Boolean.TRUE );

      final StringWriter writer = new StringWriter();
      marshaller.marshal( xmlTables, writer );

      return writer.toString();
    }
    catch( final JAXBException e )
    {
      throw new WQException( e );
    }
  }

  /**
   * Reads a WQTableSet from an InputStream
   * 
   * @see org.kalypso.commons.serializer.ISerializer#read(java.io.InputStream)
   */
  public Object read( final InputStream ins ) throws InvocationTargetException
  {
    try
    {
      return parse( new InputSource( ins ) );
    }
    catch( final WQException e )
    {
      e.printStackTrace();
      throw new InvocationTargetException( e );
    }
  }

  /**
   * @see org.kalypso.commons.serializer.ISerializer#write(java.lang.Object, java.io.OutputStream)
   */
  public void write( final Object object, final OutputStream os ) throws InvocationTargetException
  {
    try
    {
      final String xml = createXMLString( (WQTableSet) object );
      os.write( xml.getBytes() );
    }
    catch( final Exception e ) // WQException, IOException
    {
      e.printStackTrace();
      throw new InvocationTargetException( e );
    }
  }
}
