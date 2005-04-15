package org.kalypso.ogc.sensor.timeseries.wq.wqtable;

import java.io.StringWriter;
import java.util.Calendar;
import java.util.Date;
import java.util.Iterator;
import java.util.List;

import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;

import org.apache.commons.lang.ArrayUtils;
import org.kalypso.binding.wqtable.ObjectFactory;
import org.kalypso.binding.wqtable.WQTable;
import org.kalypso.binding.wqtable.WQTableList;
import org.kalypso.ogc.sensor.timeseries.wq.WQException;
import org.xml.sax.InputSource;

/**
 * WQTableFactory
 * 
 * @author schlienger
 */
public class WQTableFactory
{
  private static ObjectFactory m_objectFactory = new ObjectFactory();

  private WQTableFactory( )
  {
    // not intended to be instanciated
  }

  /**
   * Parses the xml and creates a WQTableSet object.
   * 
   * @param ins
   * @return newly created WQTableSet object
   * @throws WQException
   */
  public static WQTableSet parse( final InputSource ins )
      throws WQException
  {
    try
    {
      final Unmarshaller unm = m_objectFactory.createUnmarshaller();
      final List xmlTables = ((WQTableList) unm.unmarshal( ins )).getTable();
      final org.kalypso.ogc.sensor.timeseries.wq.wqtable.WQTable[] tables = new org.kalypso.ogc.sensor.timeseries.wq.wqtable.WQTable[xmlTables
          .size()];
      int iTable = 0;
      for( final Iterator it = xmlTables.iterator(); it.hasNext(); )
      {
        final WQTable xmlTable = (WQTable) it.next();

        final Date validity = xmlTable.getValidity().getTime();
        final int offset = xmlTable.getOffset();

        final String[] strW = xmlTable.getW().split( "," );
        final String[] strQ = xmlTable.getQ().split( "," );

        if( strW.length != strQ.length )
          throw new WQException(
              "Anzahl von W-Werte und Q-Werte ist nicht gleich" );

        final double[] W = new double[strW.length];
        final double[] Q = new double[strW.length];
        for( int i = 0; i < strW.length; i++ )
        {
          W[i] = Double.parseDouble( strW[i] );
          Q[i] = Double.parseDouble( strQ[i] );
        }

        tables[iTable++] = new org.kalypso.ogc.sensor.timeseries.wq.wqtable.WQTable(
            validity, offset, W, Q );
      }

      return new WQTableSet( tables );
    }
    catch( Exception e ) // generic exception caught for simplicity
    {
      throw new WQException( e );
    }
  }

  /**
   * Creates a XML-String from the given WQTableSet object.
   * 
   * @param wqset
   * @return xml String
   * @throws WQException
   */
  public static String createXMLString( final WQTableSet wqset )
      throws WQException
  {
    try
    {
      final WQTableList xmlTables = m_objectFactory.createTables();

      final org.kalypso.ogc.sensor.timeseries.wq.wqtable.WQTable[] tables = wqset.getTables();
      for( int i = 0; i < tables.length; i++ )
      {
        final WQTable xmlTable = m_objectFactory.createWQTable();
        final Calendar cal = Calendar.getInstance();
        cal.setTime( tables[i].getValidity() );
        xmlTable.setValidity( cal );
        xmlTable.setOffset( tables[i].getOffset() );
        
        final WQPair[] pairs = tables[i].getPairs();
        final double[] W = new double[pairs.length];
        final double[] Q = new double[pairs.length];
        WQPair.convert2doubles( pairs, W, Q );
        xmlTable.setW( ArrayUtils.toString( W ).replaceAll( "\\{", "").replaceAll( "\\}", "" ) );
        xmlTable.setQ( ArrayUtils.toString( Q ).replaceAll( "\\{", "").replaceAll( "\\}", "" ) );
        
        xmlTables.getTable().add( xmlTable );
      }

      final Marshaller marshaller = m_objectFactory.createMarshaller();
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
}
