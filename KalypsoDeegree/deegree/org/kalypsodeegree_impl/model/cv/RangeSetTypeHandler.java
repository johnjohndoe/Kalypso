package org.kalypsodeegree_impl.model.cv;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.math.BigDecimal;
import java.net.URL;
import java.util.Vector;

import javax.xml.namespace.QName;

import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.commons.xml.NS;
import org.kalypso.contribs.java.net.IUrlResolver;
import org.kalypso.contribs.java.net.UrlUtilities;
import org.kalypso.gmlschema.types.AbstractOldFormatMarshallingTypeHandlerAdapter;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;

/**
 * TypeHandler for RangeSet of RectifiedGridCoverages
 * 
 * @author N. Peiler
 */
public class RangeSetTypeHandler extends AbstractOldFormatMarshallingTypeHandlerAdapter
{
  // public static final String NSRGC = "http://www.tuhh.de/floodrisk/rasterData";

  public static final QName TYPENAME = new QName( NS.GML3, "rangeSet" );

  /**
   * @see org.kalypsodeegree_impl.extension.IMarshallingTypeHandler#getClassName()
   */
  public Class getValueClass( )
  {
    return RangeSet.class;
  }

  /**
   * @see org.kalypsodeegree_impl.extension.IMarshallingTypeHandler#getTypeName()
   */
  public QName getTypeName( )
  {
    return TYPENAME;
  }

  /**
   * @see org.kalypsodeegree_impl.extension.IMarshallingTypeHandler#marshall(java.lang.Object, org.w3c.dom.Node,
   *      java.net.URL)
   */
  @Override
  public void marshall( final Object object, final Node node, final URL context )
  {
    final RangeSet rangeSet = (RangeSet) object;
    final Document ownerDocument = node.getOwnerDocument();

    final Element e_rangeSet = ownerDocument.createElementNS( NS.GML3, "rgc:rangeSet" );
    final Element e_FileName = ownerDocument.createElementNS( NS.GML3, "gml:File" );
    final File rangeSetDataFile = new File( FileUtilities.nameWithoutExtension( context.getFile() ) + ".dat" );
    if( rangeSet.getRangeSetDataFile() == null )
    {
      final String fileName = rangeSetDataFile.getAbsolutePath();
      rangeSet.setRangeSetDataFile( fileName );
    }
    e_FileName.appendChild( ownerDocument.createTextNode( rangeSet.getRangeSetDataFile() ) );
    e_rangeSet.appendChild( e_FileName );
    final Vector rangeSetData = rangeSet.getRangeSetData();
    try
    {
      setRangeSetData( rangeSetDataFile, rangeSetData );
    }
    catch( final Exception e )
    {
      System.out.println( e );
    }

    node.appendChild( e_rangeSet );
  }

  /**
   * @see org.kalypsodeegree_impl.extension.IMarshallingTypeHandler#unmarshall(org.w3c.dom.Node, java.net.URL,
   *      org.kalypso.contribs.java.net.IUrlResolver)
   */
  @Override
  public Object unmarshall( final Node node, final URL gmlURL, final IUrlResolver urlResolver )
  {
    // Gernots Remarks on Grids: TODO: do not parse this at all, just use bindings
    // Use a wrapper class to access grid data

    // TODO do not give context here, better give resolver
    final Node node_File = ((Element) node).getElementsByTagNameNS( NS.GML3, "File" ).item( 0 );
    final Node node_FileName = ((Element) node_File).getElementsByTagNameNS( NS.GML3, "fileName" ).item( 0 );
    final String fileName = node_FileName.getFirstChild().getNodeValue();
    URL rangeSetDataURL;
    InputStreamReader rangeSetDataReader = null;
    try
    {
      rangeSetDataURL = new URL( replaceNameInURL( gmlURL.toString(), fileName ) );
      if( urlResolver != null )
      {
        rangeSetDataReader = urlResolver.createReader( rangeSetDataURL );
      }
      else
      {
        final UrlUtilities urlUtilities = new UrlUtilities();
        rangeSetDataReader = urlUtilities.createReader( rangeSetDataURL );
      }
    }
    catch( final IOException ioException )
    {
      System.out.println( ioException );
    }
    // read rangeSetData
    Vector<Vector<Double>> rangeSetData = null;
    RangeSet rangeSet = null;
    try
    {
      rangeSetData = getRangeSetData( rangeSetDataReader );
      rangeSet = new RangeSet( rangeSetData, fileName );
    }
    catch( final Exception e )
    {
      System.out.println( e );
    }
    return rangeSet;
  }

  /**
   * @see org.kalypsodeegree_impl.extension.IMarshallingTypeHandler#getShortname()
   */
  public String getShortname( )
  {
    return "rangeSetType";
  }

  /**
   * reads the rangeSetData from the rangeSetDataFile
   * 
   * @param rangeSetReader
   *            File, where rangeSetData is stored
   * @return Vector, which stores the rangeSet data; the data of each row is stored in a Vector
   */
  public static Vector<Vector<Double>> getRangeSetData( final InputStreamReader rangeSetReader ) throws Exception
  {
    final Vector<Vector<Double>> rangeSetData = new Vector<Vector<Double>>();
    final BufferedReader br = new BufferedReader( rangeSetReader );
    String line = null;
    while( (line = br.readLine()) != null )
    {
      final Vector<Double> rowData = new Vector<Double>();
      final String[] dataAsString = line.split( " " );
      for( int i = 0; i < dataAsString.length; i++ )
      {
        if( !dataAsString[i].equals( "null" ) )
        {
          rowData.addElement( new Double( dataAsString[i] ) );
        }
        else
        {
          rowData.addElement( null );
        }
      }
      // System.out.println(rowData);
      rangeSetData.addElement( rowData );
    }
    return rangeSetData;
  }

  /**
   * writes the rangeSetData to rangeSetDataFile
   * 
   * @param rangeSetDataFile
   *            File, where rangeSetData is stored
   * @param rangeSetData
   *            Vector, which stores the rangeSet data; the data of each row is stored in a Vector
   * @throws Exception
   */
  public static void setRangeSetData( final File rangeSetDataFile, final Vector rangeSetData ) throws Exception
  {
    final BufferedWriter bw = new BufferedWriter( new FileWriter( rangeSetDataFile ) );
    for( int i = 0; i < rangeSetData.size(); i++ )
    {
      final Vector rowData = (Vector) rangeSetData.get( i );
      for( int j = 0; j < rowData.size(); j++ )
      {
        if( rowData.get( j ) != null )
        {
          final double value = ((Double) rowData.get( j )).doubleValue();
          final double roundValue = round( value, 6, BigDecimal.ROUND_HALF_EVEN );
          bw.write( roundValue + " " );
        }
        else
        {
          bw.write( "null" + " " );
        }
      }// for j
      bw.newLine();
    }// for i
    bw.close();
  }

  private String replaceNameInURL( final String url, final String fileName )
  {
    final String[] urlSegments = url.split( "/" );
    urlSegments[urlSegments.length - 1] = fileName;
    final StringBuffer buffer = new StringBuffer();
    for( final String element : urlSegments )
    {
      buffer.append( element + "/" );
    }
    return buffer.toString();
  }

  private static double round( final double d, final int scale, final int mode )
  {
    final BigDecimal bd = new BigDecimal( Double.toString( d ) );
    return (bd.setScale( scale, mode )).doubleValue();
  }

  /**
   * @see org.kalypsodeegree_impl.extension.IMarshallingTypeHandler#cloneObject(java.lang.Object)
   */
  public Object cloneObject( final Object objectToClone, final String gmlVersion )
  {
    final RangeSet rangeSet = (RangeSet) objectToClone;
    final Vector rangeSetData = rangeSet.getRangeSetData();
    final String rangeSetDataFile = rangeSet.getRangeSetDataFile();
    return new RangeSet( (Vector) rangeSetData.clone(), rangeSetDataFile );
  }

  /**
   * @see org.kalypsodeegree_impl.extension.IMarshallingTypeHandler#parseType(java.lang.String)
   */
  public Object parseType( final String text )
  {
    throw new UnsupportedOperationException();
  }

  /**
   * @see org.kalypso.gmlschema.types.ITypeHandler#isGeometry()
   */
  public boolean isGeometry( )
  {
    // TODO check this
    return false;
  }

}