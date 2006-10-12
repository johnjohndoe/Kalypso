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
  public static final String NSRGC = "http://elbe.wb.tu-harburg.de/rectifiedGridCoverage";

  public static final QName TYPENAME = new QName( NSRGC, "RangeSetType" );

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
  public void marshall( Object object, Node node, URL context )

  {
    RangeSet rangeSet = (RangeSet) object;
    Document ownerDocument = node.getOwnerDocument();

    Element e_File = ownerDocument.createElementNS( NSRGC, "rgc:File" );
    Element e_FileName = ownerDocument.createElementNS( NSRGC, "rgc:fileName" );
    File rangeSetDataFile = new File( FileUtilities.nameWithoutExtension( context.getFile() ) + ".dat" );
    if( rangeSet.getRangeSetDataFile() == null )
    {
      String fileName = rangeSetDataFile.getName();
      rangeSet.setRangeSetDataFile( fileName );
    }
    e_FileName.appendChild( ownerDocument.createTextNode( rangeSet.getRangeSetDataFile() ) );
    e_File.appendChild( e_FileName );
    Vector rangeSetData = rangeSet.getRangeSetData();
    try
    {
      setRangeSetData( rangeSetDataFile, rangeSetData );
    }
    catch( Exception e )
    {
      System.out.println( e );
    }

    node.appendChild( e_File );
  }

  /**
   * @see org.kalypsodeegree_impl.extension.IMarshallingTypeHandler#unmarshall(org.w3c.dom.Node, java.net.URL,
   *      org.kalypso.contribs.java.net.IUrlResolver)
   */
  @Override
  public Object unmarshall( Node node, URL gmlURL, IUrlResolver urlResolver )
  {
    // Gernots Remarks on Grids: TODO: do not parse this at all, just use bindings
    // Use a wrapper class to access grid data
    
    // TODO do not give context here, better give resolver
    Node node_File = ((Element) node).getElementsByTagNameNS( NSRGC, "File" ).item( 0 );
    Node node_FileName = ((Element) node_File).getElementsByTagNameNS( NSRGC, "fileName" ).item( 0 );
    String fileName = node_FileName.getFirstChild().getNodeValue();
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
        UrlUtilities urlUtilities = new UrlUtilities();
        rangeSetDataReader = urlUtilities.createReader( rangeSetDataURL );
      }
    }
    catch( IOException ioException )
    {
      System.out.println( ioException );
    }
    // read rangeSetData
    Vector<Vector<Double>> rangeSetData = null;
    try
    {
      rangeSetData = getRangeSetData( rangeSetDataReader );
    }
    catch( Exception e )
    {
      System.out.println( e );
    }
    RangeSet rangeSet = new RangeSet( rangeSetData, fileName );
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
   *          File, where rangeSetData is stored
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
      String[] dataAsString = line.split( " " );
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
   *          File, where rangeSetData is stored
   * @param rangeSetData
   *          Vector, which stores the rangeSet data; the data of each row is stored in a Vector
   * @throws Exception
   */
  public static void setRangeSetData( File rangeSetDataFile, Vector rangeSetData ) throws Exception
  {
    BufferedWriter bw = new BufferedWriter( new FileWriter( rangeSetDataFile ) );
    for( int i = 0; i < rangeSetData.size(); i++ )
    {
      Vector rowData = (Vector) rangeSetData.get( i );
      for( int j = 0; j < rowData.size(); j++ )
      {
        if( rowData.get( j ) != null )
        {
          double value = ((Double) rowData.get( j )).doubleValue();
          double roundValue = round( value, 6, BigDecimal.ROUND_HALF_EVEN );
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

  private String replaceNameInURL( String url, String fileName )
  {
    String[] urlSegments = url.split( "/" );
    urlSegments[urlSegments.length - 1] = fileName;
    StringBuffer buffer = new StringBuffer();
    for( int i = 0; i < urlSegments.length; i++ )
    {
      buffer.append( urlSegments[i] + "/" );
    }
    return buffer.toString();
  }

  private static double round( double d, int scale, int mode )
  {
    BigDecimal bd = new BigDecimal( Double.toString( d ) );
    return (bd.setScale( scale, mode )).doubleValue();
  }

  /**
   * @see org.kalypsodeegree_impl.extension.IMarshallingTypeHandler#cloneObject(java.lang.Object)
   */
  public Object cloneObject( Object objectToClone, final String gmlVersion )
  {
    RangeSet rangeSet = (RangeSet) objectToClone;
    Vector rangeSetData = rangeSet.getRangeSetData();
    String rangeSetDataFile = rangeSet.getRangeSetDataFile();
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