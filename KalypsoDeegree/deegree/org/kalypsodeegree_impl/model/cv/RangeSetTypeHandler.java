package org.deegree_impl.model.cv;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.math.BigDecimal;
import java.util.Vector;

import org.deegree_impl.extension.ITypeHandler;
import org.deegree_impl.extension.TypeRegistryException;
import org.deegree_impl.gml.schema.XMLHelper;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;

/**
 * TypeHandler for RangeSet of RectifiedGridCoverages
 * 
 * @author N. Peiler
 *  
 */
public class RangeSetTypeHandler implements ITypeHandler
{
  public static final String NSRGC = "http://elbe.wb.tu-harburg.de/rectifiedGridCoverage";
  public static final String TYPENAME= NSRGC + ":" + "RangeSetType";
  /**
   * @see org.deegree_impl.extension.ITypeHandler#getClassName()
   */
  public String getClassName()
  {
    return RangeSet.class.getName();
  }

  /**
   * @see org.deegree_impl.extension.ITypeHandler#getTypeName()
   */
  public String getTypeName()
  {
    return TYPENAME;
  }

  /**
   * 
   * @see org.deegree_impl.extension.ITypeHandler#marshall(java.lang.Object,
   *      org.w3c.dom.Node)
   */
  public void marshall( Object object, Node node ) throws TypeRegistryException
  {
    RangeSet rangeSet = (RangeSet)object;
    Document ownerDocument = node.getOwnerDocument();

    Element e_File = ownerDocument.createElementNS( NSRGC, "rgc:File" );
    Element e_FileName = ownerDocument.createElementNS( NSRGC, "rgc:fileName" );
    String fileName = ( rangeSet.getRangeSetDataFile() ).getAbsolutePath();
    e_FileName.appendChild( ownerDocument.createTextNode( fileName ) );
    e_File.appendChild( e_FileName );
    Vector rangeSetData = rangeSet.getRangeSetData();
    try
    {
      setRangeSetData( rangeSet.getRangeSetDataFile(), rangeSetData );
    }
    catch( Exception e )
    {
      System.out.println( e );
    }

    node.appendChild( e_File );
  }

  /**
   * @see org.deegree_impl.extension.ITypeHandler#unmarshall(org.w3c.dom.Node)
   */
  public Object unmarshall( Node node ) throws TypeRegistryException
  {
    Node node_File = ( (Element)node ).getElementsByTagNameNS( NSRGC, "File" ).item( 0 );
    Node node_FileName = ( (Element)node_File ).getElementsByTagNameNS( NSRGC, "fileName" )
        .item( 0 );
    String fileName = node_FileName.getFirstChild().getNodeValue();
    File rangeSetDataFile = new File( fileName );
    Vector rangeSetData = null;
    try
    {
      rangeSetData = getRangeSetData( rangeSetDataFile );
    }
    catch( Exception e )
    {
      System.out.println( e );
    }
    RangeSet rangeSet = new RangeSet( rangeSetData, rangeSetDataFile );
    return rangeSet;
  }

  /**
   * @see org.deegree_impl.extension.ITypeHandler#getShortname()
   */
  public String getShortname()
  {
    return "rangeSetType";
  }

  /**
   * reads the rangeSetData from the rangeSetDataFile
   * 
   * @param rangeSetDataFile
   *          File, where rangeSetData is stored
   * @return Vector, which stores the rangeSet data; the data of each row is
   *         stored in a Vector
   */
  public static Vector getRangeSetData( File rangeSetDataFile ) throws Exception
  {
    Vector rangeSetData = new Vector();
    BufferedReader br = new BufferedReader( new FileReader( rangeSetDataFile ) );
    String line = null;
    while( ( line = br.readLine() ) != null )
    {
      Vector rowData = new Vector();
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
      //System.out.println(rowData);
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
   *          Vector, which stores the rangeSet data; the data of each row is
   *          stored in a Vector
   * @throws Exception
   */
  public static void setRangeSetData( File rangeSetDataFile, Vector rangeSetData ) throws Exception
  {
    BufferedWriter bw = new BufferedWriter( new FileWriter( rangeSetDataFile ) );
    for( int i = 0; i < rangeSetData.size(); i++ )
    {
      Vector rowData = (Vector)rangeSetData.get( i );
      for( int j = 0; j < rowData.size(); j++ )
      {
        if( rowData.get( j ) != null )
        {
          double value = ( (Double)rowData.get( j ) ).doubleValue();
          double roundValue = round( value, 6, BigDecimal.ROUND_HALF_EVEN );
          bw.write( roundValue + " " );
        }
        else
        {
          bw.write( "null" + " " );
        }
      }//for j
      bw.newLine();
    }//for i
    bw.close();
  }

  private static double round( double d, int scale, int mode )
  {
    BigDecimal bd = new BigDecimal( Double.toString( d ) );
    return ( bd.setScale( scale, mode ) ).doubleValue();
  }
}