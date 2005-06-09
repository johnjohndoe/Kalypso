package KalypsoPluginRasterImport;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.math.BigDecimal;
import java.util.Vector;

import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.model.coverage.GridRange;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureAssociationTypeProperty;
import org.kalypsodeegree.model.feature.FeatureType;
import org.kalypsodeegree.model.feature.FeatureTypeProperty;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.gml.schema.GMLSchema;
import org.kalypsodeegree_impl.gml.schema.GMLSchemaCatalog;
import org.kalypsodeegree_impl.model.cv.GridRange_Impl;
import org.kalypsodeegree_impl.model.cv.RangeSet;
import org.kalypsodeegree_impl.model.cv.RectifiedGridCoverage;
import org.kalypsodeegree_impl.model.cv.RectifiedGridCoverageFactory;
import org.kalypsodeegree_impl.model.cv.RectifiedGridDomain;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;
import org.kalypsodeegree_impl.model.feature.GMLWorkspace_Impl;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * 
 * Class for reading and writing ascii-Grid files
 * 
 * @author N. Peiler
 * 
 *  
 */
public abstract class GridUtils
{

  /**
   * exports a RectifiedGridCoverage to an Ascii-Grid
   * 
   * @param out
   *          ascii output file
   * @param grid
   *          RectifiedGridCoverage to export
   * @throws Exception
   */
  public static void exportGridArc( File out, RectifiedGridCoverage grid ) throws Exception
  {
    BufferedWriter bw = new BufferedWriter( new FileWriter( out ) );
    RectifiedGridDomain gridDomain = grid.getGridDomain();
    RangeSet rangeSet = grid.getRangeSet();
    int nCols = gridDomain.getNumColumns();
    int nRows = gridDomain.getNumRows();
    GM_Point origin = gridDomain.getOrigin( null );
    double originX = origin.getX();
    double originY = origin.getY();
    double offsetX = gridDomain.getOffsetX( origin.getCoordinateSystem() );
    int nodata = -9999;
    Vector rangeSetData = rangeSet.getRangeSetData();
    try
    {
      bw.write( "ncols " + nCols + "\nnrows " + nRows + "\nxllcorner " + originX + "\nyllcorner "
          + originY + "\ncellsize " + offsetX + "\nnodata_value " + nodata );
      bw.newLine();

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
            bw.write( nodata + " " );
          }
          /*
           * System.out.println(i + "," + j + ": " + rowData.get(j) + " ");
           */
        }//for j
        bw.newLine();
      }//for i
      bw.close();
    }// try
    catch( IOException e )
    {
      bw.write( "Error while writing ascii-arc file: " + e.getMessage() );
      throw e;
    }// catch
    finally
    {
      bw.close();
      if( out.length() == 0 )
        out.delete();
    }// finally
  }//exportGridArc

  /**
   * imports an ascii-grid file
   * 
   * @param in
   *          input file
   * @return RectifiedGridCoverage
   */
  public static RectifiedGridCoverage importGridArc( File in, CS_CoordinateSystem cs )
  {
    int nCols = 0;
    int nRows = 0;
    GM_Point origin = null;
    double[] offset = new double[2];
    String nodata = null;
    Vector rangeSetData = new Vector();
    //StringBuffer rangeData = new StringBuffer();
    Vector rangeData = new Vector();
    try
    {
      BufferedReader br = new BufferedReader( new FileReader( in ) );
      String[] data = new String[6];
      String line;
      for( int i = 0; i < data.length; i++ )
      {
        line = br.readLine();
        int index = line.indexOf( " " );
        String subString = line.substring( index );
        data[i] = subString.trim();
        System.out.println( data[i] );
      }
      nCols = new Integer( data[0] ).intValue();
      nRows = new Integer( data[1] ).intValue();
      double originX = new Double( data[2] ).doubleValue();
      double originY = new Double( data[3] ).doubleValue();
      //double originZ = 0;
      origin = GeometryFactory.createGM_Point( originX, originY, cs );
      offset[0] = ( new Double( data[4] ) ).doubleValue();
      offset[1] = ( new Double( data[4] ) ).doubleValue();
      //offset[2] = 0;
      nodata = data[5];
      while( ( line = br.readLine() ) != null )
      {
        //rangeData.append(line);
        String[] dataAsString = line.split( " " );
        //System.out.println("...");
        for( int i = 0; i < dataAsString.length; i++ )
        {
          rangeData.addElement( dataAsString[i] );
        }
      }
    }
    catch( Exception e )
    {
      System.out.println( e );
    }
    double[] low =
    {
        0.0,
        0.0 };
    double[] high =
    {
        nCols,
        nRows };
    GridRange gridRange = new GridRange_Impl( low, high );
    RectifiedGridDomain gridDomain = new RectifiedGridDomain( origin, offset, gridRange );
    //String[] dataAsString = rangeData.toString().split(" ");
    for( int i = 0; i < nRows; i++ )
    {
      Vector rowData = new Vector();
      for( int n = 0; n < nCols; n++ )
      {
        if( rangeData.get( n + ( i * nCols ) ).equals( nodata ) )
        {
          rowData.addElement( null );
        }
        else
        {
          double actualValue = Double.parseDouble( (String)rangeData.get( n + ( i * nCols ) ) );
          rowData.addElement( new Double( actualValue ) );
        }
      }
      //System.out.println(rowData);
      //System.out.println(i+" of "+nRows+"calculated");
      rangeSetData.addElement( rowData );
    }
    RangeSet rangeSet = new RangeSet( rangeSetData, null );
    RectifiedGridCoverage grid = new RectifiedGridCoverage( gridDomain, rangeSet );
    return grid;
  }

  private static double round( double d, int scale, int mode )
  {
    BigDecimal bd = new BigDecimal( Double.toString( d ) );
    return ( bd.setScale( scale, mode ) ).doubleValue();
  }

  public static RectifiedGridCoverage readRasterData( File rasterDataModelGML ) throws Exception
  {
    GMLWorkspace gmlWorkspace = GmlSerializer.createGMLWorkspace( rasterDataModelGML.toURL() );
    Feature rootFeature = gmlWorkspace.getRootFeature();
    return RectifiedGridCoverageFactory.createRectifiedGridCoverage( rootFeature );
  }

  public static void writeRasterData( File rasterDataModelGML, RectifiedGridCoverage grid )
      throws Exception
  {

    String rasterDataSchemaNS = "http://elbe.wb.tu-harburg.de/rasterData";

    // load schema
    final GMLSchema schema = GMLSchemaCatalog.getSchema( rasterDataSchemaNS );

    // create feature and workspace gml
    final FeatureType[] types = schema.getFeatureTypes();

    // create rootFeature: RasterDataModel
    Feature rootFeature = FeatureFactory.createFeature( "RasterDataModel0", types[0] );
    FeatureTypeProperty[] ftps = types[0].getProperties();
    // create feature: RectifiedGridCoverage
    Object[] properties = new Object[]
    {
        "",
        "",
        null,
        grid.getGridDomain(),
        grid.getRangeSet() };
    Feature rectifiedGridCoverageFeature = FeatureFactory.createFeature( "RectifiedGridCoverage0",
        ( (FeatureAssociationTypeProperty)ftps[3] ).getAssociationFeatureType(), properties );
    rootFeature.addProperty( FeatureFactory.createFeatureProperty( ftps[3].getName(),
        rectifiedGridCoverageFeature ) );

    //create workspace
    GMLWorkspace workspace = new GMLWorkspace_Impl( types, rootFeature, rasterDataModelGML.toURL(),
        "", schema.getTargetNS(), schema.getNamespaceMap() );

    // serialize Workspace
    FileWriter fw = new FileWriter( rasterDataModelGML );
    GmlSerializer.serializeWorkspace( fw, workspace );
    fw.close();

  }

}