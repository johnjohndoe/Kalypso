package converter;

import java.util.Hashtable;
import java.util.List;
import java.util.TreeMap;
import java.util.Vector;

import org.deegree.model.feature.Feature;
import org.deegree.model.geometry.GM_Object;
import org.deegree.model.geometry.GM_Point;
import org.deegree.model.geometry.GM_Position;
import org.deegree_impl.model.cv.RangeSet;
import org.deegree_impl.model.cv.RectifiedGridCoverage;
import org.deegree_impl.model.cv.RectifiedGridDomain;
import org.deegree_impl.model.geometry.GeometryFactory;

import view.LogView;

/**
 * Class for converting Vectordata (shapeFiles) to Rasterdata
 * (RectifiedGridCoverages)
 * 
 * @author N. Peiler
 *  
 */
public class VectorToGridConverter
{

  public VectorToGridConverter()
  {
    super();
  }

  /**
   * converts a List of Features to a RectifiedGridCoverage
   * 
   * @param featureList
   *          List of Features
   * @param propertyName
   *          Name of property of feature, which should be the value of gridcell
   * @param propertyTable
   *          Mapping of key and propertyValue (e.g. landuseTypeList)
   * @param rg
   *          RectifiedGridCoverage that defines the origin, offset and
   *          gridRange of the new grid
   * @return new RectifiedGridCoverage
   * @throws Exception
   */
  public RectifiedGridCoverage toGrid( List featureList, String propertyName,
      Hashtable propertyTable, RectifiedGridCoverage rg ) throws Exception
  {
    RectifiedGridDomain newGridDomain = new RectifiedGridDomain( rg.getGridDomain()
        .getOrigin( null ), rg.getGridDomain().getOffset(), rg.getGridDomain().getGridRange() );
    GM_Point origin = rg.getGridDomain().getOrigin( null );
    double originX = origin.getX();
    double originY = origin.getY();
    Vector newRangeSetData = new Vector();
    Vector rangeSetData = rg.getRangeSet().getRangeSetData();
    for( int i = 0; i < rangeSetData.size(); i++ )
    {
      Vector rowData = (Vector)rangeSetData.get( i );
      Vector newRowData = new Vector();
      for( int j = 0; j < rowData.size(); j++ )
      {
        double x = originX + j * rg.getGridDomain().getOffsetX( origin.getCoordinateSystem() )
            + 0.5 * rg.getGridDomain().getOffsetX( origin.getCoordinateSystem() );
        double y = originY + ( rangeSetData.size() - i )
            * rg.getGridDomain().getOffsetY( origin.getCoordinateSystem() ) - 0.5
            * rg.getGridDomain().getOffsetY( origin.getCoordinateSystem() );
        GM_Position position = GeometryFactory.createGM_Position( x, y );
        Feature actualFeature = null;
        if( rowData.get( j ) != null )
        {
          Integer key = null;
          for( int k = 0; k < featureList.size(); k++ )
          {
            actualFeature = (Feature)featureList.get( k );
            GM_Object gm_Object = actualFeature.getDefaultGeometryProperty();
            if( gm_Object.contains( position ) )
            {
              String property = actualFeature.getProperty( propertyName ).toString();
              key = (Integer)propertyTable.get( property );
              break;
            }
          }
          /*
           * Enumeration e = landuseTable.keys(); while (e.hasMoreElements()) {
           * key = (Integer)e.nextElement(); if
           * (landuseTable.get(key).equals(landuse)) { break; } }
           */
          if( key != null )
          {
            newRowData.addElement( new Double( key.doubleValue() ) );
          }
          else
          {
            newRowData.addElement( null );
          }
        }
        else
        {
          newRowData.addElement( null );
        }
      }
      newRangeSetData.addElement( newRowData );
      LogView.println( i + 1 + " rows of " + rangeSetData.size() + " calculated" );
    }
    RangeSet newRangeSet = new RangeSet( newRangeSetData, null );
    RectifiedGridCoverage newGrid = new RectifiedGridCoverage( newGridDomain, newRangeSet );
    return newGrid;
  }

  /**
   * converts a landuse ShapeFile to a LanduseGrid
   * 
   * @param landuseFeatureList
   *          List of landuseFeatures
   * @param landuseTypeList
   * @param propertyName
   *          Name of property of feature, which should be the value of gridcell
   * @param waterlevelGrids
   * @return LanduseGrid
   */
  public RectifiedGridCoverage landuseToGrid( List landuseFeatureList, Hashtable landuseTypeList,
      String propertyName, TreeMap waterlevelGrids )
  {
    //FeatureCollection fc = loadFeatureCollection(gmlFile);
    RectifiedGridCoverage waterlevelGrid = (RectifiedGridCoverage)waterlevelGrids
        .get( waterlevelGrids.firstKey() );
    RectifiedGridCoverage landuseGrid = null;
    try
    {
      landuseGrid = toGrid( landuseFeatureList, propertyName, landuseTypeList, waterlevelGrid );
      return landuseGrid;
    }
    catch( Exception e )
    {
      e.printStackTrace();
      return null;
    }
  }

  /**
   * converts a administrationUnit ShapeFile to a AdministrationUnitGrid
   * 
   * @param adminUnitFeatureList
   *          List of administrationUnitFeatures
   * @param administrationUnitTypeList
   * @param propertyName
   *          Name of property of feature, which should be the value of gridcell
   * @param waterlevelGrids
   * @return AdministrationUnitGrid
   */
  public RectifiedGridCoverage administrationUnitToGrid( List adminUnitFeatureList,
      Hashtable administrationUnitTypeList, String propertyName, TreeMap waterlevelGrids )
  {
    //FeatureCollection fc = loadFeatureCollection(gmlFile);
    RectifiedGridCoverage waterlevelGrid = (RectifiedGridCoverage)waterlevelGrids
        .get( waterlevelGrids.firstKey() );
    RectifiedGridCoverage administrationUnitGrid = null;
    try
    {
      administrationUnitGrid = toGrid( adminUnitFeatureList, propertyName,
          administrationUnitTypeList, waterlevelGrid );
      return administrationUnitGrid;
    }
    catch( Exception e )
    {
      e.printStackTrace();
      return null;
    }
  }

  /*
   * public FeatureCollection loadFeatureCollection(File gmlFile) {
   * FeatureCollection fc = null; try { Reader reader = new FileReader(gmlFile);
   * fc = GMLFeatureAdapter.wrap(reader); } catch (Exception e) {
   * System.out.println(e); } return fc; }
   */

}