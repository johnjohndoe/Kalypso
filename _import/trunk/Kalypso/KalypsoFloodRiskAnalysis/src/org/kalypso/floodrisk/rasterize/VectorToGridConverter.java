package org.kalypso.floodrisk.rasterize;

import java.util.Hashtable;
import java.util.List;
import java.util.Vector;

import org.kalypso.services.calculation.job.ICalcMonitor;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.model.cv.RangeSet;
import org.kalypsodeegree_impl.model.cv.RectifiedGridCoverage;
import org.kalypsodeegree_impl.model.cv.RectifiedGridDomain;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

/**
 * Class for converting Vectordata (shapeFiles) to Rasterdata
 * (RectifiedGridCoverages)
 * 
 * @author N. Peiler
 *  
 */
public class VectorToGridConverter
{

  /**
   * converts a List of Features to a RectifiedGridCoverage
   * 
   * @param featureList List of Features with properties "GEOM" and
   *          "RasterProperty"
   * @param propertyName Name of property of feature, which should be the value
   *          of gridcell
   * @param propertyTable Mapping of key and propertyValue (e.g.
   *          landuseTypeList)
   * @param baseGrid RectifiedGridCoverage that defines the origin, offset and
   *          gridRange of the new grid
   * @return new RectifiedGridCoverage
   * @throws Exception
   */
  public static RectifiedGridCoverage toGrid( List featureList,
      Hashtable propertyTable, RectifiedGridCoverage baseGrid,
      ICalcMonitor monitor ) throws Exception
  {
    String propertyName = "RasterProperty";
    RectifiedGridDomain newGridDomain = new RectifiedGridDomain( baseGrid
        .getGridDomain().getOrigin( null ), baseGrid.getGridDomain()
        .getOffset(), baseGrid.getGridDomain().getGridRange() );
    GM_Point origin = baseGrid.getGridDomain().getOrigin( null );
    double originX = origin.getX();
    double originY = origin.getY();
    Vector newRangeSetData = new Vector();
    Vector rangeSetData = baseGrid.getRangeSet().getRangeSetData();
    for( int i = 0; i < rangeSetData.size(); i++ )
    {
      Vector rowData = (Vector)rangeSetData.get( i );
      Vector newRowData = new Vector();
      for( int j = 0; j < rowData.size(); j++ )
      {
        double x = originX
            + j
            * baseGrid.getGridDomain()
                .getOffsetX( origin.getCoordinateSystem() )
            + 0.5
            * baseGrid.getGridDomain()
                .getOffsetX( origin.getCoordinateSystem() );
        double y = originY
            + ( rangeSetData.size() - i )
            * baseGrid.getGridDomain()
                .getOffsetY( origin.getCoordinateSystem() )
            - 0.5
            * baseGrid.getGridDomain()
                .getOffsetY( origin.getCoordinateSystem() );
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
              String property = actualFeature.getProperty( propertyName )
                  .toString();
              key = (Integer)propertyTable.get( property );
              break;
            }
          }
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
      monitor.setMessage( i + 1 + " rows of " + rangeSetData.size()
          + " calculated" );
      monitor.setProgress( 100 * i / rangeSetData.size() );
      //System.out.println(i + 1 + " rows of " + rangeSetData.size() + "
      // calculated"+ " Progress: "+100 * i / rangeSetData.size());
    }
    RangeSet newRangeSet = new RangeSet( newRangeSetData, null );
    RectifiedGridCoverage newGrid = new RectifiedGridCoverage( newGridDomain,
        newRangeSet );
    return newGrid;
  }
}