package floodRiskAnalysis;

import java.util.Enumeration;
import java.util.Hashtable;
import java.util.Vector;

import org.kalypsodeegree_impl.model.cv.RangeSet;
import org.kalypsodeegree_impl.model.cv.RectifiedGridCoverage;
import org.kalypsodeegree_impl.model.cv.RectifiedGridDomain;

import tools.Interval;
import view.LogView;
import damageAnalysis.DamageAnalysis;

/**
 * Class for creating FloodRiskMaps
 * 
 * @author N. Peiler
 *  
 */
public class FloodRiskAnalysis
{

  /**
   * returns a FloodRiskGrid as RectifiedGridCoverage for a given
   * annualDamageGrid(RectifiedGridCoverage), a
   * landuseGrid(RectifiedGridCoverage) and a riskClassTable
   * 
   * @param annualDamageGrid
   *          (RectifiedGridCoverage)
   * @param landuseGrid
   *          (RectifiedGridCoverage)
   * @param riskClassTable
   *          key=landuseKey, value=Hashtable of RiskClasses (value=riskKey,
   *          value=Interval)
   * @return floodRiskGrid(RectifiedGridCoverage)
   * @throws Exception
   */
  public static RectifiedGridCoverage defineRisk( RectifiedGridCoverage annualDamageGrid,
      RectifiedGridCoverage landuseGrid, Hashtable riskClassTable ) throws Exception
  {
    LogView.println( "Calculate FloodRiskGrid..." );
    RectifiedGridCoverage floodRiskGrid = null;
    DamageAnalysis.controlGridGeometries( annualDamageGrid.getGridDomain(), landuseGrid
        .getGridDomain() );
    RectifiedGridDomain floodRisk_gridDomain = new RectifiedGridDomain( annualDamageGrid
        .getGridDomain().getOrigin( null ), annualDamageGrid.getGridDomain().getOffset(),
        annualDamageGrid.getGridDomain().getGridRange() );
    Vector annualDamage_rangeSetData = annualDamageGrid.getRangeSet().getRangeSetData();
    Vector landuse_rangeSetData = landuseGrid.getRangeSet().getRangeSetData();
    Vector floodRisk_rangeSetData = new Vector();
    for( int i = 0; i < annualDamage_rangeSetData.size(); i++ )
    {
      Vector annualDamage_rowData = (Vector)annualDamage_rangeSetData.get( i );
      Vector landuse_rowData = (Vector)landuse_rangeSetData.get( i );
      Vector floodRisk_rowData = new Vector();
      for( int j = 0; j < annualDamage_rowData.size(); j++ )
      {
        if( annualDamage_rowData.get( j ) != null && landuse_rowData.get( j ) != null )
        {
          Integer landuseKey = new Integer( ( (Double)landuse_rowData.get( j ) ).intValue() );
          double annualDamage = ( (Double)annualDamage_rowData.get( j ) ).doubleValue();
          Integer riskClass = null;
          if( riskClassTable.containsKey( landuseKey ) )
          {
            Hashtable intervals = (Hashtable)riskClassTable.get( landuseKey );
            Enumeration enum = intervals.keys();
            while( enum.hasMoreElements() )
            {
              Integer key = (Integer)enum.nextElement();
              Interval interval = (Interval)intervals.get( key );
              if( interval.contains( annualDamage ) )
              {
                riskClass = key;
                break;
              }
            }
          }
          if( riskClass != null )
          {
            floodRisk_rowData.addElement( new Double( riskClass.doubleValue() ) );
          }
          else
          {
            floodRisk_rowData.addElement( null );
          }
        }
        else
        {
          floodRisk_rowData.addElement( null );
        }
      }//for j
      floodRisk_rangeSetData.addElement( floodRisk_rowData );
      /*
       * System.out.println(i + " rows of " + annualDamage_rangeSetData.size() + "
       * calculated");
       */
    }//for i
    RangeSet floodRisk_rangeSet = new RangeSet( floodRisk_rangeSetData, null );
    floodRiskGrid = new RectifiedGridCoverage( floodRisk_gridDomain, floodRisk_rangeSet );
    return floodRiskGrid;
  }

}