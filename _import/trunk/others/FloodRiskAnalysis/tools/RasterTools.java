package tools;

import java.util.Vector;

import org.kalypsodeegree_impl.model.cv.RangeSet;
import org.kalypsodeegree_impl.model.cv.RectifiedGridCoverage;
import org.kalypsodeegree_impl.model.cv.RectifiedGridDomain;

import damageAnalysis.DamageAnalysis;

public class RasterTools
{

  /**
   * substracts two grids (grid1 - grid2), checks each gridcell, when in a
   * gridcell of grid1 a value is stored and in the same gridcell of grid2 no
   * data value is stored, then the value of the result grid for this cell is 1;
   * otherwise it is 0
   * 
   * @param grid1
   * @param grid2
   * @return resultGrid Grid with only cellValues 0 or 1
   */
  public static RectifiedGridCoverage substractGrids( RectifiedGridCoverage grid1,
      RectifiedGridCoverage grid2 ) throws Exception
  {
    // control Geometries
    DamageAnalysis.controlGridGeometries( grid1.getGridDomain(), grid2.getGridDomain() );

    RectifiedGridDomain resultGridDomain = new RectifiedGridDomain( grid1.getGridDomain()
        .getOrigin( null ), grid1.getGridDomain().getOffset(), grid1.getGridDomain().getGridRange() );
    Vector resultRangeSetData = new Vector();
    Vector grid1RangeSetData = grid1.getRangeSet().getRangeSetData();
    Vector grid2RangeSetData = grid2.getRangeSet().getRangeSetData();
    for( int i = 0; i < grid1RangeSetData.size(); i++ )
    {
      Vector grid1_rowData = (Vector)grid1RangeSetData.get( i );
      Vector grid2_rowData = (Vector)grid2RangeSetData.get( i );
      Vector result_rowData = new Vector();
      for( int j = 0; j < grid1_rowData.size(); j++ )
      {
        if( grid1_rowData.get( j ) != null )
        {
          if( grid2_rowData.get( j ) != null )
          {
            result_rowData.addElement( new Double( 0 ) );
          }
          else
          {
            result_rowData.addElement( new Double( 1 ) );
          }
        }
        else
        {
          result_rowData.addElement( null );
        }
      }//for j (Spalten)
      resultRangeSetData.addElement( result_rowData );
    }//for i (Zeilen)
    RangeSet resultRangeSet = new RangeSet( resultRangeSetData, null );
    return new RectifiedGridCoverage( resultGridDomain, resultRangeSet );
  }

}