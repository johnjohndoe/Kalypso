package org.kalypsodeegree_impl.model.cv;

import java.util.Vector;

/**
 * Class which holds the rangeSetData of a RectifiedGridCoverage
 * 
 * @author N. Peiler
 *  
 */
public class RangeSet
{

  /**
   * Vector, which stores the rangeSet data; the data of each row is stored in a Vector
   */
  private Vector rangeSetData = null;

  /**
   * name of rangeSetData-file ("xy.dat")
   */
  String rangeSetDataFile = null;

  /**
   * constructs a RangeSet with the given rangeSetData
   * 
   * @param rangeSetData
   * @param rangeSetDataFileName
   *          name of rangeSetData-file ("xy.dat")
   */
  public RangeSet( Vector rangeSetData, String rangeSetDataFileName )
  {
    this.rangeSetData = rangeSetData;
    this.rangeSetDataFile = rangeSetDataFileName;
  }

  /**
   * @return Returns the rangeSetData.
   */
  public Vector getRangeSetData()
  {
    return rangeSetData;
  }

  /**
   * @param rangeSetData
   *          The rangeSetData to set.
   */
  public void setRangeSetData( Vector rangeSetData )
  {
    this.rangeSetData = rangeSetData;
  }

  /**
   * @return Returns the name of the rangeSetDataFile.
   */
  public String getRangeSetDataFile()
  {
    return rangeSetDataFile;
  }

  /**
   * @param rangeSetDataFileName
   *          The rangeSetDataFileName to set.
   */
  public void setRangeSetDataFile( String rangeSetDataFileName )
  {
    this.rangeSetDataFile = rangeSetDataFileName;
  }

  /**
   * @return the minValue of the rangeSetData
   */
  public double getMinValue()
  {
    double min = Double.MAX_VALUE;
    for( int i = 0; i < rangeSetData.size(); i++ )
    {
      Vector rowData = (Vector)rangeSetData.get( i );
      for( int j = 0; j < rowData.size(); j++ )
      {
        if( rowData.get( j ) != null )
        {
          double actualValue = ( (Double)rowData.get( j ) ).doubleValue();
          if( actualValue < min )
          {
            min = actualValue;
          }
        }
      }//for j
    }//for i
    return min;
  }

  /**
   * @return the maxValue of the rangeSetData
   */
  public double getMaxValue()
  {
    double max = Double.MIN_VALUE;
    for( int i = 0; i < rangeSetData.size(); i++ )
    {
      Vector rowData = (Vector)rangeSetData.get( i );
      for( int j = 0; j < rowData.size(); j++ )
      {
        if( rowData.get( j ) != null )
        {
          double actualValue = ( (Double)rowData.get( j ) ).doubleValue();
          if( actualValue > max )
          {
            max = actualValue;
          }
        }
      }//for j
    }//for i
    return max;
  }
}