package org.kalypsodeegree_impl.model.cv;

/**
 * @author N. Peiler
 *  
 */
public class RectifiedGridCoverage
{

  RectifiedGridDomain gridDomain = null;

  RangeSet rangeSet = null;

  /**
   * constructs a RectifiedGridCoverage with the given gridDomain and rangeSet
   * 
   * @param gridDomain
   * @param rangeSet
   */
  public RectifiedGridCoverage( RectifiedGridDomain gridDomain, RangeSet rangeSet )
  {
    this.gridDomain = gridDomain;
    this.rangeSet = rangeSet;
  }

  public static String getName()
  {
    return "RectifiedGridCoverage";
  }

  /**
   * @return Returns the gridDomain.
   */
  public RectifiedGridDomain getGridDomain()
  {
    return gridDomain;
  }

  /**
   * @param gridDomain
   *          The gridDomain to set.
   */
  public void setGridDomain( RectifiedGridDomain gridDomain )
  {
    this.gridDomain = gridDomain;
  }

  /**
   * @return Returns the rangeSet.
   */
  public RangeSet getRangeSet()
  {
    return rangeSet;
  }

  /**
   * @param rangeSet
   *          The rangeSet to set.
   */
  public void setRangeSet( RangeSet rangeSet )
  {
    this.rangeSet = rangeSet;
  }
}