package org.kalypsodeegree_impl.model.cv;

/**
 * @author N. Peiler
 */
public class RectifiedGridCoverage
{
  private RectifiedGridDomain m_gridDomain = null;

  private RangeSet m_rangeSet = null;

  /**
   * constructs a RectifiedGridCoverage with the given gridDomain and rangeSet
   * 
   * @param gridDomain
   * @param rangeSet
   */
  public RectifiedGridCoverage( RectifiedGridDomain gridDomain, RangeSet rangeSet )
  {
    m_gridDomain = gridDomain;
    m_rangeSet = rangeSet;
  }

  public static String getName( )
  {
    return "RectifiedGridCoverage";
  }

  /**
   * @return Returns the gridDomain.
   */
  public RectifiedGridDomain getGridDomain( )
  {
    return m_gridDomain;
  }

  /**
   * @param gridDomain
   *          The gridDomain to set.
   */
  public void setGridDomain( RectifiedGridDomain gridDomain )
  {
    this.m_gridDomain = gridDomain;
  }

  /**
   * @return Returns the rangeSet.
   */
  public RangeSet getRangeSet( )
  {
    return m_rangeSet;
  }

  /**
   * @param rangeSet
   *          The rangeSet to set.
   */
  public void setRangeSet( RangeSet rangeSet )
  {
    this.m_rangeSet = rangeSet;
  }

}