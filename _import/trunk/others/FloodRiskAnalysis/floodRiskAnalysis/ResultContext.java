package floodRiskAnalysis;

import java.util.TreeMap;

import org.deegree_impl.model.cv.RectifiedGridCoverage;

/**
 * Class which holds the ResultData
 * 
 * @author N. Peiler
 *  
 */
public class ResultContext
{

  private TreeMap damagegrids = null;

  private RectifiedGridCoverage annualDamageGrid = null;

  private RectifiedGridCoverage floodRiskGrid = null;

  public ResultContext()
  {
    super();
  }

  /**
   * @return Returns the annualDamageGrid.
   */
  public RectifiedGridCoverage getAnnualDamageGrid()
  {
    return annualDamageGrid;
  }

  /**
   * @param annualDamageGrid
   *          The annualDamageGrid to set.
   */
  public void setAnnualDamageGrid( RectifiedGridCoverage annualDamageGrid )
  {
    this.annualDamageGrid = annualDamageGrid;
  }

  /**
   * @return Returns the damagegrids.
   */
  public TreeMap getDamagegrids()
  {
    return damagegrids;
  }

  /**
   * @param damagegrids
   *          The damagegrids to set.
   */
  public void setDamagegrids( TreeMap damagegrids )
  {
    this.damagegrids = damagegrids;
  }

  /**
   * @return Returns the floodRiskGrid.
   */
  public RectifiedGridCoverage getFloodRiskGrid()
  {
    return floodRiskGrid;
  }

  /**
   * @param floodRiskGrid
   *          The floodRiskGrid to set.
   */
  public void setFloodRiskGrid( RectifiedGridCoverage floodRiskGrid )
  {
    this.floodRiskGrid = floodRiskGrid;
  }
}