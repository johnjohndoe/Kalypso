package damageAnalysis;

import java.util.Hashtable;
import java.util.TreeMap;

import org.deegree_impl.model.cv.RectifiedGridCoverage;

/**
 * Class, which holds the required InputData for a DamageAnalysis
 * 
 * @author N. Peiler
 *  
 */
public class DamageAnalysisContext
{

  /**
   * key=annuality as Double, value=RectifiedGridCoverage
   */
  private TreeMap waterlevelGrids = null;

  RectifiedGridCoverage landuseGrid = null;

  /**
   * key=landuseType as String, value=landuseTypeKey as Integer
   */
  Hashtable landuseTypeList;

  RectifiedGridCoverage administrationUnitGrid = null;

  /**
   * key=Name of administrationUnit as String, value=key as Integer
   */
  Hashtable administrationUnitList;

  /**
   * key=landuseTypeKey(String), value=damageFuntion(ParseFunction)
   */
  Hashtable damagefunctions;

  /**
   * if(administrationUnitGrid!=null){
   * key="landuseTypeKey,administrationUnitKey"(String), value=asset(Double)
   * }else{ key=landuseTypeKey(String), value=asset(Double)
   */
  Hashtable assets;

  /**
   * constructs a DamageAnalysisContext with a given TreeMap of waterlevelGrids
   * and inputContext
   */
  public DamageAnalysisContext()
  {
    super();
  }

  /**
   * @return Returns the waterlevelGrids.
   */
  public TreeMap getWaterlevelGrids()
  {
    return waterlevelGrids;
  }

  /**
   * @param waterlevelGrids
   *          The waterlevelGrids to set.
   */
  public void setWaterlevelGrids( TreeMap waterlevelGrids )
  {
    this.waterlevelGrids = waterlevelGrids;
  }

  /**
   * @return Returns the landuseGrid.
   */
  public RectifiedGridCoverage getLanduseGrid()
  {
    return landuseGrid;
  }

  /**
   * @param landuseGrid
   *          The landuseGrid to set.
   */
  public void setLanduseGrid( RectifiedGridCoverage landuseGrid )
  {
    this.landuseGrid = landuseGrid;
  }

  /**
   * @return Returns the administrationUnitGrid.
   */
  public RectifiedGridCoverage getAdministrationUnitGrid()
  {
    return administrationUnitGrid;
  }

  /**
   * @param administrationUnitGrid
   *          The administrationUnitGrid to set.
   */
  public void setAdministrationUnitGrid( RectifiedGridCoverage administrationUnitGrid )
  {
    this.administrationUnitGrid = administrationUnitGrid;
  }

  /**
   * @return Returns the administrationUnitList.
   */
  public Hashtable getAdministrationUnitList()
  {
    return administrationUnitList;
  }

  /**
   * @param administrationUnitList
   *          The administrationUnitList to set.
   */
  public void setAdministrationUnitList( Hashtable administrationUnitList )
  {
    this.administrationUnitList = administrationUnitList;
  }

  /**
   * @return Returns the landuseTypeList.
   */
  public Hashtable getLanduseTypeList()
  {
    return landuseTypeList;
  }

  /**
   * @param landuseTypeList
   *          The landuseTypeList to set.
   */
  public void setLanduseTypeList( Hashtable landuseTypeList )
  {
    this.landuseTypeList = landuseTypeList;
  }

  /**
   * @return Returns the assets.
   */
  public Hashtable getAssets()
  {
    return assets;
  }

  /**
   * @param assets
   *          The assets to set.
   */
  public void setAssets( Hashtable assets )
  {
    this.assets = assets;
  }

  /**
   * @return Returns the damagefunctions.
   */
  public Hashtable getDamagefunctions()
  {
    return damagefunctions;
  }

  /**
   * @param damagefunctions
   *          The damagefunctions to set.
   */
  public void setDamagefunctions( Hashtable damagefunctions )
  {
    this.damagefunctions = damagefunctions;
  }
}