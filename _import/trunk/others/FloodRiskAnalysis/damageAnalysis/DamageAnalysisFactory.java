package damageAnalysis;

import java.io.File;
import java.util.TreeMap;

import org.deegree_impl.model.cv.RectifiedGridCoverage;

import converter.ArcGridConverter;

/**
 * Class for creating a DamageAnalysisContext of a contextModel, a
 * landuseDataModel, an administrationUnitModel (optional) and a
 * waterlevelDataModel
 * 
 * @author N. Peiler
 *  
 */
public class DamageAnalysisFactory
{

  /**
   * 
   * creates a DamageAnalysisContext of a contextModel, a landuseDataModel, an
   * administrationUnitModel (optional) and a waterlevelDataModel
   * 
   * @param contextModelSchema
   * @param contextModelGML
   * @param landuseDataModelSchema
   * @param landuseDataModelGML
   * @param administrationUnitDataModelSchema
   * @param administrationUnitDataModelGML
   * @param waterlevelDataModelSchema
   * @param waterlevelDataModelGML
   * @return DamageAnalysisContext
   */
  public static DamageAnalysisContext createDamageAnalysisContext( File contextModelSchema,
      File contextModelGML, File landuseDataModelSchema, File landuseDataModelGML,
      File administrationUnitDataModelSchema, File administrationUnitDataModelGML,
      File waterlevelDataModelSchema, File waterlevelDataModelGML )
  {

    try
    {
      ContextModel contextModel = new ContextModel( contextModelGML, contextModelSchema );
      DamageAnalysisContext damageAnalysisContext = new DamageAnalysisContext();
      TreeMap waterlevelGrids = DataModel.createWaterlevelGrids( waterlevelDataModelGML,
          waterlevelDataModelSchema );
      damageAnalysisContext.setWaterlevelGrids( waterlevelGrids );
      RectifiedGridCoverage landuseGrid = DataModel.createLanduseGrid( landuseDataModelGML,
          landuseDataModelSchema );
      damageAnalysisContext.setLanduseGrid( landuseGrid );
      damageAnalysisContext.setLanduseTypeList( contextModel.getLanduseList() );
      RectifiedGridCoverage administrationUnitGrid = null;
      if( administrationUnitDataModelGML != null )
      {
        administrationUnitGrid = DataModel.createAdministrationUnitGrid(
            administrationUnitDataModelGML, administrationUnitDataModelSchema );
      }
      damageAnalysisContext.setAdministrationUnitGrid( administrationUnitGrid );
      damageAnalysisContext.setAdministrationUnitList( contextModel.getAdministrationUnitList() );
      damageAnalysisContext.setDamagefunctions( contextModel.getDamageFunctionList() );
      damageAnalysisContext.setAssets( contextModel.getAssetValueList() );
      return damageAnalysisContext;
    }
    catch( Exception e )
    {
      System.out.println( e );
      return null;
    }

  }

  /**
   * creates a DamageAnalysisContext of a contextModel, a landuse Ascii-Gridand
   * an administrationUnit Ascii-Grid (optional)
   * 
   * @param contextModelSchema
   * @param contextModelGML
   * @param landuseGridAsc
   * @param administrationUnitGridAsc
   * @return @throws
   *         Exception
   */
  public static DamageAnalysisContext createDamageAnalysisContext( File contextModelSchema,
      File contextModelGML, File landuseGridAsc, File administrationUnitGridAsc ) throws Exception
  {
    ContextModel contextModel = new ContextModel( contextModelGML, contextModelSchema );
    DamageAnalysisContext damageAnalysisContext = new DamageAnalysisContext();
    ArcGridConverter gridConverter = new ArcGridConverter();
    RectifiedGridCoverage landuseGridCoverage = gridConverter.importGridArc( landuseGridAsc );
    damageAnalysisContext.setLanduseGrid( landuseGridCoverage );
    damageAnalysisContext.setLanduseTypeList( contextModel.getLanduseList() );
    RectifiedGridCoverage administrationUnitGridCoverage = null;
    if( administrationUnitGridAsc != null )
    {
      administrationUnitGridCoverage = gridConverter.importGridArc( administrationUnitGridAsc );
    }
    damageAnalysisContext.setAdministrationUnitGrid( administrationUnitGridCoverage );
    damageAnalysisContext.setAdministrationUnitList( contextModel.getAdministrationUnitList() );
    damageAnalysisContext.setDamagefunctions( contextModel.getDamageFunctionList() );
    damageAnalysisContext.setAssets( contextModel.getAssetValueList() );
    return damageAnalysisContext;
  }
}