package floodRiskAnalysis;

import java.io.File;
import java.math.BigDecimal;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.List;
import java.util.TreeMap;
import java.util.Vector;

import org.kalypso.ogc.gml.serialize.ShapeSerializer;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.extension.ITypeRegistry;
import org.kalypsodeegree_impl.extension.TypeRegistrySingleton;
import org.kalypsodeegree_impl.model.cs.ConvenienceCSFactory;
import org.kalypsodeegree_impl.model.cv.RangeSet;
import org.kalypsodeegree_impl.model.cv.RangeSetTypeHandler;
import org.kalypsodeegree_impl.model.cv.RectifiedGridCoverage;
import org.kalypsodeegree_impl.model.cv.RectifiedGridDomain;
import org.kalypsodeegree_impl.model.cv.RectifiedGridDomainTypeHandler;
import org.opengis.cs.CS_CoordinateSystem;

import tools.Number;
import converter.ArcGridConverter;
import converter.VectorToGridConverter;
import damageAnalysis.ContextModel;
import damageAnalysis.DamageAnalysis;
import damageAnalysis.DamageAnalysisContext;
import damageAnalysis.DamageAnalysisFactory;
import damageAnalysis.DataModel;

/**
 * 
 * MainClass for testing
 * 
 * @author N. Peiler
 *  
 */
public class Main
{

  public Main()
  {
    super();
  }

  private static void prepareDamageCalculation( String workingDir, String schemaDir )
  {
    File contextModelGML = new File( workingDir + "Control//ContextModell.gml" );

    /*
     * File contextModelGML = new File(workingDir +
     * "ContextModell_mitAdmin.gml");
     */

    File contextModelSchema = new File( schemaDir + "ContextModell.xsd" );

    File landuseDataModelGML = new File( workingDir + "Landuse//LanduseDataModel.gml" );
    File landuseDataModelSchema = new File( schemaDir + "LanduseDataModel.xsd" );

    /*
     * File administrationUnitDataModelGML = new File(workingDir +
     * "AdministrationUnit//AdministrationUnitDataModel.gml"); File
     * administrationUnitDataModelSchema = new File(schemaDir +
     * "AdministrationUnitDataModel.xsd");
     */

    File administrationUnitDataModelGML = null;
    File administrationUnitDataModelSchema = null;

    File waterlevelDataModelGML = new File( workingDir + "Waterlevel//WaterlevelDataModel.gml" );
    File waterlevelDataModelSchema = new File( schemaDir + "WaterlevelDataModel.xsd" );

    String damageDirectory = workingDir + "Damage//";

    DamageAnalysisContext daContext = DamageAnalysisFactory.createDamageAnalysisContext(
        contextModelSchema, contextModelGML, landuseDataModelSchema, landuseDataModelGML,
        administrationUnitDataModelSchema, administrationUnitDataModelGML,
        waterlevelDataModelSchema, waterlevelDataModelGML );
    calculateDamage( damageDirectory, daContext );
  }

  private static void calculateDamage( String damageDirectory,
      DamageAnalysisContext damageAnalysisContext )
  {

    try
    {
      ArcGridConverter gridConverter = new ArcGridConverter();
      TreeMap damagePercentageGrids = DamageAnalysis
          .calculateDamagePercentages( damageAnalysisContext );
      System.out.println( "damagePercentageGrids calculated." );

      TreeMap damageGrids = DamageAnalysis.calculateDamages( damagePercentageGrids,
          damageAnalysisContext );
      Iterator it = damageGrids.keySet().iterator();
      while( it.hasNext() )
      {
        Double key = (Double)it.next();
        RectifiedGridCoverage damageGrid = (RectifiedGridCoverage)damageGrids.get( key );
        double annuality = 1 / key.doubleValue();
        String fileName = damageDirectory + "damage_HQ" + (int)annuality + ".asc";
        gridConverter.exportGridArc( new File( fileName ), damageGrid );
      }
      File damageResultModelGML = new File( damageDirectory + "DamageResultModel.gml" );
      /*
       * File damageResultModelSchema = new File(schemaDir +
       * "DamageResultModel.xsd");
       */
      ResultModel.writeDamageData( damageResultModelGML, damageGrids );
      System.out.println( "damageGrids calculated." );

      Vector tempGrids = DamageAnalysis.calculateTempGridsAnnualDamage( damageGrids );
      Object[] keys = damageGrids.keySet().toArray();
      for( int i = 0; i < keys.length; i++ )
      {
        Double key = (Double)keys[i];
        if( i < keys.length - 1 )
        {
          Double nextKey = (Double)keys[i + 1];
          double deltaP = nextKey.doubleValue() - key.doubleValue();
          System.out.println( "deltaP=" + Number.round( deltaP, 4, BigDecimal.ROUND_HALF_EVEN ) );
          RectifiedGridCoverage tempGrid_rg = (RectifiedGridCoverage)tempGrids.get( i );
          String fileName = damageDirectory + "\\tempGrid_deltaP"
              + Number.round( deltaP, 4, BigDecimal.ROUND_HALF_EVEN ) + ".asc";
          gridConverter.exportGridArc( new File( fileName ), tempGrid_rg );
        }
      }
      System.out.println( "tempGrids calculated." );

      /*
       * AnnualDamageGrid annualDamageGrid = DamageAnalysis
       * .calculateAnnualDamage(damageGrids);
       */
      RectifiedGridCoverage annualDamageGrid = DamageAnalysis.calculateAnnualDamage( tempGrids );
      gridConverter.exportGridArc( new File( damageDirectory + "annualDamage.asc" ),
          annualDamageGrid );
      File annualDamageResultModelGML = new File( damageDirectory + "AnnualDamageResultModel.gml" );
      /*
       * File annualDamageResultModelSchema = new File(schemaDir +
       * "AnnualDamageResultModel.xsd");
       */
      ResultModel.writeAnnualDamageData( annualDamageResultModelGML, annualDamageGrid );
      System.out.println( "annualDamageGrid calculated." );

    }
    catch( Exception e )
    {
      System.out.println( e );
    }

  }

  public static void prepareRiskCalculation( String workingDir, String schemaDir ) throws Exception
  {
    File riskContentModelGML = new File( workingDir + "Control//RiskContextModell.gml" );
    File riskContentModelSchema = new File( schemaDir + "RiskContextModell.xsd" );
    File landuseDataModelGML = new File( workingDir + "Landuse//LanduseDataModel.gml" );
    File landuseDataModelSchema = new File( schemaDir + "LanduseDataModel.xsd" );
    RectifiedGridCoverage landuseGrid = DataModel.createLanduseGrid( landuseDataModelGML,
        landuseDataModelSchema );
    String damageDirectory = workingDir + "Damage//";
    String riskDirectory = workingDir + "Risk//";
    calculateRisk( riskContentModelGML, riskContentModelSchema, damageDirectory, landuseGrid,
        riskDirectory, schemaDir );
  }

  public static void calculateRisk( File riskContentModelGML, File riskContentModelSchema,
      String damageDirectory, RectifiedGridCoverage landuseGrid, String riskDirectory,
      String schemaDir )
  {
    try
    {
      ArcGridConverter gridConverter = new ArcGridConverter();

      RiskContextModel riskContextModell = new RiskContextModel( riskContentModelGML,
          riskContentModelSchema );

      Hashtable riskClassTable = riskContextModell.getRiskClassLists();

      File annualDamageResultModelGML = new File( damageDirectory + "AnnualDamageResultModel.gml" );
      File annualDamageResultModelSchema = new File( schemaDir + "AnnualDamageResultModel.xsd" );
      RectifiedGridCoverage annualDamageGrid = ResultModel.createAnnualDamageGrid(
          annualDamageResultModelGML, annualDamageResultModelSchema );

      RectifiedGridCoverage floodRiskGrid = FloodRiskAnalysis.defineRisk( annualDamageGrid,
          landuseGrid, riskClassTable );
      gridConverter.exportGridArc( new File( riskDirectory + "riskGrid.asc" ), floodRiskGrid );
      File floodRiskResultModelGML = new File( riskDirectory + "FloodRiskResultModel.gml" );
      /*
       * File floodRiskResultModelSchema = new File(schemaDir +
       * "FloodRiskResultModel.xsd");
       */
      ResultModel.writeFloodRiskData( floodRiskResultModelGML, floodRiskGrid );
      System.out.println( "RiskGrid calculated." );
    }
    catch( Exception e )
    {
      System.out.println( e );
    }
  }

  public static void getStatistics( RectifiedGridCoverage damageGrid,
      RectifiedGridCoverage landuseGrid )
  {
    Hashtable statistics = null;
    try
    {
      statistics = DamageAnalysis.getStatistics( damageGrid, landuseGrid );
    }
    catch( Exception e )
    {
      e.printStackTrace();
    }
    Iterator it = statistics.keySet().iterator();
    while( it.hasNext() )
    {
      Integer key = (Integer)it.next();
      Vector statisticsVector = (Vector)statistics.get( key );
      Double sum = (Double)statisticsVector.get( 0 );
      Double min = (Double)statisticsVector.get( 1 );
      Double max = (Double)statisticsVector.get( 2 );
      System.out.println( "Statistics for landuseKey " + key + ": Sum=" + sum + ", MinValue=" + min
          + ", MaxValue=" + max );
    }
  }

  public static void getStatistics( RectifiedGridCoverage damageGrid,
      RectifiedGridCoverage landuseGrid, RectifiedGridCoverage administrationUnitGrid )
  {

    Hashtable statistics = null;
    try
    {
      statistics = DamageAnalysis.getStatistics( damageGrid, landuseGrid, administrationUnitGrid );
    }
    catch( Exception e )
    {
      e.printStackTrace();
    }
    Iterator it = statistics.keySet().iterator();
    while( it.hasNext() )
    {
      Integer administrationUnitKey = (Integer)it.next();
      Hashtable statistics_landuse = (Hashtable)statistics.get( administrationUnitKey );
      Iterator it1 = statistics_landuse.keySet().iterator();
      double sum_adminUnit = 0;
      System.out.println( "Statistics for administrationUnitKey " + administrationUnitKey + ": " );
      while( it1.hasNext() )
      {
        Integer landuseKey = (Integer)it1.next();
        Vector statisticsVector = (Vector)statistics_landuse.get( landuseKey );
        Double sum = (Double)statisticsVector.get( 0 );
        sum_adminUnit = sum_adminUnit + sum.doubleValue();
        Double min = (Double)statisticsVector.get( 1 );
        Double max = (Double)statisticsVector.get( 2 );
        System.out.println( "Landuse " + landuseKey + ": Sum=" + sum + ", MinValue=" + min
            + ", MaxValue=" + max );
      }
      System.out.println( "Summed Damage=" + sum_adminUnit + "\n" );
    }

  }

  public static void offsetRangeSetData( File importGridAsc, File exportGridAsc, double offset )
  {
    try
    {
      ArcGridConverter gridConverter = new ArcGridConverter();
      RectifiedGridCoverage rg_old = gridConverter.importGridArc( importGridAsc );
      System.out.println( "read Grid" );
      RectifiedGridDomain newGridDomain = new RectifiedGridDomain( rg_old.getGridDomain()
          .getOrigin( null ), rg_old.getGridDomain().getOffset(), rg_old.getGridDomain()
          .getGridRange() );
      Vector rg_oldRangeSet = rg_old.getRangeSet().getRangeSetData();
      Vector rg_newRangeSet = new Vector();
      for( int i = 0; i < rg_oldRangeSet.size(); i++ )
      {
        Vector rg_oldRowData = (Vector)rg_oldRangeSet.get( i );
        Vector rg_newRowData = new Vector();
        for( int j = 0; j < rg_oldRowData.size(); j++ )
        {
          if( rg_oldRowData.get( j ) != null )
          {
            Double waterlevel = (Double)rg_oldRowData.get( j );
            double newWaterlevel = waterlevel.doubleValue() + offset;
            rg_newRowData.addElement( new Double( newWaterlevel ) );
          }
          else
          {
            rg_newRowData.addElement( null );
          }
        }// for j
        rg_newRangeSet.addElement( rg_newRowData );
        System.out.println( i + " rows of " + rg_oldRangeSet.size() + " calculated" );
      }// for i
      RangeSet newRangeSet = new RangeSet( rg_newRangeSet, null );
      RectifiedGridCoverage rg_new = new RectifiedGridCoverage( newGridDomain, newRangeSet );
      gridConverter.exportGridArc( exportGridAsc, rg_new );
      System.out.println( "finished" );
    }
    catch( Exception e )
    {
      System.out.println( e );
    }
  }

  public static TreeMap readWaterlevelGrids( File waterlevelDirectory )
  {
    ArcGridConverter gridConverter = new ArcGridConverter();
    TreeMap waterlevelGrids = gridConverter.importwaterlevelGrids( waterlevelDirectory );
    return waterlevelGrids;
  }

  public static void writeWaterlevelGrids( String workingDir, TreeMap waterlevelGrids )
      throws Exception
  {
    File waterlevelDataModelGML = new File( workingDir + "Waterlevel//WaterlevelDataModel.gml" );
    /*
     * File waterlevelDataModelSchema = new File(schemaDir +
     * "WaterlevelDataModel.xsd");
     */
    DataModel.writeWaterlevelData( waterlevelDataModelGML, waterlevelGrids );
  }

  public static void convertLanduse( String workingDir, String schemaDir )
  {
    try
    {
      File waterlevelDataModelGML = new File( workingDir + "Waterlevel//WaterlevelDataModel.gml" );
      File waterlevelDataModelSchema = new File( schemaDir + "WaterlevelDataModel.xsd" );
      TreeMap waterlevelGrids = DataModel.createWaterlevelGrids( waterlevelDataModelGML,
          waterlevelDataModelSchema );

      File contextModelGML = new File( workingDir + "Control//ContextModell.gml" );
      File contextModelSchema = new File( schemaDir + "ContextModell.xsd" );
      ContextModel contextModel = new ContextModel( contextModelGML, contextModelSchema );

      String landuseShapeBaseFile = workingDir + "Landuse//landuse";
      List landuseFeatureList = getFeatureList( landuseShapeBaseFile );
      String landusePropertyName = "NUTZUNG";

      VectorToGridConverter converter = new VectorToGridConverter();
      RectifiedGridCoverage landuseGrid = converter.landuseToGrid( landuseFeatureList, contextModel
          .getLanduseList(), landusePropertyName, waterlevelGrids );

      File landuseDataModelGML = new File( workingDir + "Landuse//LanduseDataModel.gml" );
      /*
       * File landuseDataModelSchema = new File(schemaDir +
       * "LanduseDataModel.xsd");
       */
      writeLanduseData( landuseDataModelGML, landuseGrid );
      ArcGridConverter gridConverter = new ArcGridConverter();
      File landuseGridAsc = new File( workingDir + "Landuse//landuse.asc" );
      gridConverter.exportGridArc( landuseGridAsc, landuseGrid );
    }
    catch( Exception e )
    {
      System.out.println( e );
    }
  }

  public static void writeLanduseData( File landuseDataModelGML, RectifiedGridCoverage landuseGrid )
      throws Exception
  {
    DataModel.writeLanduseData( landuseDataModelGML, landuseGrid );
  }

  public static void convertAdministrationUnit( String workingDir, String schemaDir )
  {
    try
    {
      File waterlevelDataModelGML = new File( workingDir + "Waterlevel//WaterlevelDataModel.gml" );
      File waterlevelDataModelSchema = new File( schemaDir + "WaterlevelDataModel.xsd" );
      TreeMap waterlevelGrids = DataModel.createWaterlevelGrids( waterlevelDataModelGML,
          waterlevelDataModelSchema );

      File contextModelGML = new File( workingDir + "Control//ContextModell_mitAdmin.gml" );
      File contextModelSchema = new File( schemaDir + "ContextModell.xsd" );
      ContextModel contextModel = new ContextModel( contextModelGML, contextModelSchema );

      String administrationUnitShapeBaseFile = workingDir
          + "AdministrationUnit//administrationUnit";
      List adminUnitFeatureList = getFeatureList( administrationUnitShapeBaseFile );
      String administrationUnitPropertyName = "GEMEINDE";

      VectorToGridConverter converter = new VectorToGridConverter();
      RectifiedGridCoverage administrationUnitGrid = converter.administrationUnitToGrid(
          adminUnitFeatureList, contextModel.getAdministrationUnitList(),
          administrationUnitPropertyName, waterlevelGrids );

      File administrationUnitDataModelGML = new File( workingDir
          + "AdministrationUnit//AdministrationUnitDataModel.gml" );
      writeAdministrationUnitData( administrationUnitDataModelGML, administrationUnitGrid );
      ArcGridConverter gridConverter = new ArcGridConverter();
      File administrationUnitGridAsc = new File( workingDir
          + "AdministrationUnit//administrationUnit.asc" );
      gridConverter.exportGridArc( administrationUnitGridAsc, administrationUnitGrid );

    }
    catch( Exception e )
    {
      System.out.println( e );
    }
  }

  public static void writeAdministrationUnitData( File administrationUnitDataModelGML,
      RectifiedGridCoverage administrationUnitGrid ) throws Exception
  {
    DataModel.writeAdministrationUnitData( administrationUnitDataModelGML, administrationUnitGrid );
  }

  /**
   * returns a list of Features for a given shapeFile
   * 
   * @param shapeFileBase
   *          (base of shape)
   * 
   * @return List of Features
   */
  private static List getFeatureList( String shapeFileBase )
  {
    try
    {
      CS_CoordinateSystem srs = ConvenienceCSFactory.getInstance().getOGCCSByName( "EPSG:31467" );
      GMLWorkspace workspace = ShapeSerializer.deserialize( shapeFileBase, srs, null );
      Feature root = workspace.getRootFeature();
      List featureList = (List)root.getProperty( "featureMember" );
      return featureList;
    }
    catch( Exception e )
    {
      System.out.println( e );
      return null;
    }
  }

  public static void main( String[] params )
  {

    try
    {

      //String workingDir = "D://Nadja//floodRiskAnalysis//Test//";
      String schemaDir = "D://Nadja//eclipse//runtime-workspace//FloodRiskAnalysis//.model//schema//";

      //String workingDir1 =
      // "D://Nadja//floodRiskAnalysis//Kellinghusen//Grid1//";
      //String workingDir1_neu =
      // "D://Nadja//floodRiskAnalysis//Kellinghusen//Grid1_neu//";
      //String workingDir2 =
      // "D://Nadja//floodRiskAnalysis//Kellinghusen//Grid2//";
      String workingDir = "D://Nadja//eclipse//runtime-workspace//FloodRiskAnalysis//AnnualDamage//";

      ITypeRegistry typeRegistry = TypeRegistrySingleton.getTypeRegistry();
      typeRegistry.registerTypeHandler( new RectifiedGridDomainTypeHandler() );
      typeRegistry.registerTypeHandler( new RangeSetTypeHandler() );

      RectifiedGridCoverage testGrid = ResultModel.createAnnualDamageGrid( new File( workingDir
          + "AnnualDamageResultModel.gml" ), new File( schemaDir
          + "AnnualDamageResultModel.xsd" ) );
      ResultModel.writeAnnualDamageData(new File("D://temp//test.gml"), testGrid);
      ResultModel.createAnnualDamageGrid(new File("D://temp//test.gml"), new File( schemaDir
          + "AnnualDamageResultModel.xsd" ));

      /*
       * File wsp_HQ10 = new File(workingDir1_neu + "Waterlevel//wsp_HQ10.asc");
       * File wsp_HQ100 = new File(workingDir1_neu +
       * "Waterlevel//wsp_HQ100.asc"); offsetRangeSetData(wsp_HQ10, wsp_HQ100,
       * 0.2);
       */

      /*
       * File waterlevelDirectory = new File(workingDir3 + "Waterlevel");
       * TreeMap waterlevelGrids = readWaterlevelGrids(waterlevelDirectory);
       * writeWaterlevelGrids(workingDir3, schemaDir, waterlevelGrids);
       */

      //convertLanduse(workingDir2, schemaDir);
      //convertAdministrationUnit(workingDir, schemaDir);
      //prepareDamageCalculation( workingDir3, schemaDir );
      //prepareRiskCalculation(workingDir3, schemaDir);
      //ArcGridConverter gridConverter = new ArcGridConverter();
      /*
       * RectifiedGridCoverage landuseGrid = gridConverter .importGridArc(new
       * File(workingDir3 + "landuse.asc"));
       */

      /*
       * RectifiedGridCoverage administrationUnitGrid = gridConverter
       * .importGridArc(new File(workingDir + "administrationUnit.asc"));
       */

      /*
       * RectifiedGridCoverage annualDamageGrid = gridConverter
       * .importGridArc(new File(workingDir3 +
       * "Damage//tempGrid_deltaP0.01.asc"));
       */

      /*
       * getStatistics(annualDamageGrid, landuseGrid, administrationUnitGrid);
       */
      //getStatistics(annualDamageGrid, landuseGrid);
      /** ****************************************************************** */

      /*
       * read shapeFile
       * 
       * CS_CoordinateSystem srs = ConvenienceCSFactory.getInstance()
       * .getOGCCSByName("EPSG:31467"); GMLWorkspace workspace =
       * ShapeSerializer.deserialize(
       * "D://Nadja//floodRiskAnalysis//Test//Shape//landuse", srs, null);
       * Feature root = workspace.getRootFeature(); List featureList = (List)
       * root.getProperty("featureMember"); for (int k = 0; k <
       * featureList.size(); k++) { Feature actualFeature = (Feature)
       * featureList.get(k); FeatureType ft = actualFeature.getFeatureType();
       * FeatureTypeProperty[] ftps = ft.getProperties(); for (int i = 0; i <
       * ftps.length; i++) { String propertyName = ftps[i].getName();
       * System.out.println(propertyName + ": " +
       * actualFeature.getProperty(propertyName)); } }
       */

      /*
       * convert gem_nutzung.xml to gem_nutzungAsGrid
       * 
       * ArcGridConverter gridWriter = new ArcGridConverter(); RectifiedGrid
       * rectifiedGrid = gridWriter.importGridArc(new File(
       * "X://Diplomarbeit//RectifiedGrid//Kellinghusen_Test//wsp.asc"));
       * FeatureCollection fc = loadFeatureCollection(new File(
       * "X://Diplomarbeit//RectifiedGrid//Kellinghusen_Test//gem_nutzung.xml"));
       * System.out.println("FeatureCollection loaded."); String landuseProperty =
       * new String("NUTZUNG"); Hashtable landuseTable = new Hashtable(); int
       * counter = 1; for (int i = 0; i < fc.getSize(); i++) { Feature ft =
       * fc.getFeature(i); Object prop_landuse =
       * ft.getProperty(landuseProperty); if
       * (!landuseTable.containsKey(prop_landuse.toString())) {
       * System.out.println("PropertyValue: " + prop_landuse.toString() + "
       * counter=" + counter); landuseTable.put(prop_landuse.toString(), new
       * Integer( counter)); counter = counter + 1; } }
       * 
       * VectorToGridConverter converter = new VectorToGridConverter(fc,
       * landuseProperty, landuseTable, rectifiedGrid); RectifiedGrid rg_landuse =
       * converter.toGrid(); GridRasterView rasterView = new
       * GridRasterView(rg_landuse, 8); gridWriter.exportGridArc(new File(
       * "X://Diplomarbeit//RectifiedGrid//Kellinghusen_Test//gem_nutzungAsGrid.asc"),
       * rg_landuse);
       */

      /*
       * test ParseFunction
       * 
       * ParseFunction function = new ParseFunction("((6.4*x)+(4.9*x)+5)");
       * 
       * if(!function.parse()) { //this.showStatus("Failed to parse function!");
       * System.out.println("Failed to parse function!"); return; }
       * System.out.println("Result= "+function.getResult(2));
       */

    }
    catch( Exception e )
    {
      System.out.println( e );
    }
  }
}