package view;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.math.BigDecimal;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.List;
import java.util.TreeMap;
import java.util.Vector;

import javax.swing.JFileChooser;
import javax.swing.JOptionPane;

import org.kalypso.ogc.gml.serialize.ShapeSerializer;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureTypeProperty;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.cs.ConvenienceCSFactory;
import org.kalypsodeegree_impl.model.cv.RectifiedGridCoverage;
import org.opengis.cs.CS_CoordinateSystem;

import tools.Number;
import converter.ArcGridConverter;
import converter.VectorToGridConverter;
import damageAnalysis.ContextModel;
import damageAnalysis.DamageAnalysis;
import damageAnalysis.DamageAnalysisContext;
import damageAnalysis.DataModel;
import floodRiskAnalysis.FloodRiskAnalysis;
import floodRiskAnalysis.ResultModel;
import floodRiskAnalysis.RiskContextModel;

/**
 * MenuHandler for the FlooRiskAnalysisView
 * 
 * @author N. Peiler
 *  
 */
public class MenuHandler implements ActionListener
{

  private FloodRiskAnalysisView floodRiskFrame = null;

  /**
   * directory of the actual project
   */
  public String workingDir = null;

  /**
   * location of schema directory
   */
  public String schemaDir = "FloodRiskSchema";

  /**
   * damageAnalysisContext with assets, damageFunctions, landuseTypeList and
   * optional administrationUnitList for the actual project
   */
  public DamageAnalysisContext damageAnalysisContext = null;

  /**
   * flag, if WaterlevelDataModel.gml exists
   */
  boolean hasWaterlevelGML = false;

  /**
   * flag, if LanduseDataModel.gml exists
   */
  boolean hasLanduseGML = false;

  /**
   * flag, if AdministrationUnitDataModel.gml exists
   */
  boolean hasAdministrationUnitGML = false;

  private GridRasterView rasterView = null;

  private StatisticView statisticView = null;

  private SubstractGridsView substractGridsView = null;

  //private ResultContext resultContext = null;

  /**
   * constructs a MenuHandler for the given floodRiskAnalysisFrame
   */
  public MenuHandler( FloodRiskAnalysisView floodRiskFrame )
  {
    this.floodRiskFrame = floodRiskFrame;
  }

  /**
   * perform events
   */
  public void actionPerformed( ActionEvent event )
  {

    String ac = event.getActionCommand();

    if( ac.equals( "open project" ) )
    {
      LogView.println( "Open project..." );
      workingDir = getFileName( true, "Choose project directory...", JFileChooser.DIRECTORIES_ONLY );
      if( workingDir != null )
      {
        /*
         * schemaDir = getFileName(true, "Choose schema directory...",
         * JFileChooser.DIRECTORIES_ONLY); if (schemaDir != null) {
         */
        new Thread()
        {
          public void run()
          {
            try
            {
              openProject();
              LogView.println( "...finished." );
            }
            catch( Exception e )
            {
              System.out.println( e );
            }
          }
        }.start();
        /*
         * } else { LogView.println("...canceled"); }
         */
      }
      else
      {
        LogView.println( "...canceled" );
      }
    }

    if( ac.equals( "close project" ) )
    {
      workingDir = null;
      damageAnalysisContext = null;
      hasWaterlevelGML = false;
      hasLanduseGML = false;
      hasAdministrationUnitGML = false;
      if( rasterView != null && !rasterView.isClosed() )
      {
        rasterView.dispose();
      }
      if( statisticView != null && !statisticView.isClosed() )
      {
        statisticView.dispose();
      }
      floodRiskFrame.menuItem_open.setEnabled( true );
      floodRiskFrame.menuItem_close.setEnabled( false );
      floodRiskFrame.menuItem_waterlevel.setEnabled( false );
      floodRiskFrame.menuItem_landuse.setEnabled( false );
      floodRiskFrame.menuItem_adminUnit.setEnabled( false );
      floodRiskFrame.menuItem_damage.setEnabled( false );
      floodRiskFrame.menuItem_annualDamage.setEnabled( false );
      floodRiskFrame.menuItem_floodRisk.setEnabled( false );
      floodRiskFrame.menuItem_rasterView.setEnabled( false );
      floodRiskFrame.menuItem_statisticView.setEnabled( false );
      LogView.println( "Project closed." );
    }

    if( ac.equals( "convert waterlevel" ) )
    {
      new Thread()
      {
        public void run()
        {
          LogView.println( "Convert WaterlevelData..." );
          File waterlevelDirectory = new File( workingDir + "\\Waterlevel" );
          LogView.println( "Read WaterlevelGrids..." );
          TreeMap waterlevelGrids = readWaterlevelGrids( waterlevelDirectory );
          try
          {
            writeWaterlevelGrids( waterlevelGrids );
            LogView.println( "...finished" );
          }
          catch( Exception e )
          {
            System.out.println( e );
          }
        }
      }.start();
    }

    if( ac.equals( "convert landuse" ) )
    {
      new Thread()
      {
        public void run()
        {
          LogView.println( "Convert LanduseData..." );
          try
          {
            convertLanduse();
            LogView.println( "...finished" );
          }
          catch( Exception e )
          {
            System.out.println( e );
          }
        }
      }.start();
    }

    if( ac.equals( "convert adminUnit" ) )
    {
      new Thread()
      {
        public void run()
        {
          LogView.println( "Convert AdministrationUnitData..." );
          try
          {
            convertAdministrationUnit();
            LogView.println( "...finished" );
          }
          catch( Exception e )
          {
            System.out.println( e );
          }
        }
      }.start();
    }

    if( ac.equals( "calculate damage" ) )
    {
      new Thread()
      {
        public void run()
        {
          LogView.println( "Calculate Damage..." );
          try
          {
            calculateDamage();
            LogView.println( "...finished" );
          }
          catch( Exception e )
          {
            System.out.println( e );
          }
        }
      }.start();
    }

    if( ac.equals( "calculate annualDamage" ) )
    {
      new Thread()
      {
        public void run()
        {
          LogView.println( "Calculate AnnualDamage..." );
          try
          {
            calculateAnnualDamage();
            LogView.println( "...finished" );
          }
          catch( Exception e )
          {
            System.out.println( e );
          }
        }
      }.start();
    }

    if( ac.equals( "calculate floodRisk" ) )
    {
      new Thread()
      {
        public void run()
        {
          LogView.println( "Calculate FloodRisk..." );
          try
          {
            calculateRisk();
            LogView.println( "...finished" );
          }
          catch( Exception e )
          {
            System.out.println( e );
          }
        }
      }.start();
    }

    if( ac.equals( "open rasterView" ) )
    {
      rasterView = new GridRasterView( this );
      floodRiskFrame.desktop.add( rasterView );
      rasterView.moveToFront();
    }

    if( ac.equals( "open statisticView" ) )
    {
      statisticView = new StatisticView( this );
      floodRiskFrame.desktop.add( statisticView );
      statisticView.moveToFront();
    }

    if( ac.equals( "substractGrids" ) )
    {
      substractGridsView = new SubstractGridsView();
      floodRiskFrame.desktop.add( substractGridsView );
      substractGridsView.moveToFront();
    }

  }

  /**
   * invokes, when a new project is opened
   * 
   * @throws Exception
   */
  void openProject() throws Exception
  {
    floodRiskFrame.menuItem_open.setEnabled( false );
    floodRiskFrame.menuItem_close.setEnabled( true );
    LogView.println( "Project directory: " + workingDir );
    //LogView.println("Schema directory: " + schemaDir);

    damageAnalysisContext = new DamageAnalysisContext();
    File contextModelGML = new File( workingDir + "\\Control\\ContextModell.gml" );
    File contextModelSchema = new File( schemaDir + "\\ContextModell.xsd" );
    ContextModel contextModel = new ContextModel( contextModelGML, contextModelSchema );
    damageAnalysisContext.setLanduseTypeList( contextModel.getLanduseList() );
    damageAnalysisContext.setAdministrationUnitList( contextModel.getAdministrationUnitList() );
    damageAnalysisContext.setDamagefunctions( contextModel.getDamageFunctionList() );
    damageAnalysisContext.setAssets( contextModel.getAssetValueList() );

    File waterlevelDataModelGML = new File( workingDir + "\\Waterlevel\\WaterlevelDataModel.gml" );
    if( waterlevelDataModelGML.exists() )
    {
      floodRiskFrame.menuItem_waterlevel.setEnabled( false );
      /*
       * File waterlevelDataModelSchema = new File(schemaDir +
       * "\\WaterlevelDataModel.xsd"); TreeMap waterlevelGrids =
       * DataModel.createWaterlevelGrids( waterlevelDataModelGML,
       * waterlevelDataModelSchema);
       * damageAnalysisContext.setWaterlevelGrids(waterlevelGrids);
       */
      hasWaterlevelGML = true;
    }
    else
    {
      floodRiskFrame.menuItem_waterlevel.setEnabled( true );
      hasWaterlevelGML = false;
    }

    File landuseDataModelGML = new File( workingDir + "\\Landuse\\LanduseDataModel.gml" );
    if( landuseDataModelGML.exists() )
    {
      floodRiskFrame.menuItem_landuse.setEnabled( false );
      /*
       * File landuseDataModelSchema = new File(schemaDir +
       * "\\LanduseDataModel.xsd"); RectifiedGridCoverage landuseGrid =
       * DataModel.createLanduseGrid( landuseDataModelGML,
       * landuseDataModelSchema);
       * damageAnalysisContext.setLanduseGrid(landuseGrid);
       */
      hasLanduseGML = true;
    }
    else
    {
      hasLanduseGML = false;
      if( hasWaterlevelGML )
      {
        floodRiskFrame.menuItem_landuse.setEnabled( true );
      }
    }

    if( damageAnalysisContext.getAdministrationUnitList() != null )
    {
      File administrationUnitDataModelGML = new File( workingDir
          + "\\AdministrationUnit\\AdministrationUnitDataModel.gml" );
      if( administrationUnitDataModelGML.exists() )
      {
        floodRiskFrame.menuItem_adminUnit.setEnabled( false );
        /*
         * File administrationUnitDataModelSchema = new File(schemaDir +
         * "\\AdministrationUnitDataModel.xsd"); RectifiedGridCoverage
         * adminUnitGrid = DataModel .createAdministrationUnitGrid(
         * administrationUnitDataModelGML, administrationUnitDataModelSchema);
         * damageAnalysisContext.setAdministrationUnitGrid(adminUnitGrid);
         */
        hasAdministrationUnitGML = true;
      }
      else
      {
        if( hasWaterlevelGML )
        {
          floodRiskFrame.menuItem_adminUnit.setEnabled( true );
        }
        hasAdministrationUnitGML = false;
      }
    }

    if( damageAnalysisContext.getAdministrationUnitList() != null )
    {
      if( hasWaterlevelGML && hasLanduseGML && hasAdministrationUnitGML )
      {
        floodRiskFrame.menuItem_damage.setEnabled( true );
        File damageResultModelGML = new File( workingDir + "\\Damage\\DamageResultModel.gml" );
        /*
         * int numWaterlevelGrids = damageAnalysisContext
         * .getWaterlevelGrids().size();
         */
        //if (damageResultModelGML.exists() && numWaterlevelGrids > 1)
        // {
        if( damageResultModelGML.exists() )
        {
          floodRiskFrame.menuItem_annualDamage.setEnabled( true );
        }
      }
    }
    else
    {
      if( hasWaterlevelGML && hasLanduseGML )
      {
        floodRiskFrame.menuItem_damage.setEnabled( true );
        File damageResultModelGML = new File( workingDir + "\\Damage\\DamageResultModel.gml" );
        /*
         * int numWaterlevelGrids = damageAnalysisContext
         * .getWaterlevelGrids().size();
         */
        //if (damageResultModelGML.exists() && numWaterlevelGrids > 1)
        // {
        if( damageResultModelGML.exists() )
        {
          floodRiskFrame.menuItem_annualDamage.setEnabled( true );
        }
      }
    }

    //resultContext = new ResultContext();
    File damageResultModelGML = new File( workingDir + "\\Damage\\DamageResultModel.gml" );
    if( damageResultModelGML.exists() )
    {
      /*
       * File damageResultModelSchema = new File(schemaDir +
       * "\\DamageResultModel.xsd"); TreeMap damageGrids =
       * ResultModel.createDamageGrids( damageResultModelGML,
       * damageResultModelSchema); resultContext.setDamagegrids(damageGrids);
       */
      floodRiskFrame.menuItem_damage.setEnabled( false );
    }
    File annualDamageResultModelGML = new File( workingDir
        + "\\Damage\\AnnualDamageResultModel.gml" );
    if( annualDamageResultModelGML.exists() )
    {
      /*
       * File annualDamageResultModelSchema = new File(schemaDir +
       * "\\AnnualDamageResultModel.xsd"); RectifiedGridCoverage
       * annualDamageGrid = ResultModel
       * .createAnnualDamageGrid(annualDamageResultModelGML,
       * annualDamageResultModelSchema);
       * resultContext.setAnnualDamageGrid(annualDamageGrid);
       */
      floodRiskFrame.menuItem_annualDamage.setEnabled( false );
      if( hasLanduseGML )
      {
        floodRiskFrame.menuItem_floodRisk.setEnabled( true );
      }
    }

    File floodRiskResultModelGML = new File( workingDir + "\\Risk\\FloodRiskResultModel.gml" );
    if( floodRiskResultModelGML.exists() )
    {
      /*
       * File floodRiskResultModelSchema = new File(schemaDir +
       * "\\FloodRiskResultModel.xsd"); RectifiedGridCoverage floodRiskGrid =
       * ResultModel .createFloodRiskGrid(floodRiskResultModelGML,
       * floodRiskResultModelSchema);
       * resultContext.setFloodRiskGrid(floodRiskGrid);
       */
      floodRiskFrame.menuItem_floodRisk.setEnabled( false );
    }

    floodRiskFrame.menuItem_rasterView.setEnabled( true );
    floodRiskFrame.menuItem_statisticView.setEnabled( true );
  }

  /**
   * reads the asciiGrids (wsp_hqxx.asc) of the waterlevelDirectory
   * 
   * @param waterlevelDirectory
   * @return TreeMap of waterlevelGrids
   */
  TreeMap readWaterlevelGrids( File waterlevelDirectory )
  {
    ArcGridConverter gridConverter = new ArcGridConverter();
    TreeMap waterlevelGrids = gridConverter.importwaterlevelGrids( waterlevelDirectory );
    //damageAnalysisContext.setWaterlevelGrids(waterlevelGrids);
    floodRiskFrame.menuItem_waterlevel.setEnabled( false );
    if( damageAnalysisContext.getAdministrationUnitList() != null )
    {
      if( hasLanduseGML && hasAdministrationUnitGML )
      {
        floodRiskFrame.menuItem_damage.setEnabled( true );
      }
      else
      {
        if( !hasLanduseGML )
        {
          floodRiskFrame.menuItem_landuse.setEnabled( true );
        }
        if( !hasAdministrationUnitGML )
        {
          floodRiskFrame.menuItem_adminUnit.setEnabled( true );
        }
      }
    }
    else
    {
      if( hasLanduseGML )
      {
        floodRiskFrame.menuItem_damage.setEnabled( true );
      }
      else
      {
        floodRiskFrame.menuItem_landuse.setEnabled( true );
      }
    }
    return waterlevelGrids;
  }

  /**
   * stores the data of a TreeMap of waterlevelGrids in WaterlevelDataModel.gml
   * 
   * @param waterlevelGrids
   * @throws Exception
   */
  void writeWaterlevelGrids( TreeMap waterlevelGrids ) throws Exception
  {
    LogView.println("Control WaterlevelGrids...");
    // control Geometries
    Object[] keys = waterlevelGrids.keySet().toArray();
    for( int i = 0; i < keys.length; i++ )
    {
      Double key = (Double)keys[i];
      if( i < keys.length - 1 )
      {
        Double nextKey = (Double)keys[i + 1];
        RectifiedGridCoverage grid = (RectifiedGridCoverage)waterlevelGrids.get( key );
        RectifiedGridCoverage nextGrid = (RectifiedGridCoverage)waterlevelGrids.get( nextKey );
        DamageAnalysis.controlGridGeometries( grid.getGridDomain(), nextGrid.getGridDomain() );
      }
    }
    File waterlevelDataModelGML = new File( workingDir + "\\Waterlevel\\WaterlevelDataModel.gml" );
    DataModel.writeWaterlevelData( waterlevelDataModelGML, waterlevelGrids );
    hasWaterlevelGML = true;
    waterlevelGrids = null;
  }

  /**
   * converts the landuseShapeFile in Landuse-Directory to a landuseGrid
   * (RectifiedGridCoverage) and stores it in LanduseDataModel.gml and
   * landuse.asc
   * 
   * @throws Exception
   */
  void convertLanduse() throws Exception
  {
    String landuseShapeBaseFile = workingDir + "\\Landuse\\landuse";
    List landuseFeatureList = getFeatureList( landuseShapeBaseFile );
    //String landusePropertyName = "NUTZUNG";
    Feature landuseFeature = (Feature)landuseFeatureList.get( 0 );
    FeatureTypeProperty[] ftps = landuseFeature.getFeatureType().getProperties();
    String landusePropertyName = openfeaturePropertyDialog( ftps );

    if( landusePropertyName != null )
    {
      //TreeMap waterlevelGrids =
      // damageAnalysisContext.getWaterlevelGrids();
      File waterlevelDataModelGML = new File( workingDir + "\\Waterlevel\\WaterlevelDataModel.gml" );
      File waterlevelDataModelSchema = new File( schemaDir + "\\WaterlevelDataModel.xsd" );
      TreeMap waterlevelGrids = DataModel.createWaterlevelGrids( waterlevelDataModelGML,
          waterlevelDataModelSchema );
      VectorToGridConverter converter = new VectorToGridConverter();
      RectifiedGridCoverage landuseGrid = converter.landuseToGrid( landuseFeatureList,
          damageAnalysisContext.getLanduseTypeList(), landusePropertyName, waterlevelGrids );
      //damageAnalysisContext.setLanduseGrid(landuseGrid);
      floodRiskFrame.menuItem_landuse.setEnabled( false );

      File landuseDataModelGML = new File( workingDir + "\\Landuse\\LanduseDataModel.gml" );
      writeLanduseData( landuseDataModelGML, landuseGrid );
      hasLanduseGML = true;
      ArcGridConverter gridConverter = new ArcGridConverter();
      File landuseGridAsc = new File( workingDir + "\\Landuse\\landuse.asc" );
      gridConverter.exportGridArc( landuseGridAsc, landuseGrid );
      landuseGrid = null;

      if( damageAnalysisContext.getAdministrationUnitList() != null )
      {
        if( hasAdministrationUnitGML )
        {
          floodRiskFrame.menuItem_damage.setEnabled( true );
        }
      }
      else
      {
        floodRiskFrame.menuItem_damage.setEnabled( true );
      }
    }
  }

  private void writeLanduseData( File landuseDataModelGML, RectifiedGridCoverage landuseGrid )
      throws Exception
  {
    DataModel.writeLanduseData( landuseDataModelGML, landuseGrid );
  }

  /**
   * converts the administrationUnitShapeFile in AdministrationUnit-Directory to
   * an administrationUnitGrid (RectifiedGridCoverage) and stores it in
   * AdministrationUnitDataModel.gml and administrationUnit.asc
   * 
   * @throws Exception
   */
  void convertAdministrationUnit() throws Exception
  {
    String administrationUnitShapeBaseFile = workingDir
        + "\\AdministrationUnit\\administrationUnit";
    List adminUnitFeatureList = getFeatureList( administrationUnitShapeBaseFile );
    //String administrationUnitPropertyName = "GEMEINDE";
    Feature adminUnitFeature = (Feature)adminUnitFeatureList.get( 0 );
    FeatureTypeProperty[] ftps = adminUnitFeature.getFeatureType().getProperties();
    String administrationUnitPropertyName = openfeaturePropertyDialog( ftps );

    if( administrationUnitPropertyName != null )
    {
      //			TreeMap waterlevelGrids =
      // damageAnalysisContext.getWaterlevelGrids();
      File waterlevelDataModelGML = new File( workingDir + "\\Waterlevel\\WaterlevelDataModel.gml" );
      File waterlevelDataModelSchema = new File( schemaDir + "\\WaterlevelDataModel.xsd" );
      TreeMap waterlevelGrids = DataModel.createWaterlevelGrids( waterlevelDataModelGML,
          waterlevelDataModelSchema );
      VectorToGridConverter converter = new VectorToGridConverter();
      RectifiedGridCoverage administrationUnitGrid = converter.administrationUnitToGrid(
          adminUnitFeatureList, damageAnalysisContext.getAdministrationUnitList(),
          administrationUnitPropertyName, waterlevelGrids );
      /*
       * damageAnalysisContext
       * .setAdministrationUnitGrid(administrationUnitGrid);
       */
      floodRiskFrame.menuItem_adminUnit.setEnabled( false );

      File administrationUnitDataModelGML = new File( workingDir
          + "\\AdministrationUnit\\AdministrationUnitDataModel.gml" );
      writeAdministrationUnitData( administrationUnitDataModelGML, administrationUnitGrid );
      hasAdministrationUnitGML = true;
      ArcGridConverter gridConverter = new ArcGridConverter();
      File administrationUnitGridAsc = new File( workingDir
          + "\\AdministrationUnit\\administrationUnit.asc" );
      gridConverter.exportGridArc( administrationUnitGridAsc, administrationUnitGrid );
      administrationUnitGrid = null;
      if( hasLanduseGML )
      {
        floodRiskFrame.menuItem_damage.setEnabled( true );
      }
    }
  }

  private void writeAdministrationUnitData( File administrationUnitDataModelGML,
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
  private List getFeatureList( String shapeFileBase )
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

  /**
   * opens a dialog for choosing a featureTypeProperty of a shapeFile
   * 
   * @param ftps
   *          featureTypeProperties of the shapeFile
   * @return name of choosen featureTypeProperty
   */
  private String openfeaturePropertyDialog( FeatureTypeProperty[] ftps )
  {
    Object[] possibilities = new Object[ftps.length];
    for( int i = 0; i < possibilities.length; i++ )
    {
      //System.out.println(ftps[i].getName());
      possibilities[i] = ftps[i].getName();
    }
    String propertyName = (String)JOptionPane.showInputDialog( floodRiskFrame,
        "Choose property name: ", "Selection", JOptionPane.PLAIN_MESSAGE, null, possibilities,
        possibilities[0] );
    return propertyName;
  }

  /**
   * calculates the DamageGrids for the actual project and stores the reults in
   * DamageResultModel.gml and in an ascii-file for each HQ
   * 
   * @throws Exception
   */
  void calculateDamage() throws Exception
  {
    String damageDirectory = workingDir + "\\Damage\\";
    ArcGridConverter gridConverter = new ArcGridConverter();

    File waterlevelDataModelGML = new File( workingDir + "\\Waterlevel\\WaterlevelDataModel.gml" );
    File waterlevelDataModelSchema = new File( schemaDir + "\\WaterlevelDataModel.xsd" );
    TreeMap waterlevelGrids = DataModel.createWaterlevelGrids( waterlevelDataModelGML,
        waterlevelDataModelSchema );
    damageAnalysisContext.setWaterlevelGrids( waterlevelGrids );

    File landuseDataModelGML = new File( workingDir + "\\Landuse\\LanduseDataModel.gml" );
    File landuseDataModelSchema = new File( schemaDir + "\\LanduseDataModel.xsd" );
    RectifiedGridCoverage landuseGrid = DataModel.createLanduseGrid( landuseDataModelGML,
        landuseDataModelSchema );
    damageAnalysisContext.setLanduseGrid( landuseGrid );

    File administrationUnitDataModelGML = new File( workingDir
        + "\\AdministrationUnit\\AdministrationUnitDataModel.gml" );
    RectifiedGridCoverage adminUnitGrid = null;
    if( damageAnalysisContext.getAdministrationUnitList() != null )
    {
      File administrationUnitDataModelSchema = new File( schemaDir
          + "\\AdministrationUnitDataModel.xsd" );
      adminUnitGrid = DataModel.createAdministrationUnitGrid( administrationUnitDataModelGML,
          administrationUnitDataModelSchema );
      damageAnalysisContext.setAdministrationUnitGrid( adminUnitGrid );
    }

    TreeMap damagePercentageGrids = DamageAnalysis
        .calculateDamagePercentages( damageAnalysisContext );

    TreeMap damageGrids = DamageAnalysis.calculateDamages( damagePercentageGrids,
        damageAnalysisContext );
    //resultContext.setDamagegrids(damageGrids);
    floodRiskFrame.menuItem_damage.setEnabled( false );
    File damageResultModelGML = new File( damageDirectory + "\\DamageResultModel.gml" );
    ResultModel.writeDamageData( damageResultModelGML, damageGrids );
    Iterator it = damageGrids.keySet().iterator();
    while( it.hasNext() )
    {
      Double key = (Double)it.next();
      RectifiedGridCoverage damageGrid = (RectifiedGridCoverage)damageGrids.get( key );
      double annuality = 1 / key.doubleValue();
      String fileName = damageDirectory + "damage_HQ" + (int)annuality + ".asc";
      gridConverter.exportGridArc( new File( fileName ), damageGrid );
    }
    waterlevelGrids = null;
    landuseGrid = null;
    adminUnitGrid = null;
    damageGrids = null;
    floodRiskFrame.menuItem_annualDamage.setEnabled( true );
  }

  /**
   * calculates the annualDamageGrid of the actualProject and stores the result
   * in AnnualDamageResultModel.gml and annualDamage.asc, furthermore the
   * intermediate results are stored in ascii-files (tempGrid_deltaPxx.asc)
   * 
   * @throws Exception
   */
  void calculateAnnualDamage() throws Exception
  {

    String damageDirectory = workingDir + "\\Damage\\";
    ArcGridConverter gridConverter = new ArcGridConverter();

    //TreeMap damageGrids = resultContext.getDamagegrids();
    File damageResultModelGML = new File( workingDir + "\\Damage\\DamageResultModel.gml" );
    File damageResultModelSchema = new File( schemaDir + "\\DamageResultModel.xsd" );
    TreeMap damageGrids = ResultModel.createDamageGrids( damageResultModelGML,
        damageResultModelSchema );
    Vector tempGrids = DamageAnalysis.calculateTempGridsAnnualDamage( damageGrids );
    LogView.println( "Write TempGrids..." );
    Object[] keys = damageGrids.keySet().toArray();
    for( int i = 0; i < keys.length; i++ )
    {
      Double key = (Double)keys[i];
      if( i < keys.length - 1 )
      {
        Double nextKey = (Double)keys[i + 1];
        double deltaP = nextKey.doubleValue() - key.doubleValue();
        LogView.println( "deltaP=" + Number.round( deltaP, 4, BigDecimal.ROUND_HALF_EVEN ) );
        RectifiedGridCoverage tempGrid_rg = (RectifiedGridCoverage)tempGrids.get( i );
        String fileName = damageDirectory + "\\tempGrid_deltaP"
            + Number.round( deltaP, 4, BigDecimal.ROUND_HALF_EVEN ) + ".asc";
        gridConverter.exportGridArc( new File( fileName ), tempGrid_rg );
      }
    }

    RectifiedGridCoverage annualDamageGrid = DamageAnalysis.calculateAnnualDamage( tempGrids );
    tempGrids = null;
    //resultContext.setAnnualDamageGrid(annualDamageGrid);
    floodRiskFrame.menuItem_annualDamage.setEnabled( false );
    File annualDamageResultModelGML = new File( damageDirectory + "\\AnnualDamageResultModel.gml" );
    ResultModel.writeAnnualDamageData( annualDamageResultModelGML, annualDamageGrid );
    gridConverter.exportGridArc( new File( damageDirectory + "\\annualDamage.asc" ),
        annualDamageGrid );
    annualDamageGrid = null;
    floodRiskFrame.menuItem_floodRisk.setEnabled( true );
  }

  /**
   * calculates the floodRiskGrid of the actualProject and stores the result in
   * FloodRiskResultModel.gml and riskGrid.asc
   * 
   * @throws Exception
   */
  void calculateRisk() throws Exception
  {
    ArcGridConverter gridConverter = new ArcGridConverter();
    //String damageDirectory = workingDir + "\\Damage\\";
    String riskDirectory = workingDir + "\\Risk\\";

    File riskContextModelGML = new File( workingDir + "\\Control\\RiskContextModell.gml" );
    File riskContextModelSchema = new File( schemaDir + "\\RiskContextModell.xsd" );
    RiskContextModel riskContextModel = new RiskContextModel( riskContextModelGML,
        riskContextModelSchema );

    Hashtable riskClassTable = riskContextModel.getRiskClassLists();

    File annualDamageResultModelGML = new File( workingDir
        + "\\Damage\\AnnualDamageResultModel.gml" );
    File annualDamageResultModelSchema = new File( schemaDir + "\\AnnualDamageResultModel.xsd" );
    RectifiedGridCoverage annualDamageGrid = ResultModel.createAnnualDamageGrid(
        annualDamageResultModelGML, annualDamageResultModelSchema );
    //RectifiedGridCoverage
    // annualDamageGrid=resultContext.getAnnualDamageGrid();

    File landuseDataModelGML = new File( workingDir + "\\Landuse\\LanduseDataModel.gml" );
    File landuseDataModelSchema = new File( schemaDir + "\\LanduseDataModel.xsd" );
    RectifiedGridCoverage landuseGrid = DataModel.createLanduseGrid( landuseDataModelGML,
        landuseDataModelSchema );
    //RectifiedGridCoverage landuseGrid =
    // damageAnalysisContext.getLanduseGrid();

    RectifiedGridCoverage floodRiskGrid = FloodRiskAnalysis.defineRisk( annualDamageGrid,
        landuseGrid, riskClassTable );
    //resultContext.setFloodRiskGrid(floodRiskGrid);
    floodRiskFrame.menuItem_floodRisk.setEnabled( false );
    File floodRiskResultModelGML = new File( riskDirectory + "\\FloodRiskResultModel.gml" );
    ResultModel.writeFloodRiskData( floodRiskResultModelGML, floodRiskGrid );
    gridConverter.exportGridArc( new File( riskDirectory + "\\riskGrid.asc" ), floodRiskGrid );
    annualDamageGrid = null;
    landuseGrid = null;
    floodRiskGrid = null;
  }

  /**
   * returns the filename that has been selected by the file chooser dialog that
   * will be called by this method.
   */
  private String lastDir = ".";

  public String getFileName( boolean load, String dialogTitle, int fileSelectionMode )
  {
    String filename = null;
    int returnVal = -1;
    JFileChooser chooser = new JFileChooser();
    chooser.setDialogTitle( dialogTitle );
    chooser.setFileSelectionMode( fileSelectionMode );
    chooser.setCurrentDirectory( new File( lastDir ) );

    if( load )
    {
      //chooser.setFileFilter( new DeegreeFileFilter( ext ) );
      returnVal = chooser.showOpenDialog( floodRiskFrame );
    }
    else
    {
      //chooser.setFileFilter( new DeegreeFileFilter( ext ) );
      returnVal = chooser.showSaveDialog( floodRiskFrame );
    }
    if( returnVal == JFileChooser.APPROVE_OPTION )
    {
      filename = chooser.getSelectedFile().getPath();
      lastDir = chooser.getSelectedFile().getPath();
    }
    return filename;

  }

}