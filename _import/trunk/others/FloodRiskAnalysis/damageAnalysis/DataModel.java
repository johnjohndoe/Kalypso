package damageAnalysis;

import java.io.File;
import java.util.Iterator;
import java.util.List;
import java.util.TreeMap;

import org.deegree.model.feature.Feature;
import org.deegree.model.feature.GMLWorkspace;
import org.deegree.xml.XMLTools;
import org.deegree_impl.model.cv.RectifiedGridCoverage;
import org.deegree_impl.model.cv.RectifiedGridCoverageFactory;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import tools.XmlServiceTools;
import view.LogView;

/**
 * Class for reading and writing ModelData like LanduseData,
 * AdministrationUnitData and WaterlevelData
 * 
 * @author N. Peiler
 *  
 */
public class DataModel
{

  private static String NSGML = "http://www.opengis.net/gml";

  private static String NSHWS = "http://elbe.wb.tu-harburg.de/floodrisk";

  private static String NSRGC = "http://elbe.wb.tu-harburg.de/rectifiedGridCoverage";

  /**
   * creates a LanduseGrid of a LanduseDataModel
   * 
   * @param landuseDataModelGML
   *          Instance location of a LanduseDataModel
   * @param landuseDataModelSchema
   *          Schema location of the LanduseDataModel
   * @return LanduseGrid
   * @throws Exception
   */
  public static RectifiedGridCoverage createLanduseGrid( File landuseDataModelGML,
      File landuseDataModelSchema ) throws Exception
  {
    LogView.println( "Create LanduseGrid..." );
    GMLWorkspace gmlWorkspace = GmlSerializer.createGMLWorkspace( landuseDataModelGML.toURL(),
        landuseDataModelSchema.toURL() );
    Feature rootFeature = gmlWorkspace.getRootFeature();
    Feature landuse = (Feature)rootFeature.getProperty( "LanduseMember" );
    RectifiedGridCoverage landuseGrid = RectifiedGridCoverageFactory
        .createRectifiedGridCoverage( landuse );
    return landuseGrid;
  }

  /**
   * writes the LanduseData to a gml-file
   * 
   * @param landuseDataModelGML
   *          Instance location of a LanduseDataModel
   * @param landuseGrid
   * @throws Exception
   */
  public static void writeLanduseData( File landuseDataModelGML, RectifiedGridCoverage landuseGrid )
      throws Exception
  {
    LogView.println( "Write " + landuseDataModelGML.getName() + "..." );
    Document doc = XMLTools.create();

    Element root = doc.createElement( "LanduseDataModel" );
    root.setAttribute( "xmlns", NSHWS );
    root.setAttribute( "xmlns:gml", NSGML );
    root.setAttribute( "xmlns:rgc", NSRGC );

    Element e_landuseMember = doc.createElement( "LanduseMember" );
    Element e_landuse = doc.createElement( "Landuse" );
    if( landuseGrid.getRangeSet().getRangeSetDataFile() == null )
    {
      String rangeSetFileName = landuseDataModelGML.getParent() + "/landuseRangeSetData.dat";
      landuseGrid.getRangeSet().setRangeSetDataFile( new File( rangeSetFileName ) );
    }
    RectifiedGridCoverageFactory.writeRectifiedGridCoverage( landuseGrid, e_landuse );
    e_landuseMember.appendChild( e_landuse );
    root.appendChild( e_landuseMember );

    doc.appendChild( root );
    XmlServiceTools.toFile( landuseDataModelGML, doc );
  }

  /**
   * creates an AdministrationUnitGrid of an AdministrationUnitModel
   * 
   * @param administrationUnitDataModelGML
   *          Instance location of an AdministrationUnitDataModel
   * @param administrationUnitDataModelSchema
   *          Schema location of the AdministrationUnitDataModel
   * @return Administration UnitDataModel
   * @throws Exception
   */
  public static RectifiedGridCoverage createAdministrationUnitGrid(
      File administrationUnitDataModelGML, File administrationUnitDataModelSchema )
      throws Exception
  {
    LogView.println( "Create AdministrationUnitGrid..." );
    GMLWorkspace gmlWorkspace = GmlSerializer.createGMLWorkspace( administrationUnitDataModelGML
        .toURL(), administrationUnitDataModelSchema.toURL() );
    Feature rootFeature = gmlWorkspace.getRootFeature();
    Feature administrationUnit = (Feature)rootFeature.getProperty( "AdministrationUnitMember" );
    RectifiedGridCoverage administrationUnitGrid = RectifiedGridCoverageFactory
        .createRectifiedGridCoverage( administrationUnit );
    return administrationUnitGrid;
  }

  /**
   * writes the administrationUnitData to a file
   * 
   * @param administrationUnitDataModelGML
   *          Instance location of an AdministrationUnitDataModel
   * @throws Exception
   */
  public static void writeAdministrationUnitData( File administrationUnitDataModelGML,
      RectifiedGridCoverage administrationUnitGrid ) throws Exception
  {
    LogView.println( "Write " + administrationUnitDataModelGML.getName() + "..." );
    Document doc = XMLTools.create();

    Element root = doc.createElement( "AdministrationUnitDataModel" );
    root.setAttribute( "xmlns", NSHWS );
    root.setAttribute( "xmlns:gml", NSGML );
    root.setAttribute( "xmlns:rgc", NSRGC );

    Element e_administrationUnitMember = doc.createElement( "AdministrationUnitMember" );
    Element e_administrationUnit = doc.createElement( "AdministrationUnit" );
    if( administrationUnitGrid.getRangeSet().getRangeSetDataFile() == null )
    {
      String rangeSetFileName = administrationUnitDataModelGML.getParent()
          + "/administrationUnitRangeSetData.dat";
      administrationUnitGrid.getRangeSet().setRangeSetDataFile( new File( rangeSetFileName ) );
    }
    RectifiedGridCoverageFactory.writeRectifiedGridCoverage( administrationUnitGrid,
        e_administrationUnit );
    e_administrationUnitMember.appendChild( e_administrationUnit );
    root.appendChild( e_administrationUnitMember );

    doc.appendChild( root );
    XmlServiceTools.toFile( administrationUnitDataModelGML, doc );
  }

  /**
   * creates a TreeMap of WaterlevelGrids of WaterlevelDataModel
   * 
   * @param waterlevelDataModelGML
   *          Instance location of waterlevelDataModel
   * @param waterlevelDataModelSchema
   *          Schema location of WaterlevelDataModel
   * @return TreeMap of waterlevelGrids (key=annuality,
   *         value=RectifiedGridCoverage)
   * @throws Exception
   */
  public static TreeMap createWaterlevelGrids( File waterlevelDataModelGML,
      File waterlevelDataModelSchema ) throws Exception
  {
    LogView.println( "Create WaterlevelGrids..." );
    TreeMap waterlevelGrids = new TreeMap();
    GMLWorkspace gmlWorkspace = GmlSerializer.createGMLWorkspace( waterlevelDataModelGML.toURL(),
        waterlevelDataModelSchema.toURL() );
    Feature rootFeature = gmlWorkspace.getRootFeature();
    Feature waterlevelFeatureCollection = (Feature)rootFeature
        .getProperty( "WaterlevelCollectionMember" );
    List waterlevels = (List)waterlevelFeatureCollection.getProperty( "WaterlevelMember" );
    for( int i = 0; i < waterlevels.size(); i++ )
    {
      Feature waterlevel = (Feature)waterlevels.get( i );
      Double annuality = (Double)waterlevel.getProperty( "Annuality" );
      double years = 1 / ( annuality.doubleValue() );
      LogView.println( "HQ " + ( new Double( years ) ).intValue() );
      RectifiedGridCoverage waterlevelGrid = createWaterlevelGrid( waterlevel );
      waterlevelGrids.put( annuality, waterlevelGrid );
    }
    return waterlevelGrids;
  }

  /**
   * creates a WaterlevelGrid of a WaterlevelMember(Feature) of the
   * WaterlevelDataModel
   * 
   * @param waterlevel
   *          WaterlevelMember(Feature) of WaterlevelDataModel
   * @return WaterlevelGrid
   * @throws Exception
   */
  public static RectifiedGridCoverage createWaterlevelGrid( Feature waterlevel ) throws Exception
  {
    RectifiedGridCoverage waterlevelGrid = RectifiedGridCoverageFactory
        .createRectifiedGridCoverage( waterlevel );
    return waterlevelGrid;
  }

  /**
   * writes the WaterlevelData to a gml-file
   * 
   * @param waterlevelDataModelGML
   *          Instance location of waterlevelDataModel
   * @param waterlevelGrids
   *          TreeMap of waterlevelGrids (key=annuality,
   *          value=RectifiedGridCoverage)
   * @throws Exception
   */
  public static void writeWaterlevelData( File waterlevelDataModelGML, TreeMap waterlevelGrids )
      throws Exception
  {
    LogView.println( "Write " + waterlevelDataModelGML.getName() + "..." );
    Document doc = XMLTools.create();

    Element root = doc.createElement( "WaterlevelDataModel" );
    root.setAttribute( "xmlns", NSHWS );
    root.setAttribute( "xmlns:gml", NSGML );
    root.setAttribute( "xmlns:rgc", NSRGC );

    Element e_waterlevelCollectionMember = doc.createElement( "WaterlevelCollectionMember" );
    Element e_waterlevelCollection = doc.createElement( "WaterlevelCollection" );
    Iterator it = waterlevelGrids.keySet().iterator();
    while( it.hasNext() )
    {
      Double key = (Double)it.next();
      RectifiedGridCoverage waterlevelGrid = (RectifiedGridCoverage)waterlevelGrids.get( key );
      Element e_waterlevelMember = doc.createElement( "WaterlevelMember" );
      double years = 1 / key.doubleValue();
      LogView.println( "HQ " + ( new Double( years ) ).intValue() );
      if( waterlevelGrid.getRangeSet().getRangeSetDataFile() == null )
      {
        String rangeSetFileName = waterlevelDataModelGML.getParent() + "/waterlevelRangeSetData_HQ"
            + ( new Double( years ) ).intValue() + ".dat";
        waterlevelGrid.getRangeSet().setRangeSetDataFile( new File( rangeSetFileName ) );
      }
      writeWaterlevelGrid( e_waterlevelMember, doc, waterlevelGrid, key );
      e_waterlevelCollection.appendChild( e_waterlevelMember );
    }
    e_waterlevelCollectionMember.appendChild( e_waterlevelCollection );

    root.appendChild( e_waterlevelCollectionMember );

    doc.appendChild( root );
    XmlServiceTools.toFile( waterlevelDataModelGML, doc );
  }

  /**
   * writes the Data of a WaterlevelGrid to file
   * 
   * @param e_waterlevelMember
   *          xml-Element, which stores Data of the WaterlevelGrid
   * @param doc
   *          xml-Document, which stores WaterlevelData
   * @param waterlevelGrid
   * @throws Exception
   */
  public static void writeWaterlevelGrid( Element e_waterlevelMember, Document doc,
      RectifiedGridCoverage waterlevelGrid, Double annuality ) throws Exception
  {

    Element e_waterlevel = doc.createElement( "Waterlevel" );
    RectifiedGridCoverageFactory.writeRectifiedGridCoverage( waterlevelGrid, e_waterlevel );
    Element e_annuality = doc.createElement( "Annuality" );
    String stringAnnuality = annuality.toString();
    e_annuality.appendChild( doc.createTextNode( stringAnnuality ) );
    e_waterlevel.appendChild( e_annuality );
    e_waterlevelMember.appendChild( e_waterlevel );
  }
}