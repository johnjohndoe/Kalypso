package floodRiskAnalysis;

import java.io.File;
import java.util.Iterator;
import java.util.List;
import java.util.TreeMap;

import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.xml.XMLTools;
import org.kalypsodeegree_impl.model.cv.RectifiedGridCoverage;
import org.kalypsodeegree_impl.model.cv.RectifiedGridCoverageFactory;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import tools.XmlServiceTools;
import view.LogView;

/**
 * Class for reading and writing ResultData like DamageData, AnnualDamageData
 * and FloodRiskData
 * 
 * @author N. Peiler
 *  
 */
public class ResultModel
{

  private static String NSGML = "http://www.opengis.net/gml";

  private static String NSHWS = "http://elbe.wb.tu-harburg.de/floodrisk";

  private static String NSRGC = "http://elbe.wb.tu-harburg.de/rectifiedGridCoverage";

  /**
   * creates an FloodRiskGrid of a FloodRiskResultModel
   * 
   * @param floodRiskResultModelGML
   *          Instance location of a FloodRiskResultModel
   * @param floodRiskResultModelSchema
   *          Schema location of the FloodRiskResultModel
   * @return FloodRiskGrid
   * @throws Exception
   */
  public static RectifiedGridCoverage createFloodRiskGrid( File floodRiskResultModelGML,
      File floodRiskResultModelSchema ) throws Exception
  {
    LogView.println( "Create FloodRiskGrid..." );
    GMLWorkspace gmlWorkspace = GmlSerializer.createGMLWorkspace( floodRiskResultModelGML.toURL(),
        floodRiskResultModelSchema.toURL() );
    Feature rootFeature = gmlWorkspace.getRootFeature();
    Feature floodRisk = (Feature)rootFeature.getProperty( "FloodRiskMember" );
    RectifiedGridCoverage floodRiskGrid = RectifiedGridCoverageFactory
        .createRectifiedGridCoverage( floodRisk );
    return floodRiskGrid;
  }

  /**
   * writes the FloodRiskData to a gml-file
   * 
   * @param floodRiskResultModelGML
   *          Instance location of a FloodRiskResultModel
   * @param floodRiskGrid
   * @throws Exception
   */
  public static void writeFloodRiskData( File floodRiskResultModelGML,
      RectifiedGridCoverage floodRiskGrid ) throws Exception
  {
    LogView.println( "Write " + floodRiskResultModelGML.getName() + "..." );
    Document doc = XMLTools.create();

    Element root = doc.createElement( "FloodRiskResultModel" );
    root.setAttribute( "xmlns", NSHWS );
    root.setAttribute( "xmlns:gml", NSGML );
    root.setAttribute( "xmlns:rgc", NSRGC );

    Element e_floodRiskMember = doc.createElement( "FloodRiskMember" );
    Element e_floodRisk = doc.createElement( "FloodRisk" );
    if( floodRiskGrid.getRangeSet().getRangeSetDataFile() == null )
    {
      String rangeSetFileName = floodRiskResultModelGML.getParent() + "/floodRiskRangeSetData.dat";
      floodRiskGrid.getRangeSet().setRangeSetDataFile( new File( rangeSetFileName ) );
    }
    RectifiedGridCoverageFactory.writeRectifiedGridCoverage( floodRiskGrid, e_floodRisk );
    e_floodRiskMember.appendChild( e_floodRisk );
    root.appendChild( e_floodRiskMember );

    doc.appendChild( root );
    XmlServiceTools.toFile( floodRiskResultModelGML, doc );
  }

  /**
   * creates an AnnualDamageGrid of an AnnualDamageResultModel
   * 
   * @param annualDamageResultModelGML
   *          Instance location of a AnnualDamageResultModel
   * @param annualDamageResultModelSchema
   *          Schema location of the AnnualDamageResultModel
   * @return AnnualDamageGrid
   * @throws Exception
   */
  public static RectifiedGridCoverage createAnnualDamageGrid( File annualDamageResultModelGML,
      File annualDamageResultModelSchema ) throws Exception
  {
    LogView.println( "Create AnnualDamageGrid..." );
    GMLWorkspace gmlWorkspace = GmlSerializer.createGMLWorkspace( annualDamageResultModelGML
        .toURL(), annualDamageResultModelSchema.toURL() );
    Feature rootFeature = gmlWorkspace.getRootFeature();
    Feature annualDamage = (Feature)rootFeature.getProperty( "AnnualDamageMember" );
    RectifiedGridCoverage annualDamageGrid = RectifiedGridCoverageFactory
        .createRectifiedGridCoverage( annualDamage );
    return annualDamageGrid;
  }

  /**
   * writes the AnnualDamageData to a gml-file
   * 
   * @param annualDamageResultModelGML
   *          Instance location of a AnnualDamageResultModel
   * @param annualDamageGrid
   * @throws Exception
   */
  public static void writeAnnualDamageData( File annualDamageResultModelGML,
      RectifiedGridCoverage annualDamageGrid ) throws Exception
  {
    LogView.println( "Write " + annualDamageResultModelGML.getName() + "..." );
    Document doc = XMLTools.create();

    Element root = doc.createElement( "AnnualDamageResultModel" );
    root.setAttribute( "xmlns", NSHWS );
    root.setAttribute( "xmlns:gml", NSGML );
    root.setAttribute( "xmlns:rgc", NSRGC );

    Element e_annualDamageMember = doc.createElement( "AnnualDamageMember" );
    Element e_annualDamage = doc.createElement( "AnnualDamage" );
    if( annualDamageGrid.getRangeSet().getRangeSetDataFile() == null )
    {
      String rangeSetFileName = annualDamageResultModelGML.getParent()
          + "/annualDamageRangeSetData.dat";
      annualDamageGrid.getRangeSet().setRangeSetDataFile( new File( rangeSetFileName ) );
    }
    RectifiedGridCoverageFactory.writeRectifiedGridCoverage( annualDamageGrid, e_annualDamage );
    e_annualDamageMember.appendChild( e_annualDamage );
    root.appendChild( e_annualDamageMember );

    doc.appendChild( root );
    XmlServiceTools.toFile( annualDamageResultModelGML, doc );
  }

  /**
   * creates a TreeMap of DamageGrids of a DamageResultModel
   * 
   * @param damageResultModelGML
   *          Instance location of DamageResultModel
   * @param damageResultModelSchema
   *          Schema location of DamageResultModel
   * @return TreeMap of damageGrids (key=annuality, value=RectifiedGridCoverage)
   * @throws Exception
   */
  public static TreeMap createDamageGrids( File damageResultModelGML, File damageResultModelSchema )
      throws Exception
  {
    LogView.println( "Create DamageGrids..." );
    TreeMap damageGrids = new TreeMap();
    GMLWorkspace gmlWorkspace = GmlSerializer.createGMLWorkspace( damageResultModelGML.toURL(),
        damageResultModelSchema.toURL() );
    Feature rootFeature = gmlWorkspace.getRootFeature();
    Feature damageFeatureCollection = (Feature)rootFeature.getProperty( "DamageCollectionMember" );
    List damages = (List)damageFeatureCollection.getProperty( "DamageMember" );
    for( int i = 0; i < damages.size(); i++ )
    {
      Feature damage = (Feature)damages.get( i );
      Double annuality = (Double)damage.getProperty( "Annuality" );
      double years = 1 / ( annuality.doubleValue() );
      LogView.println( "HQ " + ( new Double( years ) ).intValue() );
      RectifiedGridCoverage damageGrid = createDamageGrid( damage );
      damageGrids.put( annuality, damageGrid );
    }
    return damageGrids;

  }

  /**
   * creates a DamageGrid of a DamageMember(Feature) of the DamageDataModel
   * 
   * @param damage
   *          DamageMember(Feature) of DamageDataModel
   * @return DamageGrid
   * @throws Exception
   */
  public static RectifiedGridCoverage createDamageGrid( Feature damage ) throws Exception
  {
    RectifiedGridCoverage damageGrid = RectifiedGridCoverageFactory
        .createRectifiedGridCoverage( damage );
    return damageGrid;
  }

  /**
   * writes DamageData to a gml-File
   * 
   * @param damageResultModelGML
   *          Instance location of DamageResultModel
   * @param damageGrids
   * @throws Exception
   */
  public static void writeDamageData( File damageResultModelGML, TreeMap damageGrids )
      throws Exception
  {
    LogView.println( "Write " + damageResultModelGML.getName() + "..." );
    Document doc = XMLTools.create();

    Element root = doc.createElement( "DamageResultModel" );
    root.setAttribute( "xmlns", NSHWS );
    root.setAttribute( "xmlns:gml", NSGML );
    root.setAttribute( "xmlns:rgc", NSRGC );

    Element e_damageCollectionMember = doc.createElement( "DamageCollectionMember" );
    Element e_damageCollection = doc.createElement( "DamageCollection" );
    Iterator it = damageGrids.keySet().iterator();
    while( it.hasNext() )
    {
      Double key = (Double)it.next();
      RectifiedGridCoverage damageGrid = (RectifiedGridCoverage)damageGrids.get( key );
      Element e_damageMember = doc.createElement( "DamageMember" );
      double years = 1 / key.doubleValue();
      LogView.println( "HQ " + ( new Double( years ) ).intValue() );
      if( damageGrid.getRangeSet().getRangeSetDataFile() == null )
      {
        String rangeSetFileName = damageResultModelGML.getParent() + "/damageRangeSetData_HQ"
            + ( new Double( years ) ).intValue() + ".dat";
        damageGrid.getRangeSet().setRangeSetDataFile( new File( rangeSetFileName ) );
      }
      writeDamageGrid( e_damageMember, doc, damageGrid, key );
      e_damageCollection.appendChild( e_damageMember );
    }
    e_damageCollectionMember.appendChild( e_damageCollection );

    root.appendChild( e_damageCollectionMember );

    doc.appendChild( root );
    XmlServiceTools.toFile( damageResultModelGML, doc );
  }

  /**
   * writes the data of a DamageGrid to file
   * 
   * @param e_damageMember
   *          xml-Element, which stores Data of the DamageGrid
   * @param doc
   *          xml-Document, which stores DamageData
   * @param damageGrid
   * @throws Exception
   */
  public static void writeDamageGrid( Element e_damageMember, Document doc,
      RectifiedGridCoverage damageGrid, Double annuality ) throws Exception
  {

    Element e_damage = doc.createElement( "Damage" );
    RectifiedGridCoverageFactory.writeRectifiedGridCoverage( damageGrid, e_damage );
    Element e_annuality = doc.createElement( "Annuality" );
    String stringAnnuality = annuality.toString();
    e_annuality.appendChild( doc.createTextNode( stringAnnuality ) );
    e_damage.appendChild( e_annuality );
    e_damageMember.appendChild( e_damage );
  }

}